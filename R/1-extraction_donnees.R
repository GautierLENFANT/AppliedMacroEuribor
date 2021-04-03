library(ecb)
library(eurostat)
library(zoo)
library(RJDemetra)
############################### DATA GATHERING & CLEANING ###############################  

#Choose begin date & end date#

begin <- 1999 #Q1
endate <- 2008 #Q4

#EURIBOR_3M#

euri<-get_eurostat(
  "irt_st_q",
  time_format="date",
  filters=list(geo="EA",int_rt="IRT_M3",sinceTimePeriod=paste(begin,"Q1",sep=""))
)
euribor<-ts(euri$values[1:((1+endate-begin)*4)],start=c(str_sub(euri$time[1],1,4),1,1), freq=4)

#GDP#

gdp<-get_eurostat(
  "namq_10_gdp",
  time_format="date",
  filters=list(geo="EA", s_adj="NSA", na_item="B1GQ", unit="CLV10_MEUR",sinceTimePeriod = paste(begin-1,"Q1",sep=""))
)
gdp <- ts(gdp$values[1:((2+endate-begin)*4)],start=c(substr(gdp$time[1],1,4),1), freq=4)
spec_sa <- x13_spec("RSA4c",
                    usrdef.outliersEnabled = TRUE,
                    usrdef.outliersDate = c("2020-01-01",
                                            "2020-04-01",
                                            "2020-07-01",
                                            "2020-10-01"),
                    usrdef.outliersType = rep("AO",4))
x13_gdp <- x13(gdp,spec_sa)
dlgdp <-as.ts(tail(as.zoo(diff(log(x13_gdp$final$series[,"sa"]))),-3))

#Unemployment#

unem<-get_eurostat(
  "une_rt_q",
  time_format="date",
  filters=list(age = "Y15-74", geo="EA19",sex="T",s_adj = "NSA", unit="PC_ACT",sinceTimePeriod = paste(begin,"Q1",sep=""))
)
unemp<-ts(unem$values[1:((1+endate-begin)*4)],start=c(str_sub(unem$time[1],1,4),1,1), freq=4)

#Inflation and underlying inflation (From ECB database)#

hicp <-get_data("ICP.M.U2.N.000000.4.ANR",
                filter = list(startPeriod =paste(begin,"-01",sep=""),endPeriod=paste(endate,"-12",sep=""))
)

infex <-get_data("ICP.M.U2.N.XEF000.4.ANR",
                filter = list(startPeriod =paste(begin,"-01",sep=""),endPeriod=paste(endate,"-12",sep=""))
)
long<-nrow(infex)/3

#Function: Month to Quarter converter for ECB data#

monthly_to_quarterly <- function(month) {
  quarter<-matrix(0,long,1)
  for (v in (1:long)){
    quarter[v] = mean(c(month$obsvalue[3*v],month$obsvalue[3*v-1],month$obsvalue[3*v-2]))
  }
  quarter<-ts(quarter,start=c(str_sub(hicp$obstime[1],1,4),1,1), freq=4)
  return(quarter)
}

hicpq<-monthly_to_quarterly(hicp)
infexq<-monthly_to_quarterly(infex)

data <- ts.union(euribor, dlgdp, unemp, hicpq, infexq)
colnames(data)<-cbind("EURIBOR_3M", "dlGDP","unemployment","inflation","underinf")
saveRDS(data, file="data/data.RDS")
