

library(dplyr)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(plotly)
library(magrittr)



IndMonthlyUnemployment<-read.csv('Weeklyunemploymentclaim.csv')
IndUnemploymentRate<-read.csv('IndustryUR.csv')
GenderUR<-read.csv('UnemploymentGender.csv')
RaceUR<-read.csv('UnemploymentRace.csv')
GenderUnemploymentRate<-read.csv('UnemploymentGender.CSV')
StateAbbr<-read.csv("StateAbbr.csv")

#------------Industry Unemployment Rate--------------------------
colnames(IndUnemploymentRate)[2:11]<-c('Construction','Manufacturing','Wholesale and Retail Trade','Transportation and Utilities',
                                       'Information','Financial Activities','Professional and Business Services','Education and Health Services',
                                       'Leisure and Hospitality','All Industries')
#Replace Industry code with name 

NineNonfarmInd<-tidyr::gather(IndUnemploymentRate,Industry,UR,Construction:`All Industries`)
#Transfer dataset from wide to long format

NineNonfarmInd$Crisis<-ifelse(NineNonfarmInd$DATE<='2010-12-01',
                                   'Subprime mortgage',ifelse(
                                     NineNonfarmInd$DATE>='2020-01-01','Covid-19',NA))
#Add Crisis variable to identify different economic crisis. Subprime mortgage crisis from 2008/1/1 to 2010/12/01, Covid-19 crisis from 2020/01/01 to current.

NineNonfarmIndSubprim<-NineNonfarmInd[which(NineNonfarmInd$Crisis=='Subprime mortgage'),]
#Subset the dataset during the Subprime mortgage crisis

NineNonfarmIndSubprim$Months<-apply(NineNonfarmIndSubprim,1,FUN = function(x) 
  lubridate::interval(as.Date('2008-01-01'),x[1]) %/% months(1))
#Add Months variable indicates months after the start of the recession(After 2008/1/1)

NineNonfarmIndCovid<-NineNonfarmInd[which(NineNonfarmInd$Crisis=='Covid-19'),]
#Subset the dataset during the Covid-19 crisis
NineNonfarmIndCovid$Months<-apply(NineNonfarmIndCovid,1,FUN = function(x)
  lubridate::interval(as.Date('2020-01-01'),x[1]) %/% months(1))
#Add Months variable indicates months after the start of the recession(After 2020/1/1)

NineNonfarmIndTwoCrisis<-rbind(NineNonfarmIndSubprim,NineNonfarmIndCovid)
#Combine the Subprime mortgage and Covid-19 data together

NineNonfarmIndTwoCrisis<-NineNonfarmIndTwoCrisis[-which(NineNonfarmIndTwoCrisis$Industry=='All Industries'),]
#Drop the 'All industry' variable

Industry<-c(unique(NineNonfarmIndTwoCrisis$Industry))
#Create a list for all the industry

#--------------State Unemployment------------------
ALMEUnemployment<-read.csv('fredgraph ALME.csv')
AZFLUnemployment<-read.csv('fredgraph AZFL.csv')
MDNMUnemployment<-read.csv('fredgraph MDNM.csv')
NYTXUnemployment<-read.csv('fredgraph NYTX.csv')
UTWYUnemployment<-read.csv('fredgraph UTWY.csv')
USUnemployment<-read.csv('UNRATE.csv')

USUnemployment$DATE<-as.Date(USUnemployment$DATE,"%m/%d/%Y")
#Change the date variable from chr to date

USUnemployment$DATE<-strftime(USUnemployment$DATE,"%Y-%m-%d")
#Change the format of date variable from "M/D/Y" to "Y-m-d"

StatesUnemployment<-merge(ALMEUnemployment, merge(merge(AZFLUnemployment,MDNMUnemployment,by = 'DATE',all = T),
                          merge(NYTXUnemployment,UTWYUnemployment,by='DATE',all = T),by='DATE',all=T),by='DATE',all = T)
#Merge all the states' unemployment rate data together

StatesUnemployment<-merge(StatesUnemployment,USUnemployment,by='DATE',all=T)
#Merge the states' unemployment rate with national's rate

colnames(StatesUnemployment)[1:53]<-gsub("UR","",names(StatesUnemployment))
#Remove the 'UR' from column names


StatesUnemploymentL<-tidyr::gather(StatesUnemployment,State,UR,AL:U.S.)
#Transfer the dataset from wide format to long format

StatesUnemploymentL$DATE<-as.Date(StatesUnemploymentL$DATE,"%Y-%m-%d")
#Change the date variable from chr to date.

StatesUnemploymentL<-merge(StatesUnemploymentL,StateAbbr[,c(1,3)],by.x = 'State',by.y = 'Code',all.x = T)
#Merge the state abbreviation to the dataset

colnames(StatesUnemploymentL)[c(1,4)]<-c('ST','State')
StatesUnemploymentL[is.na(StatesUnemploymentL)]<-'U.S.'

StatesUnemploymentL$Crisis<-ifelse(StatesUnemploymentL$DATE<='2010-12-01',
                                   'Subprime mortgage',ifelse(
                                     StatesUnemploymentL$DATE>='2020-01-01','Covid-19',NA))
#Add Crisis variable to identify different economic crisis. Subprime mortgage crisis from 2008/1/1 to 2010/12/01, Covid-19 crisis from 2020/01/01 to current.

StatesUnemploymentLSubPrim<-StatesUnemploymentL[which(StatesUnemploymentL$Crisis=='Subprime mortgage'),]
#Subset dataset for only during the Subprime mortgage crisis

StatesUnemploymentLSubPrim$Months<-apply(StatesUnemploymentLSubPrim,1,FUN = function(x) 
  lubridate::interval(as.Date('2008-01-01'),x[2]) %/% months(1))
#Add Months variable indicates months after the start of the recession(After 2008/1/1)

StatesUnemploymentCovid<-StatesUnemploymentL[which(StatesUnemploymentL$Crisis=='Covid-19'),]
#Subset dataset for only during the Covid-19 crisis

StatesUnemploymentCovid$Months<-apply(StatesUnemploymentCovid,1,FUN = function(x)
  lubridate::interval(as.Date('2020-01-01'),x[2]) %/% months(1))
#Add Months variable indicates months after the start of the recession(After 2020/1/1)

StatesUnemploymentTwoCrisis<-rbind(StatesUnemploymentLSubPrim,StatesUnemploymentCovid)
#Combine the Subprime mortgage and Covid-19 data together


#------------- Gender Unemployment Rate-----------------------------

GenderUR$DATE<-as.Date(GenderUR$DATE,'%m/%d/%Y')
#Change the date variable from chr to date

GenderUR$Crisis<-ifelse(GenderUR$DATE<='2010-12-01',
                                      'Subprime mortgage',ifelse(
                                        GenderUR$DATE>='2020-01-01','Covid-19',NA))
#Add Crisis variable to identify different economic crisis. Subprime mortgage crisis from 2008/1/1 to 2010/12/01, Covid-19 crisis from 2020/01/01 to current.


GenderURSubPrim<-
  GenderUR[which(GenderUR$Crisis=='Subprime mortgage'),]
#Subset dataset for only during the Subprime mortgage crisis

GenderURCovid<-
  GenderUR[which(GenderUR$Crisis=='Covid-19'),]
#Subset dataset for only during the Covid-19 crisis

GenderURSubPrim$Months<-apply(GenderURSubPrim,1,FUN = function(x)
  lubridate::interval(as.Date('2008-01-01'),x[1]) %/% months(1))
#Add Months variable indicates months after the start of the recession(After 2008/1/1)

GenderURCovid$Months<-apply(GenderURCovid,1,FUN = function(x)
  lubridate::interval(as.Date('2020-01-01'),x[1]) %/% months(1))
#Add Months variable indicates months after the start of the recession(After 2020/1/1)

GenderURSubPrimL<-tidyr::gather(GenderURSubPrim,Gender,UR,Men:Women)
GenderURCovidL<-tidyr::gather(GenderURCovid,Gender,UR,Men:Women)
#Transfer data from wide format to long format

GenderURTwoCrisis<-rbind(GenderURSubPrimL,GenderURCovidL)
#Combine the Subprime mortgage and Covid-19 data together


#------------ Racial Unemployment Rate----------------------------------------

RaceUR$DATE<-as.Date(RaceUR$DATE, '%m/%d/%Y')
#Change the date variable from chr to date

RaceUR$Crisis<-ifelse(RaceUR$DATE<='2010-12-01',
                                      'Subprime mortgage',ifelse(
                                        RaceUR$DATE>='2020-01-01','Covid-19',NA))
#Add Crisis variable to identify different economic crisis. Subprime mortgage crisis from 2008/1/1 to 2010/12/01, Covid-19 crisis from 2020/01/01 to current.


colnames(RaceUR)<-gsub("[[:punct:]]"," ",colnames(RaceUR))
#Remove "." from the column name

RaceURSubPrim<-RaceUR[which(RaceUR$Crisis=='Subprime mortgage'),]
#Subset dataset for only during the Subprime mortgage crisis

RaceURCovid<-RaceUR[which(RaceUR$Crisis=='Covid-19'),]
#Subset dataset for only during the Covid-19 crisis


RaceURSubPrim$Months<-apply(RaceURSubPrim,1,FUN = function(x)
  lubridate::interval(as.Date('2008-01-01'),x[1]) %/% months(1))
#Add Months variable indicates months after the start of the recession(After 2008/1/1)


RaceURCovid$Months<-apply(RaceURCovid,1,FUN = function(x)
  lubridate::interval(as.Date('2020-01-01'),x[1]) %/% months(1))
#Add Months variable indicates months after the start of the recession(After 2020/1/1)


RaceURSubPrimL<-tidyr::gather(RaceURSubPrim,Race,UR,`African American`:`Hispanic or Latino`)
RaceURSubCovidL<-tidyr::gather(RaceURCovid,Race,UR,`African American`:`Hispanic or Latino`)
#Transfer data from wide format to long format

RaceURTwoCrisis<-rbind(RaceURSubPrimL,RaceURSubCovidL)
#Combine the Subprime mortgage and Covid-19 data together



write.csv(NineNonfarmIndTwoCrisis,'NineNonfarmIndTwoCrisis.csv')
write.csv(RaceURTwoCrisis,'RaceURTwoCrisis.csv')
write.csv(GenderURTwoCrisis,'GenderURTwoCrisis.csv')
write.csv(StatesUnemploymentTwoCrisis,'StatesUnemploymentTwoCrisis.csv')
