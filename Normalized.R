#He Guo
library(readxl)
library(tidyverse)
library(RSQLite)
library(DBI)
library(dplyr)
dat.normalized<-read_xlsx("Top MA Donors 2016-2020 copy.xlsx"
                          ,sheet = "Direct Contributions & JFC Dist")
dat.normalized['contrib'] = gsub(dat.normalized$contrib,pattern = ", ",replacement = ",")
dat.normalized['contrib'] = gsub(dat.normalized$contrib,pattern = "\\s\\w*",replacement = "")
unique(dat.normalized$contrib)
#(1NF)
donorInfo<-select(dat.normalized, 
                     contribid, contrib, City, State, Zip,Fecoccemp, fam, orgname, ultorg)
write.csv(donorInfo,'donor Information.csv')
recipients <-select(dat.normalized,
                    recipid, recipient,  party, recipcode, cmteid, date, amount, type)
write.csv(recipients,'recipients.csv')
#Second Normal Form (2NF)
Fecoccemp<-select(dat.normalized, Fecoccemp, Zip,orgname, ultorg)
write.csv(Fecoccemp,'Fecoccemp.csv')
money<-select(dat.normalized, date, amount, type)
money %>% distinct()
write.csv(money,'money.csv')


#Third Normal Form (3NF)
normalized <- dat.normalized[ , -which(names(dat.normalized) 
                                           %in% c("cycle", "fectransid"))]
write.csv(normalized,'normalized.csv')
#Boyce-Codd Normal Form (BCNF or 3.5NF)
CKeys.donorInfo<-select(normalized, contribid, contrib)
CKeys.donorInfo<-unique(CKeys.donorInfo)
write.csv(CKeys.donorInfo,'CKeys.donorInfo.csv')
CKeys.recipients<-select(normalized, recipid, recipient)
CKeys.recipients<-unique(CKeys.recipients)
write.csv(CKeys.recipients,'CKeys.recipients.csv')


#Fourth Normal Form (4NF)
City.4NF<-select(dat.normalized, 
                   contrib, City)
City.4NF<-unique(City.4NF)
write.csv(City.4NF,'City.4NF.csv')

Fecoccemp.4NF<-select(dat.normalized, 
                 contrib, Fecoccemp)
Fecoccemp.4NF<-unique(Fecoccemp.4NF)
write.csv(Fecoccemp.4NF,'Fecoccemp.4NF.csv')

Name.4NF<-select(dat.normalized, contribid, 
                 contrib)

Name.4NF<-unique(Name.4NF)
write.csv(Name.4NF,'Name.4NF.csv')

Party.4NF<-select(dat.normalized, contribid, 
                 party)

Party.4NF<-unique(Party.4NF)
write.csv(Party.4NF,'Party.4NF.csv')

