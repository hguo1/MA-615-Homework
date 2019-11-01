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
dat.normalized$contrib[dat.normalized$contrib=="SABAN,HAIM,,"]<-"SABAN,HAIM"
dat.normalized$contrib[dat.normalized$contrib=="FISH,JOHN,,,"]<-"FISH,JOHN"
dat.normalized$contrib[dat.normalized$contrib=="RASKY,LAWRENCE,"]<-"RASKY,LAWRENCE"
dat.normalized$contrib[dat.normalized$contrib=="EGERMAN,PAUL&"]<-"EGERMAN,PAUL"
dat.normalized$contrib[dat.normalized$contrib=="FISH,JOHN,F"]<-"FISH,JOHN"


#(1NF)
donorInfo<-select(dat.normalized, 
                     contribid, contrib, City, State, Zip,Fecoccemp, fam, orgname, ultorg)
donorInfo<-donorInfo %>% distinct()

recipients <-select(dat.normalized,
                    recipid, recipient,  party, recipcode, cmteid, date, amount, type)
recipients<-recipients %>% distinct()

#Second Normal Form (2NF)
Fecoccemp<-select(donorInfo, Fecoccemp, Zip,orgname, ultorg)
Fecoccemp %>% distinct()
money<-select(recipients, date, amount, type)
money %>% distinct()



#Third Normal Form (3NF)
normalized <- dat.normalized[ , -which(names(dat.normalized) 
                                           %in% c("cycle", "fectransid"))]

#Boyce-Codd Normal Form (BCNF or 3.5NF)
CKeys.donorInfo<-select(donorInfo, contribid, contrib)
CKeys.donorInfo<-unique(CKeys.donorInfo)

CKeys.recipients<-select(recipients, recipid, recipient)
CKeys.recipients<-unique(CKeys.recipients)



#Fourth Normal Form (4NF)
City.4NF<-select(donorInfo, 
                   contrib, City)
City.4NF<-unique(City.4NF)


Fecoccemp.4NF<-select(donorInfo, 
                 contrib, Fecoccemp)
Fecoccemp.4NF<-unique(Fecoccemp.4NF)


Name.4NF<-select(donorInfo, contribid, 
                 contrib)
Name.4NF<-unique(Name.4NF)


Party.4NF<-select(recipients, recipid, 
                 party)

Party.4NF<-unique(Party.4NF)


Top_Donor<-dbConnect(SQLite(),"Top_Donor_ma")
dbWriteTable(Top_Donor, "Top MA Donor",dat.normalized)
dbWriteTable(Top_Donor, "Donor Information",donorInfo)
dbWriteTable(Top_Donor, "Recipients",recipients)
dbWriteTable(Top_Donor, "Donation",money)
dbWriteTable(Top_Donor, "Fecoccemp",Fecoccemp)
dbWriteTable(Top_Donor, "3NF",normalized)
dbWriteTable(Top_Donor, "CandidateKeys.donorInfo",CKeys.donorInfo)
dbWriteTable(Top_Donor, "CandidateKeys.recipients",CKeys.recipients)
dbWriteTable(Top_Donor, "City.4NF",City.4NF)
dbWriteTable(Top_Donor, "Fecoccemp.4NF",Fecoccemp.4NF)
dbWriteTable(Top_Donor, "Name.4NF",Name.4NF)
dbWriteTable(Top_Donor, "Party.4NF",Party.4NF)
