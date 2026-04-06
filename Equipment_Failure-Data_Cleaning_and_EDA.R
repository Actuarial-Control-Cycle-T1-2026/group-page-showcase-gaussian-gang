##Equipment Failure##
#Getting Data#
library (readxl)
library(dplyr)
equipment_failure_f <- read_excel("C:/Users/savan/OneDrive/Desktop/ACTL4001/SOA_2026_Case_Study_Materials/SOA_2026_Case_Study_Materials/srcsc-2026-claims-equipment-failure.xlsx", sheet = "freq")

##Data Cleaning#
#Claim Count
min(equipment_failure_f$claim_count)
#Claim counts should be discrete numbers between 0 and 3.
#Let NA values be zero
equipment_failure_f$claim_count[is.na(equipment_failure_f$claim_count)]<- 0
#Make negative values positive (as they seem to be a typo)
equipment_failure_f$claim_count <- ifelse(equipment_failure_f$claim_count <0, -equipment_failure_f$claim_count, equipment_failure_f$claim_count)
max(equipment_failure_f$claim_count)
#Make all above 3 equal to 3 (enforce a ceiling)
equipment_failure_f$claim_count <- ifelse(equipment_failure_f$claim_count >3, 3, equipment_failure_f$claim_count)
unique(equipment_failure_f$claim_count)

#Policy ID
#Should be 9 characters
equipment_failure_f$policy_id <- substr(equipment_failure_f$policy_id,1,9)
equipment_failure_f <- equipment_failure_f[!is.na(equipment_failure_f$policy_id),]

#Equipment ID
#Should be 9 characters
equipment_failure_f$equipment_id <- substr(equipment_failure_f$equipment_id,1,9)
equipment_failure_f <- equipment_failure_f[!is.na(equipment_failure_f$equipment_id),]

#Equipment Type
#Make categories correct
equipment_failure_f$equipment_type <- ifelse(substr(equipment_failure_f$equipment_type,1,15)=="ReglAggregators", "ReglAggregators",
                                             ifelse(substr(equipment_failure_f$equipment_type,1,10)== "Flux Rider", "Flux Rider",
                                                    ifelse(substr(equipment_failure_f$equipment_type,1,18)== "Graviton Extractor", "Graviton Extractor",
                                                           ifelse(substr(equipment_failure_f$equipment_type,1,12)=="Quantum Bore", "Quantum Bore",
                                                                  ifelse(substr(equipment_failure_f$equipment_type,1,14)=="Ion Pulverizer", "Ion Pulverizer",
                                                                         ifelse(substr(equipment_failure_f$equipment_type, 1, 16)== "FexStram Carrier", "FexStram Carrier", NA))))))
equipment_failure_f <- equipment_failure_f[!is.na(equipment_failure_f$equipment_type),]

#Equipment Age
#Should be numeric between 0 and 10
#Make negative values positive (looks like a typo)
equipment_failure_f$equipment_age <- ifelse(equipment_failure_f$equipment_age <0, -equipment_failure_f$equipment_age, equipment_failure_f$equipment_age)
#Make all above 10 equal to 10 (enforce a ceiling)
equipment_failure_f$equipment_age <- ifelse(equipment_failure_f$equipment_age >10, 10, equipment_failure_f$equipment_age)
#Remove NA's
equipment_failure_f <- equipment_failure_f[!is.na(equipment_failure_f$equipment_age),]
min(equipment_failure_f$equipment_age)
max(equipment_failure_f$equipment_age)

#Solar System
#Make categories correct
equipment_failure_f$solar_system <- ifelse(substr(equipment_failure_f$solar_system, 1, 16)== "Helionis Cluster", "Helionis Cluster",
                                           ifelse(substr(equipment_failure_f$solar_system,1,7)== "Epsilon", "Epsilon",
                                                  ifelse(substr(equipment_failure_f$solar_system,1,4)=="Zeta", "Zeta", NA)))
equipment_failure_f <- equipment_failure_f[!is.na(equipment_failure_f$solar_system),]

#Maintenance Int
#Should be numeric between 100 and 5000
#Make negative values positive (looks like a typo)
equipment_failure_f$maintenance_int <- ifelse(equipment_failure_f$maintenance_int <0, -equipment_failure_f$maintenance_int, equipment_failure_f$maintenance_int)
#Remove NA's
equipment_failure_f <- equipment_failure_f[!is.na(equipment_failure_f$maintenance_int),]
min(equipment_failure_f$maintenance_int)
#Make all above 5000 equal to 5000 (enforce a ceiling)
equipment_failure_f$maintenance_int <- ifelse(equipment_failure_f$maintenance_int >5000, 5000, equipment_failure_f$maintenance_int)
max(equipment_failure_f$maintenance_int)

#Usage intensity
#Should be numeric between 0 and 24
#Make negative values positive (looks like a typo)
equipment_failure_f$usage_int <- ifelse(equipment_failure_f$usage_int <0, -equipment_failure_f$usage_int, equipment_failure_f$usage_int)
min(equipment_failure_f$usage_int)
#Remove NA's
equipment_failure_f <- equipment_failure_f[!is.na(equipment_failure_f$usage_int),]
max(equipment_failure_f$usage_int)
#Make all above 24 equal to 24 (enforce a ceiling)
equipment_failure_f$usage_int <- ifelse(equipment_failure_f$usage_int >24, 24, equipment_failure_f$usage_int)

#Exposure
#Should be ratio between 0 and 1
#Make negative values positive (looks like a typo)
equipment_failure_f$exposure <- ifelse(equipment_failure_f$exposure <0, -equipment_failure_f$exposure, equipment_failure_f$exposure)
min(equipment_failure_f$exposure)
#Remove NA's
equipment_failure_f <- equipment_failure_f[!is.na(equipment_failure_f$exposure),]
max(equipment_failure_f$exposure)
#Make all above 1 equal to 1 (enforce a ceiling)
equipment_failure_f$exposure <- ifelse(equipment_failure_f$exposure >1, 1, equipment_failure_f$exposure)

#Severity data
equipment_failure_s <- read_excel("C:/Users/savan/OneDrive/Desktop/ACTL4001/SOA_2026_Case_Study_Materials/SOA_2026_Case_Study_Materials/srcsc-2026-claims-equipment-failure.xlsx", sheet = "sev")
#Claim amounts should be numeric between around 11,000 and 790,000
#Make negative values positive (looks like a typo)
equipment_failure_s$claim_amount <- ifelse(equipment_failure_s$claim_amount <0, -equipment_failure_s$claim_amount, equipment_failure_s$claim_amount)
min(equipment_failure_s$claim_amount)
#Slightly low, but still around 11,000
#Remove NA's
equipment_failure_s <- equipment_failure_s[!is.na(equipment_failure_s$claim_amount),]
max(equipment_failure_s$claim_amount)
#Make any above 790,000 equal to 790,000 (enforce a ceiling)
equipment_failure_s$claim_amount <- ifelse(equipment_failure_s$claim_amount >790000, 790000, equipment_failure_s$claim_amount)

#Claim Seq
#Should be between 1 and 3
min(equipment_failure_s$claim_seq)
equipment_failure_s$claim_seq <- ifelse(equipment_failure_s$claim_seq <0, -equipment_failure_s$claim_seq, equipment_failure_s$claim_seq)
unique(equipment_failure_s$claim_seq)
equipment_failure_s <- equipment_failure_s[equipment_failure_s$claim_seq %in% c(1,2,3),]

#Policy ID
#Should be 9 characters
equipment_failure_s$policy_id <- substr(equipment_failure_s$policy_id,1,9)
equipment_failure_s <- equipment_failure_s[!is.na(equipment_failure_s$policy_id),]

#Equipment ID
#Should be 9 characters
equipment_failure_s$equipment_id <- substr(equipment_failure_s$equipment_id,1,9)
equipment_failure_s <- equipment_failure_s[!is.na(equipment_failure_s$equipment_id),]

#Equipment Type
#Make categories correct
equipment_failure_s$equipment_type <- ifelse(substr(equipment_failure_s$equipment_type,1,15)=="ReglAggregators", "ReglAggregators",
                                             ifelse(substr(equipment_failure_s$equipment_type,1,10)== "Flux Rider", "Flux Rider",
                                                    ifelse(substr(equipment_failure_s$equipment_type,1,18)== "Graviton Extractor", "Graviton Extractor",
                                                           ifelse(substr(equipment_failure_s$equipment_type,1,12)=="Quantum Bore", "Quantum Bore",
                                                                  ifelse(substr(equipment_failure_s$equipment_type,1,14)=="Ion Pulverizer", "Ion Pulverizer",
                                                                         ifelse(substr(equipment_failure_s$equipment_type, 1, 16)== "FexStram Carrier", "FexStram Carrier", NA))))))
equipment_failure_s <- equipment_failure_s[!is.na(equipment_failure_s$equipment_type),]

#Equipment age
#Should be numeric between 0 and 10
#Make negative values positive (looks like a typo)
equipment_failure_s$equipment_age <- ifelse(equipment_failure_s$equipment_age <0, -equipment_failure_s$equipment_age, equipment_failure_s$equipment_age)
min(equipment_failure_s$equipment_age)
#Remove NA's
equipment_failure_s <- equipment_failure_s[!is.na(equipment_failure_s$equipment_age),]
max(equipment_failure_s$equipment_age)
#Make all above 10 equal to 10 (enforce a ceiling)
equipment_failure_s$equipment_age <- ifelse(equipment_failure_s$equipment_age >10, 10, equipment_failure_s$equipment_age)

#Maintenance Int
#Should be numeric between 100 and 5000
#Make negative values positive (looks like a typo)
equipment_failure_s$maintenance_int <- ifelse(equipment_failure_s$maintenance_int <0, -equipment_failure_s$maintenance_int, equipment_failure_s$maintenance_int)
#Remove NA's
equipment_failure_s <- equipment_failure_s[!is.na(equipment_failure_s$maintenance_int),]
min(equipment_failure_s$maintenance_int)
max(equipment_failure_s$maintenance_int)

#Usage intensity
#Should be numeric between 0 and 24
#Make negative values positive (looks like a typo)
equipment_failure_s$usage_int <- ifelse(equipment_failure_s$usage_int <0, -equipment_failure_s$usage_int, equipment_failure_s$usage_int)
min(equipment_failure_s$usage_int)
#Remove NA's
equipment_failure_s <- equipment_failure_s[!is.na(equipment_failure_s$usage_int),]
max(equipment_failure_s$usage_int)
#Make all above 24 equal to 24 (enforce a ceiling)
equipment_failure_s$usage_int <- ifelse(equipment_failure_s$usage_int >24, 24, equipment_failure_s$usage_int)

#Exposure
#Should be ratio between 0 and 1
#Make negative values positive (looks like a typo)
equipment_failure_s$exposure <- ifelse(equipment_failure_s$exposure <0, -equipment_failure_s$exposure, equipment_failure_s$exposure)
min(equipment_failure_s$exposure)
#Remove NA's
equipment_failure_s <- equipment_failure_s[!is.na(equipment_failure_s$exposure),]
max(equipment_failure_s$exposure)
#Make all above 1 equal to 1 (enforce a ceiling)
equipment_failure_s$exposure <- ifelse(equipment_failure_s$exposure >1, 1, equipment_failure_s$exposure)

dups<- equipment_failure_s$policy_id[duplicated(equipment_failure_s$policy_id)]
dupcheck <- equipment_failure_s[equipment_failure_s$policy_id %in% dups,]
exactdup<- dupcheck[duplicated(dupcheck),]
exactdups <- dupcheck[duplicated(dupcheck[,c("policy_id","equipment_id")]),]
#Although there are no exact duplicates, there are >2,000 using the same combinations of policy id and equipment id
exactdups2 <- dupcheck[duplicated(dupcheck[,c("policy_id", "equipment_id", "claim_seq", "claim_id")]),]
#There are duplicates within claim severity of the policy id and equipment id with different claim ids, causing duplicates of claims
#Note that the later claim ids usually have a similar claim amount to the first, but always have a larger claim amount than the first
#Looks like a claim has been closed then reopened?

equipment_failure_s <- equipment_failure_s %>%
  group_by(policy_id, equipment_id) %>%
  filter(claim_id==max(claim_id)) %>%
  ungroup()

dupsb <- equipment_failure_s$policy_id[duplicated(equipment_failure_s$policy_id)]
dupcheckb <- equipment_failure_s[equipment_failure_s$policy_id %in% dupsb,]
exactdupb <- dupcheckb[duplicated(dupcheckb),]
exactdupsb <- dupcheckb[duplicated(dupcheckb[,c("policy_id", "equipment_id")]),]
#No more exact duplicates

#Combine freq and sev
equipment_failure <- merge(x=equipment_failure_s, y = equipment_failure_s, by = c("policy_id", "equipment_id", "claim_id"), all.x=TRUE)
equipment_failure <- equipment_failure[complete.cases(equipment_failure),]
equipment_failure <- rename(equipment_failure, claim_count = claim_seq.y)
equipment_failure <- rename(equipment_failure, claim_seq = claim_seq.x)

sum(equipment_failure$claim_amount.x!=equipment_failure$claim_amount.y)
amountcheck <- equipment_failure[(equipment_failure$claim_amount.x!=equipment_failure$claim_amount.y),]
amountcheck <-data.frame(amountcheck$policy_id, amountcheck$claim_amount.x, amountcheck$claim_amount.y)
amountcheck$diff <- amountcheck$amountcheck.claim_amount.x-amountcheck$amountcheck.claim_amount.y
#They have been swapped around
equipment_failure$claim_amount.y <- NULL
equipment_failure <- rename(equipment_failure, claim_amount = claim_amount.x)

dupsc<- equipment_failure$policy_id[duplicated(equipment_failure$policy_id)]
dupcheckc <- equipment_failure[equipment_failure$policy_id %in% dupsc,]
exactdupc<- dupcheckc[duplicated(dupcheckc),]
exactdupsc <- dupcheckc[duplicated(dupcheckc[,c("policy_id","equipment_id")]),]
equipment_failure <- distinct(equipment_failure)

sum(equipment_failure$equipment_type.x!= equipment_failure$equipment_type.y)
#No differences
equipment_failure$equipment_type.y <- NULL
equipment_failure <- rename(equipment_failure, equipment_type = equipment_type.x)

sum(equipment_failure$equipment_age.x != equipment_failure$equipment_age.y)
#No differences
equipment_failure$equipment_age.y <- NULL
equipment_failure <- rename(equipment_failure, equipment_age = equipment_age.x)

sum(equipment_failure$solar_system.x!=equipment_failure$solar_system.y)
#No differences
equipment_failure$solar_system.y <- NULL
equipment_failure <- rename(equipment_failure, solar_system = solar_system.x)

sum(equipment_failure$maintenance_int.x != equipment_failure$maintenance_int.y)
#2 differences
equipment_failure[(equipment_failure$maintenance_int.x != equipment_failure$maintenance_int.y),]
#Claim seq swapped the maintenance int's
equipment_failure$maintenance_int.y <- NULL
equipment_failure <- rename(equipment_failure, maintenance_int = maintenance_int.x)

sum(equipment_failure$usage_int.x != equipment_failure$usage_int.y)
#No differences
equipment_failure$usage_int.y <- NULL
equipment_failure <- rename(equipment_failure, usage_int = usage_int.x)

sum(equipment_failure$exposure.x != equipment_failure$exposure.y)
#2 differences
equipment_failure[(equipment_failure$exposure.x!=equipment_failure$exposure.y),]
#Claim seq swapped the exposures
equipment_failure$exposure.y <- NULL
equipment_failure <- rename(equipment_failure, exposure = exposure.x)

#Frequency
library(ggplot2)
ggplot(equipment_failure_f, aes (x=claim_count)) +
  geom_bar()+
  labs(title = "Histogram of Equipment Failure Frequency",
       x= "Claims per Policy",
       y= "Frequency")
ggplot(equipment_failure, aes (x=claim_count)) +
  geom_bar()+
  labs(title = "Histogram of Equipment Failure Frequency",
       x= "Claims per Policy",
       y= "Frequency")

equipment_failure_fh <- equipment_failure_f[equipment_failure_f$solar_system=="Helionis Cluster",]
equipment_failureh <- equipment_failure[equipment_failure$solar_system=="Helionis Cluster",]
equipment_failure_fe <- equipment_failure_f[equipment_failure_f$solar_system=="Epsilon",]
equipment_failuree <- equipment_failure[equipment_failure$solar_system=="Epsilon",]
equipment_failure_fz <- equipment_failure_f[equipment_failure_f$solar_system=="Zeta",]
equipment_failurez <- equipment_failure[equipment_failure$solar_system=="Zeta",]

#Helionis Cluster Histograms
ggplot(equipment_failure_fh, aes(x=claim_count))+
  geom_bar()+
  labs(title = "Histogram of Equipment Failure Frequency in Helionis Cluster", x = "Claims per Policy", y = "Frequency")
ggplot(equipment_failureh, aes(x=claim_count))+
  geom_bar()+
  labs(title = "Histogram of Equipment Failure Frequency in Helionis Cluster", x ="Claims per Policy", y = "Frequency")
#Same shape as all

#Epsilon Histograms
ggplot(equipment_failure_fe, aes(x=claim_count))+
  geom_bar()+
  labs(title = "Histogram of Equipment Failure Frequency in Epsilon", x = "Claims per Policy", y = "Frequency")
ggplot(equipment_failuree, aes(x=claim_count))+
  geom_bar()+
  labs(title = "Histogram of Equipment Failure Frequency in Epsilon", x ="Claims per Policy", y = "Frequency")
#Same shape as all

#Zeta Histograms
ggplot(equipment_failure_fz, aes(x=claim_count))+
  geom_bar()+
  labs(title = "Histogram of Equipment Failure Frequency in Zeta", x = "Claims per Policy", y = "Frequency")
ggplot(equipment_failurez, aes(x=claim_count))+
  geom_bar()+
  labs(title = "Histogram of Equipment Failure Frequency in Zeta", x ="Claims per Policy", y = "Frequency")
#Same shape as all

summary(equipment_failure_f$claim_count)
#Mean claims: 0.07836
summary(equipment_failure_fh$claim_count)
#Mean Helionis Cluster claims: 0.108
summary(equipment_failure_fe$claim_count)
#Mean Epsilon claims: 0.0739
summary(equipment_failure_fz$claim_count)
#Mean claim: 0.06789

length(equipment_failure_f$policy_id)
#93,594 total policies being analysed
length(which(equipment_failure_f$claim_count ==1))
#6,512 policies made 1 claim
length(which(equipment_failure_f$claim_count==2))
#354 policies made 2 claims
length(which(equipment_failure_f$claim_count==3))
#38 policies made 3 claims

(6512 + 354 +38)/93594 *100
#7.38% of policies made claims

#Avg equipment age when claim was made vs no claims made
#Claims made:
mean(equipment_failure$equipment_age)
equipment_failureno <- equipment_failure_f[equipment_failure_f$claim_count == 0,]
mean(equipment_failureno$equipment_age)
#8.73 vs 8.16

#Avg time between maintenance cycles when claim was made vs no claims made
mean(equipment_failure$maintenance_int)
mean(equipment_failureno$maintenance_int)
#1020.16 vs 953.25

#Avg operation time per day when claim was made vs no claims made
mean(equipment_failure$usage_int)
mean(equipment_failureno$usage_int)
#14.10 vs 11.88

#Avg period of exposure when claim was made vs no claims made
mean(equipment_failure$exposure)
mean(equipment_failureno$exposure)
#0.602 vs 0.493

#Severity
#All claims
hist(equipment_failure$claim_amount)
summary(equipment_failure$claim_amount)
#Median claim: $72,757
#Mean claim: $87,350

#Helionis Cluster claims
hist(equipment_failureh$claim_amount)
summary(equipment_failureh$claim_amount)
#Median claim: $65,166
#Mean claim: $77,817

#Epsilon claims
hist(equipment_failuree$claim_amount)
summary(equipment_failuree$claim_amount)
#Median claim: $72,423
#Mean claim: $86,015

#Zeta claims
hist(equipment_failurez$claim_amount)
summary(equipment_failurez$claim_amount)
#Median claim: $80,331
#Mean claim: $96,893

#Avg equipment age when claim was above mean vs below mean
equipment_failurea <- equipment_failure[equipment_failure$claim_amount > mean(equipment_failure$claim_amount),]
equipment_failureb <- equipment_failure[equipment_failure$claim_amount <= mean(equipment_failure$claim_amount),]
mean(equipment_failurea$equipment_age)
mean(equipment_failureb$equipment_age)
#8.89 vs 8.64

#Avg time between maintenance cycles when claim was above mean vs below mean
mean(equipment_failurea$maintenance_int)
mean(equipment_failureb$maintenance_int)
#1008.18 vs 1027.42

#Avg operation time per day when claim was above mean vs below mean
mean(equipment_failurea$usage_int)
mean(equipment_failureb$usage_int)
#15.72 vs 13.13

#Avg period of exposure when claim was made vs no claims made
mean(equipment_failurea$exposure)
mean(equipment_failureb$exposure)
#0.606 vs 0.600

#Checking if there is a value for equipment types
summary(equipment_failure$claim_amount[equipment_failure$equipment_type=="Graviton Extractor"])
#Graviton Extractors range from 8,991 to 790,000 with a mean of 76,619
summary(equipment_failure$claim_amount[equipment_failure$equipment_type=="ReglAggregators"])
#ReglAggregators range from 8,416 to 790,000 with mean of 85,476
summary(equipment_failure$claim_amount[equipment_failure$equipment_type=="Flux Rider"])
#Flux Riders range from 9,621 to 790,000 with mean of 61,367
summary(equipment_failure$claim_amount[equipment_failure$equipment_type=="Ion Pulverizer"])
#Ion Pulverizers range from 12,035 to 790,000 with a mean of 86,012
summary(equipment_failure$claim_amount[equipment_failure$equipment_type=="Quantum Bore"])
#Quantum Bores range from 20,627 to 790,000 with a mean of 127,708
summary(equipment_failure$claim_amount[equipment_failure$equipment_type=="FexStram Carrier"])
#FexStram Carriers range from 9,183 to 761,418 with mean of 53,730
#Therefore although some are slightly more expense (Quantum Bores) and some are slightly cheaper (FexStram Carriers)
#There is no set value for the equipment and there is a large range for each equipment type
