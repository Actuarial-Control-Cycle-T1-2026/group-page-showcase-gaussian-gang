###Business Interruption
#Getting Data#
library (readxl)
library(dplyr)
bus_int_f <- read_excel("C:/Users/savan/OneDrive/Desktop/ACTL4001/SOA_2026_Case_Study_Materials/SOA_2026_Case_Study_Materials/srcsc-2026-claims-business-interruption.xlsx", sheet = "freq")

##Data Cleaning#
#Claim Count
min(bus_int_f$claim_count)
#Claim counts should be discrete numbers between 0 and 4.
#Let NA values be zero
bus_int_f$claim_count[is.na(bus_int_f$claim_count)]<- 0
#Make negative values positive (probably a typo)
bus_int_f$claim_count <- ifelse(bus_int_f$claim_count <0, -bus_int_f$claim_count, bus_int_f$claim_count)
max(bus_int_f$claim_count)
#Make all above 4 equal to 4 (enforce a ceiling)
bus_int_f$claim_count <- ifelse(bus_int_f$claim_count >4, 4, bus_int_f$claim_count)
unique(bus_int_f$claim_count)

#Policy ID
#Should be 9 characters long
bus_int_f$policy_id <- substr(bus_int_f$policy_id,1,9)
bus_int_f <- bus_int_f[!is.na(bus_int_f$policy_id),]

#Station ID
#Should be 2 characters long
bus_int_f$station_id <- substr(bus_int_f$station_id, 1, 2)
#Remove NA's
bus_int_f <- bus_int_f[!is.na(bus_int_f$station_id),]

#Solar System
#Make categories correct
bus_int_f$solar_system <- ifelse(substr(bus_int_f$solar_system,1,16) == "Helionis Cluster", "Helionis Cluster",
                                 ifelse(substr(bus_int_f$solar_system,1,7)== "Epsilon", "Epsilon",
                                        ifelse(substr(bus_int_f$solar_system,1,4)=="Zeta", "Zeta",NA)))
#Remove NA's
bus_int_f <- bus_int_f[!is.na(bus_int_f$solar_system),]

#Production load
#Number between 0 and 1
#Make negative values positive (looks like a typo)
bus_int_f$production_load <- ifelse(bus_int_f$production_load <0, -bus_int_f$production_load, bus_int_f$production_load)
min(bus_int_f$production_load)
#Remove NA's
bus_int_f <- bus_int_f[!is.na(bus_int_f$production_load),]
max(bus_int_f$production_load)
#Make all above 1 equal to 1 (enforce a ceiling)
bus_int_f$production_load <- ifelse(bus_int_f$production_load >1, 1, bus_int_f$production_load)

#Energy Backup Score
#Make negative values positive (looks like a typo)
bus_int_f$energy_backup_score <- ifelse(bus_int_f$energy_backup_score <0, -bus_int_f$energy_backup_score, bus_int_f$energy_backup_score)
#Remove values not in correct categories
bus_int_f <- bus_int_f[bus_int_f$energy_backup_score %in% c(1,2,3,4,5),]

#Supply chain index
#Number between 0 and 1
#Make negative values positive (looks like a typo)
bus_int_f$supply_chain_index <- ifelse(bus_int_f$supply_chain_index <0, -bus_int_f$supply_chain_index, bus_int_f$supply_chain_index)
min(bus_int_f$supply_chain_index)
#Remove NA's
bus_int_f <- bus_int_f[!is.na(bus_int_f$supply_chain_index),]
max(bus_int_f$supply_chain_index)
#Make all above 1 equal to 1 (enforce a ceiling)
bus_int_f$supply_chain_index <- ifelse(bus_int_f$supply_chain_index >1, 1, bus_int_f$supply_chain_index)

#Avg crew experience
#Number between 1 and 30
#Make negative values positive (looks like a typo)
bus_int_f$avg_crew_exp <- ifelse(bus_int_f$avg_crew_exp <0, -bus_int_f$avg_crew_exp, bus_int_f$avg_crew_exp)
min(bus_int_f$avg_crew_exp)
#Remove NA's
bus_int_f <- bus_int_f[!is.na(bus_int_f$avg_crew_exp),]
max(bus_int_f$avg_crew_exp)
#Make all above 30 equal to 30 (enforce a ceiling)
bus_int_f$avg_crew_exp <- ifelse(bus_int_f$avg_crew_exp >30, 30, bus_int_f$avg_crew_exp)

#Maintenance Frequency
#Number between 0 and 6
#Make negative values positive (looks like a typo)
bus_int_f$maintenance_freq <- ifelse(bus_int_f$maintenance_freq <0, -bus_int_f$maintenance_freq, bus_int_f$maintenance_freq)
min(bus_int_f$maintenance_freq)
#Remove NA's
bus_int_f <- bus_int_f[!is.na(bus_int_f$maintenance_freq),]
max(bus_int_f$maintenance_freq)
#Make all above 6 equal to 6 (enforce a ceiling)
bus_int_f$maintenance_freq <- ifelse(bus_int_f$maintenance_freq >6, 6, bus_int_f$maintenance_freq)

#Safety Compliance Score
unique(bus_int_f$safety_compliance)
#Make negative values positive (looks like a typo)
bus_int_f$safety_compliance <- ifelse(bus_int_f$safety_compliance <0, -bus_int_f$safety_compliance, bus_int_f$safety_compliance)
#Remove values not in correct categories
bus_int_f <- bus_int_f[bus_int_f$safety_compliance %in% c(1,2,3,4,5),]

#Exposure
#Number between 0 and 1
#Make negative values positive (looks like a typo)
bus_int_f$exposure <- ifelse(bus_int_f$exposure <0, -bus_int_f$exposure, bus_int_f$exposure)
min(bus_int_f$exposure)
#Remove NA's
bus_int_f <- bus_int_f[!is.na(bus_int_f$exposure),]
max(bus_int_f$exposure)
#Make all above 1 equal to 1 (enforce a ceiling)
bus_int_f$exposure <- ifelse(bus_int_f$exposure >1, 1, bus_int_f$exposure)

#Severity data
bus_int_s <- read_excel("C:/Users/savan/OneDrive/Desktop/ACTL4001/SOA_2026_Case_Study_Materials/SOA_2026_Case_Study_Materials/srcsc-2026-claims-business-interruption.xlsx", sheet = "sev")
#Claim amounts should be numeric between around 28,000 and 1,426,000
#Make negative values positive (looks like a typo)
bus_int_s$claim_amount <- ifelse(bus_int_s$claim_amount <0, -bus_int_s$claim_amount, bus_int_s$claim_amount)
min(bus_int_s$claim_amount)
#Remove NA's
bus_int_s <- bus_int_s[!is.na(bus_int_s$claim_amount),]
max(bus_int_s$claim_amount)
#Divide any above 1,426,000 by 100 (looks like typo)
bus_int_s$claim_amount <- ifelse(bus_int_s$claim_amount > 1426000, bus_int_s$claim_amount/100, bus_int_s$claim_amount)

#Policy ID
#Should be 9 characters long
bus_int_s$policy_id <- substr(bus_int_s$policy_id,1,9)
bus_int_s <- bus_int_s[!is.na(bus_int_s$policy_id),]

#Station ID
#Should be 2 characters long
bus_int_s$station_id <- substr(bus_int_s$station_id, 1, 2)
#Remove NA's
bus_int_s <- bus_int_s[!is.na(bus_int_s$station_id),]

#Solar System
unique(bus_int_s$solar_system)
#Make categories correct
bus_int_s$solar_system <- ifelse(substr(bus_int_s$solar_system,1,16) == "Helionis Cluster", "Helionis Cluster",
                                 ifelse(substr(bus_int_s$solar_system,1,7)== "Epsilon", "Epsilon",
                                        ifelse(substr(bus_int_s$solar_system,1,4)=="Zeta", "Zeta",NA)))
#Remove NA's
bus_int_s <- bus_int_s[!is.na(bus_int_s$solar_system),]

#Production load
#Number between 0 and 1
#Make negative values positive (looks like a typo)
bus_int_s$production_load <- ifelse(bus_int_s$production_load <0, -bus_int_s$production_load, bus_int_s$production_load)
min(bus_int_s$production_load)
#Remove NA's
bus_int_s <- bus_int_s[!is.na(bus_int_s$production_load),]
max(bus_int_s$production_load)
#Make all above 1 equal to 1 (enforce a ceiling)
bus_int_s$production_load <- ifelse(bus_int_s$production_load >1, 1, bus_int_s$production_load)

#Energy Backup Score
unique(bus_int_s$energy_backup_score)
#Make negative values positive (looks like a typo)
bus_int_s$energy_backup_score <- ifelse(bus_int_s$energy_backup_score <0, -bus_int_s$energy_backup_score, bus_int_s$energy_backup_score)
#Remove values not in correct categories
bus_int_s <- bus_int_s[bus_int_s$energy_backup_score %in% c(1,2,3,4,5),]

#Safety Compliance Score
unique(bus_int_s$safety_compliance)
#Make negative values positive (looks like a typo)
bus_int_s$safety_compliance <- ifelse(bus_int_s$safety_compliance <0, -bus_int_s$safety_compliance, bus_int_s$safety_compliance)
#Remove values not in correct categories
bus_int_s <- bus_int_s[bus_int_s$safety_compliance %in% c(1,2,3,4,5),]

#Exposure
#Number between 0 and 1
#Make negative values positive (looks like a typo)
bus_int_s$exposure <- ifelse(bus_int_s$exposure <0, -bus_int_s$exposure, bus_int_s$exposure)
min(bus_int_s$exposure)
#Remove NA's
bus_int_s <- bus_int_s[!is.na(bus_int_s$exposure),]
max(bus_int_s$exposure)
#Make all above 1 equal to 1 (enforce a ceiling)
bus_int_s$exposure <- ifelse(bus_int_s$exposure >1, 1, bus_int_s$exposure)

#Combine freq and sev
bus_int <- merge(x=bus_int_s, y =bus_int_f, by = c("policy_id", "station_id"), all.x=TRUE)
bus_int <- bus_int[complete.cases(bus_int),]
sum(bus_int$solar_system.x!=bus_int$solar_system.y)
# No differences
bus_int$solar_system.y <- NULL
bus_int <- rename(bus_int, solar_system = solar_system.x)
sum(bus_int$production_load.x!=bus_int$production_load.y)
#21 differences
prodcheck <- bus_int[(bus_int$production_load.x!=bus_int$production_load.y),]
prodcheck <- data.frame(prodcheck$policy_id, prodcheck$production_load.x, prodcheck$production_load.y)
prodcheck$diff <- prodcheck$prodcheck.production_load.x - prodcheck$prodcheck.production_load.y
bus_int$production_load.y <- NULL
bus_int <- rename(bus_int, production_load = production_load.x)
sum(bus_int$exposure.x!=bus_int$exposure.y)
#21 differences
expcheck <- bus_int[(bus_int$exposure.x!=bus_int$exposure.y),]
expcheck <- data.frame(expcheck$policy_id, expcheck$exposure.x, expcheck$exposure.y)
expcheck$diff <- expcheck$expcheck.exposure.x - expcheck$expcheck.exposure.y
bus_int$exposure.y <- NULL
bus_int <- rename(bus_int, exposure = exposure.x)
sum(bus_int$energy_backup_score.x!=bus_int$energy_backup_score.y)
#0 differences
bus_int$energy_backup_score.y <- NULL
bus_int <- rename(bus_int, energy_backup_score = energy_backup_score.x)
sum(bus_int$safety_compliance.x!=bus_int$safety_compliance.y)
#0 differences
bus_int$safety_compliance.y <- NULL
bus_int <- rename(bus_int, safety_compliance = safety_compliance.x)

sum(bus_int$claim_count==0)
bus_int <- bus_int[(bus_int$claim_count!=0),]

#Frequency
library(ggplot2)
#All claims histograms
ggplot(bus_int_f, aes (x=claim_count)) +
  geom_bar()+
  labs(title = "Histogram of Business Interruption Frequency",
       x= "Claims per Policy",
       y= "Frequency")
ggplot(bus_int, aes (x=claim_count)) +
  geom_bar()+
  labs(title = "Histogram of Business Interruption Frequency",
       x= "Claims per Policy",
       y= "Frequency")


bus_int_fh <- bus_int_f[bus_int_f$solar_system=="Helionis Cluster",]
bus_inth <- bus_int[bus_int$solar_system=="Helionis Cluster",]
bus_int_fe <- bus_int_f[bus_int_f$solar_system=="Epsilon",]
bus_inte<- bus_int[bus_int$solar_system=="Epsilon",]
bus_int_fz <- bus_int_f[bus_int_f$solar_system=="Zeta",]
bus_intz<- bus_int[bus_int$solar_system=="Zeta",]

#Helionis Cluster histograms
ggplot(bus_int_fh, aes (x=claim_count)) +
  geom_bar()+
  labs(title = "Histogram of Business Interruption Frequency in Helionis Cluster",
       x= "Claims per Policy",
       y= "Frequency")
ggplot(bus_inth, aes (x=claim_count)) +
  geom_bar()+
  labs(title = "Histogram of Business Interruption Frequency in Helionis Cluster",
       x= "Claims per Policy",
       y= "Frequency")
#Same shape as all

#Epsilon histograms
ggplot(bus_int_fe, aes (x=claim_count)) +
  geom_bar()+
  labs(title = "Histogram of Business Interruption Frequency in Epsilon",
       x= "Claims per Policy",
       y= "Frequency")
ggplot(bus_inte, aes (x=claim_count)) +
  geom_bar()+
  labs(title = "Histogram of Business Interruption Frequency in Epsilon",
       x= "Claims per Policy",
       y= "Frequency")
#Same shape as all

#Zeta histograms
ggplot(bus_int_fz, aes (x=claim_count)) +
  geom_bar()+
  labs(title = "Histogram of Business Interruption Frequency in Zeta",
       x= "Claims per Policy",
       y= "Frequency")
ggplot(bus_intz, aes (x=claim_count)) +
  geom_bar()+
  labs(title = "Histogram of Business Interruption Frequency in Zeta",
       x= "Claims per Policy",
       y= "Frequency")
#Same shape as all

summary(bus_int_f$claim_count)
#Mean claims: 0.1008
summary(bus_int_fh$claim_count)
#Mean Helionis Cluster claims: 0.09698
summary(bus_int_fe$claim_count)
#Mean Epsilon claims: 0.1025
summary(bus_int_fz$claim_count)
#Mean Zeta claims: 0.101

length(bus_int_f$policy_id)
#98,579 total policies being analysed
length(which(bus_int_f$claim_count ==1))
#4,927 policies made 1 claim
length(which(bus_int_f$claim_count==2))
#1,314 policies made 2 claims
length(which(bus_int_f$claim_count==3))
#509 policies made 3 claims
length(which(bus_int_f$claim_count==4))
#213 policies made 4 claims

(4927 + 1314 + 509 + 213)/98579 *100
#7.06% of policies made claims

#Avg production load when claim was made vs no claims made
mean(bus_int$production_load)
bus_intno <- bus_int_f[bus_int_f$claim_count == 0,]
mean(bus_intno$production_load)
#0.497 vs 0.500

#Avg claims made per energy backup score
mean(bus_int_f$claim_count[bus_int_f$energy_backup_score == 1])
mean(bus_int_f$claim_count[bus_int_f$energy_backup_score == 2])
mean(bus_int_f$claim_count[bus_int_f$energy_backup_score == 3])
mean(bus_int_f$claim_count[bus_int_f$energy_backup_score == 4])
mean(bus_int_f$claim_count[bus_int_f$energy_backup_score == 5])
#1 = 0.099, 2 = 0.094, 3 = 0.099, 4 = 0.105, 5 = 0.106

#Avg supply chain index when claim was made vs no claims made
mean(bus_int$supply_chain_index)
mean(bus_intno$supply_chain_index)
#0.506 vs 0.498

#Avg crew experience when claim was made vs no claims made
mean(bus_int$avg_crew_exp)
mean(bus_intno$avg_crew_exp)
#15.41 vs 15.55

#Avg maintenance freq when claim was made vs no claims made
mean(bus_int$maintenance_freq)
mean(bus_intno$maintenance_freq)
#2.94 vs 3.00

#Avg claims made per compliance score
mean(bus_int_f$claim_count[bus_int_f$safety_compliance == 1])
mean(bus_int_f$claim_count[bus_int_f$safety_compliance == 2])
mean(bus_int_f$claim_count[bus_int_f$safety_compliance == 3])
mean(bus_int_f$claim_count[bus_int_f$safety_compliance == 4])
mean(bus_int_f$claim_count[bus_int_f$safety_compliance == 5])
#1 = 0.100, 2 = 0.102, 3 = 0.096, 4 = 0.103, 5 = 0.103

#Avg exposure when claim was made vs no claims made
mean(bus_int$exposure)
mean(bus_intno$exposure)
#0.522 vs 0.500

#Severity
#All claims
hist(bus_int$claim_amount)
summary(bus_int$claim_amount)
#Median claim: $77,733
#Mean claim: $309,751

#Helionis Cluster claims
hist(bus_inth$claim_amount)
summary(bus_inth$claim_amount)
#Median claim: $85,299
#Mean claim: $312,050

#Epsilon claims
hist(bus_inte$claim_amount)
summary(bus_inte$claim_amount)
#Median claim: $77,700
#Mean claim: $317,745

#Zeta claims
hist(bus_intz$claim_amount)
summary(bus_intz$claim_amount)
#Median claim: $74,954
#Mean claim: $300,609

bus_inta <- bus_int[bus_int$claim_amount > mean(bus_int$claim_amount),]
bus_intb <- bus_int[bus_int$claim_amount <= mean(bus_int$claim_amount),]

#Avg production load when claim was above mean vs below mean
mean(bus_inta$production_load)
mean(bus_intb$production_load)
#0.500 vs 0.496

#Avg claim amount per energy backup score
mean(bus_int$claim_amount[bus_int$energy_backup_score == 1])
mean(bus_int$claim_amount[bus_int$energy_backup_score == 2])
mean(bus_int$claim_amount[bus_int$energy_backup_score == 3])
mean(bus_int$claim_amount[bus_int$energy_backup_score == 4])
mean(bus_int$claim_amount[bus_int$energy_backup_score == 5])
#1 = $302,092.6, 2 = $298,735.9, 3 = $312,431.8, 4 = $329,328.7, 5 = $304,819.1

#Avg claim amount per compliance score
mean(bus_int$claim_amount[bus_int$safety_compliance == 1])
mean(bus_int$claim_amount[bus_int$safety_compliance == 2])
mean(bus_int$claim_amount[bus_int$safety_compliance == 3])
mean(bus_int$claim_amount[bus_int$safety_compliance == 4])
mean(bus_int$claim_amount[bus_int$safety_compliance == 5])
#1 = $306,813.3, 2 = $304,734.4 3 = $311,014.5, 4 = $311,848.1, 5 = $314,434.8

#Avg exposure when claim was above mean vs below mean
mean(bus_inta$exposure)
mean(bus_intb$exposure)
#0.524 vs 0.522