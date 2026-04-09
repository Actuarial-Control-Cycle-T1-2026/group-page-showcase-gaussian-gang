# Purpose: Clean workers compensation dataset ---------------------------------

# Initialise workspace ---------------------------------------------------------

library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(bbmle)
library(stats)
library(goftest)
library(actuar)
library(fitdistrplus)
library(boot)

# Import dataset --------------------------------------------------------------

setwd('/Users/sophievonwiller/Library/CloudStorage/OneDrive-UNSW/ACTL4001 Assignment/SOA_2026_Case_Study_Materials/Claims data')

workers_comp_freq <- read_xlsx("srcsc-2026-claims-workers-comp.xlsx", "freq")
workers_comp_sev <- read_xlsx("srcsc-2026-claims-workers-comp.xlsx", "sev")

# Number of rows in each dataset pre-cleaning 
nrow(workers_comp_freq) #134947
nrow(workers_comp_sev) #1917

# Summary of datasets 
summary(workers_comp_freq)
summary(workers_comp_sev)

# Clean workers compensation frequency dataset --------------------------------

# Remove any additional characters and numbers from columns 
frequency_edit <- c("policy_id",
                    "worker_id",
                    "solar_system",
                    "station_id",
                    "occupation",
                    "employment_type")

workers_comp_freq <- workers_comp_freq %>%
  mutate(across(all_of(frequency_edit),
                ~ sub("_[^_]*$", "", .)
  ))

# Take absolute value of numeric columns 
frequency_abs <- c("experience_yrs",
                   "accident_history_flag",
                   "psych_stress_index",
                   "hours_per_week",
                   "supervision_level",
                   "gravity_level",
                   "safety_training_index",
                   "protective_gear_quality",
                   "base_salary",
                   "exposure")

workers_comp_freq <- workers_comp_freq %>%
  mutate(across(all_of(frequency_abs),
                ~ abs(.)))

# Claim count: set NA's to 0 & enforce claim ceiling of 2 
workers_comp_freq$claim_count[is.na(workers_comp_freq$claim_count)] <- 0 
workers_comp_freq$claim_count <- pmax(pmin(workers_comp_freq$claim_count, 2), 0)


# Experience years: enforce value range {0.2 - 40}
workers_comp_freq$experience_yrs <- pmax(pmin(workers_comp_freq$experience_yrs, 40), 0.2)

# Accident history flag: enforce value level {1, 0}
workers_comp_freq$accident_history_flag <- pmax(pmin(workers_comp_freq$accident_history_flag, 1), 0)

# Psych stress index: enforce value levels {1, 2, 3, 4, 5}
workers_comp_freq$psych_stress_index <- pmax(pmin(workers_comp_freq$psych_stress_index, 5), 1)

# Hours per week: enforce value level {20, 25, 30, 35, 40}
workers_comp_freq$hours_per_week <- pmax(pmin(workers_comp_freq$hours_per_week, 40), 20)

# Supervision level: enforce value range {0 - 1}
workers_comp_freq$supervision_level <- pmax(pmin(workers_comp_freq$supervision_level, 1), 0)

# Gravity level: enforce value range {0.75 - 1.50}
workers_comp_freq$gravity_level <- pmax(pmin(workers_comp_freq$gravity_level, 1.5), 0.75)

# Safety training index: enforce value level {1, 2, 3, 4, 5}
workers_comp_freq$safety_training_index <- pmax(pmin(workers_comp_freq$safety_training_index, 5), 1)

# Protective gear quality: enforce value level {1, 2, 3, 4, 5}
workers_comp_freq$protective_gear_quality <- pmax(pmin(workers_comp_freq$protective_gear_quality, 5), 1)

# Base salary: enforce value range {20k - 130k}
workers_comp_freq$base_salary <- pmax(pmin(workers_comp_freq$base_salary, 130000), 20000)

# Exposure: fitler for rows within value range {0 - 1}
workers_comp_freq$exposure <- pmax(pmin(workers_comp_freq$exposure, 1), 0)

# Remove rows with NA inputs 
workers_comp_freq <- na.omit(workers_comp_freq)

# Number of rows post cleaning 
nrow(workers_comp_freq) #133089

# Clean workers compensation severity dataset ----------------------------------
# Remove any additional characters and numbers from columns 
severity_edit <- c("claim_id",
                   "policy_id",
                   "worker_id",
                   "solar_system",
                   "station_id",
                   "occupation",
                   "employment_type",
                   "injury_type",
                   "injury_cause")

workers_comp_sev <- workers_comp_sev %>%
  mutate(across(all_of(severity_edit),
                ~ sub("_[^_]*$", "", .)
  ))

# Take absolute value of numeric columns 
severity_abs <- c("claim_seq",
                  "experience_yrs",
                  "accident_history_flag",
                  "psych_stress_index",
                  "hours_per_week",
                  "supervision_level",
                  "gravity_level",
                  "safety_training_index",
                  "protective_gear_quality",
                  "base_salary",
                  "exposure",
                  "claim_length",
                  "claim_amount")

workers_comp_sev <- workers_comp_sev %>%
  mutate(across(all_of(severity_abs),
                ~ abs(.)))

# Claim seq: enforce value level {0, 1, 2}
workers_comp_sev$claim_seq <- pmax(pmin(workers_comp_sev$claim_seq, 2), 0)

# Experience years: enforce value range {0.2 - 40}
workers_comp_sev$experience_yrs <- pmax(pmin(workers_comp_sev$experience_yrs, 40), 0.2)

# Accident history flag: enforce value level {0, 1}
workers_comp_sev$accident_history_flag <- pmax(pmin(workers_comp_sev$accident_history_flag, 1), 0)

# Psych stress index: enforce value level {1, 2, 3, 4, 5}
workers_comp_sev$psych_stress_index <- pmax(pmin(workers_comp_sev$psych_stress_index, 5), 1)

# Hours per week: enforce value level {20, 25, 30, 35, 40}
workers_comp_sev$hours_per_week <- pmax(pmin(workers_comp_sev$hours_per_week, 40), 20)

# Supervision level: enforce value range {0 - 1}
workers_comp_sev$supervision_level <- pmax(pmin(workers_comp_sev$supervision_level, 1), 0)

# Gravity level: enforce value range {0.75 - 1.5}
workers_comp_sev$gravity_level <- pmax(pmin(workers_comp_sev$gravity_level, 1.5), 0.75)

# Safety training index: enforce value level {1, 2, 3, 4, 5}
workers_comp_sev$safety_training_index <- pmax(pmin(workers_comp_sev$safety_training_index, 5), 1)

# Protective gear quality: enforce value level {1, 2, 3, 4, 5}
workers_comp_sev$protective_gear_quality <- pmax(pmin(workers_comp_sev$protective_gear_quality, 5), 1)

# Base salary: enforce value range {20k - 130k}
workers_comp_sev$base_salary <- pmax(pmin(workers_comp_sev$base_salary, 130000), 20000)

# Exposure: enforce value range {0 - 1}
workers_comp_sev$exposure <- pmax(pmin(workers_comp_sev$exposure, 1), 0)

# Claim length: enforce value range {3 - 1000}
workers_comp_sev$claim_length <- pmax(pmin(workers_comp_sev$claim_length, 1000), 3)

# Claim amount: Appears values provided in cents. Enforce range {5 - 170}
workers_comp_sev$claim_amount_edit <- workers_comp_sev$claim_amount / 100
workers_comp_sev$claim_amount_edit <- pmax(pmin(workers_comp_sev$claim_amount_edit, 170), 5)

# Remove rows with NA inputs 
workers_comp_sev <- na.omit(workers_comp_sev)

# Number of rows in data post cleaning 
nrow(workers_comp_sev) # 1878

# Join frequency and severity dataset ------------------------------------------

# Define columns to keep from frequency dataset (to prevent duplicate columns)
freq_columns <- c("policy_id",
                  "worker_id",
                  "station_id",
                  "claim_count")

workers_freq_sev <- merge(x = workers_comp_sev, y = workers_comp_freq[freq_columns], by = c("policy_id", "worker_id", "station_id"), all.x = TRUE)
workers_freq_sev <- workers_freq_sev[complete.cases(workers_freq_sev), ]

# Columns with differences between frequency and severity: 
# experience_yrs
# psych_stress_index
# hours_per_week
# supervision_level
# gravity_level
# safety_training_index
# base_salary
# exposure

# Manually adjust three claims with 'claim_count' = 0 to 1 
workers_freq_sev <- workers_freq_sev %>% 
  mutate(claim_count = ifelse(claim_count == 0, 
                              1, 
                              claim_count))


# Determine benefits under proposed product ------------------------------------

# Create columns needed in dataset 
workers_freq_sev <- workers_freq_sev %>%
  mutate(
    # Set incomes to align with Cosmic Quarry Income 
    cosmic_salary = case_when(
      occupation == "Engineer" ~ 95000,
      occupation == "Maintenance Staff" ~ 65000,
      occupation == "Drill Operator" ~ 60000,
      occupation == "Scientist" ~ 120000,
      occupation == "Safety Officer" ~ 80000,
      occupation == "Administrator" ~ 93750, # This is average of HR, IT, Legal and Finance & Accounting Salary
      occupation == "Spacecraft Operator" ~ 85000, # Average salary of Navigation officers 
      occupation == "Executive" ~ 500000,
      occupation == "Technology Officer" ~ 75000, # This is average salary of robotics technican 
      occupation == "Planetary Operations" ~ 81250, # Average salary of all occupations under exploration and extraction operations
      occupation == "Manager" ~ 150000 # Average salary of Director 
    ),
    
    # Set occupations to categories aligned with Cosmic Quarry 
    cosmic_occupation_cat = case_when(
      occupation == "Engineer" ~ "Extraction Operations",
      occupation == "Maintenance Staff" ~ "Extraction Operations",
      occupation == "Drill Operator" ~ "Extraction Operations",
      occupation == "Scientist" ~ "Exploration Operations",
      occupation == "Safety Officer" ~ "Environmental & Safety",
      occupation == "Administrator" ~ "Administration", 
      occupation == "Spacecraft Operator" ~ "Spacecraft Operations", 
      occupation == "Executive" ~ "Management",
      occupation == "Technology Officer" ~ "Extraction Operations", 
      occupation == "Planetary Operations" ~ "Extraction Operations", 
      occupation == "Manager" ~ "Administration"
    )
  )


# Recreate columns in frequency dataset 
workers_comp_freq <- workers_comp_freq %>%
  mutate(
    # Set incomes to align with Cosmic Quarry Income 
    cosmic_salary = case_when(
      occupation == "Engineer" ~ 95000,
      occupation == "Maintenance Staff" ~ 65000,
      occupation == "Drill Operator" ~ 60000,
      occupation == "Scientist" ~ 120000,
      occupation == "Safety Officer" ~ 80000,
      occupation == "Administrator" ~ 93750, # This is average of HR, IT, Legal and Finance & Accounting Salary
      occupation == "Spacecraft Operator" ~ 85000, # Average salary of Navigation officers 
      occupation == "Executive" ~ 500000,
      occupation == "Technology Officer" ~ 75000, # This is average salary of robotics technican 
      occupation == "Planetary Operations" ~ 81250, # Average salary of all occupations under exploration and extraction operations
      occupation == "Manager" ~ 150000 # Average salary of Director 
    ),
    
    # Set occupations to categories aligned with Cosmic Quarry 
    cosmic_occupation_cat = case_when(
      occupation == "Engineer" ~ "Extraction Operations",
      occupation == "Maintenance Staff" ~ "Extraction Operations",
      occupation == "Drill Operator" ~ "Extraction Operations",
      occupation == "Scientist" ~ "Exploration Operations",
      occupation == "Safety Officer" ~ "Environmental & Safety",
      occupation == "Administrator" ~ "Administration", 
      occupation == "Spacecraft Operator" ~ "Spacecraft Operations", 
      occupation == "Executive" ~ "Management",
      occupation == "Technology Officer" ~ "Extraction Operations", 
      occupation == "Planetary Operations" ~ "Extraction Operations", 
      occupation == "Manager" ~ "Administration"
    )
  )


# Due to new products, need to recreate outcome if injury occurred under new benefit structure 
workers_freq_sev <- workers_freq_sev %>% 
  mutate(
    # Number of weeks off work 
    claim_weeks  = ceiling(claim_length / 7),
    
    # Number of half-years off work 
    claim_half_yr =  ceiling(claim_weeks / 26),
    
    # Medical allowance 
    medical_allowance = min(1000 * claim_half_yr, 4000), 
    
    # Weekly income - BASED OFF COSMIC SALARY
    weekly_income = round(cosmic_salary / 52, 2),
    
    # Benefits Paid - cap weekly payments after two years 
    weekly_benefit = ifelse(claim_weeks > 104, 
                            weekly_income * 104 * 0.9 + medical_allowance,
                            weekly_income * claim_weeks * 0.9 + medical_allowance),
    
    # Flag for permanent impairment - assume that after two years permanently impaired 
    pi_flag = ifelse(claim_weeks > 104, 
                     1,
                     0),
    
    # Lump sum benefits paid 
    total_benefit = ifelse(claim_weeks > 104,
                           754000,
                           weekly_benefit))

# Save cleaned and edited datasets as RDS Files -------------------------------
setwd('/Users/sophievonwiller/Library/CloudStorage/OneDrive-UNSW/ACTL4001 Assignment/Code and Workings')
saveRDS(workers_freq_sev, "clean_workers_comp_dataset.RDS")
saveRDS(workers_comp_freq, "clean_workers_comp_freq_only_dataset.RDS")

# EDA: Frequency data ----------------------------------------------------------
# Plot histogram of claim count per policy 
ggplot(workers_comp_freq, aes(x = claim_count)) + 
  geom_bar() + 
  labs(title = "Histogram of Workers Compensation Frequency") + 
  xlab("Claim Count per Policy") + 
  ylab("Frequency") + 
  theme_minimal()

mean(workers_comp_freq$claim_count) # 0.01420102
var(workers_comp_freq$claim_count) # 0.01437515
nrow(workers_comp_freq) #133,089

# Examine distribution by solar system 
workers_comp_freq_eps <- workers_comp_freq %>% filter(solar_system == "Epsilon")
workers_comp_freq_zeta <- workers_comp_freq %>% filter(solar_system == "Zeta")
workers_comp_freq_helionis <- workers_comp_freq %>% filter(solar_system == "Helionis Cluster")

# Epsilon solar system 
ggplot(workers_comp_freq_eps, aes(x = claim_count)) + 
  geom_bar() + 
  labs(title = "Epsilon: Histogram of Epsilon Workers Compensation Frequency") + 
  xlab("Claim Count per Policy") + 
  ylab("Frequency") + 
  theme_minimal()

mean(workers_comp_freq_eps$claim_count) # 0.0129706
var(workers_comp_freq_eps$claim_count) # 0.01309572
nrow(workers_comp_freq_eps) #54,585

# Zeta solar system 
ggplot(workers_comp_freq_zeta, aes(x = claim_count)) + 
  geom_bar() + 
  labs(title = "Zetas: Histogram of Zeta Workers Compensation Frequency") + 
  xlab("Claim Count per Policy") + 
  ylab("Frequency") + 
  theme_minimal()

mean(workers_comp_freq_zeta$claim_count) # 0.01524866
var(workers_comp_freq_zeta$claim_count) #0.01559404
nrow(workers_comp_freq_zeta) #51,939

# Helionis Cluster 
ggplot(workers_comp_freq_helionis, aes(x = claim_count)) + 
  geom_bar() + 
  labs(title = "Helionis: Histogram of Helionis Cluster Workers Compensation Frequency") + 
  xlab("Claim Count per Policy") + 
  ylab("Frequency") + 
  theme_minimal()

mean(workers_comp_freq_helionis$claim_count) #0.01468097
var(workers_comp_freq_helionis$claim_count) #0.01461656
nrow(workers_comp_freq_helionis) # 26,565



# EDA: Severity data ----------------------------------------------------------
# Filter claims that are weekly payments
workers_comp_weekly <- workers_freq_sev %>%
  filter(pi_flag == 0)

mean(workers_comp_weekly$weekly_benefit) #9172.209
sd(workers_comp_weekly$weekly_benefit) #25318.33

# Plot histogram of weekly compensation under new benefit structure 
ggplot(workers_comp_weekly, aes(x = weekly_benefit)) + 
  geom_histogram(bins = 30) + 
  labs(title = "Histogram of Workers Compensation New Weekly Benefit") + 
  xlab("Claim Amount") + 
  ylab("Frequency") + 
  theme_minimal()

# Examine distribution by Solar System 
workers_comp_sev_eps <- workers_comp_weekly %>% filter(solar_system == "Epsilon")
workers_comp_sev_zeta <- workers_comp_weekly %>% filter(solar_system == "Zeta")
workers_comp_sev_helionis <- workers_comp_weekly %>% filter(solar_system == "Helionis Cluster")

# EPSILON Plot histogram of weekly compensation under new benefit structure 
ggplot(workers_comp_sev_eps, aes(x = weekly_benefit)) + 
  geom_histogram(bins = 30) + 
  labs(title = "Epsilon: Histogram of Workers Compensation New Weekly Benefit") + 
  xlab("Claim Amount") + 
  ylab("Frequency") + 
  theme_minimal()

# ZETA Plot histogram of weekly compensation under new benefit structure 
ggplot(workers_comp_sev_zeta, aes(x = weekly_benefit)) + 
  geom_histogram(bins = 30) + 
  labs(title = "Zeta: Histogram of Workers Compensation New Weekly Benefit") + 
  xlab("Claim Amount") + 
  ylab("Frequency") + 
  theme_minimal()

# HELIONIS Plot histogram of weekly compensation under new benefit structure 
ggplot(workers_comp_sev_helionis, aes(x = weekly_benefit)) + 
  geom_histogram(bins = 30) + 
  labs(title = "Helionis: Histogram of Workers Compensation New Weekly Benefit") + 
  xlab("Claim Amount") + 
  ylab("Frequency") + 
  theme_minimal()
