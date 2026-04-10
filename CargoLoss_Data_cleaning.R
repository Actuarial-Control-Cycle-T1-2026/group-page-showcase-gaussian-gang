library(ggplot2)
library(dplyr)
library(actuar)
library(fitdistrplus)
library(goftest)
library(PerformanceAnalytics)
library(MASS)
library(ggpubr)

cargo_freq <- readxl::read_xlsx("srcsc-2026-claims-cargo.xlsx", "freq")
cargo_sev <- readxl::read_xlsx("srcsc-2026-claims-cargo.xlsx", "sev")

cargo_freq[sapply(cargo_freq, is.numeric)] <- lapply(cargo_freq[sapply(cargo_freq, is.numeric)], abs)

cargo_freq$claim_count <- ifelse(is.na(cargo_freq$claim_count), 0, cargo_freq$claim_count)
cargo_freq$claim_count <- pmax(pmin(cargo_freq$claim_count, 5), 0)

cargo_freq <- na.omit(cargo_freq)

cargo_freq$cargo_value <- pmax(pmin(cargo_freq$cargo_value, 680000000), 50000)
cargo_freq$weight <- pmax(pmin(cargo_freq$weight, 250000), 1500)
cargo_freq$distance <- pmax(pmin(cargo_freq$distance, 100), 1)
cargo_freq$transit_duration <- pmax(pmin(cargo_freq$transit_duration, 60), 1)
cargo_freq$pilot_experience <- pmax(pmin(cargo_freq$pilot_experience, 30), 1)
cargo_freq$vessel_age <- pmax(pmin(cargo_freq$vessel_age, 50), 1)
cargo_freq$solar_radiation <- pmax(pmin(cargo_freq$solar_radiation, 1), 0)
cargo_freq$debris_density <- pmax(pmin(cargo_freq$debris_density, 1), 0)
cargo_freq$route_risk <- pmax(pmin(cargo_freq$route_risk, 5), 0)
cargo_freq <- cargo_freq %>% filter(!grepl("[0-9]", container_type))

cargo_sev[sapply(cargo_sev, is.numeric)] <- lapply(cargo_sev[sapply(cargo_sev, is.numeric)], abs)

cargo_sev$claim_amount <- ifelse(is.na(cargo_sev$claim_amount), 0, cargo_sev$claim_amount)
cargo_sev$claim_amount <- pmax(pmin(cargo_sev$claim_amount, 680000000), 31000 )

cargo_sev <- na.omit(cargo_sev)

cargo_sev$cargo_value <- pmax(pmin(cargo_sev$cargo_value, 680000000), 50000)
cargo_sev$weight <- pmax(pmin(cargo_sev$weight, 250000), 1500)
cargo_sev$distance <- pmax(pmin(cargo_sev$distance, 100), 1)
cargo_sev$transit_duration <- pmax(pmin(cargo_sev$transit_duration, 60), 1)
cargo_sev$pilot_experience <- pmax(pmin(cargo_sev$pilot_experience, 30), 1)
cargo_sev$vessel_age <- pmax(pmin(cargo_sev$vessel_age, 50), 1)
cargo_sev$solar_radiation <- pmax(pmin(cargo_sev$solar_radiation, 1), 0)
cargo_sev$debris_density <- pmax(pmin(cargo_sev$debris_density, 1), 0)
cargo_sev$route_risk <- pmax(pmin(cargo_sev$route_risk, 5), 0)

combined_freq_sev <- merge(x = cargo_sev, y = cargo_freq, by = c("policy_id", "shipment_id"), all.x = TRUE)
combined_freq_sev <- combined_freq_sev %>% filter(!grepl("[0-9]", container_type.x))
combined_freq_sev$debris_density = combined_freq_sev$debris_density.x
combined_freq_sev$solar_radiation = combined_freq_sev$solar_radiation.x
combined_freq_sev$container_type = combined_freq_sev$container_type.x
combined_freq_sev$route_risk = combined_freq_sev$route_risk.x
combined_freq_sev$cargo_value = combined_freq_sev$cargo_value.x
combined_freq_sev$weight = combined_freq_sev$weight.x
