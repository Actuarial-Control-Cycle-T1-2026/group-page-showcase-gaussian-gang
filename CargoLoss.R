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

vessels <- c("DeepSpace Haulbox", "DockArc Freight Case", "HardSeal Transit Crate", "LongHaul Vault Canister", "QuantumCrate Module")
maxWeight <- c(25000, 50000, 100000, 150000, 250000)
weightDf <- data_frame(container_type = vessels, maxWeight = maxWeight)

combined_freq_sev <- merge(combined_freq_sev, weightDf, by = c("container_type"), all.x = TRUE)

combined_freq_sev <- na.omit(combined_freq_sev)

summary(cargo_freq$claim_count)
hist(cargo_freq$claim_count)
mean(cargo_freq$claim_count)
var(cargo_freq$claim_count)

fitNegBinomial <- fitdist(cargo_freq$claim_count, "nbinom", method = "mle")
gofstat(fitNegBinomial)
cdfcomp(fitNegBinomial, xlab = "claim_count")
plot(fitNegBinomial)

sizeNB <- fitNegBinomial$estimate["size"]
muNB <- fitNegBinomial$estimate["mu"]

fitGeometric <- fitdist(cargo_freq$claim_count, "geom", method = "mle")
gofstat(fitGeometric)
plot(fitGeometric)

fitPoisson <- fitdist(cargo_freq$claim_count, "pois", method = "mle")
gofstat(fitPoisson)
plot(fitPoisson)



fitWeibull <- fitdist(combined_freq_sev$claim_amount, "weibull", method = "mle")
gofstat(fitWeibull)
plot(fitWeibull)

fitLnorm <- fitdist(combined_freq_sev$claim_amount, "lnorm", method = "mle")
gofstat(fitLnorm)
plot(fitLnorm)

fitIG <- fitdist(combined_freq_sev$claim_amount, "invgamma", method = "mle")
gofstat(fitIG)
plot(fitIG)

fitPareto <- fitdist(combined_freq_sev$claim_amount, "pareto", method = "mle")
gofstat(fitPareto)
plot(fitPareto)

sizeNB <- fitNegBinomial$estimate["size"]
muNB <- fitNegBinomial$estimate["mu"]

meanL <- fitLnorm$estimate["meanlog"]
sdL <- fitLnorm$estimate["sdlog"]

cdfcomp(fitLnorm, xlab = "claim_amount")
cdfcomp(fitNegBinomial, xlab = "claim_count")

frequencyModel <- glm.nb(claim_count ~ container_type + pilot_experience + route_risk + offset(log(exposure)), data = cargo_freq)
severityModel <- glm(log(claim_amount) ~ container_type + solar_radiation + debris_density + route_risk, family=gaussian, data=combined_freq_sev)

par(mfrow = c(2, 2))
plot(frequencyModel)
plot(severityModel)
plot(fitLnorm)

cargo_value <- c(mean(combined_freq_sev[combined_freq_sev$container_type == "DockArc Freight Case", ]$cargo_value),
                 mean(combined_freq_sev[combined_freq_sev$container_type == "QuantumCrate Module", ]$cargo_value),
                 mean(combined_freq_sev[combined_freq_sev$container_type == "DeepSpace Haulbox", ]$cargo_value),
                 mean(combined_freq_sev[combined_freq_sev$container_type == "LongHaul Vault Canister", ]$cargo_value),
                 mean(combined_freq_sev[combined_freq_sev$container_type == "HardSeal Transit Crate", ]$cargo_value))

helionisVessels <- c(58, 116, 580, 232, 174)
bayesiaVessels <- c(56, 113, 564, 226, 169)
orynVessels <- c(39, 77, 387, 155, 116)

helionisSolar <- quantile(combined_freq_sev$solar_radiation, probs = 0.1)
helionisDebris <- quantile(combined_freq_sev$debris_density, probs = 0.7)

orynSolar <- quantile(combined_freq_sev$solar_radiation, probs = 0.5)
orynDebris <- quantile(combined_freq_sev$debris_density, probs = 0.9)

bayesiaSolar <- quantile(combined_freq_sev$solar_radiation, probs = 0.7)
bayesiaDebris <- quantile(combined_freq_sev$debris_density, probs = 0.1)

helionisDf <- data_frame(system = rep("Helionis", 5), 
                         container_type = vessels, 
                         no_containers = helionisVessels, 
                         debris_density = rep(helionisDebris, 5),
                         solar_radiation = rep(helionisSolar, 5),
                         pilot_experience = rep(mean(cargo_freq$pilot_experience), 5),
                         route_risk = rep(2, 5),
                         exposure = rep(1, 5),
                         cargo_value = cargo_value)

orynDf <- data_frame(system = rep("Oryn", 5), 
                         container_type = vessels, 
                         no_containers = orynVessels, 
                         debris_density = rep(orynDebris, 5),
                         solar_radiation = rep(orynSolar, 5),
                         pilot_experience = rep(mean(cargo_freq$pilot_experience), 5),
                         route_risk = rep(4, 5),
                         exposure = rep(1, 5),
                         cargo_value = cargo_value)

bayesiaDf <- data_frame(system = rep("Bayesia", 5), 
                         container_type = vessels, 
                         no_containers = bayesiaVessels, 
                         debris_density = rep(bayesiaDebris, 5),
                         solar_radiation = rep(bayesiaSolar, 5),
                         pilot_experience = rep(mean(cargo_freq$pilot_experience), 5),
                         route_risk = rep(3, 5),
                         exposure = rep(1, 5), 
                         cargo_value = cargo_value)

predictorDf <- rbind(helionisDf, orynDf, bayesiaDf)

predictorDf$mu_freq <- predict(frequencyModel, predictorDf, type = "response")
predictorDf$size <- frequencyModel$theta

predictorDf$mean_log <- predict(severityModel, predictorDf, type = "response")
predictorDf$sd_log <- sigma(severityModel)

totalLosses <- numeric(1000000)

rnbinomFunc <- function(no_containers, mu, size) {
  x <- rnbinom(no_containers, mu = mu, size = size)
  x <- ifelse(x > 5, 5, x)
  x <- ifelse(x < 0, 0, x)
  
  return(sum(x))
} 

rlnormFunc <- function(claimFreq, mean, sd) {
  if (claimFreq == 0) {
    return(0)
  }
  x <- rlnorm(claimFreq, meanlog = mean, sdlog = sd)
  
  x <- ifelse(x > 680000000, 680000000, x)
  x <- ifelse(x < 31000, 31000, x)
  return(sum(x))
} 

totalLosses <- numeric(100000)
set.seed(1)
for (i in seq(1, 100000, 1)) {
  predictorDf$claimFreq <- mapply(rnbinomFunc, predictorDf$no_containers, predictorDf$mu_freq, predictorDf$size)
  predictorDf$claimSize <- mapply(rlnormFunc, predictorDf$claimFreq, predictorDf$mean_log, predictorDf$sd_log)
  totalLosses[i] <- sum(predictorDf$claimSize)
}

hist(totalLosses,
     main = "Total Loss Distribution",
     xlab = "Total Loss")

varTL <- var(totalLosses)
meanTL <- mean(totalLosses)
sdTL <- sqrt(varTL)

VaR_9375 <- quantile(totalLosses, probs = 0.9375)
VaR_95 <- quantile(totalLosses, probs = 0.95)
VaR_99 <- quantile(totalLosses, probs = 0.99)
VaR_5 <- quantile(totalLosses, probs = 0.05)
ES99 <- mean(totalLosses[totalLosses > VaR_99])

# Scenario 1
quantile(totalLosses, probs = 0.997)

alpha <- (1.05*meanTL - meanTL)/sqrt(varTL)
premium <- meanTL + alpha*sqrt(varTL)

qqnorm(totalLosses, main='Normal')
qqline(totalLosses)

skewness(totalLosses)
