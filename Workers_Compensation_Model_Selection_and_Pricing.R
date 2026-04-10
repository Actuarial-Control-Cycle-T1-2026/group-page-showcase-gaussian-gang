# Purpose: To Create Aggregate Loss Distribution ------------------------------

# Initialise workspace ---------------------------------------------------------

library(dplyr)
library(ggplot2)
library(stats)
library(goftest)
library(actuar)
library(fitdistrplus)
library(PerformanceAnalytics)
library(MASS)



# Import Cleaned data ---------------------------------------------------------
setwd('/Users/sophievonwiller/Library/CloudStorage/OneDrive-UNSW/ACTL4001 Assignment/Code and Workings')

workers_freq_sev <- readRDS("clean_workers_comp_dataset.RDS")
workers_comp_freq <- readRDS("clean_workers_comp_freq_only_dataset.RDS")

# Naive Frequency Model Selection ----------------------------------------------
# General variables 
f <- as.vector(workers_comp_freq$claim_count)
obs_freq <- table(f)
x_vals <- as.numeric(names(obs_freq))

# Negative Binomial 
nbin_fitted <- fitdist(f, "nbinom", method = "mle")

size_fitted <- nbin_fitted$estimate["size"]
mu_fitted <- nbin_fitted$estimate["mu"]


# Chi Square Test: Goodness of Fit for Negative Binomial 
prob_nbinom <- dnbinom(x_vals,
                       size = size_fitted,
                       mu = mu_fitted)

chisq.test(obs_freq,
           p = prob_nbinom, 
           rescale.p = TRUE) # p-value of 0.9873


# Poisson Distribution
pois_fitted <- fitdist(f, "pois", method = "mle")

lambda_fitted <- pois_fitted$estimate["lambda"]

# Chi Square Test: Goodness of Fit for Poisson 
prob_pois <- dpois(x_vals, 
                   lambda = lambda_fitted)

chisq.test(obs_freq,
           p = prob_pois,
           rescale.p = TRUE) # P value of 0.004601


# Naive Severity Model Selection -----------------------------------------------
# Filter claims that are permanently impaired 
workers_comp_weekly <- workers_freq_sev %>%
  filter(pi_flag == 0)

# Checking distributions 

## Weekly Payments 
x <- as.vector(workers_comp_weekly$weekly_benefit)

# Inverse Gamma distribution 
invgamma_fit <- fitdist(x, "invgamma", method = "mle")

# Fitted Variables 
invgamma_shape_fitted <- invgamma_fit$estimate["shape"]
invgamma_scale_fitted <- invgamma_fit$estimate["scale"]

# KS Test
invgamma_ks <- ks.test(x, "pinvgamma", shape = invgamma_shape_fitted, scale = invgamma_scale_fitted)
invgamma_ks$p.value #4.087482e-54

# Other metrics 
gofstat(invgamma_fit)

# Q-Q Plot
qqcomp(invgamma_fit)
qqcomp(invgamma_fit, xlogscale = TRUE, ylogscale = TRUE)
qqcomp(invgamma_fit, xlim = c(0, 150000) )

# LogNormal distribution 
lnormal_fit <- fitdist(x, "lnorm", method = "mle")

#Fitted variables 
lnormal_mean_fitted <- lnormal_fit$estimate["meanlog"]
lnormal_sd_fitted <- lnormal_fit$estimate["sdlog"]

# KS Test 
lnormal_ks <- ks.test(x, "plnorm", meanlog = lnormal_mean_fitted, sdlog = lnormal_sd_fitted)
lnormal_ks$p.value #1.341712e-90

# Other metrics 
gofstat(lnormal_fit)

# Q-Q Plot 
qqcomp(lnormal_fit)

# Pareto Distribution 
pareto_fit <- fitdist(x, "pareto", method = "mle")

# Fitted Distribution 
pareto_shape_fitted <- pareto_fit$estimate["shape"]
pareto_scale_fitted <- pareto_fit$estimate["scale"]

# KS Test 
pareto_ks <- ks.test(x, "ppareto", shape = pareto_shape_fitted, scale = pareto_scale_fitted)
pareto_ks$p.value #1.47064e-211

# Other metrics 
gofstat(pareto_fit)

# Q-Q Test
qqcomp(pareto_fit)
qqcomp(pareto_fit, xlogscale = TRUE, ylogscale = TRUE)
qqcomp(pareto_fit, xlim = c(0,100000))

png("wc_pareto_qq_plot.png")
qqcomp(pareto_fit, xlim = c(0,100000), main = "Q-Q Plot WC Weekly Payments/Pareto (X-axis limited to 100k)")
dev.off()
# Graph for report 
png("wc_pareto_qq_cdf_plot.png", width = 500, height = 500)

cdfcomp(pareto_fit, main = "Empricial vs theoretical CDF WC Weekly Payments/Pareto")

dev.off()
getwd()
# Weibull Distribution 
weibull_fit <- fitdist(x, "weibull", method = "mle")

# Fitted Distrbution 
weibull_shape_fitted <- weibull_fit$estimate["shape"]
weibull_scale_fitted <- weibull_fit$estimate["scale"]

# KS Test
weibull_ks <- ks.test(x, "pweibull", shape = weibull_shape_fitted, scale = weibull_scale_fitted)
weibull_ks$p.value #7.595718e-181

# Other metrics 
gofstat(weibull_fit)

# Q-Q Test
qqcomp(weibull_fit)

# Burr Distribution 
burr_fit <- fitdist(x, distr = "burr", method = "mle",
                    start = list(shape1 = 1.5, shape2 = 1.5, scale = median(x)),
                    lower = c(0.05, 0.05, 0.05),   
                    upper = c(Inf, Inf, Inf))

# Fitted Distribution 
burr_shape1_fitted <- burr_fit$estimate["shape1"]
burr_shape2_fitted <- burr_fit$estimate["shape2"]
burr_scale_fitted <- burr_fit$estimate["scale"]

# KS Test
burr_ks <- ks.test(x, "pburr", shape1 = burr_shape1_fitted, shape2 = burr_shape2_fitted, scale = burr_scale_fitted)
burr_ks$p.value # 7.539666e-23

# Other metrics 
gofstat(burr_fit)

# Q-Q Plot 
qqcomp(burr_fit)
qqcomp(burr_fit, xlogscale = TRUE, ylogscale = TRUE)

# Inverse Weibull 
invweibull_fit <- fitdist(x, "invweibull", method = "mle")

# Fitted Distribution 
invweibull_shape_fitted <- invweibull_fit$estimate["shape"]
invweibull_scale_fitted <- invweibull_fit$estimate["scale"]

# KS Test 
invweibull_ks <- ks.test(x, "pinvweibull", shape = invweibull_shape_fitted, scale = invweibull_scale_fitted)
invweibull_ks$p.value #  9.847788e-44

# Other metrics 
gofstat(invweibull_fit)

# Q-Q Plot 
qqcomp(invweibull_fit)

# Pareto 1 Distribution 
pareto1_fit <- fitdist(x, "pareto1", method = "mle")

# Fitted Variables
pareto1_shape_fitted <- pareto1_fit$estimate["shape"]
pareto1_min_fitted <- pareto1_fit$estimate["min"]

# KS Test 
pareto1_ks <- ks.test(x, "ppareto1", shape = pareto1_shape_fitted, min = pareto1_min_fitted)
pareto1_ks$p.value #4.708092e-56

# Other metrics 
gofstat(pareto1_fit)

#Q-Q Plot 
qqcomp(pareto1_fit)
qqcomp(pareto1_fit, xlim = c(0, 150000))

# Gamma Distribution 
gamma_fit <- fitdist(x, "gamma", method = "mle",
                     start = list(shape = mean(x)^2 / var(x),
                                  rate  = mean(x) / var(x)),
                     lower = c(1e-6, 1e-6))  

# Fitted Distribution 
gamma_shape_fitted <- gamma_fit$estimate["shape"]
gamma_rate_fitted <- gamma_fit$estimate["rate"]

# KS Test
gamma_ks <- ks.test(x, "pgamma", shape = gamma_shape_fitted, rate = gamma_rate_fitted)
gamma_ks$p.value # 8.066069e-182

# Other metrics 
gofstat(gamma_fit)

# Q-Q Plot
qqcomp(gamma_fit)

# Distribution selected: Pareto 

# Fit GLM Models Frequency + Severity  ----------------------------------------
library(VGAM) # loaded here to prevent actuar functions from being masked 

NBFrequency <- glm.nb(claim_count ~ cosmic_occupation_cat + gravity_level + log(offset(exposure)), data = workers_comp_freq)
ParetoSeverity <- vglm(total_benefit ~ cosmic_occupation_cat , family = paretoff(), data = workers_comp_weekly)

summary(NBFrequency)
summary(ParetoSeverity)
occupation_categories <- c("Management", 
                           "Administration", 
                           "Environmental & Safety",
                           "Exploration Operations", 
                           "Extraction Operations", 
                           "Spacecraft Operations")

employment_types <- c("Full-time", "Contract")

headofficeOccupation <- c(177, 1335, 0, 0, 0, 0, 0, 227, 0, 0, 0, 0)
helionisOccupation <- c(0,	0,	2093,	405,	8822,	3255, 0,	0, 232,	253,	1972,	1551)
bayesiaOccupation <- c(0,	0,	1047,	202,	4411,	1628, 0,	0,	116,	126,	986,	775)
orynOccupation <- c(0,	0,	698,	135,	2941,	1085, 0,	0, 77,	84,	657,	517)

headofficeGravity <- 1
helionisGravity <- mean(workers_comp_freq$gravity_level[workers_comp_freq$solar_system == "Helionis Cluster"])
bayesiaGravity <- quantile(workers_comp_freq$gravity_level, probs = 0.9)
orynGravity <- quantile(workers_comp_freq$gravity_level, probs = 0.75)

headquartersDF <- data_frame(system = rep("Headquarters", 12), 
                             cosmic_occupation_cat = rep(occupation_categories, 2),
                             employment_type = rep(employment_types, each = length(occupation_categories)),
                             no_employees = headofficeOccupation, 
                             gravity_level = rep(headofficeGravity, 12),
                             exposure = rep(1, 12))

helionisDF <- data_frame(system = rep("Helionis", 12), 
                         cosmic_occupation_cat = rep(occupation_categories, 2),
                         employment_type = rep(employment_types, each = length(occupation_categories)),
                         no_employees = helionisOccupation, 
                         gravity_level = rep(helionisGravity, 12),
                         exposure = rep(1, 12))

bayesiaDF <- data_frame(system = rep("Bayesia", 12), 
                        cosmic_occupation_cat = rep(occupation_categories, 2),
                        employment_type = rep(employment_types, each = length(occupation_categories)),
                        no_employees = bayesiaOccupation, 
                        gravity_level = rep(bayesiaGravity, 12),
                        exposure = rep(1, 12))

orynDF <- data_frame(system = rep("Oryn", 12), 
                     cosmic_occupation_cat = rep(occupation_categories, 2),
                     employment_type = rep(employment_types, each = length(occupation_categories)),
                     no_employees = orynOccupation, 
                     gravity_level = rep(orynGravity, 12),
                     exposure = rep(1, 12))

predictorDf <- rbind(headquartersDF, helionisDF, orynDF, bayesiaDF)
predictorDf <- predictorDf %>% 
                  filter(no_employees > 0)

predictorDf$mu_freq <- predict(NBFrequency, predictorDf, type = "response")
predictorDf$size_freq <- NBFrequency$theta

predictorDf$theta_sev <- exp(coef(ParetoSeverity)[1]) # Theta (scale parameter is modelled as intercept)
predictorDf$alpha_sev <- predict(ParetoSeverity, predictorDf, type = "response")

# Set NA values to mean 
predictorDf$alpha_sev[is.na(predictorDf$alpha_sev)] <- mean(predictorDf$alpha_sev, na.rm = TRUE)

# Lump Sum Probabilities -------------------------------------------------------
total_claims <- nrow(workers_freq_sev) #1854

# number of claims NOT reaching 750,000
not_pi <- nrow(workers_freq_sev %>%
                 filter(pi_flag == 0)) #1805

# Number of claims reaching 750,000
pi <- nrow(workers_freq_sev %>%
             filter(pi_flag == 1)) #49

# Apply scaling to account for deaths 
scale <- 1.1
pi_scale <- pi * scale # 53.9 
not_pi_scale <- total_claims - pi_scale # 1800.1

# Calculate weightings  - adjusted empricial probabilities
pi_weighting <- pi_scale / total_claims
not_pi_weighting <- not_pi_scale / total_claims

# Check 
pi_weighting + not_pi_weighting

# Aggregate Loss Simulation ----------------------------------------------------
rnbinomFunc <- function(no_employees, mu, size) {
  return(sum(rnbinom(no_employees, mu = mu, size = size)))
} 

rparetoFunc <- function(claimFreq, alpha, theta) {
  return(sum(actuar::rpareto(claimFreq, shape = alpha, scale = theta)))
} 

totalLosses <- numeric(100000)

set.seed(1)

for(i in 1:100000){
  
  # simulate claim counts for each risk cell
  claimFreq <- mapply(
    rnbinomFunc,
    predictorDf$no_employees,
    predictorDf$mu_freq,
    predictorDf$size_freq
  )
  
  portfolio_loss <- 0
  
  # loop over risk cells
  for(k in 1:nrow(predictorDf)){
    
    if(claimFreq[k] > 0){
      
      for(j in 1:claimFreq[k]){
          
          if(runif(1) < pi_weighting){
            
            portfolio_loss <- portfolio_loss + 754000
            
          } else {
            
            sev <- rparetoFunc(
              1,
              predictorDf$alpha_sev[k],
              predictorDf$theta_sev[k]
            )
            
            portfolio_loss <- portfolio_loss + sev
          }
        }
      }
    }
  
  totalLosses[i] <- portfolio_loss
}

simulated_mean <- mean(totalLosses) 
simulated_sd <- sd(totalLosses) 
simulated_var <- var(totalLosses)

png("wc_total_loss_dist.png")
hist(totalLosses,
     main = "Total Loss Distribution",
     xlab = "Total Loss")
dev.off()


# Premium 
alpha <- 0.1
premium <- simulated_mean + alpha * simulated_sd

# Check premium 
loss <- sum(totalLosses > premium)
loss / 1000000 # 0.044561

# Upper and Lower Bounds of expected total cost 
lower <- quantile(totalLosses, probs = 0.025) #6032000
upper <- quantile(totalLosses, probs = 0.975) #12064000

median(totalLosses)

# Tail risk 
VaR_99 <- quantile(totalLosses, probs = 0.99)

(VaR_99 - simulated_mean)/simulated_sd

# For stress testing scenario 2
quantile(totalLosses, probs = 0.996)

