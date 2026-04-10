[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/FxAEmrI0)
# Actuarial Theory and Practice A Assignment - 2026 SOA Research Challenge
_by Sophie Vonwiller, Matthew Vucic and Savannah Worth_

This page details our approach to the research challenge, the analysis we were able to draw from the data, and the final recommendations that we provided to the client.

# Challenge Overview
Our objective was to inform the senior management of Galaxy General Insurance Company (GGIC) on a suitable proposal in response to Cosmic Quarry Mining Corporation's (CQMC) request for space mining insurance coverage. More specifically, CQMC was seeking coverage for four hazard areas; Cargo Loss, Business Interruption, Equipment Failure and Workers' Compensation. 

To ensure that senior management were well informed our team has: 
  1. Conducted a thorough risk assessment of CQMC's current operations 
  2. Designed suitable products for each hazard area tailorised to CQMC's risk exposure 
  3. Presented aggregate estimates and ranges on expected loss, revenue and profit.

The following study presents the methodology and results undertaken to achieve the three objectives outlined above.   

# Data Cleaning and Exploratory Data Analysis
All data used in this analysis was provided by SOA. These files were: 
- [Historical Business Interruption Claims](srcsc-2026-claims-business-interruption.xlsx)
- [Historical Cargo Loss Claims](srcsc-2026-claims-cargo.xlsx)
- [Historical Workers' Compensation Claims](srcsc-2026-claims-workers-comp.xlsx)

Throughout the data cleaning, we have relied upon the data description as a source of truth. The key elements undertaken in the data cleaning process were: 
- Minimum and maximum bounds set within the data description were used to cap any values outside of this range (with the exception of maximum values in claim amounts). 
- NA values within claim counts were assumed to be 0. All other NA’s were removed. 
- Negative values were adjusted to be positive. 
- Strings with additional characters on the end were assumed to be typos and were adjusted to match the examples in the data description. 
- Where claim amounts appear to be around 100 times too high, it was assumed that the data had been inputted in cents instead of dollars, and was thus divided by 100 to correct this.

The data cleaning code can be found here for [Cargo Loss](), [Business Interruption](Business_Interruption-Data_Cleaning_and_EDA.R), [Equipment Failure](Equipment_Failure-Data_Cleaning_and_EDA.R) and [Workers' Compensation](Workers_Compensation_Data_Cleaning_and_EDA.R).

^REQUIRE DATA CLEANING CODE FOR CL 

# Risk Assessment
(discuss our risk section)

# Distribution Selection
The following sections detail the distribution selection process for each hazard. To select the most appropriate distributions for claim frequency and severity, several distributions were naively fitted using MLE and compared against historical claims data using a range of statistical methods, including empirical CDFs, P-P plots and AIC comparisons. The selected distributions were then used to fit GLMs, so that CQMC's current risks and exposures were considered, with an exposure offset equal to 1. Frequency and severity distributions were combined to create an aggregate loss distribution.

## Cargo Loss Analysis

For claim frequency, the mean was 0.25 claims, and variance was 0.36. The best fitting distribution was Negative Binomial. A histogram of the data shows that a large majority of policies never make a claim, and the amount of claims made decreases at a decreasing rate. A negative binomial distribution, known to handle over-dispersed data, was the best-fitting distribution. Other distributions tested include Binomial and Poisson. 

A negative binomial GLM was then fit to the frequency data. For the negative binomial GLM, the statistically significant variables were pilot experience, route type, cargo value, and weight. 

**Claims Frequency**

A sample of the code for testing the Negative Binomial distribution is below.
```{r}
fitNegBinomial <- fitdist(cargo_freq$claim_count, "nbinom", method = "mle")
gofstat(fitNegBinomial)
cdfcomp(fitNegBinomial, xlab = "claim_count")

sizeNB <- fitNegBinomial$estimate["size"]
muNB <- fitNegBinomial$estimate["mu"]

cdfcomp(fitNegBinomial, xlab = "claim_count")

frequencyModel <- glm.nb(claim_count ~ container_type + pilot_experience + route_risk + offset(log(exposure)), data = cargo_freq)
```
**Claims Severity**

**Claims Frequency**

For claim severity, the mean was $7,788,328, and standard deviation was $22, 859, 713. The log normal distribution was chosen. 

A log-normal GLM was then fit to the cargo loss severity data. The statistically significant variables for the log normal GLM were max weight, solar radiation, debris density, and route risk. 

A sample of the code for testing the Lognormal distribution is below.
```{r}
fitLnorm <- fitdist(combined_freq_sev$claim_amount, "lnorm", method = "mle")
gofstat(fitLnorm)
plot(fitLnorm)

meanL <- fitLnorm$estimate["meanlog"]
sdL <- fitLnorm$estimate["sdlog"]

cdfcomp(fitLnorm, xlab = "claim_amount")

severityModel <- glm(log(claim_amount) ~ container_type + solar_radiation + debris_density + route_risk, family=gaussian, data=combined_freq_sev)
```
**Aggregate Distribution**

To simulate the aggregate loss distribution for Cargo Loss, the exposure for CQMC was first established. This involved determining and estimating the value of features used within the frequency and severity GLMs. These were deerived from the the exposure data and context provided within the encyclopedia. Pilot experience was set to the mean of the claims frequency data for all systems. Route risk was set based on the asteroid and solar context of each system, i.e., HC was attributed a low value due to low solar activity, and an outer asteroid belt.

For each system, the parameters for the GLMs of both frequency and severity were then predicted, using the estimated features. Following this, using monte carlo simulation, the frequency was simulated for each policy (i.e., each container within each solar system), followed by the severity of each of the simulated claims. Summing the results provides the total loss distribution, which is displayed here.

The total loss distribution produced had a mean loss of $14,084,596,613 and a standard deviation of $1,696,016,956. The total loss is slightly positively skewed, with a median of $1,696,016,956. The 95% VaR is $16,991,852,635 and the expected shortfall above the 99th percentile is $19,089,376,617.  

A sample of the code for the aggregate loss distribution is visible below.
```{r}
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
```

## Business Interruption Analysis
>The entire code used to select the best fitting frequency and severity distributions and create the final aggregate loss distribution for Business Interruption can be found [here](BusinessInterruption-ModelSelectionandPricing.R).

**Claims Frequency**

The claims frequency data for Business Interruption had a mean of 0.100 and a variance of 0.174, meaning that the data is over-dispersed. A [histogram](BI-F_Hist.png) of the data shows that a large majority of policies never make a claim, and the amount of claims made decreases at a decreasing rate. A negative binomial distribution, known to handle over-dispersed data, was the best-fitting distribution. Other distributions tested include Binomial and Poisson. The [ECDF](BI-F_CDF.png) produced by the negative binomial distribution was almost identical to that of the true data, and the points on the [P-P](BI-F_P-P.png) plot were close to the guide line. The negative binomial distribution had the smallest AIC out of all of the distributions analysed, and gave a mean and variance of 0.101 and 0.184, respectively.

A sample of the code for testing the Negative Binomial distribution is below.
```{r}
##Option Three: Negative Binomial
nbinomfit <- fitdist(bus_int_f$claim_count,"nbinom")
#Comparing Empirical CDF
cdfcomp(nbinomfit)
#Almost an exact fit
#P-P Plot
plot(pnbinom(bus_int_f$claim_count, size=nbinomfit$estimate[1], mu=nbinomfit$estimate[2]), empirical(bus_int_f$claim_count),
     xlab= "Theoretical Probability", ylab= "Sample Probability", main="P-P Plot Business Interruption / Negative Binomial")
abline(0,1,col=3)
#Mean and variance from Negative Binomial
nbinomfit$estimate[2]
nbinomfit$estimate[2] + ((nbinomfit$estimate[2])^2)/nbinomfit$estimate[1]
#Mean of 0.101 and variance of 0.184
```
A negative binomial GLM was then fitted to the frequency data, using the covariates of Energy Backup Score, Supply Chain Index and Maintenance Frequency. All covariates, except Maintenance Frequency, were significant at 0.05.

**Claims Severity**

The Claims Severity data for Business Interruption had a mean of $309,750.9 and a standard deviation of $399,719.6. A [histogram](BI-S_Hist.png) of the data shows that majority of claims are below $100,000 and that the number of claims above $100,000 remains reasonably constant in each cost bracket. A Pareto 4 distribution, known for its flexible tails, was the best distribution. Other distributions tested include Weibull, Gamma, Log-Normal, Exponential, Burr, Inverse Gamma and Generalised Beta. Most of these distributions were found through the actuar package in R. The [histogram](BI-S_HistComp.png) comparison demonstrates that it has the same general shape as the historical claims data. The poor tail fit, underestimating severity, can be attributed to the lack of extreme historical claims. When there isn’t any extreme data, the model tends to underestimate the probability of extreme events occurring. As the Business Interruption product has a maximum claim limit, this underestimation will not pose a material risk to GGIC.

A sample of the code for testing the Pareto 4 distribution is below.

```{r}
##Option Eight: Pareto 4
par4fit<- fitdist(bus_int$claim_amount,"pareto4")
#Comparing Histograms
hist(bus_int$claim_amount,breaks=100,prob=TRUE,xlab="Claim Severity", main= "Histogram Business Interruption / Pareto 4")
lines(xgrids,dpareto4(xgrids,min=par4fit$estimate[1],shape1=par4fit$estimate[2], shape2=par4fit$estimate[3],scale=par4fit$estimate[4]),col=2)
#Comparing ECDF
cdfcomp(par4fit)
#P-P Plot
plot(ppareto4(bus_int$claim_amount, min=par4fit$estimate[1],shape1=par4fit$estimate[2], shape2=par4fit$estimate[3],scale=par4fit$estimate[4]), empiricals(bus_int$claim_amount),
     xlab= "Theoretical probability", ylab= "Sample probability", main="P-P Plot Business Interruption / Pareto 4")
abline(0,1,col=3)
#Goodness-of-Fit Stats
gofstat(par4fit)
#K-S Test gave 0.0881
#AIC gave 260,264.2
#Mean
mpareto4(order =1, min=par4fit$estimate[1],shape1=par4fit$estimate[2], shape2=par4fit$estimate[3],scale=par4fit$estimate[4])
#Mean of $421,167.3
```

A generalised pareto GLM was fit to the severity data using the covariates Energy Backup Score and Safety Compliance. Note that no covariates were significant for this dataset, but these were the most significant covariates. The GLM was used to estimate the minimum and shape1 parameters, while the original Pareto 4 fit was the source of the shape2 and scale parameters. This allowed the model to account for current risks while also fitting the tail to historical data.

**Aggregate Distribution**

The aggregate distribution used for Business Interruption pricing was created by first fitting the frequency and severity GLMs to CQMC's current resources and exposures, then simulating 1,000,000 possible claims frequencies. The output of these simulations were fed into the claims severity simulations to produce 1,000,000 simulations of the aggregate loss. The claims severity simulations were adjusted to account for the product features that we had designed: a deductible of $20,000 and a maximum claim limit of $1,500,000. The final 1,000,000 simulations created an empirical loss distribution with a mean of $6,105,802 and a standard deviation of $3,688,022. The [distribution](BI-Agg.png) has an extensive right tail, with the possibility for extremely large values. For example, the 97.5% VaR is $14,597,556. Due to this, GGIC may wish to consider an excess-of-loss reinsurance policy. 

An excerpt from the simulation code is below.
```{r}
rnbinomFunc <- function(number,mu,size) { 
  x <- rnbinom(number,mu=mu,size=size)
  x <- ifelse(x >4, 4, x)
  x <- ifelse(x <0, 0,x)
  return(sum(x))
} 
#Note that there is a deductible of $20,000 and a max claim limit of $1,500,000
rparFunc <- function(claimFreq,mu_par,alpha_par,shape2,scale) { 
  if (claimFreq==0) return(0)
  x <- rpareto4(claimFreq,min=mu_par,shape1=alpha_par,shape2=par4fit$estimate["shape2"],scale=par4fit$estimate["scale"])
  x <- ifelse(x > 1500000, 1500000,x)
  x <- ifelse(x < 20000,0,x)
  return(sum(x))
} 

set.seed(1)
all_claim_counts <- list()
all_claim_sizes <- list()
totalLosses <- numeric(1000000)

for (i in seq(1,1000000,1)) { 
  claim_counts_list <- mapply(rnbinomFunc,predictorDf$number, 
                              predictorDf$mu_freq,predictorDf$size)
  all_claim_counts[[i]] <- unlist(claim_counts_list)
  claim_sizes_list <- mapply(rparFunc,claim_counts_list, 
                             predictorDf$mu_par,predictorDf$alpha_par)
  all_claim_sizes[[i]] <- unlist(claim_sizes_list)
  totalLosses[i] <- sum(unlist(claim_sizes_list)) 
}
```

## Equipment Failure Analysis
>The entire code used to select the best fitting frequency and severity distributions and create the final aggregate loss distribution for Equipment Failure can be found [here](Equipment_Failure-Model_Selection_and_Pricing.R).

**Claims Frequency**

The claims frequency data for Equipment Failure had a mean of 0.078 and a variance of 0.082, meaning that the data is slightly over-dispersed. A [histogram](EF-F_Hist.png) of the data shows that a large majority of policies never make a claim, and the amount of claims made decreases at a decreasing rate. A negative binomial distribution was the best-fitting distribution. Other distributions tested include Binomial and Poisson. The [ECDF](EF-F_ECDF.png) produced by the negative binomial distribution was an exact fit to the true data, and the points on the [P-P plot](EF-F_P-P.png) were extremely close to the guideline. The negative binomial distribution had the smallest AIC out of all of the distributions analysed, and gave a mean and variance of 0.078 and 0.082, respectively. Hence, the negative binomial distribution was the most appropriate choice.


A sample of the code for testing the Negative Binomial distribution is below.
```{r}
##Option Three: Negative Binomial
enbinomfit <-fitdist(equipment_failure_f$claim_count,"nbinom")
#Comparing Empirical CDF
cdfcomp(enbinomfit)
#Almost an exact fit
#P-P Plot
plot(pnbinom(equipment_failure_f$claim_count, size=enbinomfit$estimate[1], mu=enbinomfit$estimate[2]), eempirical(equipment_failure_f$claim_count),
     xlab= "Theoretical probability", ylab= "Sample probability", main="P-P Plot Equipment Failure / Negative Binomial")
abline(0,1,col=3)
#Goodness-of-Fit Stats
gofstat(enbinomfit)
#AIC gave 52,561.3
#Mean and variance from Negative Binomial
enbinomfit$estimate[2]
enbinomfit$estimate[2] + ((enbinomfit$estimate[2])^2)/enbinomfit$estimate[1]
#Mean of 0.078 and variance of 0.082
```
A negative binomial GLM was then fitted to the frequency data, using the covariates of Equipment Type, Equipment Age, Maintenance Intensity and Usage Intensity. All of these covariates were significant, with the equipment type ‘Flux Rider’ being the only covariate not significant at 0.001. 

**Claims Severity**

The Claims Severity data for Equipment Failure had a mean of $87,349.9 and a standard deviation of $61,610.67. A [histogram](EF-S_Hist.png) of the data shows that majority of claims are between $50,000 and $100,000, and the overall shape of the claims distribution is similar to a highly-right skewed bell curve with a large tail. A log-normal distribution, suitable for positive, right-skewed and right-tailed data, was found to be the closest distribution. Other distributions tested include Weibull, Gamma, Normal, Exponential and Pareto. The [histogram comparison](EF-S_HistComp.png) and [P-P plot](EF-S_P-P.png) demonstrate that the distribution is almost an identical fit to the historical claims data provided.

A sample of the code for testing the Log-Normal distribution is below.

```{r}
##Option Four: Log-Normal
#Fit
elnormfit<-fitdist(equipment_failure$claim_amount,"lnorm")
#Comparing Histograms
hist(equipment_failure$claim_amount, breaks=100, prob=T, xlab="Claim Severity", main = "Histogram Equipment Failure / Log Normal")
lines(exgrids, dlnorm(exgrids,elnormfit$estimate[1], elnormfit$estimate[2]),col=2)
#Almost exactly the same
#Comparing Empirical CDF
cdfcomp(elnormfit)
#P-P Plot
plot(plnorm(equipment_failure$claim_amount, elnormfit$estimate[1], elnormfit$estimate[2]), eempiricals(equipment_failure$claim_amount),
     xlab= "Theoretical probability", ylab= "Sample probability", main="P-P Plot Equipment Failure / Log Normal", cex=0.45)
abline(0,1,col=3)
#Goodness-of-Fit Stats
gofstat(elnormfit)
#K-S Test gave 0.0099
#AIC gave 194,345.2
#Mean and variance from Log-Normal
exp(elnormfit$estimate[1]+(1/2)*elnormfit$estimate[2])
#Mean of 98,058.53
(exp(elnormfit$estimate[2])-1)*exp(2*elnormfit$estimate[1]+elnormfit$estimate[2])
#Variance of 7,786,027,952
```

A log-normal GLM was then fitted to the severity data using the covariates Equipment Age, Equipment Type and Usage Intensity. All of these covariates are significant at 0.001. Thus, the claims data was modelled by finding a suitable distribution and then using historical dependencies to estimate current risks, and hence create a loss distribution.

**Aggregate Distribution**

The aggregate distribution used for Equipment Failure pricing was created by first fitting the frequency and severity GLMs to CQMC's current resources and exposures, then simulating 1,000,000 possible claims frequencies. The output of these simulations were fed into the claims severity simulations to produce 1,000,000 simulations of the aggregate loss. The claims severity simulations were adjusted to account for the product features that we had designed: a deductible of $10,000 and a maximum claim limit of $800,000. The final 1,000,000 simulations created an empirical loss [distribution](EF-Agg.png) that is almost symmetrical and has light tails on both sides. It has an expected value of $105,834,521, a standard deviation of $3,514,315 and a 97.5% VaR of $112,811,760. As the standard deviation is small compared to the mean, the tail risk can be managed by prudent reviews of premiums and claim limits.

An excerpt from the simulation code is below.
```{r}
ernbinomFunc <- function(number,mu,size) { 
 x <- rnbinom(number,mu=mu,size=size)
 x <- ifelse(x >3, 3, x)
 x <- ifelse(x <0, 0,x)
 return(sum(x))
} 
#Note that there is a deductible of $10,000 and a max claim limit of $800,000
erlnormFunc <- function(claimFreq,mean,sd) { 
  if (claimFreq==0) return(0)
  x <- rlnorm(claimFreq,meanlog=mean,sdlog=sd)
  x <- ifelse(x > 800000, 800000,x)
  x <- ifelse(x < 10000,0,x)
  return(sum(x))
} 

set.seed(1)
eall_claim_counts <- list()
eall_claim_sizes <- list()
etotalLosses <- numeric(1000000)

for (i in seq(1,1000000,1)) { 
  claim_counts_list <- mapply(ernbinomFunc,epredictorDf$number, 
                                   epredictorDf$mu_freq,epredictorDf$size)
  eall_claim_counts[[i]] <- unlist(claim_counts_list)
  claim_sizes_list <- mapply(erlnormFunc,claim_counts_list, 
                                   epredictorDf$mean_log,epredictorDf$sd_log)
  eall_claim_sizes[[i]] <- unlist(claim_sizes_list)
  etotalLosses[i] <- sum(unlist(claim_sizes_list)) 
  }
```

## Workers' Compensation Analysis
>The entire code used to select the best fitting frequency and severity distributions and create the final aggregate loss distribution for Workers Compensation can be found [here]().

**Claims Frequency**
Similar to equipment failure and business interruption, the frequency of historical Workers Compensation claims was slightly over dispersed with a mean of 0.142 and variance of the 0.144. As illustrated in the [histogram](), majority of policies never make a claim and for a single policy the maximum claim count was 2. Negative binomial distribution was deemed most appropriate as the fitted distribution produced a p-value of 0.987 under the chi-square goodness of fit test indicating no evidence against the null hypothesis; the fitted distribution is identical to the empirical. In addition to Negative Binomial, the Poisson distribution was fitted to assess its suitability for modelling claims frequency.  

The code for fitting and testing the Negative Binomial Distribution is below. 
```{r}
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
```

A negative binomial GLM was fitted to the frequency data using the covariates of occupation category and gravity level. This required mapping occupations recorded in the history data to CQMC departments. The mapping is outlined in the table below. 
| Historical Occupation   | CQMC Department        |
|------------------------|------------------------|
| Engineer               | Extraction Operations  |
| Maintenance Staff      | Extraction Operations  |
| Drill Operator         | Extraction Operations  |
| Scientist              | Exploration Operations |
| Safety Officer         | Environmental & Safety |
| Administrator          | Administration         |
| Spacecraft Operator    | Spacecraft Operations  |
| Executive              | Management             |
| Technology Officer     | Extraction Operations  |
| Planetary Operations   | Extraction Operations  |
| Manager                | Administration         |

All covariates except occupational categories ‘Management’ and ‘Spacecraft Operators’ were significant at 0.001.

**Claims Severity**
To ensure both the probability mass of the lump sum payment and the continuous nature of weekly payments was captured, a mixed distribution approach was taken to model claim severity.

To derive the severity distribution, first the benefit that would’ve been paid under the new proposed product was derived using the available historical data and information provided by CQMC. This entailed using CQMC average salaries by occupation and claim length to determine weekly payments. If an individual’s claim length was greater than 2 years, it was assumed the individual was permanently impaired and thus received the lump sum payment only.  

The CQMC average salary assigned to each occupation in the historical data are outlined below. 
| Historical Occupation | CQMC Salary | Additional Notes                                                      |
|----------------------|--------------|------------------------------------------------------------------------|
| Engineer             | 95000        |                                                                        |
| Maintenance Staff    | 65000        |                                                                        |
| Drill Operator       | 60000        |                                                                        |
| Scientist            | 120000       |                                                                        |
| Safety Officer       | 80000        |                                                                        |
| Administrator        | 93750        | Average of HR, IT, Legal, and Finance & Accounting salaries           |
| Spacecraft Operator  | 85000        | Average salary of navigation officers                                 |
| Executive            | 500000       |                                                                        |
| Technology Officer   | 75000        | Average salary of robotics technicians                                |
| Planetary Operations | 81250        | Average salary across exploration and extraction operations           |
| Manager              | 150000       | Average salary of Director                                            |

Under the new benefit scheme, weekly payments had a mean of $9172.21 and standard deviation of $25,318.33. As illustrated in the [histogram](), the distribution was positively skewed with a median of $3077. The pareto distribution was deemed best suited to modelling weekly payments. The distribution produced the closest relative fit to empirical quantiles in [Q-Q plot](). The heavy tail is indicative of a conservative fit which is acceptable given no historical CQMC data was available. The other distributions considered and tested were Inverse Gamma, LogNormal, Weibull, Burr, Inverse Weibull, Pareto 1 and Gamma. 

A sample of teh code for testing the Pareto distribution is below. 
```{r}
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
```
A pareto GLM was fitted using occupation category as a covariate. All categories except ‘Exploration Operations’ and ‘Management’ where significant at 0.05.

The claim type distribution was Bernoulli with a probability of 0.029 for a lump sum claim. This probability represents the proportion of permanent impairment injuries in the historical data with a 10% increase to account for the risk of death. No deaths were recorded in the historical data. 

**Aggregate Distribution**
The derive the aggregate loss distirbution for Workers' Compensation, first the frequency and severity GLMs were fitted to CQMC's exposure. CQMC's exposure was calculated from the personnel data provided by the firm. The frequency of claims for each operation area (Helionis Cluster, Bayesia Cluster, Oryn Delta and headquarters) was then simulated 1,000,000 and the severity of each claim was estimated using the fitted GLMs. To derive the severity of each claim a uniform random value between 0 and 1 was generated, if the value was less than the probability of permanent impairment or death the loss was set to $754,000 (lump sum payment) otherwise the loss was determined by the fitted GLM (weekly payments). 

The code to simulate the aggregate loss distribution is included below. 
```{r}
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
``
The [aggregate loss distribution]() produced an expected value of $24,807,746 and a standard deviation of $4,308,317. The distribution is roughly a bell-shaped curve, with light tails which are attributable to the proposed limitations on claim length and payments. The 99% VaR, $35,438,000, was within 3 standard deviations of the mean. 

# Pricing 
The premiums for each hazard were calculated using the standard deviation principle, $\mu + \alpha*\sigma$, where μ is the average loss, calculated from the empirical aggregate loss distribution, σ is the standard deviation of the loss distribution, and α was selected to achieve a given profit as a percentage of total average losses. This method accounts for risk and affordability by targeting a small percentage of profit. Premiums increase proportionally to increases in exposure and will increase annually in line with expected inflation. Premiums will be continually monitored and refined with experience, as it is not GGIC’s intent to unreasonably exploit risk. Any data collected in this process will be stored securely to avoid sensitive leakage.

After premiums have been selected, final profits were caculated in a [spreadsheet](Aggregate_Profit_Projections.xlsx) combining all four hazards. To calculate this, we assumed that premiums were all received at the beginning of the year, claims were received in the middle of the year on average and claims are immediately paid out. We also estimated inflation and interest rates using a mean regression. The final profit was a sum of retained earnings + annually inflated premiums + half a year of interest on premium and retained profit + inflated claims losses + half a year of interest on the remaining profit.

## Cargo Loss
## Business Interruption
For Business Interruption ,the target profit was 3%. This profit was chosen to meet GGIC's costs of doing business, plus provide some profits. The α which is suitable for this profit target is α=0.05. As business interruption has a mean total loss of $6,105,802 and a standard deviation of $3,688,022, this produced a final premium of $6,290,203 per year, based on current risks, exposures and economic conditions. This premium makes up 0.01% of CQMC’s total profit in 2174, so it is extremely inexpensive. The table below contains the present value of profit projections for GICC, including the value of profit if claims are made at the mean and a 95% probability range for claims losses.

| PV of Profit | Lower Bound | Expected | Upper Bound |
| :--- | :--- | :--- | :--- |
| Profit - 1 Year | -$8,354,696.72 | $164,598.28 | $6,163,769.17 |
| Profit - 3 Years | -$25,631,960.17 | $503,957.44 | $18,908,512.78 |
| Profit - 10 Years | -$85,461,228.61 | $1,676,300.57 | $63,037,355.56 |

## Equipment Failure
For Equipment Failure, the target profit was 2.5%. This is a reasonable percentage to cover overhead expenses while still allowing GGIC to retain profits or issue dividends to shareholders. The α which meets this target profit is α=0.76. As equipment failure has a mean total loss of $105,834,251 and a standard deviation of $3,514,315, this produced a final premium of $108,505,130 per year, which makes up 0.18% of CQMC’s total profit in 2174. The table below describes the expected present value of profit over the short, medium and long-term, and provides 95% probability ranges for the projected profits. 

| PV of Profit | Lower Bound | Expected | Upper Bound |
| :--- | :--- | :--- | :--- |
| Profit - 1 Year | -$4,672,508.26 | $2,327,630.64 | $9,154,003.79 |
| Profit - 3 Years | -$14,351,895.78 | $7,123,479.08 | $28,065,766.73 |
| Profit - 10 Years | -$47,916,718.29 | $23,682,493.62 | $93,504,385.02 |

## Workers' Compensation
The target profit was set at 2% due to CQMC’s investment in growing its safety team. The selected α, α=0.1, produced a final premium of $25,238,578 per year. Whilst this achieved a profit of 1.74%, it was deemed reasonable as aggregate losses were conservatively modelled. The final premium is considered affordable as it represents 0.17% of CQMC’s 2174 profit. The table below contains the present value of profit projections.

| PV of Profit        | Lower Bound        | Expected           | Upper Bound        |
|---------------------|-------------------|--------------------|--------------------|
| Profit - 1 Year     | -$17,690,446.15   | $71,981.81         | $8,558,981.31      |
| Profit - 3 Years    | -$37,703,487.92   | $28,776,747.29     | $37,729,153.54     |
| Profit - 10 Years   | -$113,906,971.72  | $104,444,119.04    | $114,412,149.24    |


## Aggregate Profit Projections


# Stress Testing
(describe our stress testing scenarios and calculations)

## Scenario 1: 
## Scenario 2: Management Response to Poor Financial Conditions
In 2178 there is an unexpected spike in inflation. GGIC has increased their premiums in line with expected inflation, but claims costs increase in line with true inflation. CQMC takes extensive cost-cutting measures, at the expense of equipment and infrastructure maintenance, and outsources core business activities. This causes increases to the expected losses from BI and EF, because there is increased third-party operational risk due to reliance on vendors, and the lack of maintenance has made CQMC’s equipment more prone to breakdowns, which may eventually increase the risk of worker injury.  

| Profit PV | 4 Yrs Stressed | 4 Yrs Unstressed | 10 Yrs Stressed | 10 Yrs Unstressed |
| :---: | :---: | :--: | :---: | :---: |
| Lower Bound | -$10.6Bn | -$10.2Bn | -$25.4Bn | -$25.0Bn |
| Average | $0.2Bn | $0.6Bn | $1.2Bn | $1.6Bn |
| Higher Bound | $12.9Bn | $13.2Bn | $32.5Bn | $32.8Bn |

The table above shows that, on average, GGIC will lose $400 million from this stressor, but will remain profitable, so there is no drastic need for action. However, if claims losses are high, GGIC should consider increasing premiums, decreasing maximum claims limits or purchasing reinsurance, so that a reserve is available to absorb losses from shocks.

Note that this scenario was designed to test GGIC’s ability to withstand poor management decisions in the face of economic stress. The key inputs that were altered as part of this scenario include equipment maintenance being half as frequent, changes to the proportion of inputs sourced externally and the emergency power systems, and increase in equipment age and usage. Furthermore, as this scenario takes place 3 years into the lifespan of the product, a significant reserve has built up to be able to absorb this shock. In earlier years, when this reserve is not yet available, GGIC may become unprofitable from a shock of this magnitude.

These values were calculated by 1) adjusting the existing aggregate loss distribution code to change the resource inputs for Business Interruption and Equipment Failure, and 2) adjusting the inflation for all hazards in the final profit calculation [spreadsheet](Scenario_Test2.xlsx). A sample of the code for change 1) in Business Interruption is below.

```{r}
##Stress Test 2##
#Maintenance Frequency has halved
stresshelionisMFreq <- ((24*365)/1260.17)/2
stressbayesiaMFreq <- ((24*365)/872.6744)/2
stressorynMFreq <- ((24*365)/345.9302)/2


stresshelionisDf <- data_frame(system="Helionis",
                         number=helmines,
                         energy_backup_score=4.5,
                         supply_chain_index=0.93,
                         maintenance_freq=stresshelionisMFreq,
                         safety_compliance=mean(bus_int$safety_compliance),
                         exposure=1)
stressbayesiaDf <- data_frame(system="Bayesia",
                        number=baymines,
                        energy_backup_score=4.5,
                        supply_chain_index=0.93,
                        maintenance_freq=stressbayesiaMFreq,
                        safety_compliance=mean(bus_int$safety_compliance),
                        exposure=1)
stressorynDf <- data_frame(system="Oryn",
                     number=orymines,
                     energy_backup_score=4.5,
                     supply_chain_index=0.93,
                     maintenance_freq=stressorynMFreq,
                     safety_compliance=mean(bus_int$safety_compliance),
                     exposure=1)
```
``` {r}
rnbinomFunc <- function(number,mu,size) { 
  x <- rnbinom(number,mu=mu,size=size)
  x <- ifelse(x >4, 4, x)
  x <- ifelse(x <0, 0,x)
  return(sum(x))
} 
#Note that there is a deductible of $20,000 and a max claim limit of $1,500,000
rparFunc <- function(claimFreq,mu_par,alpha_par,shape2,scale) { 
  if (claimFreq==0) return(0)
  x <- rpareto4(claimFreq,min=mu_par,shape1=alpha_par,shape2=par4fit$estimate["shape2"],scale=par4fit$estimate["scale"])
  x <- ifelse(x > 1500000, 1500000,x)
  x <- ifelse(x < 20000,0,x)
  return(sum(x))
} 

set.seed(1)
stress_claim_counts <- list()
stress_claim_sizes <- list()
stresstotalLosses <- numeric(1000000)

for (i in seq(1,1000000,1)) { 
  claim_counts_list <- mapply(rnbinomFunc,stresspredictorDf$number, 
                              stresspredictorDf$mu_freq,stresspredictorDf$size)
  stress_claim_counts[[i]] <- unlist(claim_counts_list)
  claim_sizes_list <- mapply(rparFunc,claim_counts_list, 
                             stresspredictorDf$mu_par,stresspredictorDf$alpha_par)
  stress_claim_sizes[[i]] <- unlist(claim_sizes_list)
  stresstotalLosses[i] <- sum(unlist(claim_sizes_list)) 
}
```

## Scenario 3:

This scenario considers an extreme macroeconomic environment, where inflation and risk-free rates reach extreme levels. It is generally accepted that inflation and risk-free rates move together, as governments will often respond to changes in inflation by either increasing or decreasing the overnight lending rate. During a scenario in which inflation and the risk-free rate move together, there is a minimal impact on profit. This is because the increase in the risk-free rate offsets the increase in inflation, i.e., premiums and costs are both inflated by higher rates, however, then discounted by a higher rate too. The primary stress occurs when inflation move significantly upwards to 10%, while the risk-free rate remains stagnant. This may occur during a major hyperinflationary environment, where monetary policy is ineffective due to the issuance of large volumes of debt. 

| PV of Profit | Lower Bound | Expected | Upper Bound |
|---|---|---|---|
| Profit - 1 Year | -$2,415,481,459 | $192,261,841 | $3,246,789,981 |
| Profit - 3 Years | -$7,329,036,286 | $6,826,184,201 | $9,886,843,155 |
| Profit - 10 Years | -$23,810,520,826 | $29,521,285,681 | $32,400,930,155 |

In the table above, there is significant damage to average profit during an extreme inflation environment. The average 10 year profit falls from $1.6 billion to -$5.4 billion. In addition to this, the range increases to [-44 billion, 40 billion], which exposes GGIC to significantly more risk.

# Assumptions
(repeat of assumptions section from report)

# Data
(repeat of data section from report)

# Final Recommendations
(repeat of conclusion from report)










_"Tell me and I forget. Teach me and I remember. Involve me and I learn." – Benjamin Franklin_

---

### Congrats on completing the [2026 SOA Research Challenge](https://www.soa.org/research/opportunities/2026-student-research-case-study-challenge/)!


> Now it's time to build your own website to showcase your work.  
> Creating a website using GitHub Pages is simple and a great way to present your project.

This page is written in Markdown.
- Click the [assignment link](https://classroom.github.com/a/FxAEmrI0) to accept your assignment.

---

> Be creative! You can embed or link your [data](player_data_salaries_2020.csv), [code](sample-data-clean.ipynb), and [images](ACC.png) here.

More information on GitHub Pages can be found [here](https://pages.github.com/).

![](Actuarial.gif)
