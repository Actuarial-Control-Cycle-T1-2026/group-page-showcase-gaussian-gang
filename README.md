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

(can talk about what the task was and give a little overview of our initial approach)

# Data Cleaning and Exploratory Data Analysis
All data used in this analysis was provided by SOA. These files were: 
- [Historical Business Interruption Claims](srcsc-2026-claims-business-interruption.xlsx)
- [Historical Cargo Loss Claims](srcsc-2026-claims-cargo.xlsx)
- [Historical Workers' Compensation Claims](srcsc-2026-claims-workers-comp.xlsx)

(talk about our code and the assumptions made here, as well as some key insights)

# Risk Assessment
(discuss our risk section)

# Distribution Selection
The following sections detail the distribution selection process for each hazard. To select the most appropriate distributions for claim frequency and severity, several distributions were naively fitted using MLE and compared against historical claims data using a range of statistical methods, including empirical CDFs, P-P plots and AIC comparisons. The selected distributions were then used to fit GLMs, so that CQMC's current risks and exposures were considered, with an exposure offset equal to 1. Frequency and severity distributions were combined to create an aggregate loss distribution.

## Cargo Loss Analysis
Hi

## Business Interruption Analysis
>The entire code used to select the best fitting frequency and severity distributions and create the final aggregate loss distribution for Business Interruption can be found [here](BusinessInterruption-ModelSelectionandPricing.R).

**Claims Frequency**

The claims frequency data for Business Interruption had a mean of 0.100 and a variance of 0.174, meaning that the data is over-dispersed. A [histogram](BI-F_Hist.png) of the data shows that a large majority of policies never make a claim, and the amount of claims made decreases at a decreasing rate. A negative binomial distribution, known to handle over-dispersed data, was the best-fitting distribution. The [ECDF](BI-F_CDF.png) produced by the negative binomial distribution was almost identical to that of the true data, and the points on the [P-P](BI-F_P-P.png) plot were close to the guide line. The negative binomial distribution had the smallest AIC out of all of the distributions analysed, and gave a mean and variance of 0.101 and 0.184, respectively.

A sample of the code for testing the negative binomial distribution is below.
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

The Claims Severity data for Business Interruption had a mean of $309,750.9 and a standard deviation of $399,719.6. A [histogram](BI-S_Hist.png) of the data shows that majority of claims are below $100,000 and that the number of claims above $100,000 remains reasonably constant in each cost bracket. A Pareto 4 distribution, known for its flexible tails, was the best distribution. The [histogram](BI-S_HistComp.png) comparison demonstrates that it has the same general shape as the historical claims data. The poor tail fit, underestimating severity, can be attributed to the lack of extreme historical claims. When there isn’t any extreme data, the model tends to underestimate the probability of extreme events occurring. As the Business Interruption product has a maximum claim limit, this underestimation will not pose a material risk to GGIC.

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
(talk about the code and key findings from the frequency, severity and GLM distribution fitting)

## Workers' Compensation Analysis
(talk about the code and key findings from the frequency, severity and GLM distribution fitting)

# Pricing 
(talk about premium pricing for each hazard and final premium projections overall w/ calculations)

# Stress Testing
(describe our stress testing scenarios and calculations)

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
