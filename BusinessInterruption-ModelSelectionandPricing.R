#Ensure data cleaning code has been run
###Model Selection - Claims Frequency###
library(MASS)
library(actuar)
library(fitdistrplus)
##Option One: Binomial
#MLE as fitdist doesn't work
#Number of successes/number of trials for p
bipf <- sum(bus_int_f$claim_count)/length(bus_int_f$claim_count)
#Comparing Empirical CDF
xgridf <- seq(min(bus_int_f$claim_count), max(bus_int_f$claim_count), by=1)
empirical <- ecdf(bus_int_f$claim_count)
plot(xgridf,empirical(xgridf),type="l",xlab="claims",ylab="cdf",main="ECDF")
lines(xgridf,pbinom(xgridf,size=4,prob=bipf),col=2)
#P-P Plot
plot(pbinom(bus_int_f$claim_count, size=4, prob=bipf), empirical(bus_int_f$claim_count),
     xlab= "theoretical probability", ylab= "sample probability", main="P-P plot / Binomial", cex=0.45)
abline(0,1,col=3)
#Chi-squared test
chisq.test(table(bus_int_f$claim_count),p=dbinom(xgridf,size=4,prob=bipf))
#Gave p-value < 2.2e-16, so reject the null hypothesis that they are the same
#AIC
binloglik<- sum(dbinom(bus_int_f$claim_count,size=4,prob=bipf,log=TRUE))
2-2*binloglik
#Gave 107,473.5
#Mean and variance from binomial
4*bipf
#Mean of 0.403
4*bipf*(1-bipf)
#Variance of 0.362

#Data has:
mean(bus_int_f$claim_count)
#Mean of 0.100
var(bus_int_f$claim_count)
#Variance of 0.174

##Option Two: Poisson
poisfit <- fitdist(bus_int_f$claim_count,"pois")
#Comparing Empirical CDF
cdfcomp(poisfit)
#P-P Plot
plot(ppois(bus_int_f$claim_count,lambda=poisfit$estimate), empirical(bus_int_f$claim_count),
     xlab= "theoretical probability", ylab= "sample probability", main="P-P plot / Poisson", cex=0.45)
abline(0,1,col=3)
#Goodness-of-Fit tests
gofstat(poisfit)
#Chi-squared p-value of 0, so reject the null hypothesis that they are the same
#AIC of 70,464.43
#Mean and variance from Poisson
poisfit$estimate
#Mean and variance of 0.101

#Data has:
mean(bus_int_f$claim_count)
#Mean of 0.100
var(bus_int_f$claim_count)
#Variance of 0.174

##Option Three: Negative Binomial
nbinomfit <- fitdist(bus_int_f$claim_count,"nbinom")
#Comparing Empirical CDF
cdfcomp(nbinomfit)
#Almost an exact fit
#P-P Plot
plot(pnbinom(bus_int_f$claim_count, size=nbinomfit$estimate[1], mu=nbinomfit$estimate[2]), empirical(bus_int_f$claim_count),
     xlab= "Theoretical Probability", ylab= "Sample Probability", main="P-P Plot Business Interruption / Negative Binomial")
abline(0,1,col=3)
#Goodness of Fit Statistics
gofstat(nbinomfit)
#Chi Squared gave p-value of 0.00047, so reject the null hypothesis that they are the same
#Mean and variance from Negative Binomial
nbinomfit$estimate[2]
nbinomfit$estimate[2] + ((nbinomfit$estimate[2])^2)/nbinomfit$estimate[1]
#Mean of 0.101 and variance of 0.184

#Data has:
mean(bus_int_f$claim_count)
#Mean of 0.100
var(bus_int_f$claim_count)
#Variance of 0.174

#Therefore, frequency data is Negative Binomial with size = 0.1214 and mu = 0.1008

###Model Selection - Claims Severity###
##Option One: Weibull
#MLE
weifit<-fitdist(bus_int$claim_amount,"weibull")
#Comparing Histograms
hist(bus_int$claim_amount, breaks=100, prob=T, xlab="claims", main = "Histogram / Weibull")
xgrids<- seq(min(bus_int$claim_amount), max(bus_int$claim_amount), length=1000)
lines(xgrids, dweibull(xgrids,weifit$estimate[1], weifit$estimate[2]),col=2)
#Comparing Empirical CDF
cdfcomp(weifit)
#P-P Plot
empiricals <- ecdf(bus_int$claim_amount)
plot(pweibull(bus_int$claim_amount, weifit$estimate[1], weifit$estimate[2]), empiricals(bus_int$claim_amount),
     xlab= "theoretical probability", ylab= "sample probability", main="P-P plot / Weibull", cex=0.45)
abline(0,1,col=3)
#Goodness-of-Fit Stats
gofstat(weifit)
#K-S Test of 0.1520
#AIC of 264,225.4
#Mean and variance from Weibull
k <- weifit$estimate[1]
l <- weifit$estimate[2]
l*gamma(1+1/k)
#Mean of 316,555.4
l^2*(gamma(1+2/k)-(gamma(1+1/k))^2)
#Variance of 204,858,944,918 

#Data has:
mean(bus_int$claim_amount)
#Mean of 309,750.9
var(bus_int$claim_amount)
#Variance of 159,775,784,751
sqrt(159775784751)

##Option Two: Gamma
#MLE - Newton Raphson as fitdist doesn't work
gammle <- function(x) {
  gamn <- length(x)
  gammean <- mean(x)
  A <- log(gammean)-mean(log(x))
  k <- (3-A+sqrt((A-3)^2 + 24*A))/(12*A)
  for (i in 1:20){
    k <- k - (log(k) - digamma(k)-A)/(1/k-trigamma(k))
  }
  gamtheta <- gammean/k
  list(shape=k, scale=gamtheta)
}
gamfit<-gammle(bus_int$claim_amount)
#Gave shape of 0.6160 and scale of 502,809
#Comparing Histograms
hist(bus_int$claim_amount, breaks=100, prob=T, xlab="claims", main = "Histogram / Gamma")
lines(xgrids, dgamma(xgrids, shape=gamfit$shape, scale=gamfit$scale),col=2)
#Comparing Empirical CDF
empiricals <- ecdf(bus_int$claim_amount)
plot(xgrids,empiricals(xgrids), type="l", xlab="claims", ylab="cdf", main = "ECDF")
lines(xgrids, pgamma(xgrids, shape=gamfit$shape, scale=gamfit$scale),col=2)
#P-P Plot
plot(pgamma(bus_int$claim_amount, shape=gamfit$shape, scale=gamfit$scale), empiricals(bus_int$claim_amount),
     xlab= "theoretical probability", ylab= "sample probability", main="P-P plot / Gamma", cex=0.45)
abline(0,1,col=3)
#K-S Test
ksgam<-empiricals(xgrids)-pgamma(xgrids, shape=gamfit$shape, scale=gamfit$scale)
max(abs(ksgam))
#Gave 0.1668
#AIC
gamloglik <- sum((gamfit$shape-1)*log(bus_int$claim_amount)-bus_int$claim_amount/gamfit$scale-gamfit$shape*log(gamfit$scale)-lgamma(gamfit$shape))
2*2 - 2*gamloglik
#Gave 264,630.7
#Mean and variance from Gamma
gamfit$shape*gamfit$scale
#Mean of 309,750.9
gamfit$shape*(gamfit$scale^2)
#Variance of 155,745,531,472

#Data has:
mean(bus_int$claim_amount)
#Mean of 309,750.9
var(bus_int$claim_amount)
#Variance of 159,775,784,751

##Option Three: Log-Normal
#Fit
lnormfit<-fitdist(bus_int$claim_amount,"lnorm")
#Comparing Histograms
hist(bus_int$claim_amount, breaks=100, prob=T, xlab="claims", main = "Histogram / Log Normal")
lines(xgrids, dlnorm(xgrids,lnormfit$estimate[1], lnormfit$estimate[2]),col=2)
#Comparing Empirical CDF
cdfcomp(lnormfit)
#P-P Plot
plot(plnorm(bus_int$claim_amount, lnormfit$estimate[1], lnormfit$estimate[2]), empiricals(bus_int$claim_amount),
     xlab= "theoretical probability", ylab= "sample probability", main="P-P plot / Log Normal", cex=0.45)
abline(0,1,col=3)
#Goodness-of-Fit Stats
gofstat(lnormfit)
#K-S Test gave 0.1145
#AIC gave 262,892
#Mean and variance from Log-Normal
exp(lnormfit$estimate[1]+(1/2)*lnormfit$estimate[2])
#Mean of 239,748.9
(exp(lnormfit$estimate[2])-1)*exp(2*lnormfit$estimate[1]+lnormfit$estimate[2])
#Variance of 196,505,494,085

#Data has:
mean(bus_int$claim_amount)
#Mean of 309,750.9
var(bus_int$claim_amount)
#Variance of 159,775,784,751

##Option Four: Exponential
#MLE
expfit<-fitdistr(bus_int$claim_amount,"exponential")
#Comparing Histograms
hist(bus_int$claim_amount, breaks=100, prob=T, xlab="claims", main = "Histogram / Exponential")
lines(xgrids, dexp(xgrids,expfit$estimate[1]),col=2)
#Comparing Empirical CDF
plot(xgrids,empiricals(xgrids), type="l", xlab="claims", ylab="cdf", main = "ECDF")
lines(xgrids, pexp(xgrids, expfit$estimate[1]),col=2)
#P-P Plot
plot(pexp(bus_int$claim_amount, expfit$estimate[1]), empiricals(bus_int$claim_amount),
     xlab= "theoretical probability", ylab= "sample probability", main="P-P plot / Exponential", cex=0.45)
abline(0,1,col=3)
#K-S Test
ksexp<-empiricals(xgrids)-pexp(xgrids, expfit$estimate[1])
max(abs(ksexp))
#Gave 0.2782
#AIC
AIC(expfit,k=2)
#Gave 266,487.3

##Option Five: Burr
burrfit<- fitdist(bus_int$claim_amount,"burr")
#Comparing Histograms
hist(bus_int$claim_amount,breaks=100,prob=TRUE,xlab="claims", main= "Histogram / Burr")
lines(xgrids,dburr(xgrids,shape1=burrfit$estimate[1],shape2=burrfit$estimate[2],scale=burrfit$estimate[3]),col=2)
#Comparing ECDF
cdfcomp(burrfit)
#P-P Plot
plot(pburr(bus_int$claim_amount, shape1=burrfit$estimate[1], shape2=burrfit$estimate[2],scale=burrfit$estimate[3]), empiricals(bus_int$claim_amount),
     xlab= "theoretical probability", ylab= "sample probability", main="P-P plot / Burr", cex=0.45)
abline(0,1,col=3)
#Goodness-of-Fit Stats
gofstat(burrfit)
#K-S Test gave 0.1081
#AIC gave 261,336.6

##Option Six: Inverse Gamma
invgamfit<- fitdist(bus_int$claim_amount,"invgamma")
#Comparing Histograms
hist(bus_int$claim_amount,breaks=100,prob=TRUE,xlab="claims", main= "Histogram / Inverse Gamma")
lines(xgrids,dinvgamma(xgrids,shape=invgamfit$estimate[1],scale=invgamfit$estimate[2]),col=2)
#Comparing ECDF
cdfcomp(invgamfit)
#P-P Plot
plot(pinvgamma(bus_int$claim_amount, shape=invgamfit$estimate[1],scale=invgamfit$estimate[2]), empiricals(bus_int$claim_amount),
     xlab= "theoretical probability", ylab= "sample probability", main="P-P plot / Inverse Gamma", cex=0.45)
abline(0,1,col=3)
#Goodness-of-Fit Stats
gofstat(invgamfit)
#K-S Test gave 0.1091
#AIC gave 262,161

##Option Seven: Generalised Beta
genbetafit<- fitdist(bus_int$claim_amount,"genbeta")
#Comparing Histograms
hist(bus_int$claim_amount,breaks=100,prob=TRUE,xlab="claims", main= "Histogram / Generalised Beta")
lines(xgrids,dgenbeta(xgrids,shape1=genbetafit$estimate[1],shape2=genbetafit$estimate[2],shape3=genbetafit$estimate[3],scale=genbetafit$estimate[4]),col=2)
#Comparing ECDF
cdfcomp(genbetafit)
#P-P Plot
plot(pgenbeta(bus_int$claim_amount, shape1=genbetafit$estimate[1],shape2=genbetafit$estimate[2],shape3=genbetafit$estimate[3],scale=genbetafit$estimate[4]), empiricals(bus_int$claim_amount),
     xlab= "theoretical probability", ylab= "sample probability", main="P-P plot / Generalised Beta", cex=0.45)
abline(0,1,col=3)
#Goodness-of-Fit Stats
gofstat(genbetafit)
#K-S Test gave 0.1831
#AIC gave 265,099

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


##Severity is a Pareto 4 distribution with min = 13549.67, shape1 = 2.74, shape2 = 0.68 and scale = 555337.9

###Aggregate Loss Distribution
library(glmmTMB)
library(VGAM)
frequencyModel <- glmmTMB(claim_count~production_load+energy_backup_score+supply_chain_index+avg_crew_exp+maintenance_freq+safety_compliance+offset(log(exposure)),data=bus_int_f,family=nbinom2)
severityModel <- vglm(claim_amount~production_load+energy_backup_score+supply_chain_index+avg_crew_exp+maintenance_freq+safety_compliance,family=gpd(),data=bus_int)
#Can use equipment usage percentages with number of equipment to estimate production load
#Energy backup score, supply chain index, safety compliance will take average from data
#Can use average age of employees and number of employees to estimate avg crew experience
#Can use maintenace schedule to estimate maintenance frequency

summary(frequencyModel)
#Production load, average crew experience and safety compliance are not significant, so I will remove (AIC = 62,499.4)
frequencyModel <- glmmTMB(claim_count~energy_backup_score+supply_chain_index+maintenance_freq,data=bus_int_f,family=nbinom2)
summary(frequencyModel)
#All predictors are now somewhat significant (AIC = 62,495.7)
summary(severityModel)
#No predictors are significant, so I will keep the three most significant and see how it impacts AIC (AIC = 263,984)
severityModel <- vglm(claim_amount~energy_backup_score+supply_chain_index+safety_compliance,family=gpd(),data=bus_int)
summary(severityModel)
#Still no significance, will remove one more (AIC = 263,979.9)
severityModel <- vglm(claim_amount~energy_backup_score+safety_compliance,family=gpd(),data=bus_int)
summary(severityModel)
#Still no significance, will leave it at this (AIC = 263,978.9)

#Number of mines
helmines <- 30
baymines <- 15
orymines <- 10

#Maintenance Frequency
#Maintenance intensity for each piece of equipment
helionisMaint <- c(750,750,375,1500,1500,1000)
bayesiaMaint<- c(600,600,400,1000,1000,750)
orynMaint <- c(500,500,250,300,300,500)
#Average maintenance intensity, based on intensity per unit and number of units
(750*300 + 750*240 + 375*150 +1500*300 +1500*1500 +1000*90)/(300+240+150+300+1500+90)
#Helionis has avg maintenance intensity of 1260.174hrs
helionisMFreq <- (24*365)/1260.17
(600*150 + 600*120 + 400*75 + 1000*150 + 1000*750 + 750*45 )/(150+120+75+150+750+45)
#Bayesia has avg maintenance intensity of 872.6744
bayesiaMFreq <- (24*365)/872.6744
(500*100 + 500*80 + 250*50 + 300*100 + 300*500 + 500*30)/(100+80+50+100+500+30)
#Oryn has avg maintenance intensity of 345.9302
orynMFreq <- (24*365)/345.9302


helionisDf <- data_frame(system="Helionis",
                         number=helmines,
                         energy_backup_score=mean(bus_int$energy_backup_score),
                         supply_chain_index=mean(bus_int$supply_chain_index),
                         maintenance_freq=helionisMFreq,
                         safety_compliance=mean(bus_int$safety_compliance),
                         exposure=1)
bayesiaDf <- data_frame(system="Bayesia",
                        number=baymines,
                        energy_backup_score=mean(bus_int$energy_backup_score),
                        supply_chain_index=mean(bus_int$supply_chain_index),
                        maintenance_freq=bayesiaMFreq,
                        safety_compliance=mean(bus_int$safety_compliance),
                        exposure=1)
orynDf <- data_frame(system="Oryn",
                     number=orymines,
                     energy_backup_score=mean(bus_int$energy_backup_score),
                     supply_chain_index=mean(bus_int$supply_chain_index),
                     maintenance_freq=orynMFreq,
                     safety_compliance=mean(bus_int$safety_compliance),
                     exposure=1)

predictorDf <- rbind(helionisDf,bayesiaDf,orynDf) 
predictorDf$mu_freq <- predict(frequencyModel,predictorDf,type="response") 
predictorDf$size <- sigma(frequencyModel) 

coef_matrix <- coef(severityModel,matrix=TRUE)
predictorDf$mu_par <- exp(coef_matrix[1,1]+coef_matrix[2,1]*predictorDf$energy_backup_score+
                            coef_matrix[3,1]*predictorDf$safety_compliance)
predictorDf$alpha_par <- coef_matrix[1,2]
#min = mu_par, shape = alpha_par
#Get shape2 and scale from my par4fit

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

all_claim_counts <- unlist(all_claim_counts)
all_claim_sizes <- unlist(all_claim_sizes)

hist(totalLosses,main="Total Loss Distribution", xlab="Total Loss")
summary(totalLosses)
#Min of 0, max of 34,337,423, mean of 6,105,802

hist(all_claim_counts)
summary(all_claim_counts)
#Min of 0, max of 20, mean of 1.655

hist(all_claim_sizes)
summary(all_claim_sizes)
#Min of 0, max of 28,164,505, mean of 2,035,267

var <- var(totalLosses)
mean <- mean(totalLosses)
sd<- sqrt(var)

#95% cost range
quantile(totalLosses, probs=0.975)
#Upper Bound of Costs: 14,597,556
quantile(totalLosses,probs=0.025)
#Lower Bound of Costs: 126,025.1 

#Want a 3% profit above expected loss, using standard deviation principle
#Which simplifies to alpha*standard deviation / mean = 3%
prof <- numeric(200)
alphalist <- numeric(200)
for (i in 1:200) {
  alphalist[i] <- i/100
  prof[i] <- i*sd/mean
}
prof <- data.frame(alphalist,prof)
View(prof)
#Alpha = 0.05 is first above 3%

prem <- mean + 0.05*sd
#Total premium is $6,290,203
(6290203/1000000)/61491*100
#Which 0.01% of CQMC's total profit in 2174


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

stresspredictorDf <- rbind(stresshelionisDf,stressbayesiaDf,stressorynDf) 
stresspredictorDf$mu_freq <- predict(frequencyModel,stresspredictorDf,type="response") 
stresspredictorDf$size <- sigma(frequencyModel) 

stresscoef_matrix <- coef(severityModel,matrix=TRUE)
stresspredictorDf$mu_par <- exp(stresscoef_matrix[1,1]+stresscoef_matrix[2,1]*stresspredictorDf$energy_backup_score+
                                  stresscoef_matrix[3,1]*stresspredictorDf$safety_compliance)
stresspredictorDf$alpha_par <- stresscoef_matrix[1,2]
#min = mu_par, shape = alpha_par
#Get shape2 and scale from my par4fit

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

stress_claim_counts <- unlist(stress_claim_counts)
stress_claim_sizes <- unlist(stress_claim_sizes)

hist(stresstotalLosses,main="Total Loss Distribution", xlab="Total Loss")
summary(stresstotalLosses)
#Unstressed has: Min of 0, max of 34,337,423, mean of 6,105,802
#Stressed has: Min of 0, max of 38,982,078, mean of 7,012,533

hist(stress_claim_counts)
summary(stress_claim_counts)
#Unstressed has: Min of 0, max of 20, mean of 1.655
#Stressed has: Min of 0, max of 23, mean of 1.9

hist(stress_claim_sizes)
summary(stress_claim_sizes)
#Unstressed has: Min of 0, max of 28,164,505, mean of 2,035,267
#Stressed has:Min of 0, max of 32,982,078 , mean of 2,337,511

stressvar <- var(stresstotalLosses)
stressmean <- mean(stresstotalLosses)
stresssd<- sqrt(stressvar)

#Unstressed Mean:$6,105,802 Stressed Mean:$7,012,533
#Unstressed SD: $3,688,022 Stressed SD:$4,024,464

#95% cost range
quantile(stresstotalLosses, probs=0.975)
#Unstressed has: Upper Bound of Costs: 14,597,556
#Stressed has: 16,234,678
quantile(stresstotalLosses,probs=0.025)
#Unstressed has: Lower Bound of Costs: 126,025.1 
#Stressed has: 557,121.2
