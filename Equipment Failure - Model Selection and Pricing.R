#Ensure that data cleaning code has been run first
#Finding frequency distribution for entire population
library(MASS)
library(fitdistrplus)
library(actuar)
##Option One: Binomial
#MLE as fitdist doesn't work
#Number of successes/number of trials for p
ebipf <- sum(equipment_failure_f$claim_count)/length(equipment_failure_f$claim_count)
#Comparing Empirical CDF
exgridf <- seq(min(equipment_failure_f$claim_count), max(equipment_failure_f$claim_count), by=1)
eempirical <- ecdf(equipment_failure_f$claim_count)
plot(exgridf,eempirical(exgridf),type="l",xlab="claims",ylab="cdf",main="ECDF")
lines(exgridf,pbinom(exgridf,size=3,prob=ebipf),col=2)
#P-P Plot
plot(pbinom(equipment_failure_f$claim_count, size=3, prob=ebipf), eempirical(equipment_failure_f$claim_count),
     xlab= "theoretical probability", ylab= "sample probability", main="P-P plot / Binomial", cex=0.45)
abline(0,1,col=3)
#Chi-squared test
chisq.test(table(equipment_failure_f$claim_count),p=dbinom(exgridf,size=3,prob=ebipf))
#Gave p-value < 2.2e-16, so reject the null hypothesis that they are the same
#AIC
ebinloglik<- sum(dbinom(equipment_failure_f$claim_count,size=3,prob=ebipf,log=TRUE))
2-2*ebinloglik
#Gave 66,893.99
#Mean and variance from binomial
3*ebipf
#Mean of 0.235
3*ebipf*(1-ebipf)
#Variance of 0.217

#Data has:
mean(equipment_failure_f$claim_count)
#Mean of 0.078
var(equipment_failure_f$claim_count)
#Variance of 0.082

##Option Two: Poisson
#MLE
epoisfit <- fitdist(equipment_failure_f$claim_count,"pois")
#Comparing Empirical CDF
cdfcomp(epoisfit)
#Almost exactly the same
#P-P Plot
plot(ppois(equipment_failure_f$claim_count, lambda=epoisfit$estimate), eempirical(equipment_failure_f$claim_count),
     xlab= "theoretical probability", ylab= "sample probability", main="P-P plot / Poisson", cex=0.45)
abline(0,1,col=3)
#Goodness-of-Fit Stats
gofstat(epoisfit)
#Chi-squared test gave p-value of 1.99e-15, so reject the null hypothesis that they are the same
#AIC gave 52,648.18
#Mean and variance from Poisson
epoisfit$estimate
#Mean and variance of 0.078

#Data has:
mean(equipment_failure_f$claim_count)
#Mean of 0.078
var(equipment_failure_f$claim_count)
#Variance of 0.082

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

#Data has:
mean(equipment_failure_f$claim_count)
#Mean of 0.078
var(equipment_failure_f$claim_count)
#Variance of 0.082

#Therefore, frequency data is Negative Binomial with size = 1.6508 and mu = 0.0784
#As equipment is all proportional between mining sites, no use in splitting it into equipment types

###Severity###
###Model Selection - Claims Severity###
##Option One: Weibull
eweifit<-fitdist(equipment_failure$claim_amount,"weibull")
#Comparing Histograms
hist(equipment_failure$claim_amount, breaks=100, prob=T, xlab="claims", main = "Histogram / Weibull")
exgrids<- seq(min(equipment_failure$claim_amount), max(equipment_failure$claim_amount), length=1000)
lines(exgrids, dweibull(exgrids,eweifit$estimate[1], eweifit$estimate[2]),col=2)
#Comparing Empirical CDF
cdfcomp(eweifit)
#P-P Plot
eempiricals <- ecdf(equipment_failure$claim_amount)
plot(pweibull(equipment_failure$claim_amount, eweifit$estimate[1], eweifit$estimate[2]), eempiricals(equipment_failure$claim_amount),
     xlab= "theoretical probability", ylab= "sample probability", main="P-P plot / Weibull", cex=0.45)
abline(0,1,col=3)
#Goodness-of-Fit Stats
gofstat(eweifit)
#K-S Test gave 0.0770
#AIC gave 196,121
#Mean and variance from Weibull
ek <- eweifit$estimate[1]
el <- eweifit$estimate[2]
el*gamma(1+1/ek)
#Mean of 86,753.38
el^2*(gamma(1+2/ek)-(gamma(1+1/ek))^2)
#Variance of 3,112,346,040

#Data has:
mean(equipment_failure$claim_amount)
#Mean of 87,349.9
var(equipment_failure$claim_amount)
#Variance of 3,795,874,232
sqrt(3795874232)

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
egamfit<-gammle(equipment_failure$claim_amount)
#Gave shape of 2.919 and scale of 29,922.4
#Comparing Histograms
hist(equipment_failure$claim_amount, breaks=100, prob=T, xlab="claims", main = "Histogram / Gamma")
lines(exgrids, dgamma(exgrids, shape=egamfit$shape, scale=egamfit$scale),col=2)
#Comparing Empirical CDF
eempiricals <- ecdf(equipment_failure$claim_amount)
plot(exgrids,eempiricals(exgrids), type="l", xlab="claims", ylab="cdf", main = "ECDF")
lines(exgrids, pgamma(exgrids, shape=egamfit$shape, scale=egamfit$scale),col=2)
#P-P Plot
plot(pgamma(equipment_failure$claim_amount, shape=egamfit$shape, scale=egamfit$scale), eempiricals(equipment_failure$claim_amount),
     xlab= "theoretical probability", ylab= "sample probability", main="P-P plot / Gamma", cex=0.45)
abline(0,1,col=3)
#K-S Test
ksegam<-eempiricals(exgrids)-pgamma(exgrids, shape=egamfit$shape, scale=egamfit$scale)
max(abs(ksegam))
#Gave 0.0478
#AIC
egamloglik <- sum((egamfit$shape-1)*log(equipment_failure$claim_amount)-equipment_failure$claim_amount/egamfit$scale-egamfit$shape*log(egamfit$scale)-lgamma(egamfit$shape))
2*2 - 2*egamloglik
#Gave 195,040.4
#Mean and variance from Gamma
egamfit$shape*egamfit$scale
#Mean of 87,349.9
egamfit$shape*(egamfit$scale^2)
#Variance of 2,613,719,002

#Data has:
mean(equipment_failure$claim_amount)
#Mean of 87,349.9
var(equipment_failure$claim_amount)
#Variance of 3,795,874,232

##Option Three: Normal
#MLE
enormfit<-fitdist(equipment_failure$claim_amount,"norm")
#Comparing Histograms
hist(equipment_failure$claim_amount, breaks=100, prob=T, xlab="claims", main = "Histogram / Normal")
lines(exgrids, dnorm(exgrids,enormfit$estimate[1], enormfit$estimate[2]),col=2)
#Comparing Empirical CDF
cdfcomp(enormfit)
#P-P Plot
plot(pnorm(equipment_failure$claim_amount, enormfit$estimate[1], enormfit$estimate[2]), eempiricals(equipment_failure$claim_amount),
     xlab= "theoretical probability", ylab= "sample probability", main="P-P plot / Normal", cex=0.45)
abline(0,1,col=3)
#Goodness-of-Fit Stats
gofstat(enormfit)
#K-S Test gave 0.1329
#AIC gave 200,034.8
#Mean and variance from Normal
enormfit$estimate[1]
#Mean of 87,349.9
enormfit$estimate[2]^2
#Variance of 3,795,401,815

#Data has:
mean(equipment_failure$claim_amount)
#Mean of 87,349.9
var(equipment_failure$claim_amount)
#Variance of 3,795,874,232

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

#Data has:
mean(equipment_failure$claim_amount)
#Mean of 87,349.9
var(equipment_failure$claim_amount)
#Variance of 3,795,874,232

##Option Five: Exponential
#MLE
eexpfit<-fitdistr(equipment_failure$claim_amount,"exponential")
#Comparing Histograms
hist(equipment_failure$claim_amount, breaks=100, prob=T, xlab="claims", main = "Histogram / Exponential")
lines(exgrids, dexp(exgrids,eexpfit$estimate[1]),col=2)
#Comparing Empirical CDF
plot(exgrids,eempiricals(exgrids), type="l", xlab="claims", ylab="cdf", main = "ECDF")
lines(exgrids, pexp(exgrids, eexpfit$estimate[1]),col=2)
#P-P Plot
plot(pexp(equipment_failure$claim_amount, eexpfit$estimate[1]), eempiricals(equipment_failure$claim_amount),
     xlab= "theoretical probability", ylab= "sample probability", main="P-P plot / Exponential", cex=0.45)
abline(0,1,col=3)
#K-S Test
kseexp<-eempiricals(exgrids)-pexp(exgrids, eexpfit$estimate[1])
max(abs(kseexp))
#Gave 0.2286
#AIC
AIC(eexpfit,k=2)
#Gave 198,911.3

##Option Six: Pareto
eparfit<- fitdist(equipment_failure$claim_amount,"pareto")
#Comparing Histograms
hist(equipment_failure$claim_amount,breaks=100,prob=TRUE,xlab="claims", main= "Histogram / Pareto")
lines(exgrids,dpareto(exgrids,shape=eparfit$estimate[1],scale=eparfit$estimate[2]),col=2)
#Comparing ECDF
cdfcomp(eparfit)
#P-P Plot
plot(ppareto(equipment_failure$claim_amount, shape=eparfit$estimate[1], scale=eparfit$estimate[2]), eempiricals(equipment_failure$claim_amount),
     xlab= "theoretical probability", ylab= "sample probability", main="P-P plot / Pareto", cex=0.45)
abline(0,1,col=3)
#Goodness-of-Fit Stats
gofstat(eparfit)
#K-S Test gave 0.2296
#AIC gave 198,913.3

#Severity is a Log-Normal distribution with mu 11.1967 and sigma 0.5932


###Aggregate Loss Distribution
library(glmmTMB)
efrequencyModel <- glmmTMB(claim_count~equipment_type+equipment_age+maintenance_int+usage_int+offset(log(exposure)),data=equipment_failure_f,family=nbinom2)
eseverityModel <- glm(log(claim_amount)~equipment_type+equipment_age+maintenance_int+usage_int,data=equipment_failure,family=gaussian)
#Equipment Type, I have in data
#Equipment age, I can take from service years
#Maintenance int, I can take from maintenance schedule
#Usage int, I can take from equipment usage

summary(efrequencyModel)
#All predictors are significant
summary(eseverityModel)
#Maintenance int is not significant, so I will remove (AIC = 11,984)
eseverityModel <- glm(log(claim_amount)~equipment_type+equipment_age+usage_int,data=equipment_failure,family=gaussian)
summary(eseverityModel)
#All remaining predictors are significant (AIC = 11,983)

#Equipment Type
equipment <- c("Quantum Bore", "Graviton Extractor", "FexStram Carrier", "ReglAggregators", 
               "Flux Rider", "Ion Pulverizer")
helionisEquipment <- c(300,240,150,300,1500,90)
bayesiaEquipment <- c(150,120,75,150,750,45)
orynEquipment <- c(100,80,50,100,500,30)

#Equipment Age
helionisqAge <- (((5+0)/2)*30 + ((9+5)/2)*45 + ((14+10)/2)*180 + ((19+15)/2)*30 + 20*15)/(30+45+180+30+15)
helionisgAge <- (((5+0)/2)*24 + ((9+5)/2)*36 + ((14+10)/2)*144 + ((19+15)/2)*24 + 20*12)/(24+36+144+24+12)
helionisfAge <- (((5+0)/2)*15 + ((9+5)/2)*23 + ((14+10)/2)*89 + ((19+15)/2)*15 + 20*8)/(15+23+89+15+8)
helionisrAge <- (((5+0)/2)*30 + ((9+5)/2)*45 + ((14+10)/2)*180 + ((19+15)/2)*30 + 20*15)/(30+45+180+30+15)
helionisflAge <- (((5+0)/2)*150 + ((9+5)/2)*225 + ((14+10)/2)*900 + ((19+15)/2)*150 + 20*75)/(150+225+900+150+75)
helionisiAge <- (((5+0)/2)*9 + ((9+5)/2)*14 + ((14+10)/2)*53 + ((19+15)/2)*9 + 20*5)/(9+14+53+9+5)
helionisAge <- c(helionisqAge,helionisgAge,helionisfAge,helionisrAge,helionisflAge,helionisiAge)

bayesiaqAge <- (((5+0)/2)*45 + ((9+5)/2)*38 + ((14+10)/2)*59 + ((19+15)/2)*8)/(45+38+59+8)
bayesiagAge <- (((5+0)/2)*36 + ((9+5)/2)*30 + ((14+10)/2)*48 + ((19+15)/2)*6)/(36+30+48+6)
bayesiafAge <- (((5+0)/2)*23 + ((9+5)/2)*19 + ((14+10)/2)*29 + ((19+15)/2)*4)/(23+19+29+4)
bayesiarAge <- (((5+0)/2)*45 + ((9+5)/2)*38 + ((14+10)/2)*59 + ((19+15)/2)*8)/(45+38+59+8)
bayesiaflAge <- (((5+0)/2)*225 + ((9+5)/2)*187 + ((14+10)/2)*300 + ((19+15)/2)*38)/(225+187+300+38)
bayesiaiAge <- (((5+0)/2)*14 + ((9+5)/2)*11 + ((14+10)/2)*18 + ((19+15)/2)*2)/(14+11+18+2)
bayesiaAge <- c(bayesiaqAge,bayesiagAge,bayesiafAge,bayesiarAge,bayesiaflAge,bayesiaiAge)

orynqAge <- (((5+0)/2)*75 + ((9+5)/2)*15 + ((14+10)/2)*10)/(75+15+10)
oryngAge <- (((5+0)/2)*60 + ((9+5)/2)*12 + ((14+10)/2)*8)/(60+12+8)
orynfAge <- (((5+0)/2)*37 + ((9+5)/2)*8 + ((14+10)/2)*5)/(37+8+5)
orynrAge <- (((5+0)/2)*75 + ((9+5)/2)*15 + ((14+10)/2)*10)/(75+15+10)
orynflAge <- (((5+0)/2)*375 + ((9+5)/2)*75 + ((14+10)/2)*50)/(375+75+50)
oryniAge <- (((5+0)/2)*22 + ((9+5)/2)*5 + ((14+10)/2)*3)/(22+5+3)
orynAge <- c(orynqAge,oryngAge,orynfAge,orynrAge,orynflAge,oryniAge)

#Maintenance int
helionisMaint <- c(750,750,375,1500,1500,1000)
bayesiaMaint<- c(600,600,400,1000,1000,750)
orynMaint <- c(500,500,250,300,300,500)

#Usage Int
helionisUsage <- c(0.95*24,0.95*24,0.9*24,0.8*24,0.8*24,0.5*24)
bayesiaUsage <- c(0.8*24,0.8*24,0.75*24,0.75*24,0.8*24,0.6*24)
orynUsage <- c(0.75*24,0.75*24,0.7*24,0.7*24,0.75*24,0.5*24)

ehelionisDf <- data_frame(system=rep("Helionis",6),
                          equipment_type=equipment,
                          number=helionisEquipment,
                          equipment_age=helionisAge,
                          maintenance_int=helionisMaint,
                          usage_int=helionisUsage,
                          exposure=rep(1,6))
ebayesiaDf <- data_frame(system=rep("Bayesia",6),
                         equipment_type=equipment,
                         number=bayesiaEquipment,
                         equipment_age=bayesiaAge,
                         maintenance_int=bayesiaMaint,
                         usage_int=bayesiaUsage,
                         exposure=rep(1,6))
eorynDf <- data_frame(system=rep("Oryn",6),
                      equipment_type=equipment,
                      number=orynEquipment,
                      equipment_age=orynAge,
                      maintenance_int=orynMaint,
                      usage_int=orynUsage,
                      exposure=rep(1,6))

epredictorDf <- rbind(ehelionisDf,ebayesiaDf,eorynDf) 
epredictorDf$mu_freq <- predict(efrequencyModel,epredictorDf,type="response") 
epredictorDf$size <- sigma(efrequencyModel) 

epredictorDf$mean_log <- predict(eseverityModel,epredictorDf,type="response") 
epredictorDf$sd_log <- sigma(eseverityModel) 

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

eall_claim_counts <- unlist(eall_claim_counts)
eall_claim_sizes <- unlist(eall_claim_sizes)

hist(etotalLosses,main="Total Loss Distribution", xlab="Total Loss")
summary(etotalLosses)
#Min of 90,330,548, max of 123,224,685, mean of 105,834,251

hist(eall_claim_counts)
summary(eall_claim_counts)
#Min of 0, max of 732, mean of 73.52

hist(eall_claim_sizes)
summary(eall_claim_sizes)
#Min of 0, max of 53,811,707, mean of 5,879,681

evar <- var(etotalLosses)
emean <- mean(etotalLosses)
esd<- sqrt(evar)

#95% cost range
quantile(etotalLosses, probs=0.975)
#Upper Bound of Costs: 112,811,760
quantile(etotalLosses,probs=0.025)
#Lower Bound of Costs: 99,029,946

#Want a 2.5% profit above expected loss, using standard deviation principle
#Which simplifies to alpha*standard deviation / mean = 2.5%
eprof <- numeric(200)
ealphalist <- numeric(200)
for (i in 1:200) {
  ealphalist[i] <- i/100
  eprof[i] <- i*esd/emean
}
eprof <- data.frame(ealphalist,eprof)
View(eprof)
#Alpha = 0.76 is first above 2.5%

eprem <- emean + 0.76*esd
#Total premium is $108,505,130
(108505130/1000000)/61491*100
#Which 0.18% of CQMC's total profit in 2174


##Stress Test 2##
#Equipment Age is increasing by 5 years from mean
stresshelionisAge <- mean(helionisAge)+5
stressbayesiaAge <- mean(bayesiaAge)+5
stressorynAge <- mean(orynAge)+5

#Maintenance int is doubling
stresshelionisMaint <- helionisMaint*2
stressbayesiaMaint<- bayesiaMaint*2
stressorynMaint <- orynMaint*2

#Usage Int is increasing
stresshelionisUsage <- c(1*24,1*24,1*24,1*24,1*24,0.75*24)
stressbayesiaUsage <- c(1*24,1*24,0.75*24,0.75*24,1*24,0.75*24)
stressorynUsage <- c(1*24,1*24,0.75*24,0.75*24,0.75*24,0.75*24)

stressehelionisDf <- data_frame(system=rep("Helionis",6),
                                equipment_type=equipment,
                                number=helionisEquipment,
                                equipment_age=rep(stresshelionisAge,6),
                                maintenance_int=stresshelionisMaint,
                                usage_int=stresshelionisUsage,
                                exposure=rep(1,6))
stressebayesiaDf <- data_frame(system=rep("Bayesia",6),
                               equipment_type=equipment,
                               number=bayesiaEquipment,
                               equipment_age=rep(stressbayesiaAge,6),
                               maintenance_int=stressbayesiaMaint,
                               usage_int=stressbayesiaUsage,
                               exposure=rep(1,6))
stresseorynDf <- data_frame(system=rep("Oryn",6),
                            equipment_type=equipment,
                            number=orynEquipment,
                            equipment_age=rep(stressorynAge,6),
                            maintenance_int=stressorynMaint,
                            usage_int=stressorynUsage,
                            exposure=rep(1,6))

stressepredictorDf <- rbind(stressehelionisDf,stressebayesiaDf,stresseorynDf) 
stressepredictorDf$mu_freq <- predict(efrequencyModel,stressepredictorDf,type="response") 
stressepredictorDf$size <- sigma(efrequencyModel) 

stressepredictorDf$mean_log <- predict(eseverityModel,stressepredictorDf,type="response") 
stressepredictorDf$sd_log <- sigma(eseverityModel) 

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
estress_claim_counts <- list()
estress_claim_sizes <- list()
stressetotalLosses <- numeric(1000000)

for (i in seq(1,1000000,1)) { 
  claim_counts_list <- mapply(ernbinomFunc,stressepredictorDf$number, 
                              stressepredictorDf$mu_freq,stressepredictorDf$size)
  estress_claim_counts[[i]] <- unlist(claim_counts_list)
  claim_sizes_list <- mapply(erlnormFunc,claim_counts_list, 
                             stressepredictorDf$mean_log,stressepredictorDf$sd_log)
  estress_claim_sizes[[i]] <- unlist(claim_sizes_list)
  stressetotalLosses[i] <- sum(unlist(claim_sizes_list)) 
}

estress_claim_counts <- unlist(estress_claim_counts)
estress_claim_sizes <- unlist(estress_claim_sizes)

hist(stressetotalLosses,main="Total Loss Distribution", xlab="Total Loss")
summary(stressetotalLosses)
#Unstressed: Min of 90,330,548, max of 123,224,685, mean of 105,834,251
#Stressed: Min 272,950,089, max of 342,432,653, mean of 303,396,504

hist(estress_claim_counts)
summary(estress_claim_counts)
#Unstressed: Min of 0, max of 732, mean of 73.52
#Stressed:Min of 0, max of 1754, mean of 173

hist(estress_claim_sizes)
summary(estress_claim_sizes)
#Unstressed: Min of 0, max of 53,811,707, mean of 5,879,681
#Stressed: Min of 0, max of 159,216,153, mean of 16,855,361

stressevar <- var(stressetotalLosses)
stressemean <- mean(stressetotalLosses)
stressesd<- sqrt(stressevar)

#Unstressed Mean:$105,834,251 Stressed Mean:$303,396,504
#Unstressed SD: $3,514,315 Stressed SD:$6,412,504

#95% cost range
quantile(stressetotalLosses, probs=0.975)
#Unstressed: Upper Bound of Costs: 112,811,760
#Stressed: $316,058,228
quantile(stressetotalLosses,probs=0.025)
#Unstressed: Lower Bound of Costs: 99,029,946
#Stressed:290,914,565