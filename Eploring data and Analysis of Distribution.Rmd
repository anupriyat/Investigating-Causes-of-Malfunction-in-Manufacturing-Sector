---
title: "LNL_Course_Proj_Part1"
author: "Anupriya Thirumurthy"
date: "8/22/2018"
output:
  pdf_document: default
  html_document: default
---

# 1. Problem Description

The business analytics group of a company is asked to investigate causes of malfunctions in technological process of one of the manufacturing plants that result in significant increase of cost for the end product of the business.

One of suspected reasons for malfunctions is deviation of temperature during the technological process from optimal levels. The sample in the provided file contains times of malfunctions in seconds since the start of measurement and minute records of temperature.

```{r}
conditionalEcho<-F
```

# 2. Data

```{r 1}
dataPath <- "/Users/anupriyathirumurthy/Documents/University/MScA_UoC/Courses/LinearAndNonLinearModels/Project"
Course.Project.Data<-read.csv(file=paste(dataPath,"MScA_LinearNonLinear_MalfunctionData.csv",sep="/"))
```

# 3. Creating Counting Process & Exploring Cumulative Intensity

Counting process is a step function of time that jumps by 1 at every moment of a new malfunction event being logged.

```{r 2}
Counting.Process<-as.data.frame(cbind(Time=Course.Project.Data$Time,Count=1:length(Course.Project.Data$Time)))
Counting.Process[1:20,]
```

```{r}
plot(Counting.Process$Time,Counting.Process$Count,type="s")
```

The counting process trajectory looks pretty smooth and grows steadily.

## Character of malfunctions and the reasons causing them

The above graph shows Time Vs Malfunction Counts.

We could infer that the number of malfunctions is directly proportional to the length of the period during which they are observed. The graph has a stead increase. We also notice that the rate at which events occur is constant. These inferences on the counts makes us suspicious that it might be a Poisson model.

# 3.1 Exploring cumulative intensity of the process.

Cumulative intensity = Nt/t, 
where,
Nt is the number of events during the time interval [0,t]
For our data t is the sequence of time stamps and Nt is the count up until t.

```{r}
plot(Counting.Process$Time,Counting.Process$Count/Counting.Process$Time,type="l",ylab="Cumulative Intensity", xlab = "Time")
abline(h=Counting.Process$Count[length(Counting.Process$Count)]/
         Counting.Process$Time[length(Counting.Process$Time)], col = "red")
abline(h=mean(Counting.Process$Count/Counting.Process$Time), col = "purple")
legend("bottomright", legend=c("Mean cummulative intensity", "last cumulative intensity"),
       col=c("purple", "red"), lty=1, cex=1.0, title="Intensity levels")
```

The cumulative intensity seems to converge to a stable level.

```{r}
c(Last.Intensity=Counting.Process$Count[length(Counting.Process$Count)]/
         Counting.Process$Time[length(Counting.Process$Time)],
  Mean.Intensity=mean(Counting.Process$Count/Counting.Process$Time))
```

# 4. Checking for over-dispersion.

Checking for overdispersion by calculating the one-minute event counts and temperatures. 

Looking at the first 20 rows of the data.

```{r}
Course.Project.Data[1:29,]
# The time column is in seconds.
```

Note that the first 7 rows (events) occurred during the first minute. The temperature measurement for the first minute was 91.59307 degree F.

The following 10 rows happen during the second minute and the second minute temperature is 97.3086 degree F.

The third minute had 7 events at temperature 95.98865 degree F.

The fourth minute had 4 events at 100.3844 degree F.

And the following fifth minute had only 1 event at 99.9833 degree F.

## Constructing a data frame of one-minute counts and the corresponding temperatures 

```{r}
library(magrittr)

suppressMessages(library(plyr))

Minute.Temps <- Course.Project.Data$Temperature
One.Minute.Counts.Temps <- 
  Course.Project.Data %>% cbind(Count=Counting.Process$Count) %>%
    ddply( ~Minute.Temps, function(minute_frame) {
      data.frame(Minute.times = unique(floor(minute_frame$Time/60)*60+30), # The column Minute.Times contains mids of minute periods.
               Minute.counts = nrow(minute_frame))
    })

# Reorder by time recorded
One.Minute.Counts.Temps <- One.Minute.Counts.Temps[order(One.Minute.Counts.Temps$Minute.times), c(2,3,1)]

rownames(One.Minute.Counts.Temps) <- NULL
colnames(One.Minute.Counts.Temps) <- c('Minute.times','Minute.counts','Minute.Temps' )

unique(diff(One.Minute.Counts.Temps$Minute.times))

ExtraMinutes<- data.frame(Minute.times = NULL, Minute.counts = NULL, Minute.Temps = NULL);

for (i in 2:nrow(One.Minute.Counts.Temps)) {
    if((One.Minute.Counts.Temps$Minute.times[i] - 
        One.Minute.Counts.Temps$Minute.times[i-1]) != 60) {
      ExtraRow <- data.frame(Minute.times = (One.Minute.Counts.Temps$Minute.times[i] + 
                                               One.Minute.Counts.Temps$Minute.times[i-1])/2,
                             Minute.counts = 0,
                             Minute.Temps =NA)
      ExtraMinutes <- rbind(ExtraMinutes, ExtraRow)
    }
}

One.Minute.Counts.Temps <- rbind(One.Minute.Counts.Temps, ExtraMinutes)
One.Minute.Counts.Temps <- One.Minute.Counts.Temps[order(One.Minute.Counts.Temps$Minute.times),]
head(One.Minute.Counts.Temps)

head(One.Minute.Counts.Temps)
```

Plot of the One-Minute Counts

```{r}
plot(One.Minute.Counts.Temps$Minute.times,One.Minute.Counts.Temps$Minute.counts)
```

# 4.1 Methods for Testing Over-Dispersion

## 4.1.1 A quick and rough method.

Looking at the output of glm() and comparing the residual deviance with the number of degrees of freedom. If the assumed model is correct, deviance is asymptotically distributed as Chi-squared2 with degrees of freedom n k where n is the number of observations and k is the number of parameters. For a 2 distribution the mean is the number of degrees of freedom n k. If the residual deviance returned by glm() is greater than n k, then it might be a sign of over-dispersion.

Test the method on simulated Poisson data.

The function simulates a sample from Poisson distribution, estimates parameter which is simultaneously the mean value and the variance, then it checks if DevianceDeg.Freedom 1 belongs to the interval (   1.96,1.96]. If yes, the result is 1. Otherwise it is 0.

```{r}
Test.Deviance.Overdispersion.Poisson<-function(Sample.Size,Parameter.Lambda){
  my.Sample<-rpois(Sample.Size,Parameter.Lambda)
  Model<-glm(my.Sample~1,family=poisson)
  Dev<-Model$deviance
  Deg.Fred<-Model$df.residual
  (((Dev/Deg.Fred-1)/sqrt(2/Deg.Fred)>-1.96)&((Dev/Deg.Fred-1)/sqrt(2/Deg.Fred)<=1.96))*1
} 
Test.Deviance.Overdispersion.Poisson(100,1)
```

Now repeat the call of the function 300 times to see how many times it returns one and how many times zero.

```{r}
sum(replicate(300,Test.Deviance.Overdispersion.Poisson(100,1)))
```

We see that the Poisson distribution passes the test. 

The estimate of the parameter given by glm() is e^Coefficient:

```{r}
exp(glm(rpois(1000,2)~1,family=poisson)$coeff)
```

Perform the same test on negative binomial data

```{r}
Test.Deviance.Overdispersion.NBinom<-function(Sample.Size,Parameter.prob){
  my.Sample<-rnbinom(Sample.Size,2,Parameter.prob)
  Model<-glm(my.Sample~1,family=poisson)
  Dev<-Model$deviance
  Deg.Fred<-Model$df.residual
  (((Dev/Deg.Fred-1)/sqrt(2/Deg.Fred)>-1.96)&((Dev/Deg.Fred-1)/sqrt(2/Deg.Fred)<=1.96))*1
} 
sum(replicate(300,Test.Deviance.Overdispersion.NBinom(100,.2)))
```

We see that the over-dispersed negative binomial distribution sample never passes the test.

Now apply the test to the one-minute event counts.

```{r}
GLM.model<-glm(One.Minute.Counts.Temps$Minute.counts~1,family=poisson)
GLM.model
```

## Signs of over-dispersion?

I see signs of over-dispersion because this is not a good model as we can see that the residual deviance is is higher than the number of degrees of freedom.

# 4.1.2 Regression test by Cameron-Trivedi

The test implemented in AER is described in Cameron, A.C. and Trivedi, P.K. (1990). Regression-based Tests for Over-dispersion in the Poisson Model. Journal of Econometrics, 46, 347 364.

In a Poisson model, the mean is E(Y)=?? and the variance is V(Y)=?? as well.
They are equal. The test has a null hypothesis c=0 where Var(Y)=??+c???f(??), c<0 means under-dispersion and c>0 means over-dispersion.
The function f(.) is some monotonic function (linear (default) or quadratic).
The test statistic used is a t statistic which is asymptotically standard normal under the null.

```{r}
suppressMessages(library(car))
suppressWarnings(library(AER))

Disp.Test <- dispersiontest(object = GLM.model, alternative = 'two.sided')
Disp.Test
```

## Overdispersion?

Yes, the p-value is extremely small, thus we reject the null hypothesis that its poisson(The hypothesis H0 : ???dispersion=1???) and accept the alternative hypothesis of dispresion being not equal to 1. We also see that the estimated dispersion is around 7 confirming that there are signs of over dispersion.

# 4.1.3 Test against Negative Binomial Distribution

The null hypothesis of this test is that the distribution is Poisson as particular case of Negative binomial against Negative Binomial.

The references are:
A. Colin Cameron and Pravin K. Trivedi (1998) Regression analysis of count data. New York: Cambridge University Press.
Lawless, J. F. (1987) Negative Binomial and Mixed Poisson Regressions. The Canadian Journal of Statistics. 15:209-225.

Required packages are MASS (to create a negative binomial object with glm.nb) and pscl the test function is odTest.

```{r}
library("MASS")
GLM.model.nb <- glm.nb(One.Minute.Counts.Temps$Minute.counts ~ 1)
summary(GLM.model.nb)
```

Theta is so small which confirms that its negative binomial and there is overdispersion

```{r}
suppressMessages(library(pscl))
odTest(GLM.model.nb)
```

## Overdispersion?

Yes, p-value is so small and we reject the hypothesis, this shows overdispersion.

# 5. Find the distribution of Poisson intensity.

# 5.1. Kolmlgorov-Smirnov test.

Kolmogorov-Smirnov test is used to test hypotheses of equivalence between two empirical distributions or equivalence between one empirical distribution and one theoretical distribution.

```{r}
suppressWarnings(library(lattice))
suppressWarnings(library(latticeExtra))
sample1=rnorm(100)
sample2=rnorm(100,1,2)
Cum.Distr.Functions <- data.frame(sample1,sample2)
ecdfplot(~ sample1 + sample2, data=Cum.Distr.Functions, auto.key=list(space='right'))
```

Checking equivalence of empirical distributions for the two samples.

```{r}
ks.test(sample1,sample2)
```

## Equivalence of the two distributions?

p value is so small and we reject the null hypothesis of the Kolmogorov-Smirnov Test. Hence, It suggests that the two samples are not sampled from the same distribution.

Checking equivalence of empirical distribution of sample1 and theoretical distribution Norm(0,1).

```{r}
ks.test(sample1,"pnorm",mean=0,sd=1)
```

The null hypothesis that the two samples are drawn from the same distribution cannot be rejected at this level of p-value.

Check equivalence of the empirical distribution of sample2 and theoretical distribution Norm(0,1).

```{r}
ks.test(sample2,"pnorm",mean=0,sd=1)
```


p value is so small and we reject the null hypothesis of the Kolmogorov-Smirnov Test. Hence, It suggests that the two samples are not sampled from the same distribution.

# 5.2. Checking the distribution for the entire period.

Apply Kolmogorov-Smirnov test to Counting.Process$Time and theoretical exponential distribution with parameter equal to average intensity.

Here, the empirical distribution should be estimated for time intervals between malfunctions.

```{r}
Time.Intervals <- diff(Counting.Process$Time)
Mean.Intensity<- mean(Counting.Process$Count/Counting.Process$Time)
(KS.Test.Event.Intervals <- ks.test(Time.Intervals, "pexp", Mean.Intensity))
c(KS.Test.Event.Intervals$statistic,p.value=KS.Test.Event.Intervals$p.value)
```

Ploting empirical cumulative distribution function for time intervals between malfunctions.

```{r}
plot(ecdf(diff(Counting.Process$Time)), xlab = "Time Interval", ylab = "CDF", main = "ECDF")
```

## 5.3. Checking distribution of one-minute periods

Use at least 5 different candidates for distribution of Poisson intensity of malfunctions.

Find one-minute intensities Event.Intensities. Hint. One-minute intensity by definition is the number of events per unit of time (second).

```{r}
Event.Intensities <- One.Minute.Counts.Temps$Minute.counts/60
hist(Event.Intensities)
```

I guess for the distribution of time intervals between events, this histogram suggest a gamma or exponential distribution.

Suggesting 5 candidates for the distribution.

Fiting each of you 5 candidate distributions to Event.Intensities using fitdistr() from MASS.

Starting with fitting normal and exponential distributions first.

```{r}
Fitting.Normal <- fitdistr(Event.Intensities, densfun = "normal")
Fitting.Normal
```

```{r}
Fitting.Exponential <- fitdistr(Event.Intensities, densfun = "exponential")
Fitting.Exponential
```

Testing the fitted distributions with Kolmogorov-Smirnov test.

```{r}
KS.Normal <- ks.test(Event.Intensities, "pnorm", Fitting.Normal$estimate[1], Fitting.Normal$estimate[2])
c(KS.Normal$statistic,P.Value=KS.Normal$p.value)
KS.Normal
```

The null hypothesis that the two samples are drawn from the same distribution is rejected at this level of p-value. Thus, two samples are drawn from different distribution.

```{r}
KS.Exp <- ks.test(Event.Intensities, "pexp", Fitting.Exponential$estimate)
c(KS.Exp$statistic,P.Value=KS.Exp$p.value)
KS.Exp
```

The null hypothesis that the two samples are drawn from the same distribution is rejected at this level of p-value. Thus, two samples are drawn from different distribution.

Trying to fit gamma distribution directly using fitdistr()

```{r}
Event.Intensities.Mean <- mean(Event.Intensities)
Event.Intensities.Variance <- var(Event.Intensities)*(length(Event.Intensities)-1)/length(Event.Intensities)
(Moments.Rate <- Event.Intensities.Mean/Event.Intensities.Variance)
(Moments.Shape <- Event.Intensities.Mean^2/Event.Intensities.Variance)
```

Check gamma distribution with these parameters as a theoretical distribution using Kolmogorov-Smirnov test.

```{r}
KS.Test.Moments <- ks.test(Event.Intensities, "pgamma", Moments.Shape, Moments.Rate)
KS.Test.Moments
```

The null hypothesis that the two samples are drawn from the same distribution cannot be rejected at this level of p-value.

Finding at least 2 more candidates and test them by Kolmogorov-Smirnov.

```{r}
Event.Intensities.Nonzero <- ifelse(Event.Intensities==0,0.00001,Event.Intensities)

KS.Candidate.4.fit<-fitdistr(Event.Intensities.Nonzero,"lognormal")
KS.Candidate.4<-ks.test(Event.Intensities.Nonzero,"plnorm",KS.Candidate.4.fit$estimate[1],KS.Candidate.4.fit$estimate[2])
KS.Candidate.4
```

The null hypothesis that the two samples are drawn from the same distribution is rejected at this level of p-value. Thus, two samples are drawn from different distribution.

```{r}
minutes.intensities.vec <- unlist(One.Minute.Counts.Temps$Minute.counts)
minutes.intensities.vec <- ifelse(is.na(minutes.intensities.vec),0,minutes.intensities.vec)

KS.Candidate.5.fit<-fitdistr(minutes.intensities.vec,"poisson")
KS.Candidate.5<-ks.test(minutes.intensities.vec,"ppois",KS.Candidate.5.fit$estimate)
KS.Candidate.5
```

The null hypothesis that the two samples are drawn from the same distribution is rejected at this level of p-value. Thus, two samples are drawn from different distribution.

Collecting all estimated distributions together and making the best choice.

```{r}
rbind(KS.Moments=c(KS.Test.Moments$statistic,P.Value=KS.Test.Moments$p.value),
      KS.Candidate.4=c(KS.Candidate.4$statistic,P.Value=KS.Candidate.4$p.value),
      KS.Candidate.5=c(KS.Candidate.5$statistic,P.Value=KS.Candidate.5$p.value),
      KS.Exp=c(KS.Exp$statistic,P.Value=KS.Exp$p.value),
      KS.Normal=c(KS.Normal$statistic,KS.Normal$p.value))
```

## What distribution for the one-minute intensity of malfunctions to choose?

Based on the ks.test performed, I choose the Gamma distribution for one-minute intensity of malfunction as the p-value of ks test was greater than 0.05, which indicates the Event.Intensities is from gamma distribution.


## What distribution of one-minute malfunctions counts follow my choice?

It follows Negative binomial distribution, as shown in step 4 because the p-value of ks test was greater than 0.05.


Writing One.Minute.Counts.Temps to file OneMinuteCountsTemps.csv to continue working on Part 2.

```{r}
write.csv(One.Minute.Counts.Temps,file="OneMinuteCountsTemps.csv",row.names=FALSE)
```
