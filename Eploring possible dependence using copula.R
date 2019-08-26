---
title: "LNL_Course_Proj_Part2"
author: "Anupriya Thirumurthy"
date: "8/24/2018"
output:
  pdf_document: default
  html_document: default
---

# Course Project Part 2: 

Read the data and create a data frame with one-minute breaks counts and temperature measurements.

Ceate data frame with necessary data.

```{r 1}
dataPath <- "/Users/anupriyathirumurthy/Documents/University/MScA_UoC/Courses/LinearAndNonLinearModels/Project"
Part2.Data<-read.csv(file=paste(dataPath,"OneMinuteCountsTemps.csv",sep="/"))
head(Part2.Data)
dim(Part2.Data)
```

Removing rows with NA.

```{r 2}
Part2.Data<-Part2.Data[complete.cases(Part2.Data),]
dim(Part2.Data)
```

Adding column with intensities.

```{r}
Part2.Data<-as.data.frame(cbind(Part2.Data,Part2.Data[,2]/60))
colnames(Part2.Data)<-c("Times","Counts","Temperatures","Intensities")
head(Part2.Data)
```

Visualizing the data.

```{r}
plot(Part2.Data$Temperatures,Part2.Data$Intensities)
```

## Interpreting the plot. 

I could see that there is an direct and possitive relationship between temperature and Intensities.

Analyzing empirical copula.

```{r}
plot(rank(Part2.Data$Temperatures),rank(Part2.Data$Intensities))
```

##  Type of dependency in empirical copula

This looks like a Gumbel copula as the data is more data centered on the top right corner. 

## Distribution of temperatures?

Based on the below histogram, the distribution of temperatures looks like normal distribution. 

Load package MASS to estimate distributions

```{r}
suppressWarnings(library(MASS))
```

Observing the histogram

```{r}
hist(Part2.Data$Temperatures)
```

Estimating and testing normal distribution using fitdistr() from MASS.

Using Kolmogorov-Smirnov test function ks.test() to confirm correctness of normal assumption for temperature.

```{r}
Fitting.Normal <- fitdistr(Part2.Data$Temperatures, densfun = "normal")
Fitting.Normal
(KS.Normal <- ks.test(Part2.Data$Temperatures,"pnorm", mean=mean(Part2.Data$Temperatures), sd=sd(Part2.Data$Temperatures)))
```

## Result

The null hypothesis that the two samples are drawn from the same distribution cannot be rejected at this level of p-value. 

# Fiting a copula

Select a parametric copula appropriate for the observed type of dependence.

Fit the copula Copula.Fit and use it for simulation of rare events.

```{r}
suppressWarnings(library(copula))

dat <-  Part2.Data[,c(1,4)]
head(dat)
par(mfrow=c(2,2))

#Gumbel Copula 

Gumbel.Copula.2<-gumbelCopula(param=2,dim=2)

Copula.Object<-gumbelCopula(param=5,dim=2)
Copula.Fit<-fitCopula(Copula.Object, 
          pobs(Part2.Data[,3:4],ties.method = "average"), 
          method = "ml",
          optim.method = "BFGS", 
          optim.control = list(maxit=1000))

summary(Copula.Fit)

```

Simulating data using Copula.Fit with one variable normally distributed, as temperature and the other with the distribution of your choice for the intensities.

In order to make comparison possible, using set.seed(8301735).

First simulating 250 observations and making a 4-panel graph that we use to represent copula.

Creating a copula object before running simulation.


```{r}
par(mfrow=c(2,2))
Gumbel.Copula.1<-gumbelCopula(param=Copula.Fit@estimate,dim=2)
set.seed(8301735)
Simulated.Gumbel.Copula.1<-rCopula(250,Gumbel.Copula.1)
persp(Gumbel.Copula.1, dCopula, main="pdf",xlab="u", ylab="v", zlab="c(u,v)")
contour(Gumbel.Copula.1,dCopula, main="pdf",xlab="u", ylab="v")
SimulatedN<-length(Simulated.Gumbel.Copula.1[,1])
plot(Simulated.Gumbel.Copula.1,main="Simulated Copula",xlab="Temperature",ylab="Intensity")
plot(apply(Simulated.Gumbel.Copula.1,2,rank)/SimulatedN,main="Empirical Copula",
     xlab="Temperature",ylab="Intensity")
title("Copula.Fit",outer=TRUE,line=-1)
```

Now running longer simulation to observe more tail events using estimated parameters for distributions of temperatures and intensities.

Simulating 5000 pairs of intensities and temperatures using the estimated copula.

Using the same seed.

```{r}
set.seed(8301735)
Simulated.Gumbel.new<-rCopula(5000,Gumbel.Copula.1)
Simulated.Temperature<-qnorm(Simulated.Gumbel.new[,2],Fitting.Normal$estimate[1],
                             Fitting.Normal$estimate[2])
Simulated.Intensities<-qgamma(Simulated.Gumbel.new[,1],shape=1.655739,rate=8.132313)
```

Ploting the simulated variables and their empirical copula.

```{r}
plot(Simulated.Temperature,Simulated.Intensities)
```

```{r}
plot(rank(Simulated.Temperature),rank(Simulated.Intensities))
```

Now we use the simulated data to analyze the tail dependency.

Selecting the simulated pairs with intensity greater than 0.5 and temperature greater than 110.

Using these data to fit negative binomial regression.

Using the initial sample of intensities and temperatures to fit the negative binomial regression for more regular ranges of intensity and temperature.

First, fiting the model to the sample, the name of the fitted model is NB.Fit.To.Sample.

```{r}
NB.Fit.To.Sample<-suppressWarnings(glm.nb(Counts ~ Temperatures, Part2.Data))
summary(NB.Fit.To.Sample)
```

Analyze the summary of the fit. Below are the returned parameters.

```{r}
NB.Fit.To.Sample$coefficients
NB.Fit.To.Sample$deviance
NB.Fit.To.Sample$df.residual
NB.Fit.To.Sample$aic
```

Creating the simulated sample for tail events.

```{r}
Simulated.Tails<-as.data.frame(
  cbind(round(Simulated.Intensities[(Simulated.Temperature>110)&(Simulated.Intensities>.5)]*60),
        Simulated.Temperature[(Simulated.Temperature>110)&(Simulated.Intensities>.5)]))
colnames(Simulated.Tails)<-c("Counts","Temperatures")
```

Ploting the simulated tail events.

```{r}
plot(Simulated.Tails$Temperatures,Simulated.Tails$Counts)
```

Fiting negative binomial model to the tail observations Simulated.Tails.

```{r}
NB.Fit.To.Sample2<-suppressWarnings(glm.nb(Counts ~ Temperatures, Simulated.Tails))
summary(NB.Fit.To.Sample2)
```

Comparing the summaries of the two models. 

The first model has residual deviance larger than the degrees of freedom, therefore, it is not a good fit. The second model has a residual deviance smaller than the degrees of freedom, denoting a good fit.

Note that the parameter ?? estimated by glm.nb() defines the variance of the model as ??+??2/??, where ?? is the mean. In other words, ?? defines overdispersion.

The first model has a theta of 4.203, and variance is larger than degrees of freedom, therefore, there is an overdispersion. As for the second model, it has a extremely large theta of 385983, and the variance is smaller than the degrees of freedome. Thus, it tells me there is no overdispersion.

Additionally I might may be try to fit a Poisson Model.

## Relationships between the temperature and the counts?

Higher the temperature, higher is the counts. This confirms, there is a direct and possitive relationship between temperature and counts. 

Fiting poisson model to Simulated.Tails$Counts and comparing the fit with the nagative binomial fit for Part2.Data.

```{r}
Poisson.Fit<-glm(Counts~Temperatures,data=Simulated.Tails,family=poisson)
summary(Poisson.Fit)
```

```{r}
Poisson.Fit$deviance
Poisson.Fit$df.residual
Poisson.Fit$aic
```

## Overdispersion in the Poisson fit?

We notice that the residual deviance is 80.043 and is lower than the degrees of freedom which is 82, indicating roboust fit. They also are pretty much close to each other. This tells us there is no overdispersion.