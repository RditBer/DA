library(readr)
library(EnvStats)
library(nortest)
# set working directory (relative path)
setwd("C:/Users/ardit/Downloads/Lab 1-selected")
# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

### Explore Variable ###
ECO <- epi.data$ECO.new
NAs <- is.na(ECO)
rownums <- which(NAs)
ECO[rownums]
ECO.complete <- ECO[!NAs]

BDH <- epi.data$BDH.new
BDH
NAs <- is.na(BDH)
rownums <- which(NAs)
BDH[rownums]
BDH.complete <- BDH[!NAs]
BDH.complete
summary(ECO.complete)
summary(BDH.complete)
boxplot(ECO.complete, BDH.complete, names = c("ECO","BDH"))

### Histograms ###
hist(ECO.complete)
x <- seq(10, 100, 5)
hist(ECO, x, prob=TRUE)
lines(density(ECO.complete,bw="SJ"))
rug(ECO.complete)

## plot  histogram again
hist(ECO, x, prob=TRUE) 
x1<-seq(20,80,1)
d1 <- dnorm(x1,mean=46, sd=11,log=FALSE)
lines(x1,d1)

### Empirical Cumulative Distribution Function ###
plot(ecdf(ECO.complete), do.points=FALSE, verticals=TRUE) 
plot(ecdf(BDH.complete), do.points=FALSE, verticals=TRUE) 

### Quantile-quantile Plots ###
qqnorm(ECO); qqline(ECO)
x <- rnorm(180, mean=46, sd=10)
qqnorm(x); qqline(x)
qqplot(ECO.complete, BDH.complete, xlab = "Q-Q plot for ECO & BDH") 
qqplot(epi.data$ECO.new, epi.data$BDH.new, xlab = "Q-Q plot for ECO.new & BDH.new") 

## Statistical Tests
shapiro.test(ECO.complete)
shapiro.test(BDH.complete)

ad.test(ECO.complete)
ad.test(BDH.complete)

ks.test(ECO.complete, BDH.complete)     # identical distribution test
wilcox.test(ECO.complete, BDH.complete) # optional alternative

var.test(ECO.complete,BDH.complete)
t.test(ECO.complete,BDH.complete)
hist(BDH.complete, col='red3')
hist(ECO.complete, col='darkorchid3', add=TRUE)

### THE END ###
