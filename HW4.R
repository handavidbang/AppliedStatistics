setwd("~/Desktop/R")
data(sat,package="faraway")
attach(sat)
lm <- lm(total~expend+ratio+salary)
summary(lm)

lm1 <- lm(total~expend+ratio)
anova(lm1,lm)

lm2 <- lm(total~1)
summary(lm2)

anova(lm2,lm)

#Bonferroni’s procedure
n<-dim(sat)[1]
p<-4
G1<-qt(p=1-0.05/4, df=n-p)

#Scheffé’s procedure
G2<-sqrt(2*qf(0.95, 2, n-p))

#Bonferroni’s CI for salary
c(summary(lm)$coef[4,1]-G1*summary(lm)$coef[4,2], summary(lm)$coef[4,1]+ G1*summary(lm)$coef[4,2])

#Scheffé’s CI for salary
c(summary(lm)$coef[4,1]-G2*summary(lm)$coef[4,2],summary(lm)$coef[4,1]+G2*summary(lm)$coef[4,2])

#Bonferroni’s CI for ratio
c(summary(lm)$coef[3,1]-G1*summary(lm)$coef[3,2], summary(lm)$coef[3,1]+G1*summary(lm)$coef[3,2])

#Scheffé’s CI for ratio
c(summary(lm)$coef[3,1]-G2*summary(lm)$coef[3,2],summary(lm)$coef[3,1]+G2*summary(lm)$coef[3,2])

#Bootstrap CI with 500 replicates
set.seed(123)
nb<-500
coefmat<-matrix(NA, nb, 4)
resids<-residuals(lm)
preds<-fitted(lm)
for (i in 1: nb){
booty<-preds+sample(resids, rep=TRUE)
bmod<-update(lm, booty ~.)
coefmat[i,]<-coef(bmod)
}
colnames(coefmat)<-c('Intercept', colnames(sat[, 1:3]))
coefmat<-data.frame(coefmat)
apply(coefmat,2,function(x) quantile(x, c(0.025,0.975)))

lm3 <- lm(total~expend+ratio+salary+takers)
summary(lm3)

lm4 <- lm(total~expend+ratio+salary)
summary(lm4)

anova(lm4,lm3,test='F')
tstat<--2.9045/0.2313
2*pt(tstat, 45)

#Shapiro-Wilkes Test
SWtest <- function(Y, X, nrep = 1000){
n <- length(Y)
p <- ncol(X)
#---fit linear regresson and get the residual to calculate test statistics
fit <- lm(Y ~ X)
r <- residuals(fit)
#order the residual
r_sort <- sort(r, decreasing = FALSE)
#calculate the z by qnorm
z <- qnorm((1:n - 0.375) / (n + 0.25))
#test_statistics W is:
W <- sum(r_sort * z) / sqrt(sum(r_sort ^ 2) * sum(z ^ 2))
#---use monto carlo to get sampling
W_N <- as.numeric(nrep)
for(i in 1:nrep){
y <- rnorm(n)
r_i <- residuals(lm(y ~ X))
r_i <- sort(r_i, decreasing = FALSE)
W_N[i] <- sum(r_i * z) / sqrt(sum(r_i ^ 2) * sum(z ^ 2))
}
#calculate the p_value based on sampling quantile
p_value <- sum(W_N < W) / nrep
return(list(test_statistics = W, p_value = p_value))
}
coal_data <- read.table(file = "co.dat", header = TRUE)
SWtest(Y = coal_data$y, X = coal_data$x, nrep = 10000)
