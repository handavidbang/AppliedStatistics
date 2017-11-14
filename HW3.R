setwd("~/Desktop/664Solution")
library(faraway)
head(prostate)

lm1<-lm(lpsa~., data=prostate)
confint(lm1, level = 0.95)

confint(lm1, level = 0.9)

library(ellipse)
plot(ellipse(lm1, c(4, 5)), type="l", ylim=c(-0.1, 0.3))
points(0,0, pch=19)

nreps<-4000
set.seed(123)
tstats<-numeric(nreps)
for (i in 1: nreps){
lmm<-lm(lpsa~lcavol+lweight+svi+lbph+sample(age)+lcp+pgg45+gleason, data=prostate)
2
tstats[i]<-summary(lmm)$coef[6, 3]
}
mean(abs(tstats)>abs(summary(lm1)$coef[4,3]))

lm2<-lm(lpsa~lcavol+lweight+svi, data=prostate)
anova(lm2,lm1)

coal_data <- read.table(file = "co.dat.txt", header = TRUE)
summary(coal_data)
hist(coal_data$x)
hist(coal_data$y)

coal_fit <- lm(y ~ x, data = coal_data)
summary(coal_fit)

confint(coal_fit, level = 0.95)


r <- residuals(coal_fit)
shapiro.test(r)

anova(lm(y ~ x, data = coal_data), lm(y ~ factor(x), data = coal_data))
predict(coal_fit, newdata = data.frame(x = 2),interval = "predict", level = 0.9)
predict(coal_fit, newdata = data.frame(x = 2),interval = "confidence", level = 0.9)
