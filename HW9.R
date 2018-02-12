library(faraway)
lm1<-lm(crawling ~ temperature, data=crawl, weights=n/SD^2) 
summary(lm1)

#Problem 2
library(MASS)
data(ozone)
a <- boxcox(lm(O3~., data = ozone), lambda = seq(0, .5, by = .05))

#Problem 3
library(mgcv)
library(faraway)
data(cheddar)
gm1<-gam(taste~s(Acetic)+s(H2S)+s(Lactic), data=cheddar) plot(gm1)
