library(faraway) lm1<-lm(divorce~unemployed+femlab+marriage+birth+military, data=divusa) x<-model.matrix(lm1)[, -1]
e<-eigen(t(x)%*%x)
e$val

sqrt(e$val[1]/e$val)

vif(x)

summary(lm1)

vif(x[, c(-1,-5)])

#Problem 4
require(pls)

lm2<-lm(hipcenter~HtShoes+Ht+Seated+Arm+Thigh+Leg, data=seatpos) 
summary(lm2)

test<-data.frame(Age=64.8, Weight=263.7, HtShoes=181.08, Ht=178.56, Seated=91.44, Arm=35.64, Thigh=40.95)
seatpos_pca1<-prcomp(seatpos[, 3:8], scale=TRUE) 
summary(seatpos_pca1)

plot(seatpos_pca1$sdev[1:6], type='l', ylab='SD of PC', xlab='PC number')
 
seatpos_pcr1<-pcr(hipcenter~ HtShoes + Ht + Seated + Arm + Thigh + Leg, data=seatpos, ncomp=6, scale=TRUE) 
predict(seatpos_pcr1, test, ncomp=3)

seatpos_pca2<-prcomp(seatpos, scale=TRUE) 
summary(seatpos_pca2)

plot(seatpos_pca2$sdev[1:6], type='l', ylab='SD of PC', xlab='PC number')

seatpos_pcr2<-pcr(hipcenter~ ., data=seatpos, ncomp=6, scale=TRUE) 
predict(seatpos_pcr2, test, ncomp=4)

#Problem 5
library(faraway)
data(fat)
index <- seq(10, 250, by = 10)
train <- fat[-index, -c(1,3,8)]
test <- fat[index, -c(1,3,8)]
rmse <- function(x,y)sqrt(mean((x-y)^2))

 g <- lm(siri~., data = train) 
 rmse(g$fit, train$siri)
 rmse(predict(g, test[,-1]), test$siri)
 a <- step(g)
 rmse(predict(a), train$siri)
 rmse(predict(a, test[,-1]), test$siri)
 library(pls)
 pcrg <- pcr(siri~ ., data = train, ncomp = 14, validation = "CV", segments = 10) 
 which.min(RMSEP(pcrg)$val[1,,])
 
 pcrpred.tr <- predict(pcrg, newdata = train, ncomp = 7) 
 rmse(pcrpred.tr, train$siri)
 pcrpred.te <- predict(pcrg, newdata = test, ncomp = 7) 
 rmse(pcrpred.te, test$siri)
 plsg <- plsr(siri~ ., data = train, ncomp = 14, validation = "CV", segments = 10) 
 which.min(RMSEP(plsg)$val[1,,])
 plspred.tr <- predict(plsg, newdata = train, ncomp = 8) 
 rmse(plspred.tr, train$siri)
 plspred.te <- predict(plsg, newdata=test, ncomp=8) 
 rmse(plspred.te, test$siri)
 
library(MASS)
trainy <- train[,1]
trainy <- trainy - mean(trainy)
colmean <- apply(train[,-1], 2, mean)
trainx <- as.matrix(sweep(train[,-1], 2, colmean))
testx <- as.matrix(sweep(test[,-1], 2, colmean))
gridge <- lm.ridge(trainy~trainx,lambda = seq(0, 10, 0.01)) 
select(gridge)

which.min(gridge$GCV)
rmse(scale(trainx, FALSE, gridge$scales)%*%gridge$coef[,110] + mean(train$siri), train$siri)
rmse(scale(testx, FALSE, gridge$scales)%*%gridge$coef[,110] + mean(train$siri), test$siri)
