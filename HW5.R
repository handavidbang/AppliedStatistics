rm(list = ls())
y<-c(62, 60, 63, 59, 63, 67, 71, 64, 65, 66, 68, 66, 71, 67, 68, 68, 56, 62, 60, 61, 63, 64, 63, 59)
x<-t(rbind(c(rep(1, 4), rep(0, 20)),c(rep(0, 4),rep(1, 6), rep(0, 14)),c(rep(0, 10),rep(1, 6), rep(0, 8)),c(rep(0, 16),rep(1, 8))))
xx<-t(rbind(c(rep(1, 16), rep(0, 8)),c(rep(0, 16),rep(1, 8))))
n<-24
p<-4
q<-2
lm <- lm(y~-1+x[, 1]+x[, 2]+x[, 3]+x[, 4])
lmm<-lm(y~-1+xx[, 1]+xx[, 2])
anova(lm,lmm,test='F')

testF<-anova(lm,lmm,test='F')
re<-c(y[1:4]-mean(y[1:4]),y[5:10]-mean(y[5:10]),y[11:16]-mean(y[11:16]),y[17:24]-mean(y[17:24]))
nb<-500
L<-NULL
for (i in 1: nb){
booty<-fitted(lmm)+sample(re, rep=TRUE) ##H1: lm, H0: lmm

######this may easier#######
#bmod1<-update(lm, booty ~.) 1
#bmod2<-update(lmm, booty ~.) 0
#btestF<-anova( bmod1, bmod2, test='F')

#r*#
SSE0<-sum((booty[1:16]-mean(booty[1:16]))^2)+sum((booty[17:24]-mean(booty[17:24]))^2)
SSE1<-sum((booty[1:4]-mean(booty[1:4]))^2)+sum((booty[5:10]-mean(booty[5:10]))^2)+sum((booty[11:16]-mean(booty[11:16]))^2)+sum((booty[17:24]-mean(booty[17:24]))^2)
rstar<-(SSE0-SSE1)*(n-p)/(SSE1*q)
L<-c(L, rstar>testF$F[2])
}
sum(L)/nb #This is p-value

#Different Method for finding residuals
resids<-residuals(lm)
preds<-fitted(lmm)
nb<-500
L<-NULL
for (i in 1: nb){
booty<-preds+sample(resids, rep=TRUE)
#r*#
SSE0<-sum((booty[1:16]-mean(booty[1:16]))^2)+sum((booty[17:24]-mean(booty[17:24]))^2)
SSE1<-sum((booty[1:4]-mean(booty[1:4]))^2)+sum((booty[5:10]-mean(booty[5:10]))^2)+sum((booty[11:16]-mean(booty[11:16]))^2)+sum((booty[17:24]-mean(booty[17:24]))^2)
rstar<-(SSE0-SSE1)*(n-p)/(SSE1*q)
L<-c(L, rstar>testF$F[2])
}
sum(L)/nb #This is p-value

#Calculating the Power of the Test
h0<-as.matrix(c(4,4))
h1<-as.matrix(c(0,0))
C<- matrix(c(-1,0,1,-1,0,1,0,0), 2, 4)
prod<-t(h1-h0)%*%solve(C%*%solve(t(x)%*%x)%*%t(C))%*%(h1-h0)
delta<-sqrt(prod/5.5)
df1<-length(C[,1])
df2<-n-p
phi<-delta/sqrt(1+df1)
alpha<-0.05
pearsonhartley<-function(phi,alpha,df1,df2){
b<-0.5*phi*phi*(1+df1)
q<-qf(1-alpha,df1,df2)
y<-1-1/(1+df1*q/df2)
p<-pbeta(y,df1/2,df2/2)
p1<-1
jmax<-1000
for(j in 1:jmax){
p1<-p1*b/j
p<-p+pbeta(y,j+df1/2,df2/2)*p1
}
1-p*exp(-b)
}
pearsonhartley(phi,alpha,df1,df2)

#Bootstrap method 
yy<-c(y[1:4], y[5:10]-4, y[11:16]-8, y[17:24])
lmmm<-lm(yy~-1+xx[, 1]+xx[, 2]) ###h0:lmm, h1:lmmm
SSE1<-sum(lmmm$residuals**2)
SSE0<-sum(lmm$residuals**2)
r<-qf(0.95, df1, df2) #####Let alpha = 0.05
#r<-(SSE0-SSE1)*(n-p)/(SSE1*q)
nb<-500
L<-NULL
for (i in 1: nb){
booty1<-fitted(lmmm)+sample(residuals(lmmm), rep=TRUE)
booty0<-c(booty1[1:4], booty1[5:10]+4, booty1[11:16]+8, booty1[17:24])

######this may easier#######
bmod0<-update(lmm, booty0 ~.)
bmod1<-update(lmmm, booty1 ~.)
bSSE0<-sum(bmod0$residuals**2)
bSSE1<-sum(bmod1$residuals**2)
rstar<-(bSSE0-bSSE1)*(n-p)/(bSSE1*q)
L<-c(L, rstar>r)
}
sum(L)/nb #This is the power
