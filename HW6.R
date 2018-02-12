#set working directory
setwd("~/Desktop/R")
fiber <- read.table("fiber.dat", header=F) attach(fiber)
x1 <- log(V2)
x2 <- log(V3)
x4 <- log(V5)
am <- lm(x1~x2+x4)
summary(am)

p <- 3
n <- length(x1)
sigma <- summary(am)$sigma
inf <- lm.influence(am)
par(mfrow = c(3,4))
plot(x2,x4,pch=16,main='plot of explanatory variables') std <- am$resid / sigma
plot(am$fitted.value,std) abline(h=c(-2,2),lty="dotted")
title("Standardized residual plot")
indent = (max(am$fitted.value)-min(am$fitted.value))/20

for(i in 1:n){
if(abs(std[i])> 2) text(am$fitted.value[i]+indent,std[i],i,cex=0.6)
}
source("diagnose.R") Internally.Studentized.Residual<-stanres(am) plot(Internally.Studentized.Residual); title("Internally Studentized Residual") abline(h=c(-2,2),lty="dotted")
for(i in 1:n){
if(abs(Internally.Studentized.Residual[i])>2) text(i+indent,Internally.Studentized.Residual[i],i,cex=0.6)
}
Enternally.Studentized.Residual <- studres(am)
plot(Enternally.Studentized.Residual);
title("Externally Studentized Residual")
abline(h=c(-2,2),lty="dotted")

for(i in 1:n){ if(abs(Enternally.Studentized.Residual[i])> 2)text(i+indent,Enternally.Stud qqnorm(Enternally.Studentized.Residual);abline(c(0,0),c(1,1)) plot(inf$hat);title("leverage plot")
abline(h=2*p/n,lty=3) ## high leverage points
leverage<-c(inf$hat>2*p/n) ## pick up high leverage points
for(i in 1:n){ if(leverage[i]==T)text(i+indent,inf$hat[i],i,cex=0.6)}
DFFITS <- dffits(am) plot(DFFITS);abline(h=2*sqrt(p/n),lty=2);abline(h=-2*sqrt(p/n),lty=2);title("DFFITS") DF.detected <- c(abs(DFFITS)> 2*sqrt(p/n))
for(i in 1:n){
if(DF.detected[i]==T) text(i+indent,DFFITS[i],i,cex=0.6) }
DFBETAS <- dfbetas(am) if (p >1 ) {
for (k in 1:p) {
points(DFBETAS[,k],pch=k)
DFB.detected<- c(abs(DFBETAS[,k]) > 2*sqrt(1/n)) for(i in 1:n){
if(DFB.detected[i]==T) text(i+indent,DFBETAS[i,k],i,cex=0.6)
} }
}
legend("bottomleft", c("beta0","beta1"), pch = c(1:p))
plot(am, which=4)
first <- ((n-p-1) / (n-p))+ Enternally.Studentized.Residual^2/(n-p) COVRATIO <- first^(-p) /(1-inf$hat)
plot(COVRATIO)
abline(h=3*p/n+1,lty=2)
abline(h=-3*p/n+1,lty=2)
title("COVRATIO")
CR.detected <- c(abs(COVRATIO-1)>3*p/n)
for(i in 1:n){
  if(CR.detected[i]==T)
entized.Residu
  
text(i+indent,COVRATIO[i],i,cex=0.6) }
source("dnsim.R") par(mfrow=c(1,2))

dnsim(x1,cbind(x2,x4),nsim=1000)

fiber <- subset(fiber, V1!=19) x1 <- log(fiber$V2)
x2 <- log(fiber$V3)
x4 <- log(fiber$V5)
bm <- lm(x1~x2+x4) shapiro.test(bm$residual)

shapiro.test(am$residual)

#Problem 2 
data(sat,package="faraway")
attach(sat)
lm <- lm(total~expend+ratio+salary+takers) summary(lm)

lm1 <- lm(total~ratio+salary+takers) summary(lm1)
anova(lm1,lm,test="F")

p <- 4
n <- length(total)
sigma <- summary(lm1)$sigma
inf <- lm.influence(lm1)
par(mfrow=c(3,4)) plot(ratio,salary,pch=16,main="ratio vs salary")

plot(ratio,takers,pch=16,main="ratio vs takers")
plot(takers,salary,pch=16,main="takers vs salary")
std <- lm1$resid / sigma plot(lm1$fitted.value,std);abline(h=c(-2,2),lty="dotted");title("Standardized residual plot") 
indent = (max(lm1$fitted.value)-min(lm1$fitted.value))/20
                                                                  
for(i in 1:n){ if(abs(std[i])> 2)
text(lm1$fitted.value[i]+indent,std[i],i,cex=0.6) }
indent = n/25
Internally.Studentized.Residual<-stanres(lm1) plot(Internally.Studentized.Residual);title("Internally Studentized Residual") abline(h=c(-2,2),lty="dotted")
for(i in 1:n){
if(abs(Internally.Studentized.Residual[i])>2) text(i+indent,Internally.Studentized.Residual[i],i,cex=0.6)
}
Enternally.Studentized.Residual<-studres(lm1) plot(Enternally.Studentized.Residual);title("Externally Studentized Residual") abline(h=c(-2,2),lty="dotted")
for(i in 1:n){
if(abs(Enternally.Studentized.Residual[i])>2) text(i+indent,Enternally.Studentized.Residual[i],i,cex=0.6)
}
qqnorm(Enternally.Studentized.Residual) abline(c(0,0),c(1,1))
plot(inf$hat);title("leverage plot")
abline(h=2*p/n,lty=3) ## high leverage points leverage<-c(inf$hat>2*p/n) ## pick up high leverage points for(i in 1:n){
if(leverage[i]==T) text(i+indent,inf$hat[i],i,cex=0.6)
}
DFFITS <- dffits(lm1)
plot(DFFITS)
abline(h=2*sqrt(p/n),lty=2) abline(h=-2*sqrt(p/n),lty=2) title("DFFITS")
DF.detected <- c(abs(DFFITS)> 2*sqrt(p/n)) for(i in 1:n){
if(DF.detected[i]==T) text(i+indent,DFFITS[i],i,cex=0.6)
}
DFBETAS <- dfbetas(lm1) if (p>1){
for (k in 1:p){
points(DFBETAS[,k],pch=k)
DFB.detected <- c(abs(DFBETAS[,k])> 2*sqrt(1/n)) for(i in 1:n){
if(DFB.detected[i]==T) text(i+indent,DFBETAS[i,k],i,cex=0.6)
} 
}
 
 }
legend("bottomleft", c("beta0","beta1"), pch = c(1:p))
plot(lm1, which=4)
first <- ((n-p-1) / (n-p))+ Enternally.Studentized.Residual^2/(n-p) COVRATIO <- first^(-p) /(1-inf$hat)
plot(COVRATIO)
abline(h=3*p/n+1,lty=2)
abline(h=-3*p/n+1,lty=2)
title("COVRATIO")
CR.detected <- c(abs(COVRATIO-1)>3*p/n)
for(i in 1:n){
if(CR.detected[i]==T) text(i+indent,COVRATIO[i],i,cex=0.6)
} par(mfrow=c(1,2))
