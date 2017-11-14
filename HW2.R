library(faraway)
data(prostate)
lm1<- lm(lpsa ~ lcavol, data = prostate)
lm2<- lm(lpsa ~ lcavol+lweight, data = prostate)
lm3 <- lm(lpsa ~ lcavol+lweight+svi, data = prostate)
lm4 <- lm(lpsa ~ lcavol+lweight+svi+lbph+age, data = prostate)
lm5 <- lm(lpsa ~ lcavol+lweight+svi+lbph+age+lcp, data = prostate)
lm6 <- lm(lpsa ~ lcavol+lweight+svi+lbph+age+lcp+pgg45, data = prostate)
lm7<- lm(lpsa ~ lcavol+lweight+svi+lbph+age+lcp+pgg45+gleason, data = prostate)
x<-cbind(summary(lm1)$sigma,summary(lm2)$sigma,summary(lm3)$sigma,summary(lm4)$sigma,summary(lm5)$sigma,summary(lm6)$sigma,summary(lm7)$sigma)
y<-cbind(summary(lm1)$r.squared,summary(lm2)$r.squared,summary(lm3)$r.squared,summary(lm4)$r.squared,summary(lm5)$r.squared,summary(lm6)$r.squared,summary(lm7)$r.squared)
par(mfrow=c(1,2))
plot(c(1:7),x, xlab="", ylab="residual standard error", type="l")
plot(c(1:7),y, xlab="", ylab="R square", type="l")

plot(prostate$lcavol, prostate$lpsa, xlab="lcavol", ylab="lpsa")
lm8<-lm(lpsa~lcavol, prostate)
lm9<-lm(lcavol~lpsa, prostate)

abline(a=lm8$coefficient[1], b=lm8$coefficient[2])
abline(a=-lm9$coefficient[1]/lm9$coefficient[2], b=1/lm9$coefficient[2], lty=2)

lf<-matrix(c(-1,lm8$coefficient[2],-1,1/lm9$coefficient[2]),nrow=2,byrow=TRUE)
rf<-matrix(c(-lm8$coefficient[1],lm9$coefficient[1]/lm9$coefficient[2]),nrow=2)
solve(lf,rf)

data(truck)
truck$B<-sapply(truck$B, function(x) ifelse(x=="-",-1,1))
truck$C<-sapply(truck$C, function(x) ifelse(x=="-",-1,1))
truck$D<-sapply(truck$D, function(x) ifelse(x=="-",-1,1))
truck$E<-sapply(truck$E, function(x) ifelse(x=="-",-1,1))
truck$O<-sapply(truck$O, function(x) ifelse(x=="-",-1,1))
summary(lm(height ~ ., data=truck))

summary(lm(height ~ B+C+D+E, data=truck))

X1<-model.matrix(~B+C+D+E+O,truck)
X2<-model.matrix(~B+C+D+E,truck)
as.matrix(solve(t(X1)%*%X1)%*%t(X1))[1:5,]-as.matrix(solve(t(X2)%*%X2)%*%t(X2))

A<-truck$B+truck$C+truck$D+truck$E
summary(lm(truck$height ~ A+truck$B+truck$C+truck$D+truck$E))

X<-model.matrix(~A+B+C+D+E+O,truck)
y<-truck$height

qrx<-qr(X)
(f<-t(qr.Q(qrx))%*%y)

backsolve(qr.R(qrx), f)

qr.coef(qr(X), y)# betahat
