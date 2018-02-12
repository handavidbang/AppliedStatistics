library(faraway)
data(prostate)
g <- lm(lpsa~., data = prostate) summary(g)

 library(MASS)
step <- stepAIC(g, direction="both")

step$anova # display results

step(g)

library(leaps)
gs <- regsubsets(lpsa~.,prostate) (gsum <- summary(gs))

(1:8)[which.max(gsum$adjr2)] ## Get best R_a^2

min((1:8)[gsum$cp<c(2:9)]) ## Get smallest C_p where C_p < p
