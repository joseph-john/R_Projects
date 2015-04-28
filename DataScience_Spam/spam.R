library(kernlab)
data(spam)

set.seed(3435)

trainIndicator <- rbinom(nrow(spam),size=1,prob=0.5)

trainSpam <- spam[trainIndicator==1,]

testSpam <- spam[trainIndicator==0,]

trainSpam$numType <- as.numeric(trainSpam$type) - 1

costFuntion <- function(x,y) sum(x != (y>0.5))

cvError <- rep(NA,55)

library(boot)

for(i in 1:55){
  lmFormula = reformulate(names(trainSpam)[i], response = "numType")
  glmFit = glm(lmFormula, family="binomial", data=trainSpam)
  cvError[i] <- cv.glm(trainSpam,glmFit,costFuntion,2)$delta[2]
}

names(trainSpam)[which.min(cvError)]