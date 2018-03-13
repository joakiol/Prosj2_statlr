library(ISLR)
library(leaps)
library(glmnet)

ourAuto=data.frame("mpg"=Auto$mpg,"cylinders"=factor(cut(Auto$cylinders,2)),
                   "displace"=Auto$displacement,"horsepower"=Auto$horsepower,
                   "weight"=Auto$weight,"acceleration"=Auto$acceleration, 
                   "year"=Auto$year,"origin"=as.factor(Auto$origin))
colnames(ourAuto)
ntot=dim(ourAuto)[1]
ntot
set.seed(4268)
testids=sort(sample(1:ntot,ceiling(0.2*ntot),replace=FALSE))
ourAutoTrain=ourAuto[-testids,]
ourAutoTest=ourAuto[testids,]


library(gam)
gamobject = gam(mpg ~ bs(displace, knots  = 290) + poly(horsepower, 2)  + weight + s(acceleration, 3) + factor(origin),data=ourAutoTrain)
par(mfrow=c(2,3))
plot(gamobject,se=TRUE,col="blue")


