library(ISLR)
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

library(glmnet)
set.seed(4268)

x=model.matrix(mpg~.,ourAutoTrain)[,-1] #-1 to remove the intercept.
head(x)
y=ourAutoTrain$mpg

lambda=c(seq(from=5,to=0.1,length.out=150),0.01,0.0001) #Create a set of tuning parameters, adding low value to also see least squares fit
cv.out=cv.glmnet(x,y,alpha=1,nfolds=10,lambda=lambda, standardize=TRUE) #alpha=1 gives lasso, alpha=0 gives ridge

plot(cv.out)

cv.out$lambda.min
cv.out$lambda.1se

coef(cv.out,s="lambda.1se")
predict(cv.out, newx =matrix(c(0,150,100,3000,10,82,1,0),nrow=1), s = "lambda.1se")
