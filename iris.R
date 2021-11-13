#библиотеки
library(e1071)
library(ROCR)

#рабочая директория
setwd("D:/R/kt_iris")

#чтение файла
icsv <- read.csv("iris.csv",header = T, sep = ",")

#график1
for(i in 1:100){ 
  icsv[i,5] = 2
}
for(i in 101:150){
  icsv[i,5] = 1
}
icsv <- lapply(icsv, as.numeric)

x <- data.frame(icsv$sepal.length, icsv$sepal.width, icsv$petal.length, icsv$petal.width)
y <- icsv$variety

set.seed(1233) #зерно случайных чисел

plot(x, col = (73 - y))


#график2
dat = data.frame(x = x, y = as.factor(y))

svmfit = svm(y ~., data = dat, kernel = "linear",
             cost = 10, scale = F) 

plot(svmfit, dat,x.icsv.petal.width ~ x.icsv.petal.length,
     slice = list(x.icsv.sepal.width = 3, x.icsv.sepal.length = 4))

#график3
svmfit$index
summary(svmfit)

set.seed(1) #зерно случайных чисел
tune.out = tune(svm, y~., data = dat, kernel = "linear",
                ranges = list(cost = c(0.001, 0.1, 1, 5, 10, 100)))
summary(tune.out)

bestmod = tune.out$best.model
summary(bestmod)

set.seed(1) #зерно случайных чисел
train = sample(150, 75)

ypred = predict(bestmod, dat[-train,])
table(predict = ypred, truth = dat[-train,"y"]) 

rocplot = function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf, ...)
}


svmfit.opt = svm(y ~., data = dat[train,], kernel = "radial",
                 gamma = 2, cost = 1, decision.values = T)

fitted = attributes(predict(svmfit.opt, dat[train,], 
                            decision.values = T))$decision.values
par(mfrow = c(1,2))
rocplot(fitted, dat[train, "y"], main = "trainingData")
svmfit.flex = svm(y ~., data = dat[train,], kernel = "radial",
                  gamma = 50, cost = 1, decision.values = T)

fitted = attributes(predict(svmfit.flex, dat[train,], 
                            decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"], main = "trainingData", add = T, col = "blue")


fitted = attributes(predict(svmfit.opt, dat[-train,], 
                            decision.values = T))$decision.values
rocplot(fitted, dat[-train,"y"], main = "testData")
fitted = attributes(predict(svmfit.flex, dat[-train,], 
                            decision.values = T))$decision.values
rocplot(fitted, dat[-train,"y"], add = T, col = "blue")