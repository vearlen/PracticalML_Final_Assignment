library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

nrow(segmentationOriginal)
names(segmentationOriginal)
inTrain <- createDataPartition(y=segmentationOriginal$Case, p =.6,list=FALSE)
training <- segmentationOriginal[ inTrain,]
testing <- segmentationOriginal[-inTrain,]
dim(training);dim(testing)

set.seed(125)

rpart.model <- train(Class~.,data=training,method="rpart")
rpart.model$finalModel
plot(rpart.model$finalModel)
text(rpart.model$finalModel,use.n = TRUE,all=TRUE,cex=1)

suppressMessages(library(rattle))
library(rpart.plot)
fancyRpartPlot(rpart.model$finalModel)


x <-tibble( TotalIntench2 = 23.000, FiberWidthCh1 = 10, PerimStatusCh1=2)
y <- tibble(TotalIntench2 = 50.000, ShapeP2Ach1 = 2000, Cell = 100, Case="Test")
predict(rpart.model,newdata=y)

# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 PS

# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 WS

# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 PS

# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2  NA



# Question2 ---------------------------------------------------------------
install.packages("pgmm")
library(pgmm)
data(olive)
olive = olive[,-1]
tree_olive <- train(factor(Area) ~., olive, method = "rpart")
newdata = as.data.frame(t(colMeans(olive)))
fancyRpartPlot(tree_olive$finalModel)
predict_olive <- predict(tree_olive,newdata)
predict_olive



# Question3 ---------------------------------------------------------------
install.packages("ElemStatLearn")
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
