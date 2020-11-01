library(AppliedPredictiveModeling)

library(caret)
data("concrete")
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]


library(tidyverse)
training <- mutate(training,index=1:nrow(training))

library(Hmisc)
x <- names(training)



p1 <-ggplot(training,aes(x=index,y=CompressiveStrength))+
  geom_point(aes(color=cut2(Cement, g=3)))

p2 <-ggplot(training,aes(x=index,y=CompressiveStrength))+
  geom_point(aes(color=cut2(BlastFurnaceSlag, g=3)))

p3 <-ggplot(training,aes(x=index,y=CompressiveStrength))+
  geom_point(aes(color=cut2(FlyAsh, g=3)))

p4 <-ggplot(training,aes(x=index,y=CompressiveStrength))+
  geom_point(aes(color=cut2(Water, g=3)))

p5 <-ggplot(training,aes(x=index,y=CompressiveStrength))+
  geom_point(aes(color=cut2(Superplasticizer, g=3)))

p6 <-ggplot(training,aes(x=index,y=CompressiveStrength))+
  geom_point(aes(color=cut2(CoarseAggregate, g=3)))

p7 <-ggplot(training,aes(x=index,y=CompressiveStrength))+
  geom_point(aes(color=cut2(FineAggregate, g=3)))

p8 <-ggplot(training,aes(x=index,y=CompressiveStrength))+
  geom_point(aes(color=cut2(Age, g=3)))


rm(list = ls())

library(gridExtra)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol=3)


ggplot(training)+
  geom_histogram(aes(x=log10(Superplasticizer)))

                 
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

names(training)
sub.Alz <- training %>% select (diagnosis,c(58:69)) 
names(sub.Alz)
sub.Alz.test <- testing %>%  select (diagnosis,c(58:69))
names(sub.Alz.test)

pca_obj <- preProcess(sub.Alz[,-1],method = c("pca"),thresh = 0.8)
pca_obj

pca.Alz <- predict(pca_obj,newdata=sub.Alz[,-1])
pca.Alz <- bind_cols(pca.Alz,diagnosis=sub.Alz[,1])

lin_pca <- train(diagnosis ~.,data=pca.Alz, method = "glm")
lin_pca

lin_raw <- train (diagnosis ~., data=sub.Alz, method = "glm")
lin_raw
raw_res <- predict(lin_raw,newdata = sub.Alz.test[,-1])
raw_res
confusionMatrix(sub.Alz.test[,1],raw_res)

pca.sub.Alz.test <- predict(pca_obj, newdata = sub.Alz.test[,-1])
pca.sub.Alz.test <- bind_cols(pca.sub.Alz.test, diagnosis=sub.Alz.test[,1])

pca_res <- predict(lin_pca,newdata = pca.sub.Alz.test)
pca_res
confusionMatrix(sub.Alz.test[,1],pca_res)
