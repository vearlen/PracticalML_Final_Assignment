#load data
train <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
test <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

str(train,list.len=161)
summary(train$var_yaw_forearm)
#some NA values are "NA","" or "#DIV/0!" which needs to be removed
train <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                  na.strings  =c("NA","#DIV/0!", ""))
test <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
                 na.strings  =c("NA","#DIV/0!", ""))
str(train,list.len = 160)

library(tidyverse)
#calculate number of NA's in each column and filter where it's higher than 70%
ColLeft<- tibble(Name=names(train),sumNAs= colSums(is.na(train))) %>%
  mutate(ratio=sumNAs/nrow(train))%>%
  filter(ratio<0.7) 
#generate list for selection of columns
list <- dput(as.character(ColLeft$Name))

#create filtered train dataset without columns where NA's more than 70%
train.flt <- train  %>%
  select(list)

names(train.flt)
str(train.flt)


#EDA

trainClasseFreq <- train.flt %>%
  group_by(classe)%>%
  summarize(count=n())

ggplot(trainClasseFreq)+
  geom_col(aes(x=classe,y=count))

#cleaning data
library(caret)
NZVal <- nearZeroVar(train.flt, saveMetrics = TRUE)
NZVal
#one variable comes out of this - new_window
#in addition we remove index - X, user-name, timestamps, num_window

train.flt.clean <- train.flt %>%
  select(-c('X','user_name','raw_timestamp_part_1','raw_timestamp_part_2','new_window','num_window',
            'cvtd_timestamp'))
train.flt.clean$classe <- factor(train.flt.clean$classe)

names(train.flt.clean)
str(train.flt.clean)
#EDA #2
library(corrplot)
corrplot(cor(train.flt.clean[, -53]), method = "color", tl.cex = 0.5)

#prepare data for training
set.seed(123)
index <- createDataPartition(train.flt.clean$classe, p=0.7,list=FALSE)
train.model <- train.flt.clean[index,]
valid.model <- train.flt.clean[-index,]

#train models
lda <- train(classe ~., data=train.model,method ="lda")
pred.lda <- predict(lda,valid.model)
ac.lda <- confusionMatrix(factor(valid.model$classe),pred.lda)$overall[1]

qda <- train(classe ~., data=train.model,method ="qda")
pred.qda <- predict(qda,valid.model)
ac.qda <- confusionMatrix(factor(valid.model$classe),pred.qda)$overall[1]
confusionMatrix(factor(valid.model$classe),pred.qda)
tibble(method = c("Linear DA","Quadrtic DA"), accuracy = c(ac.lda,ac.qda))

rpart <- train(classe ~., data=train.model,method ="rpart")
pred.rpart <- predict(rpart,valid.model)
confusionMatrix(factor(valid.model$classe),pred.rpart)

#random forest
library(randomForest)
# train.model$classe <- factor(train.model$classe)

index2 <- sample(row.names(train.model),size=nrow(train.model)*.05)
train.model.sub <- train.model[index2,]

# rf <- train(classe ~., data=train.model.sub, method = "rf", prox = TRUE)
rf <- randomForest(classe ~. , data=train.model,importance=TRUE,
                        proximity=TRUE)
# rf_test
pred.rf <- predict(rf,valid.model)
confusionMatrix(factor(valid.model$classe),pred.rf)

#predict test data
predict(qda,test[,-length(names(test))])
predict(rf,test[,-length(names(test))])

NZVal <- nearZeroVar(train.flt, saveMetrics = TRUE)
NZVal%>% filter(nzv == TRUE)

train.flt <- train  %>%
  select(list)

saveRDS(qda, file='qda')
saveRDS(lda, file = 'lda')
saveRDS (rf, file = 'rf')
lda
acc <- c(ac.lda,ac.qda,ac.rf)
acc
oose <- sapply(acc,function(x){round(1-x,3)})
oose
