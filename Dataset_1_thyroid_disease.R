thyroid_data=read.csv("C:\\Users\\kupekarraj\\Desktop\\DMML project\\thyroid.csv")
View(thyroid_data)
dim(thyroid_data)
#df$Q1c<-recode(df$Q1c,"c('1','2')='yes';c('3','4','9')='no'")
thyroid_data$Category <-factor(thyroid_data$Category,levels = c("negative","hyperthyroid","hypothyroid","sick"),labels = c("1","2","3","4"))
# Bar plot for target (thyroid disease) 
ggplot(thyroid_data, aes(x=thyroid_data$Category, fill=thyroid_data$Category)) + 
  geom_bar() +
  xlab("thyroid Disease Category") +
  ylab("Count") +
  ggtitle("Analysis of thyroid Disease ") +
  scale_fill_discrete(name = "thyroid Disease", labels = c("negative", "hyperthyroid","hypothyroid","sick"))
#Distribution of Male and Female population across Age parameter affected by disease
ggplot(thyroid_data, aes(x=Age, fill=thyroid_data$Category)) + 
  geom_histogram() +
  xlab("Age") +
  ylab("Count") +
  ggtitle("Analysis of thyroid Disease  ") +
  scale_fill_discrete(name = "thyroid Disease", labels = c("negative", "hyperthyroid","hypothyroid","sick"))
library(caret)
Train1<-createDataPartition(thyroid_data$Category,p=0.7,list = FALSE)
train1<-thyroid_data[Train1,]
test1<-thyroid_data[-Train1,]
#Implementing the SVM model
library(e1071)
svmmodel1<-svm(Category~.,data=train1,kernel='linear',gamma=1,cost=1)
summary(svmmodel1)
dim(train1)
test1$result1<-predict(svmmodel1,test1)
table(test1$result1,test1$Category)
confusionMatrix(test1$Category,test1$result1)
# radial model
svmmodel2<-svm(Category~.,data=train1,kernel='radial',gamma=1,cost=1)
summary(svmmodel2)
test1$result2<-predict(svmmodel2,test1)
table(test1$result2,test1$Category)
confusionMatrix(test1$Category,test1$result2)


#tuning the svm and setting the cost parameter to 10 and gamma 0.5
## best paramter value
tune.results <- tune(svm,train.x=thyroid_data[1:27],train.y=thyroid_data[,28],kernel='radial', ranges = list(cost=c(0.1,1,10),gamma=c(0.5,1,2)))
summary(tune.results)
# tuned model for linear
tuned_mod1 <- svm(Category~., data = train1, kernel='linear',cost=10,gamma=0.5)
summary(tuned_mod1)
pred1 = predict(tuned_mod1,test1)
table(pred1,test1$Category)
confusionMatrix(pred1,test1$Category)
#tuned model for radial
tuned_mod2 <- svm(Category~., data = train1, kernel='radial',cost=10,gamma=0.5)
summary(tuned_mod2)
pred2 = predict(tuned_mod2,test1)
table(pred2,test1$Category)
confusionMatrix(pred2,test1$Category)


##########random forest
library(caret)
library(rpart)
library(rattle)
library(dtree)
library(ISLR)
library(randomForest)
ranmod<-randomForest(Category~.,data = train1)
print(ranmod)
#prediction
ranmod$predicted
ranmod$ntree
pred<-predict(ranmod,test1,type = "class")
table(pred,test1$Category)
confusionMatrix(pred, test1$Category)
library(rpart.plot)
plot(ranmod)
#Cross Validation 
trctrl<-trainControl(method = "repeatedcv",number = 10, repeats = 3, search="grid")
tunegrid<-expand.grid(.mtry=c(1:10))
tunegrid
rancv<-train(Category~.,data = train1,method="rf",tuneGrid=tunegrid,trControl=trctrl)
rancv
#ploting the cross validated random forest model
plot(rancv)
test1$rancv<-predict(rancv,test1$Category)
predcv <- predict(rancv,test1)
confusionMatrix(predcv,test1$Category)




