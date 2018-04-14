#install.packages("randomForest")
#install.packages("caret")
#=================== SetWorking Dierctory==================================

setwd("C:\\Users\\ROS\\Documents\\R Tutorial Data\\R Tutorial Data Sets\\ROSDA\\Module2")

#=================== Loading libraries====================================

library(readxl)
library(caret)  
library(randomForest)
library(arules)
#====================Data import==========================================

mydata_raw <- read_excel("Survey_Key_and_Complete_Responses_excel.xlsx",sheet = 2)
my_data_raw <- read_excel("Survey_Key_and_Complete_Responses_excel.xlsx",sheet = 2)
mydata_predict <- read.csv("SurveyIncomplete.csv")
mydata= mydata_raw
#mydata= mydata_predict
ggplot(mydata)+ 
  geom_bar(aes(x =brand,fill="yellow"))


#===================Preprocessing==========================================
range(mydata$age)
range(mydata$elevel)
range(mydata$car)
range(mydata$zipcode)
range(mydata$credit) 
summary(mydata$credit)
mydata$elevel <- factor(mydata$elevel) 
mydata$brand <- factor(mydata$brand, levels = c(0,1), labels = c("Acer", "Sony"))
mydata$car <- factor(mydata$car)
mydata$zipcode <- factor(mydata$zipcode)
head(mydata,n=5)

#===================== Vizualization======================================
plot(mydata$credit,mydata$salary)
plot(mydata$elevel,mydata$salary)
str(trainSet) #structure of the dataset
#=======================data slicing method 2============================
# slicing using Caret keeping the proportions of the class label the
indexes <- createDataPartition(y=mydata$age, times=1,p=0.7,list=FALSE)
trainSet<- mydata[indexes,]
testSet<-mydata[-indexes,]
prop.table(table(trainSet$age)) * 100
prop.table(table(testSet$age)) * 100

#===========================model======================================== 
set.seed(234) 
ctrl <- trainControl(method="repeatedcv",number=2,repeats = 2) 
#RF <- randomForest(brand~ ., data=trainSet, ntree=50,
#                   importance=TRUE, proximity=TRUE)
RF <- train(brand~., data=trainSet, method= "rf", metric= "Accuracy" , trControl = ctrl,
            preProcess = c("center","scale"), tuneLength = 10)
#===========================Testing of RF=================
#varImpPlot(RF)
RF
ls(getNamespace("randomForest"), all.names=TRUE)
RFpred = predict(RF, newdata=testSet)
rf_performance<- c(round(postResample(RFpred,testSet$brand),digits = 3))
rf_performance
testset1 = testSet
testset1$brand = RFpred
testset1$salary <- discretize(testset1$salary,method = "interval",categories = 3,
                         labels = NULL,ordered = FALSE,onlycuts = FALSE) 
ggplot(testset1,aes(x =salary,y=age, color = brand))+ 
  geom_jitter(alpha=0.93)
#head(testSet)
#confusionMatrix(knnpred, testSet$brand )

#===========================predict of RF ============================
prediction=predict(RF, newdata=mydata)
head(prediction)
mydata2 = mydata
mydata2$brand = prediction
mydata2$salary <- discretize(mydata2$salary,method = "interval",categories = 3,
                              labels = NULL,ordered = FALSE,onlycuts = FALSE) 
ggplot(mydata2,aes(x =salary,y=age, color = brand))+ 
  geom_jitter(alpha=0.93)
ggplot(mydata2)+ 
  geom_bar(aes(x =brand,fill="yellow"))

write.csv(mydata, file = "predictionsRF.csv")
