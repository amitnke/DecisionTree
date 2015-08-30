DB <- read.csv("Diabetes.csv", head = T)
class(DB)
head(DB)
str(DB)
nrow(DB)

flags <- sample(1:2, nrow(DB), replace = T, prob = c(0.8, 0.2))
trainset = DB[flags == 1, ]
testset = DB[flags == 2, ]
nrow(trainset)
nrow(testset)
library(rpart)
names(DB)
#formula: class_label ~ independent_variable
dt = rpart(Class.variable ~ . , trainset, maxdepth = 10)
class(dt)
plot(dt)
text(dt)
summary(dt)
library(rpart.plot)
rpart.plot(dt)


actualLabels = testset$Diabet
actualLabels
names(testset)
testset



####Predict####
dt_predictions = predict(dt, testset, type = "class")
dt_predictions

cbind(as.character(testset$Class.variable),as.character(dt_predictions))
table(testset$Class.variable,dt_predictions )

#comparison between actual and prediction
table(actualLabels, dt_predictions)
#accuracy
(81+29)/nrow(testset)

TN = CM[1,1]
TP = CM[2,2]
FP = CM[2,1]
FN = CM[1,2] 
TP = 37
TN = 83
FN = 13
FP = 18

accuracy
#####confusion matrix######
table(as.character(testset$Class.variable),as.character(dt_predictions))


####Predict-Prob#####
pred1<-predict(dt,testset,type=c("prob"))
pred1

head(dt_predictions)
head(pred1)

attributes(dt)

dt$variable.importance


#####New Prediction######
New<-read.csv("New.csv",head=T)
predict(dt,New,type=c("class"))


##RF

rf = randomForest(Class.variable ~ . , trainset, ntree = 50, mtry =3)
rf_predictions = predict(rf, testset, type = "class")
table(testset$Class.variable, dt_predictions)
table(testset$Class.variable, rf_predictions)

perf = function(actuals, predictions){
        tab = table(actuals, predictions)
        TP = tab[2,2]
        TN = tab[1,1]
        FP = tab[1,2]
        FN = tab[2,1]
        accuracy = (TP+TN)/(TP+TN+FP+FN)
        precision = TP/((TP+FP))
        recall = TP / (TP+FN)
        print(paste("Accuracy = ", accuracy, ", Precision = ", precision, ", Recall = ", recall))
}
perf(testset$Class.variable, dt_predictions)
perf(testset$Class.variable, rf_predictions)
