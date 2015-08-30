

# --------------------
# Entropy Calculations
# --------------------

-0.5*log2(0.5) - 0.5*log2(0.5)
-0.2*log2(0.2) - 0.8*log2(0.8)
-0.3*log2(0.3) - 0.7*log2(0.7)
-0.05*log2(0.05) - 0.95*log2(0.95)
entropy = function(p) -p*log2(p) - (1-p)*log2(1-p)
plot(entropy)
-(2/5)*log2(2/5) - (3/5)*log2(3/5)
entropy(2/5)

# ------------------------------------------------------------------------------------
# ---- Decision Tree (CART) -----
# ------------------------------------------------------------------------------------
setwd("DataScience/Class5")
dir()
D = read.csv("Diabetes.csv", header=T)
str(D)
attributes(D)

# Divide the dataset into training and test sets
# Method - 1
set.seed(564)
flags = sample(2,nrow(D), replace=TRUE, prob=c(0.7,0.3))
trainset = D[which(flags==1), ]
testset = D[which(flags==2),]
str(trainset)
str(testset)

# Method - 2 to create train and test sets
index = sample(1:nrow(D), nrow(D)*0.7, replace=FALSE)
trainset = D[index,]
testset = D[-index,]
str(trainset)
str(testset)

# Build a decision tree using rpart
?rpart
dtree = rpart(Class.variable ~ ., data=trainset, control=rpart.control(10))
str(dtree)
dtree
plot(dtree)
text(dtree)
attributes(dtree)
dtree$variable.importance

# Make prediction using "dtree"
predictedDT = predict(dtree, testset, type=c("class"))
predictedDT
table(predictedDT, testset$Class.variable) # Confusion Matrix

