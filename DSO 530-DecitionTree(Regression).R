library(MASS)
attach(Boston)
library(tree)
set.seed(1)

train = sample(1:nrow(Boston), nrow(Boston)/2)
test = -train

training_data = Boston[train,]
testing_data = Boston[test,]
#testing medium value that we are going to predict
testing_medv = medv[test]
## all fields are number, so we are good for regression
tree_model = tree(medv~., training_data)
tree_model


pdf("Tree1.pdf",width=7,height=5)
plot(tree_model)
dev.off()
text(tree_model, preet = 0)
dev.off()
##Check the model is doing using the testing dataset
tree_pred = predict(tree_model, testing_data)
mean((tree_pred - testing_medv)^2)

##Cross Validation on prunning the tree
cv_tree = cv.tree(tree_model) 
names(cv_tree)
pdf("Tree2.pdf",width=7,height=5)
plot(cv_tree$size, cv_tree$dev, type = "b", xlab = "Tree Size", ylab = "MSE")
dev.off()

which.min(cv_tree$dev)
cv_tree$size[1]

##prune the tree to size 4
pruned_model = prune.tree(tree_model, best =4)

plot(pruned_model)
text(pruned_model)

##Check the accuracy of model using test data
tree_pred = predict(pruned_model, testing_data)
mean((tree_pred - testing_medv)^2)


