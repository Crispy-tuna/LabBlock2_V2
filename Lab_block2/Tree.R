# Tree--------------------------------------------------------------------------
library(tree)
library(ggplot2)
library(rpart)
# Read data---------------------------------------------------------------------
data_read = read.csv("adult.data")
data = data_read
# Clean and devide data---------------------------------------------------------
for(i in 1:nrow(data)){
  for(j in 1:ncol(data)){
    if(isTRUE(as.character(data[i,j]) == " ?")){
      data = data[-i,]
    }
  }
}
data$X..50K = as.factor(data$X..50K)
# divide data 
n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n * 0.4))
train = data[id, ]
id1 = setdiff(1:n, id)
set.seed(12345)
id2 = sample(id1, floor(n * 0.3))
valid = data[id2, ]
id3 = setdiff(id1, id2)
test = data[id3, ]

# Default tree------------------------------------------------------------------
tree_a = tree(as.factor(X..50K)~., train)
#--train data
Yfit = predict(tree_a, train, type="class")
train_error1 = 1 - sum(diag(table(train$X..50K, Yfit))) / sum(table(train$X..50K,Yfit))
#--valid data
Yfit = predict(tree_a, valid, type="class")
valid_error1 = 1 - sum(diag(table(valid$X..50K,Yfit))) / sum(table(valid$X..50K,Yfit))
valid_error1 # 0.2104217

# Smallest node size 5000-------------------------------------------------------
tree_b = tree(as.factor(X..50K)~., train, minsize = 5000)
#--train data
Yfit = predict(tree_b, train, type="class")
train_error2 = 1 - sum(diag(table(train$X..50K,Yfit))) / sum(table(train$X..50K,Yfit))
#--valid data
Yfit = predict(tree_b, valid, type="class")
valid_error2 = 1 - sum(diag(table(valid$X..50K,Yfit))) / sum(table(valid$X..50K,Yfit))

# Minimum deviance 0.0005-------------------------------------------------------
tree_c = tree(as.factor(X..50K)~., train, mindev = 0.0005)
#--train data
Yfit = predict(tree_c, train, type="class")
train_error3 = 1 - sum(diag(table(train$X..50K,Yfit))) / sum(table(train$X..50K,Yfit))
#--valid data
Yfit = predict(tree_c, valid, type="class")
valid_error3 = 1 - sum(diag(table(valid$X..50K,Yfit))) / sum(table(valid$X..50K,Yfit))
# 0.1806138

# Misclassification rates
data.frame(tree_a=c(train_error1,valid_error1),
           tree_b=c(train_error2,valid_error2),
           tree_c=c(train_error3,valid_error3),
           row.names = c("Train_error_rates: ","Valid_error_rates: "))

# Optimal tree------------------------------------------------------------------
# Choose the optimal tree depth
trainScore = rep(0,50)
validScore = rep(0,50)

for(i in 2:50){ 
  prunedTree = prune.tree(tree_c, best=i)
  pred = predict(prunedTree, newdata=valid, type="tree")
  trainScore[i] = deviance(prunedTree)
  validScore[i] = deviance(pred)
}

df = data.frame(trainScore[2:50],validScore[2:50])
ggplot(df)+
  geom_point(aes(x=2:50, y=validScore[2:50],color = "Validation data"))+
  geom_point(aes(x=2:50, y=trainScore[2:50],color = "Training data"))+
  geom_line(aes(x=2:50, y=validScore[2:50],color = "Validation data"))+
  geom_line(aes(x=2:50, y=trainScore[2:50],color = "Training data"))+
  xlab("Number of leaves  [2:50]") + ylab("Deviance")+
  ggtitle("Dependence of deviances on the number of leaves")+
  scale_colour_manual("",
                      breaks = c("Training data","Validation data"),
                      values = c("steelblue","red"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_set(theme_bw())

# Optimal amount of leaves: 42
which.min(validScore[2:50]) + 1

# Variable importance
optimal_tree = prune.tree(tree_c, best=42)

# prediction--------------------------------------------------------------------
# Optimal tree
tree_optimal = prune.tree(tree_c, best=42)
Yfit = predict(tree_optimal, test, type="class")
#--Confusion matrix
confusion_optimal = table(test$X..50K, Yfit)
confusion_optimal
#--Accuracy rate
accuracy_optimal = sum(diag(confusion_optimal)) / sum(confusion_optimal)
accuracy_optimal # 0.8220554
