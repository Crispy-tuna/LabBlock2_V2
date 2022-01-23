# Lab block2----------------------------------------------------------------------------
library(kernlab)

# Read data---------------------------------------------------------------------------
data_read = read.csv("adult.data")
data = data_read
data$X..50K = as.factor(data$X..50K)
# Clean data
for(i in 1:nrow(data)){
  for(j in 1:ncol(data)){
    if(isTRUE(as.character(data[i,j]) == " ?")){
      data = data[-i,]
    }
  }
}
# Divide data
n <- sample(nrow(data))
data <- data[n,]
valid <- data[1:9058, ] # training data-set
train <- data[9059:18116, ] # validation data-set
trva <- data[1:18117, ] # training combine validation data-set
test <- data[18119:30194, ]  # test data-set

# SVM---------------------------------------------------------------------------
# choose C 
by = 0.1
err_va = NULL
for(i in seq(by,5,by)){ # set C parameters from 0.1 to 5.0
  filter = ksvm(X..50K~.,data=train,kernel="rbfdot",kpar=list(sigma=0.05),C=i,scaled=FALSE)
  mailtype = predict(filter,valid[,-15])
  t = table(mailtype,valid[,15])
  err_va = c(err_va,(t[1,2]+t[2,1])/sum(t))
}

# Svm model
c = which.min(err_va)*by # 0.8
svm_model <- ksvm(X..50K~., data = rbind(train,valid), kernel="rbfdot",kpar=list(sigma=0.05),
                  C = c, scaled=FALSE)

# Reset test data
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

pre <- predict(svm_model, test[,-15]) # test error
t <- table(pre, test[,15])
error <- (t[1,2]+t[2,1])/sum(t)
1 - error
