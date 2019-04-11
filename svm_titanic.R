data <- read.csv('titanic.csv')
data <- data[ , -3]
data$Sex = factor(data$Sex, levels=c('male', 'female'), labels = c(0, 1))
library(caTools)
set.seed(42) 
split = sample.split(data$Survived, 0.75)
training_set = subset(data, split= TRUE)
test_set = subset(data, split = FALSE)

library(e1071)
classifier = svm(formula = Survived ~ .,data = training_set, type = 'C-classification', kernel = 'linear')
prob_predict = predict(classifier, type = 'response', logRegData1 = test_set[-1])
# Class_predict = ifelse(prob_predict > 0.5, 1, 0)
svmConMatrix = table(test_set[,1], prob_predict)
svmConMatrix

results_matrix = data.matrix(svmConMatrix)
LR_true_zero = as.numeric(results_matrix[1, 1])
LR_false_zero = as.numeric(results_matrix[1, 2])
LR_true_one = as.numeric(results_matrix[2, 2])
LR_false_one = as.numeric(results_matrix[2, 1])
LR_accuracy = (LR_true_one + LR_true_zero)/(LR_true_one + LR_true_zero + LR_false_one + LR_false_zero)
print("Logistic Regression Confusion/Clarity Matrix)")
svmConMatrix
print(paste("SVM Accuracy: ", toString(LR_accuracy*100) ))
