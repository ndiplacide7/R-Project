library(e1071)
library(caret)
#Reading in the data
bayes.data <- read.csv("D:\\datasets\\DATA.SMALL.BUSS.csv",header=TRUE)
attach(bayes.data)
 summary(as.factor(bayes.data$DEFAULT))
#Sampling Training and Testing Data

Indices.Training <- sample(1:nrow(bayes.data), round((2/3)*nrow(bayes.data),0),replace=FALSE )

#Testing Set
Training.set  <- bayes.data[Indices.Training,]

#Testing Set
Testing.set  <- bayes.data[-Indices.Training,]

#Checking for samples adequacy especially on the Default Variable

summary(Training.set)
summary(Testing.set)

#Naive Bayes for the Training set

model.naive.training <- naiveBayes(DEFAULT ~BUSAGE + DAYSDELQ, data=Training.set, laplace=0)
model.naive.training
#IN-SAMPLE PERFORMANCE

#Classifications for Training Data
prob.training <- predict(model.naive.training, newdata=Training.set,  type = "raw")
class.training <- predict(model.naive.training, newdata=Training.set,  type = "class")

View(data.frame(prob.training,class.training,Training.set$DEFAULT))

#Confusion Matrix for Training Data

confusionMatrix(class.training,as.factor(Training.set$DEFAULT))

#OUT-OF-SAMPLE PERFORMANCE
#Class Probabilities
prob.testing <- predict(model.naive.training, newdata=Testing.set,  type = "raw")

#Classifications for Testing Data
class.testing <- predict(model.naive.training, newdata=Testing.set,  type = "class")

View(data.frame(prob.testing,class.testing,Testing.set$DEFAULT))

#Confusion Matrix for Training Data

confusionMatrix(class.testing,as.factor(Testing.set$DEFAULT))



 



