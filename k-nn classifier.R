
library(class)
library(caret)
#Reading in the data
knn.data <- read.csv("C:\\Users\\Administrator\\Desktop\\LECTURE NOTES\\KNN\\DATA.SMALL.BUSS.csv",header=TRUE)
attach(knn.data)

#Sampling Training and Testing Data
set.seed(1234)
Indices.Training <- sample(1:nrow(knn.data), round((2/3)*nrow(knn.data),0),replace=FALSE )

#Testing Set
Training.set  <- knn.data[Indices.Training,]

#Testing Set
Testing.set  <- knn.data[-Indices.Training,]

#Checking for samples adequacy especially on the Default Variable

summary(Training.set)
summary(Testing.set)

#Fitting the K-NN model
training.predictors <- Training.set[,c(1,2)]
training.outcome <- Training.set[,3] 
test.predictors <- Testing.set[,c(1,2)] 
test.outcome <- Testing.set[,3]

knn.modell <- knn(training.predictors, test.predictors, training.outcome , k = 1, prob = FALSE)

#Viewing the Original data and the Predicted Outcome
View(cbind(test.predictors,test.outcome,knn.modell))

#Tables
table(knn.modell)
summary1 <- table(knn.modell,test.outcome)
summary1
summary2 <- prop.table(summary1)
summary2
# % of correct classifications
sum(diag(summary2)) #You may fine tune the model by adjusting K against the reported overall Percentage

#Confusion Matrix and associated Statistics and Tests in R 

confusionMatrix(summary1)


