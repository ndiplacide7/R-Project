
library(datasets)
library(nnet)
library(caret)
data("iris")
attach(iris)

 

indices <- sample(1:nrow(iris),0.75*nrow(iris), replace=FALSE)

Entire.Training.Data <- iris[indices,]
Predictors.Training.Data <- iris[indices,c(1,2,3,4)]
True.Training.Class <- iris[indices,5]

Predictors.Testing.Data <- iris[-indices,c(1,2,3,4)]
True.Testing.Class <- iris[-indices,5]

summary(Entire.Training.Data)

#Fitting the nnet model with 2 hidden nodes iin 1 hidden layer 
nnet.model <- nnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = Entire.Training.Data, 
  size = 2, rang = 0.1, decay = 5e-04, maxit = 200, trace = FALSE)

### CONSIDER USING NEURALNETTOOLS FOR BETTER VISUALIZATIONS

summary(nnet.model)

#Visualisation
plotnet(nnet.model)

################################################################################ 
#IN-SAMPLE STATISTICS
################################################################################
Classification.Training.Probabilities <-  nnet.model$fitted.values
Predicted.Training.Class <-  predict(nnet.model, Predictors.Training.Data, type = "class")

View(data.frame(Classification.Training.Probabilities,Predicted.Training.Class,True.Training.Class))
confusionMatrix(True.Training.Class,as.factor(Predicted.Training.Class))
   

################################################################################ 
#OUT-OF-SAMPLE STATISTICS
################################################################################

Predicted.Testing.Class <-  predict(nnet.model, newdata = Predictors.Testing.Data, type = "class")

View(data.frame(Predicted.Testing.Class,True.Testing.Class))
confusionMatrix(True.Testing.Class,as.factor(Predicted.Testing.Class))


##############################################################################
#Multi Layer Perceptron
### CONSIDER USING NEURALNETTOOLS FOR BETTER VISUALIZATIONS
##############################################################################
  
library(NeuralNetTools)
library(RSNNS)

Predictors.Training.Data <- as.data.frame(Predictors.Training.Data)
True.Training.Class <- decodeClassLabels(True.Training.Class)
model.mp <- mlp(Predictors.Training.Data, True.Training.Class, size=c(2,2), learnFuncParams=c(0.1), 
              maxit=50)
plotnet(model.mp)
summary(model.mp)

#In-Sample Confusion matrix

predicted.training.class.mlp <- predict(model.mp, newdata=Predictors.Training.Data)  

confusionMatrix(True.Training.Class,predicted.training.class.mlp)

#out-of--Sample Confusion matrix

predicted.testing.class.mlp <- predict(model.mp, newdata=Predictors.Testing.Data)  

confusionMatrix(True.Testing.Class,predicted.testing.class.mlp) 







