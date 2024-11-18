
library(datasets)
data("iris")
attach(iris)
library(e1071)
library(caret)
 

indices <- sample(1:nrow(iris),0.75*nrow(iris), replace=FALSE)

Entire.Training.Data <- iris[indices,]
Predictors.Training.Data <- as.data.frame(iris[indices,c(1,2,3,4)])
summary(Entire.Training.Data)
True.Training.Class <- as.factor(iris[indices,5])

#Assessing the variable importance.
#Note that variable importance is on the bigger the better basis.
#For classification models, Variable importance is per class of the target variable
filterVarImp(Entire.Training.Data[,-5],  Entire.Training.Data[,5] , nonpara = FALSE)

model.svm <- svm(Species ~  Sepal.Length +Sepal.Width+ Petal.Length+ Petal.Width,data = Entire.Training.Data,
 type = "C-classification",kernel = "linear")





#################################################################################################
##IN-SAMPLE STATISTICS AND PLOTS
#################################################################################################
# Predicting the Test set results 
Pred.Training.Data = predict(model.svm, newdata = Predictors.Training.Data) 
View(data.frame(Pred.Training.Data,True.Training.Class))

confusionMatrix(Pred.Training.Data,True.Training.Class) 
#Model Statistics
summary(model.svm)
#Visualisations

plot(model.svm, data=Entire.Training.Data, Petal.Width ~ Petal.Length,svSymbol = "S", dataSymbol = "D",
 symbolPalette = rainbow(4),color.palette = terrain.colors,slice = list(Sepal.Width = 3, Sepal.Length = 4))

#################################################################################################
##OUT-OF-SAMPLE STATISTICS AND PLOTS
#################################################################################################
True.Testing.Class <- as.factor(iris[-indices,5])
Predictors.Testing.Data <- as.data.frame(iris[-indices,c(1,2,3,4)])
Pred.Testing.Data = predict(model.svm, newdata = Predictors.Testing.Data) 
View(data.frame(Pred.Testing.Data,True.Testing.Class))
 
 






