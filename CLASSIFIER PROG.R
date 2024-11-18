
# Classification Tree with rpart
library(rpart)
library(caret)
attach(kyphosis)
help(kyphosis)
boxplot(Age~Kyphosis)
boxplot(Number~Kyphosis)
boxplot(Start~Kyphosis)
summary(Kyphosis)

# grow tree
fit <- rpart(Kyphosis ~ Age + Number + Start,
   method="class", data=kyphosis)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE,
   main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)




predicted.class <- predict(fit, type = "class")  # factor

summary1 <- table(predicted.class,Kyphosis)
summary1


#Confusion Matrix and associated Statistics and Tests in R 

confusionMatrix(summary1)



# prune the tree 
pfit<- prune(fit, cp=0.019608 ) # from cptable   


# plot the pruned tree 
plot(pfit, uniform=TRUE, 
   main="Pruned Classification Tree for Kyphosis")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)


predicted.class.p <- predict(pfit, type = "class")  # factor
#Tables
summary1p <- table(predicted.class.p,Kyphosis)
summary1p
confusionMatrix(summary1p)







