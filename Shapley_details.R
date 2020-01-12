# Shapley value experiment
# Using Interpretable machine learning

rm(list=ls())
data("Boston", package  = "MASS")
head(Boston)
set.seed(42)
#install.packages("iml")
library("iml")
library("randomForest")
data("Boston", package  = "MASS")
rf = randomForest(medv ~ ., data = Boston, ntree = 50, importance=TRUE)

importance(rf)
varImpPlot(rf,type=1)

# using the imp Predictor
X = Boston[which(names(Boston) != "medv")]
predictor = Predictor$new(rf, data = X, y = Boston$medv)

# Feature importance
imp = FeatureImp$new(predictor, 
                     loss = "mse"
                     #loss="ce" fro classification
)
imp
plot(imp)

# explaining single predictions using Shapley
shapley=Shapley$new(predictor, x.interest=X[1,])
head(shapley$results) #plots phi, labels feature.value FROM THE DATA ROW 1
shapley$plot()
plot(shapley) #gives the same plot

# (use a different row e.g. the second row)
shapley$explain(x.interest=X[2,])
head(shapley$results)
shapley$plot()

#Explaining an average prediction
X1=rbind(X,colMeans(round(X,4)))
shapley=Shapley$new(predictor, x.interest =X1[dim(X1)[1],], sample.size=100)
plot(shapley)
