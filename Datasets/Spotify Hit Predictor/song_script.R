# Music hit prediction using logistic regression and naive bayes.
library(dplyr)
#Importing files
song60s <- read.csv("dataset-of-60s.csv") 
song70s <- read.csv("dataset-of-70s.csv") 
song80s <- read.csv("dataset-of-80s.csv") 
song90s <- read.csv("dataset-of-90s.csv") 
song00s <- read.csv("dataset-of-00s.csv") 
song10s <- read.csv("dataset-of-10s.csv") 
#Combining all
songsAll <- rbind(song60s, song70s, song80s, song90s, song00s, song10s)
glimpse(songsAll)
songsAll <- songsAll[,4:19]
#making sure there are no NA 
a <- sum(is.na(songsAll$track))
a <- sum(is.na(songsAll$artist))
a <- sum(is.na(songsAll$uri))
a <- sum(is.na(songsAll$danceability))
a <- sum(is.na(songsAll$energy))
a <- sum(is.na(songsAll$key))
a <- sum(is.na(songsAll$loudness))
a <- sum(is.na(songsAll$mode))
a <- sum(is.na(songsAll$speechiness))
a <- sum(is.na(songsAll$acousticness))
a <- sum(is.na(songsAll$instrumentalness))
a <- sum(is.na(songsAll$liveness))
a <- sum(is.na(songsAll$valence))
a <- sum(is.na(songsAll$tempo))
a <- sum(is.na(songsAll$duration_ms))
a <- sum(is.na(songsAll$time_signature))
a <- sum(is.na(songsAll$chorus_hit))
a <- sum(is.na(songsAll$sections))
a <- sum(is.na(songsAll$target))
rm(a)

songsAll[, 'mode'] = factor(songsAll[,'mode'])
songsAll[, 'target'] = factor(songsAll[,'target'])

table(songsAll$target)
#PCA
pca <- prcomp(songsAll[,c(1:4,6:15)], center = TRUE, scale. = TRUE)
summary(pca)
plot(pca$x[,1], pca$x[,2])

#install.packages("caTools")
library("caTools")
set.seed(123)
split = sample.split(songsAll$target, SplitRatio = 0.8)
training_set = subset(songsAll, split == TRUE)
test_set = subset(songsAll, split == FALSE)

#Feature scaling
training_set[,1:4] = scale(training_set[,1:4])
training_set[,6:15] = scale(training_set[,6:15])
test_set[,1:4] = scale(test_set[,1:4])
test_set[,6:15] = scale(test_set[,6:15])


#NAive Bayes
library(e1071)
classifier = naiveBayes( x = training_set[-16], y = training_set$target)
y_pred = predict(classifier, newdata = test_set[-16])
cm = table(test_set[,16], y_pred)
confusionMatrix(test_set[,16], y_pred)


#lOGISTIC REGRESSION
classifier = glm(formula = target ~ ., family = binomial, data = training_set)
prob_pred = predict(classifier, type = "response", newdata = test_set[-16])
y_pred = ifelse(prob_pred > 0.5, 1, 0)
cm = table(test_set[, 16], y_pred)
confusionMatrix(test_set[, 16], factor(y_pred))





#Random Forest Classification
library(randomForest)
classifier = randomForest(x = training_set[-16], y = training_set$target, ntree = 120)
y_pred = predict(classifier, newdata = test_set[-16])
cm = table(test_set[, 16], y_pred)
confusionMatrix(test_set[, 16], y_pred)

#Logistic Regression and  K Fold Cross Validation
#install.packages("caret")
library(caret)
folds = createFolds(training_set$target,k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = test_set[x,]
  classifier = glm(formula = target ~ ., family = binomial, data = training_fold)
  prob_pred = predict(classifier, type = "response", newdata = test_fold[-16])
  y_pred = ifelse(prob_pred > 0.5, 1, 0)
  cm = table(test_fold[, 16], y_pred)
  roc.curve(test_fold$target, y_pred)
  accuracy = (cm[1,1] + cm[2,2])/(cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return (accuracy)
  })
accuracy = mean(as.numeric(cv))
accuracy

#Naive Bayes Classification

folds = createFolds(training_set$target,k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = test_set[x,]
  classifier = naiveBayes( x = training_fold[-16], y = training_fold$target)
  y_pred = predict(classifier, newdata = test_fold[-16])
  cm = table(test_fold[,16], y_pred)
  accuracy = (cm[1,1] + cm[2,2])/(cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return (accuracy)
})
accuracy = mean(as.numeric(cv))
accuracy
