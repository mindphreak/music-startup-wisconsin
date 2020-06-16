library(dplyr)
library(tidyr)

crunchbase_data <- read.csv("investments_VC.csv")
crunchbase_data <- crunchbase_data %>% select("seed", "venture", "debt_financing", "angel", "funding_rounds", "round_A", "round_B", "round_C", "status") 

#Removing NAs
crunchbase_data <- mutate_all(crunchbase_data, list(~na_if(.,"")))

library(VIM)
url_plot <- aggr(crunchbase_data, col=c('green', 'red'), combined = TRUE,
                 numbers=TRUE, sortVars=TRUE,
                 labels=names(crunchbase_data), cex.axis=.7,
                 gap=3, ylab=c("Missing data","Pattern"))
startups <- na.omit(crunchbase_data)
table(startups$status)
#trimming leading and trailing white space from status
startups$status <- trimws(startups$status, which = c("both"))



startups <- startups[!(startups$status == "closed" | startups$status==""),]
startups <- mutate_all(startups, list(~na_if(.,"")))
startups <- na.omit(startups)

table(startups$status)

startups$status = factor(startups$status, levels = c('acquired', 'operating'), labels = c(0,1))
glimpse(startups)

pca <- prcomp(startups[,c(1:8)], center = TRUE, scale. = TRUE)
summary(pca)
plot(pca$x[,1], pca$x[,2])


#Splitting into training and test
library(caret)
train.index <- createDataPartition(startups$status, p = .7, list = FALSE)
training_set <- startups[ train.index,]
testing_set  <- startups[-train.index,]
table(training_set$status)
table(testing_set$status)


#Feature scaling
training_set[,1:8] = scale(training_set[,1:8])
testing_set[,1:8] = scale(testing_set[,1:8])


set.seed(123)
#install.packages("ROSE")
library(ROSE)
startups.rose <- ROSE(status ~ ., data = training_set, seed = 1)$data
table(startups.rose$status)
startups.rose$status <- factor(startups.rose$status, levels = rev(levels(startups.rose$status)))



#SMOTE sampling
#install.packages("DMwR")
library(DMwR)
startups.smote <- SMOTE(status~., data = training_set, k = 10, perc.over = 200)
table(startups.smote$status)







#Fitting Logistic Regression to the training set

classifier = glm(formula = status ~ ., family = binomial, data = startups.rose)
summary(classifier)

#Predicting test results
prob_pred = predict(classifier, type = "response", newdata = testing_set[,1:8])
y_pred = ifelse(prob_pred > 0.5, 1, 0)
confusionMatrix(testing_set[,9], factor(y_pred))

#Decision tree
library(rpart)
tree.rose <- rpart(status ~ ., data = startups.rose)
y_pred <- predict(tree.rose, newdata = testing_set[-9], type = "class")
roc.curve(testing_set$status, y_pred)
confusionMatrix(testing_set[,9], y_pred)

#SVM
#install.packages("e1071")
library(e1071)
classifier = svm(formula = status ~ ., data = startups.rose, type = 'C-classification', kernel = 'linear')
y_pred = predict(classifier, newdata = testing_set[-9])
cm = table(testing_set[, 9], y_pred)
confusionMatrix(testing_set[, 9], y_pred)


