wls <- read.csv("wls_subset - highschool score and iq.csv")
#Removing NAs
library(dplyr)
data <- mutate_all(wls, list(~na_if(.,"-3")))
data <- data[,c(4:15,3,2)]
summary(data)


library(VIM)
url_plot <- aggr(data, col=c('green', 'red'), combined = TRUE,
                 numbers=TRUE, sortVars=TRUE,
                 labels=names(data), cex.axis=.7,
                 gap=3, ylab=c("Missing data","Pattern"))

data = na.omit(data)


pca <- prcomp(data[,c(1:13)], center = TRUE, scale. = TRUE)
summary(pca)
plot(pca$x[,1], pca$x[,2])
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100,1)
barplot(pca.var.per)


mini <- data %>% select("ghncr_j", "gwiiq_f", "hsrankq",  "hsrscorq") 
pca <- prcomp(mini[,c(1:3)], center = TRUE, scale. = TRUE)
summary(pca)
plot(pca$x[,1], pca$x[,2])

write.csv(data,"wlscsv.csv")



corr <- cor(center_colmeans)
library(car)





library("caTools")
set.seed(123)
split = sample.split(mini$hsrscorq, SplitRatio = 0.8)
training_set = subset(mini, split == TRUE)
test_set = subset(mini, split == FALSE)

#Feature scaling
training_set[,1:14] = scale(training_set[,1:14])
test_set[,1:14] = scale(test_set[,1:14])

install.packages("randomForest")
library(randomForest)
set.seed(123)
regressor = randomForest( formula = hsrscorq~., data= training_set)
plot(regressor)

y_pred =predict(regressor, test_set)
plot(y_pred,test_set$hsrscorq)



#Multiple linear regression


regressor = lm(formula = hsrscorq~., data = training_set )
summary(regressor)
alias(regressor)
car::vif(regressor)
y_pred = predict(regressor, newdata = test_set)
plot (test_set$hsrscorq, y_pred)


