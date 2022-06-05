library(caret)
library(outliers)
library(ROCR)
library(BBmisc)
library(pROC)

data <- read.csv("data_imputed.csv", header = TRUE, sep = ",")

#normalizacja danych
data$training_hours <- normalize(data$training_hours, method = "standardize", range = c(0, 1),
                        margin = 1L, on.constant = "quiet")

make.names(data)~.

#usunięcie outlierów pod względem training_hours
gt <- grubbs.test(data$training_hours, type = 10, opposite = FALSE, two.sided = FALSE)
#brak outlierów

data$target <- ifelse(data$target=="1","Yes","No")

#dla wybranej techniki ML przeprowadzić tuningowanie hiperparametrów
#podział danych na uczące i testowe
div <- createDataPartition(data$target, p = 0.75, list = FALSE)
train  <- data[div,]
train$target <- as.factor(train$target)
test <- data[-div,]
test$target <- as.factor(test$target)
str(train)

#KNN z walidacją krzyżową 80/20
accuracies <- c()
sequence <- seq(3,39,by=2)
aucs <- c()
for (i in seq(3,39,by=2))
{
  knn_model3 <- train(target~., data=train, method="knn",
                      trControl=trainControl(method="cv", number=5), tuneGrid=data.frame(k=i))
  knn_pr_3 <- predict(knn_model3, test)
  m <- confusionMatrix(knn_pr_3,test$target)$table
  acc <- (m[1,1]+m[2,2])/sum(m)
  accuracies <- append(accuracies,acc)
  aucs <- append(aucs,roc(knn_pr_3, as.ordered(test$target))$auc)
}
accs <- data.frame(cbind(sequence,accuracies))
aucs2 <- data.frame(cbind(sequence,aucs))
plot(accs, col="dodgerblue", xlab="k", ylab="accuracies", type="l")
plot(aucs2, col="pink", xlab="k", ylab="AUC", type="l")
