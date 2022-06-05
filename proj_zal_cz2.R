library(caTools)
library(polycor)
library(utils)
library(corrplot)
library(purrr)
library(DALEX)
library(reshape2)
library(ls)
library(dplyr)
library(BBmisc)
library(tidyr)
library(MASS)
library(randomForest)
library(ggplot2)
library(ggpubr)
library(knitr)
library(kableExtra)
library(caret)
library(pROC)
library(outliers)
#dane
set.seed(2137)
data <- read.csv("data_imputed_copy.csv", header = TRUE, sep=",")
data$training_hours <- normalize(data$training_hours, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
make.names(data)~.

data$target <- ifelse(data$target=="1","Yes","No")
div <- createDataPartition(data$target, p = 0.75, list = FALSE)
train  <- data[div,]
train$target <- as.factor(train$target)
test <- data[-div,]
test$target <- as.factor(test$target)

#knn k=33
knn_model <- train(target~., data=train, method="knn", trControl=trainControl(method="cv", number=5), tuneGrid=data.frame(k=37))
knn_pr <- predict(knn_model, test)

m <- confusionMatrix(knn_pr,test$target)$table
m
acc_knn <- (m[1,1]+m[2,2])/sum(m)
plot(roc(knn_pr, as.ordered(test$target)), col="darkturquoise",main="ROC Curve for KNN (k=33)", print.auc=TRUE)
auc_knn <- auc(roc(knn_pr, as.ordered(test$target)))

#NB
train$target <- ifelse(train$target=="Yes",1,0)
test$target <- ifelse(test$target=="Yes",1,0)

x <- as.data.frame(train[,-9])
y <- as.factor(train$target)
naive_bayes_model <- train(x,y,'nb',trControl=trainControl(method='cv',number=5))

pred_nb <- predict(naive_bayes_model, newdata=test)
cm_nb <- confusionMatrix(factor(pred_nb), reference=factor(test$target))$table
acc_nb <- (cm_nb[1,1]+cm_nb[2,2])/sum(cm_nb)

par(pty = "s")
plot(roc(pred_nb, test$target), col="salmon", main="ROC Curve for Naive Bayes", print.auc=TRUE)
auc_nb <- auc(roc(pred_nb, test$target))

#QDA
model_qda <- qda(train$target~., train)
pred_qda <- predict(model_qda, test)
#confusion matrix dla danych testowych
m_qda <- table(pred_qda$class, test$target)
acc_qda <- sum(m_qda[1,1]+m_qda[2,2])/sum(m_qda)

plot(roc(pred_qda$class, test$target), col="orchid", main="ROC Curve for QDA", print.auc=TRUE)
auc(roc(pred_qda$class, test$target))
auc_qda <- roc(pred_qda$class, as.ordered(test$target))$auc

#Random Forest
train$target <- as.factor(train$target)
test$target <- as.factor(test$target)
train_rf <- randomForest(target~., data=train,importance=TRUE, proximity=TRUE,
                         ntree=500,mtry=3, nodesize=1)
rf_prog <- predict(train_rf, newdata=test)
rf_cm <- table(test$target, rf_prog)
acc_rf <- sum(diag(rf_cm))/sum(rf_cm)

roc_rf <- roc(test$target, as.numeric(rf_prog))
plot(roc_rf, col="deepskyblue3", main="ROC Curve for Random Forest (n=500)", print.auc=TRUE)
auc_rf <- auc(roc_rf)

important <- importance(train_rf, type=1)

Important_Features <- data.frame(Feature=row.names(important),Importance=important[, 1])
Important_Features$Importance<-Important_Features$Importance/sum(Important_Features$Importance)
plot_ <- ggplot(Important_Features, aes(x=reorder(Feature, Importance) , y=Importance) ) +
  geom_bar(stat="identity",fill="darkseagreen2",width=0.75) +
  coord_flip() + xlab(NULL) + ylab(NULL) +
  ggtitle("Procentowy wpływ zmiennych na prawdopodobieństwo\n zostania targetem rekrutacji w Polsce jak w lesie (100 drzew)") +
  theme(plot.title = element_text(size=8)) + theme_minimal()
plot_

ACC <- c(acc_knn, acc_nb, acc_qda, acc_rf)
ACC <- round(ACC,3)
AUC <- c(auc_knn, auc_nb, auc_qda, auc_rf)
AUC <- round(AUC,3)
names <- c("KNN","Naive Bayes","QDA","Random Forest")
results <- data.frame(ACC,AUC,names)
df2 <- melt(results, id.vars='names')
head(df2)

ggplot(df2, aes(x=names, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge', width=0.8) +
  labs(title="ACC and AUC for test set", x="Method", y="ACC / AUC") +
  geom_text(aes(label=value),col="white", size=2.5, vjust=10, position=position_dodge(.8)) + theme_minimal()
