library(bnpa)
library(caTools)
library(class)
library(outliers)
library(stats)
library(BBmisc)
library(caret)
library(pROC)
library(dplyr)
library(kknn)
library(e1071)
library(tidyr)
library(ggplot2)
library(mice)
library(DMwR2)
library(farver)
#wybrać zbiór danych i przeprowadzić jego analizę opisową
#(statystyki opisowe, związki między zmiennymi objaśniającymi,
#związki między objaśniającą i objaśnianą, ew. analizę danych odstających
#oraz ew. imputację braków danych)

data_raw <- read.csv("HR_data.csv", header = TRUE, sep = ",", na.strings=c(""," ","NA"))
#struktura danych
dim(data_raw)
head(data_raw,5)
str(data_raw)

#usuwamy kolumnę z id, city development index,...
data <- data_raw[,!(names(data_raw) %in% c("enrollee_id","city","city_development_index","company_size","company_type"))]
str(data)

#braki danych
check.na(data)
#imputacja Random Forest
miceMod <- mice(data, method="rf")
data <- complete(miceMod)
anyNA(data) #check.na(data) equally

data2 <- data
data <- data2
data$experience <- as.character(data$experience)

#podmieniamy
for(i in 1:nrow(data))
{
  if(data$experience[i]=="1" || data$experience[i]=="2" || data$experience[i]=="3" || data$experience[i]=="4")
    data$experience[i] <- "1-4"
  else if(data$experience[i]=="5" || data$experience[i]=="6" || data$experience[i]=="7" || data$experience[i]=="8" || data$experience[i]=="9")
    data$experience[i] <- "5-9"
  else if(data$experience[i]=="10" || data$experience[i]=="11" || data$experience[i]=="12" || data$experience[i]=="13" || data$experience[i]=="14")
    data$experience[i] <- "10-14"
  else if(data$experience[i]=="15" || data$experience[i]=="16" || data$experience[i]=="17" || data$experience[i]=="18" || data$experience[i]=="19" || data$experience[i]=="20")
    data$experience[i] <- "15-20"
}
data$experience <- as.factor(data$experience)
str(data)

write.csv(data,"/Users/magdalenanitefor/Documents/Magisterka/II\ semestr/Uczenie\ statystyczne\ w\ R/data_imputed.csv",row.names = FALSE)
