library(tidyverse)
library(dplyr)
library(randomForest)
library(caret)
library(pROC)

#Preprocessor
#source("prepare_heart_uci.R)
fname<-"heart_uci.rds"

if(file.exists(fname)) {
  cat("loading data from:",fname)
  data<-readRDS(fname)
} else {
  cat("unable to find: ", fname)
  stop("No DataAvaialables")
}


p1 <- ggplot(data, aes(x=Age,fill=AHD)) +
  geom_histogram( alpha= 0.6, position ="identity", bins=20) + 
  labs(title="Età per diagnosi", x="Età", y="Count")+
  theme_minimal()
print(p1)

trainIndex <- createDataPartition(data$AHD, p=0.75, list=FALSE)

train_data <- data[trainIndex,]
test_data <- data[-trainIndex, ]

print(table(train_data$AHD))
print(table(test_data$AHD))

preProc<-  preProcess(train_data, method = c("center","scale"))

train_proc <- predict(preProc,train_data)
test_proc <- predict(preProc,test_data)

#Hyper Params

train_control <- trainControl(
  method="cv",
  number=5,
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)

library(caret)
rf_heart <- train( AHD ~ ., data=train_proc, method="rf",
                   trControl= train_control,
                   ntree=500,importance=TRUE,metric= "ROC")

print(rf_heart)

#Test
pred_class= predict(rf_heart, test_proc )
pred_prob = predict(rf_heart, test_proc, type="prob")

confusion_matrix <- confusionMatrix(pred_class,test_proc$AHD, positive = "Yes")

print(confusion_matrix)

accuracy <- confusion_matrix$overall['Accuracy']
specific <- confusion_matrix$overall['Specificify']


#Metrica 
roc_obj <- roc(test_proc$AHD,pred_prob$Yes)
auc_val <- auc(roc_obj)


plot(roc_obj, main=paste("ROC Curve AUC=", round(auc_val,3)),
     col="darkred",lwd=2,print.auc=FALSE)


abline(a=0,b=1,lty=2, col="gray")
grid()

saveRDS(rf_heart,"rf_heart.rds")
