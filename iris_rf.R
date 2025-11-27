set.seed(42)

library(tidyverse)
library(ggplot)
library(dplyr)
library(randomForest)

data <-iris

head(data)

summary(data)

table_species <- table(data$Species)

print(table_species)

missing_counts <- colSums(is.na(data))

print(missing_counts)

if(sum(missing_counts)==0)  {
  cat("No missing data!")
} else {
  cat("Missing data: elaborate!")
  
}

p1 <- ggplot(data, aes(x=Sepal.Length, fill=Species))+
    geom_histogram(alpha=0.6, position="identity", bins=20) +
    labs(
      title ="Distribuzione per lunghezza sepalo per specie",
      x ="Len Sepalo",
      y= "Freq"
    ) + theme_minimal()+
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        legend.position = "bottom")
    


print(p1)

data_long <- data %>% tidyr::pivot_longer(
  cols = -Species,
  names_to = "Var",
  values_to ="Value"
)

p2<- ggplot( data_long, aes( x=Species, y=Value, fill= Species)) +
   geom_boxplot(alpha=0.7) +
    facet_wrap(~ Var,scales="free_y", ncol=2)+
  labs(
    title="BoxPlot",
    y ="Val (cm)",
    x ="Specie"
  ) +
  theme_minimal() +
  theme(
    plot.title= element_text(hjust=0.5, face="bold"),
    axis.text.x = element_text(angle=45,hjust=1),
    legend.position = "none"
  )


print(p2)

cor_matrix = cor(data[,1:4])

cat("Matrice di correlazione")
print(round(cor_matrix,3))

library(randomForest)

trainIndex <- sample(1:nrow(data), 0.8* nrow(data))

trainData <- data[trainIndex,]
testData <- data[-trainIndex,]

rf_iris <- randomForest( Species ~ ., data=trainData)


print(rf_iris)

library(caret)

predictions <- predict(rf_iris, testData)


print(predictions)

confusionMatrix(predictions, testData$Species)

rf_tun <- randomForest( Species ~ . , data=trainData, ntree=500, mtry=2)


predictions2 <- predict(rf_tun,testData)

confusionMatrix(predictions2, testData$Species)

importance(rf_iris)
varImpPlot(rf_iris)

saveRDS(rf_iris, "modello_rf_iris.rds")
saveRDS(rf_tun,"modello_rf_iris_tuned500.rds")

rf_ok <- readRDS("modello_rf_iris.rds")
