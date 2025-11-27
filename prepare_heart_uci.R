library(dplyr)
library(tidyverse) #Non credo ci servirà


url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

cols_names <- c (
  "age","sex","cp","trestbps","chol",
  "fbs","restecg","thalach","exang",
  "oldpeak","slope","ca","thal","num"
)

data_raw <- read.csv(url, header=FALSE, col.names=cols_names, na.string="?")

cat("Dataset: ", nrow(data_raw), " righe di ", ncol(data_raw), "colonne!\n")

#Must
missing_count <- colSums(is.na(data_raw))

print(missing_count[missing_count>0]) #ops

print(data_raw$num)

data_clean <- data_raw %>% na.omit() %>%
  mutate(AHD= ifelse(num==0,"No","Yes"),
         AHD = factor(AHD,levels=c("No","Yes"))
         ) %>% select(-num)
cat("Dataset pulito: ", nrow(data_clean), " righe di ", ncol(data_clean), "colonne!\n")

data_clean <- data_clean %>%
  mutate(
    Sex=factor(ifelse(sex==1,"Male","Female")),
    ChestPain = factor(case_when(
      cp==1 ~ "typical",
      cp==2 ~ "atypical",
      cp==3 ~ "nonanginal",
      cp==4 ~ "asyntomatic"),
      levels= c("typical","atypical","nonanginal","asyntomatic")
    ),
    Fbs = factor(ifelse(fbs==1,"Yes","no")),
    RestECG= factor( case_when(
      restecg ==0 ~ "normal",
      restecg ==1 ~ "abnormal",
      restecg ==2 ~ "hypertrophy"
    ),
    levels = c( "normal","abnormal","hypertrophy")
    ),
    ExAng = factor(ifelse(exang == 1, "Yes", "No")),
    
    Slope = factor(
      case_when(
        slope == 1 ~ "up",
        slope == 2 ~ "flat",
        slope == 3 ~ "down"
      ),
      levels = c("up", "flat", "down")
    ),
    Ca = factor(ca),
    Thal = factor(
      case_when(
        thal == 3 ~ "normal",
        thal == 6 ~ "fixed",
        thal == 7 ~ "reversable"
      ),
      levels = c("normal", "fixed", "reversable")
    )
  ) %>%
  select(-c(sex, cp, fbs, restecg, exang, slope, ca, thal))
        
#Rinomino le colonnee metto il nostro Target (AHD) alla fine
data_final <- data_clean %>%
  rename(
    Age = age,
    RestBP = trestbps,
    Chol = chol,
    MaxHR = thalach,
    Oldpeak = oldpeak
  ) %>%
  select(Age, Sex, ChestPain, RestBP, Chol, Fbs, RestECG, 
         MaxHR, ExAng, Oldpeak, Slope, Ca, Thal, AHD)
