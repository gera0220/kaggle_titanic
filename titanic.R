library(tidyverse)
train <- read_csv("train.csv")
test <- read_csv("test.csv")

#cabin and ticket column is not pretty important

train <- train %>% 
  select(-Cabin, -Ticket)

summary(train)

#There are NA values in age column, I'll replace them with the age mean
#summary(train$Age)

train <- train %>% mutate(Age = ifelse(is.na(Age), 29, Age))

train <- train %>% 
  na.omit(Embarked)

#Dummies variables

train <- train %>% mutate(Sex =ifelse(Sex == "female", 0 , 1))

train <- train %>% mutate(Embarked_C =ifelse(Embarked == "C", 1 , 0))

train <- train %>% mutate(Embarked_S = ifelse(Embarked == "S", 1, 0))

train <- train %>% mutate(Embarked_Q = ifelse(Embarked == "Q", 1, 0))

write_csv(train, "train_clean.csv")


