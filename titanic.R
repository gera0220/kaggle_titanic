library(tidyverse)
train <- read_csv("train.csv")
test <- read_csv("test.csv")

train <- mutate(train, is.train = TRUE)
test <- mutate(test, is.train = FALSE)
test <- mutate(test, Survived = NA)

#combine train and test to clean data

full <- rbind(train, test)
full <- mutate(full, Embarked = replace(Embarked, is.na(Embarked), "S"))

#cabin and ticket column are not pretty important
full <- full %>% 
  select(-Cabin, -Ticket)

#There are NA values in age column, I'll replace them with the age median
#summary(full$Age)

full <- full %>% 
  mutate(Age = replace(Age, is.na(Age), median(full$Age, na.rm = TRUE)))

#Same way, I'll replace NA value in fare with the median

full <- full %>% 
  mutate(Fare = replace(Fare, is.na(Fare), median(full$Fare, na.rm = TRUE)))

#Factors

full$Pclass <- as.factor(full$Pclass)
full$Sex <- as.factor(full$Sex)
full$Embarked <- as.factor(full$Embarked)

#Splitting full df

train <- filter(full, is.train == TRUE)
test <- filter(full, is.train == FALSE)

#Factors again without NAs for Survived

train$Survived <- as.factor(train$Survived)

write_csv(train, "train_clean.csv")
write_csv(test, "test_clean.csv")

#Modeling

formula <- as.formula("Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                       Fare + Embarked")

library(randomForest)

model <- randomForest(formula = formula, data = train, ntree = 500, mtry = 3,
                      nodesize = 0.01 * nrow(test))

PassengerId <- select(test, PassengerId) %>% 
  mutate(Survived = as.character(predict(model, test)))

write_csv(PassengerId, "results.csv")

