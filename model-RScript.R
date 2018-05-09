insuranceDataset = read.csv("insurance.csv", header = TRUE)

head(insuranceDataset)
tail(insuranceDataset)
summary(insuranceDataset)

#---Visualizing data using ggplot2--

#install.packages('ggplot2')
#library(ggplot2)

#Plotting the age of participants and their count with respective age.
ggplot(insuranceDataset, aes(age, fill = age ) ) +
  geom_bar()

#Plotting number of males and number of females in the dataset with respect to their region.
ggplot(insuranceDataset, aes(sex, fill = region) ) +
  geom_bar(position = "stack")

#Checking to see if their is a pattern between region and person smoking.
ggplot(insuranceDataset, aes(smoker, fill = region) ) +
  geom_bar(position = "stack")

#Plotting age and BMI to see if their is a noticible pattern.
ggplot(insuranceDataset, aes(age, bmi)) +
  geom_line()

#Corelation Matrix
pairs(insuranceDataset)

#---Cleaning and transforming data to so that it better data can be used to train and test model.---

#Males are represented by 1's while females are given a numeric of 0 for identification purposes.
insuranceDataset$sex<-ifelse(insuranceDataset$sex =='male', 1,0)

#If a person is a smoker, it is indicated as 1 and if they are not, they are indicated with a 0.
insuranceDataset$smoker<-ifelse(insuranceDataset$smoker =='yes', 1,0)

#---Dividing into train and test set---

set.seed(1234)
i <- sample(1:nrow(insuranceDataset), nrow(insuranceDataset)*0.75, replace=FALSE)
train <- insuranceDataset[i,]
test <- insuranceDataset[-i,]

#---Linear Regression Model to predict charge

lm1 <- lm(charges ~ ., data=train)
lm1$coefficients

summary(lm1)

#Evaluating the model
pred <- predict(lm1, newdata=test)
cor(pred, test$charges)

#---Creating another linear regression modell but only with predictors that have ***

lm2 <- lm(charges ~  age+bmi+children+smoker, data=train)
summary(lm2)

pred2 <- predict(lm2, newdata=test)
cor(pred2, test$charges)

#---Comparision of two Linear Regression Models
anova(lm1, lm2)

