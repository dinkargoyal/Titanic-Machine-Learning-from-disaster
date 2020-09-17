titanic_train <- read.csv("titanictrain.csv", header = TRUE)
View(titanic_train)

#passengerid
titanic_train <- titanic_train[,-1]

#pclass
summary(titanic_train$Pclass)
cor(titanic_train$Pclass, titanic_train$Survived)

#name
match("Name", names(titanic_train))
titanic_train <- titanic_train[,-3]

#sex
summary(titanic_train$Sex)

#age
summary(titanic_train$Age)
titanic_train$Age[is.na(titanic_train$Age)] <- mean(titanic_train$Age,na.rm = TRUE)
summary(titanic_train$Age)
cor(titanic_train$Age,titanic_train$Survived)

#sibsp
summary(titanic_train$SibSp)
table(titanic_train$SibSp)
cor(titanic_train$SibSp, titanic_train$Survived)
uv = 3* quantile(titanic_train$SibSp, 0.99)
titanic_train$SibSp[titanic_train$SibSp > uv] <- uv
summary(titanic_train$SibSp)
boxplot(titanic_train$SibSp)

#parch
summary(titanic_train$Parch)
cor(titanic_train$Parch, titanic_train$Survived)
table(titanic_train$Parch)

#ticket
titanic_train <- titanic_train[,-7]

#fare
summary(titanic_train$Fare)
boxplot(titanic_train$Fare)
uv = 3* quantile(titanic_train$Fare, 0.95)
titanic_train$Fare[titanic_train$Fare > uv] <- uv
summary(titanic_train$Fare)
hist(titanic_train$Fare)

#cabin
summary(titanic_train$Cabin)
titanic_train <- titanic_train[,-8]

#embarked
summary(titanic_train$Embarked)

#creating dummy variable
library(dummies)
titanic_train <- dummy.data.frame(titanic_train)

#removal of dummy variable
titanic_train <- titanic_train[,-9]

titanic_test <- read.csv("titanictest.csv", header = TRUE)
View(titanic_test)

#passengerid
titanic_test <- titanic_test[,-1]

#name
titanic_test <- titanic_test[,-2]

#sex
summary(titanic_test$Sex)

#age
summary(titanic_test$Age)
titanic_test$Age[is.na(titanic_test$Age)] <- mean(titanic_test$Age,na.rm = TRUE)
summary(titanic_test$Age)

#sibsp
summary(titanic_test$SibSp)
table(titanic_test$SibSp)
uv = 3* quantile(titanic_test$SibSp, 0.99)
titanic_test$SibSp[titanic_test$SibSp > uv] <- uv
summary(titanic_test$SibSp)
boxplot(titanic_test$SibSp)


#parch
summary(titanic_test$Parch)
table(titanic_test$Parch)

#ticket
titanic_test <- titanic_test[,-6]

#fare
summary(titanic_test$Fare)
titanic_test$Fare[is.na(titanic_test$Fare)] <- mean(titanic_test$Fare,na.rm = TRUE)
summary(titanic_test$Fare)

boxplot(titanic_test$Fare)
uv = 3* quantile(titanic_test$Fare, 0.95)
titanic_test$Fare[titanic_test$Fare > uv] <- uv
summary(titanic_test$Fare)
hist(titanic_test$Fare)

#cabin
summary(titanic_test$Cabin)
titanic_test <- titanic_test[,-7]


#embarked
summary(titanic_test$Embarked)


#creating dummy variable
library(dummies)
titanic_test <- dummy.data.frame(titanic_test)


#training model
model1 <- glm(Survived ~ ., family = 'binomial', data = titanic_train)
summary(model1)

#prediction
pred1 = predict(model1, type = 'response', newdata = titanic_test)
y_pred1 = ifelse(pred1 > 0.5, 1, 0)
View(y_pred1)

summary(y_pred1)
table(y_pred1)

#backward elimination
titanic_train <- titanic_train[,-9:-11]
titanic_train <- titanic_train[,-4]

titanic_test <- titanic_test[,-8:-10]
titanic_test <- titanic_test[,-3]


#training model
model2 <- glm(Survived ~ ., family = 'binomial', data = titanic_train)
summary(model2)

#prediction
pred2 = predict(model2, type = 'response', newdata = titanic_test)
y_pred2 = ifelse(pred2 > 0.5, 1, 0)
View(y_pred2)

summary(y_pred2)
table(y_pred2)


#backward elimination
titanic_train <- titanic_train[,-6]
titanic_test <- titanic_test[,-5]


#training model
model3 <- glm(Survived ~ ., family = 'binomial', data = titanic_train)
summary(model3)

#prediction
pred3 = predict(model3, type = 'response', newdata = titanic_test)
y_pred3 = ifelse(pred3 > 0.5, 1, 0)
View(y_pred3)

summary(y_pred3)
table(y_pred3)

#backward elimination
titanic_train <- titanic_train[,-6]
titanic_test <- titanic_test[,-5]


#training model
model4 <- glm(Survived ~ ., family = 'binomial', data = titanic_train)
summary(model4)

#prediction
pred4 = predict(model4, type = 'response', newdata = titanic_test)
y_pred4 = ifelse(pred4 > 0.5, 1, 0)
View(y_pred4)

summary(y_pred4)
table(y_pred4)


write.table(y_pred4, file = "data.csv", sep = "\t", row.names = F)
            
library(caTools)
set.seed(119)
split = sample.split(titanic_train,SplitRatio = 0.8)
training_set = subset(titanic_train,split == TRUE)
test_set = subset(titanic_train, split == FALSE)
View(training_set)
View(test_set)


#training model 
model4 <- glm(Survived ~ ., family = 'binomial', data = training_set)
summary(model4)

#prediction
pred4 = predict(model4, type = 'response', newdata = titanic_test)
y_pred4 = ifelse(pred4 > 0.5, 1, 0)
View(y_pred4)
table(y_pred4)

#confusion matrix
cm_1 = table(test_set[,1], y_pred4 > 0.5)
cm_1

#accuracy
acc_1 <- sum(diag(cm_1))/sum(cm_1)
acc_1

write.table(y_pred4, file = "pred2.csv", sep = "\t", row.names = F)


#precision
prec1 <- diag(cm_1)/colSums(cm_1,2)
prec1

#recall
rc1 <- diag(cm_1)/rowSums(cm_1,2)
rc1

#f1 score
f1_score1 <- 2*prec1*rc1/(prec1+rc1)
f1_score1







