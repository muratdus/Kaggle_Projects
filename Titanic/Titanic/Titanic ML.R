library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)
library(tm)
library(Hmisc)
library(stringr)
data_1 <- read.csv("./train.csv", stringsAsFactors = FALSE, na.strings = "")
data_2 <- read.csv("./test.csv", stringsAsFactors = FALSE, na.strings = "")


train <- data_1
test <- data_2


##################Clean Up######################

head(train)


sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))
#we have bunch of NAs in Cabin,m and in Age.
#let's remove NAs from the Age for now. 
#However there are many NAs in cabin. So I will add an unknown variable to them.
train$Cabin <- replace_na(train$Cabin, 'X0')
test$Cabin <- replace_na(test$Cabin, 'X0')
#let's dummy variable the sex 
train <- train %>% mutate(sexFac = as.factor(Sex))
train$sexFac <- ifelse(train$Sex == 'male', 0, 1)
levels(train$sexFac) <- c('male','female')
train$sexFac <- as.numeric(train$sexFac)



test <- test %>% mutate(sexFac = as.factor(Sex))
test$sexFac <- ifelse(test$Sex == 'male', 0, 1)
levels(train$sexFac) <- c('male','female')
test$sexFac <- as.numeric(test$sexFac)

#To impute NA values for age, we 'll deduce those numbers based on the titles. 
#so let's extract titles from the names

Surnames <- str_split(train$Name, ", ", simplify = TRUE) 
Surnames <- as.data.frame(Surnames)
Names <- as.data.frame(str_split(Surnames$V2, ". " , n = 2 , simplify = TRUE))
Titles <- as.data.frame(Names$V1)
colnames(Titles) = "Titles"
Names <- as.data.frame(Names[,-1])
colnames(Names) = "FirstNames"
Surnames <- as.data.frame(Surnames[,-2])
colnames(Surnames) = "Surnames"

train <- mutate(train, Titles, Names, Surnames, .after = Pclass)
train <- train %>% select(-Name)
unique(train$Titles)

Surnames <- str_split(test$Name, ", ", simplify = TRUE) 
Surnames <- as.data.frame(Surnames)
Names <- as.data.frame(str_split(Surnames$V2, ". " , n = 2 , simplify = TRUE))
Titles <- as.data.frame(Names$V1)
colnames(Titles) = "Titles"
Names <- as.data.frame(Names[,-1])
colnames(Names) = "FirstNames"
Surnames <- as.data.frame(Surnames[,-2])
colnames(Surnames) = "Surnames"

test <- mutate(test, Titles, Names, Surnames, .after = Pclass)
test <- test %>% select(-Name)

#add group by title and get median age. Add median to the NA columns of Age
age_by_title <-  train %>% group_by(Titles) %>% summarise(median = median(Age, na.rm = TRUE)) 

train <- merge(train, age_by_title)
train[is.na(train$Age), 'Age'] <- train[is.na(train$Age), 'median']
train <- select(train, -median)

#important to remove training sets median from test not test sets to prevent problems.
test[test$Titles == 'Dona', 'Titles'] = 'Mrs'
test <- merge(test, age_by_title)
test[is.na(test$Age), 'Age'] <- test[is.na(test$Age), 'median']
test<- select(test, -median)


#Imputing NA in fares in similar manner.

fare_per_class <- train %>% group_by(Pclass) %>% summarise(median = median(Fare, na.rm =TRUE))

train <-  merge(train, fare_per_class)
train[is.na(train$Fare), 'Fare'] <- train[is.na(train$Fare), 'median']
train <- train %>% select(-median)

test <-  merge(test, fare_per_class)
test[is.na(test$Fare), 'Fare'] <- test[is.na(test$Fare), 'median']
test <- test %>% select(-median)


#Filling the missing embarked with mode
table(train$Embarked)
train$Embarked <- replace_na(train$Embarked, 'S')

#No NA values left
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))


#Too many categories in the cabin not informative. Remove the numbers leave just the letters.
train$Cabin <- removeNumbers(train$Cabin)
unique(train$Cabin)
test$Cabin <- removeNumbers(test$Cabin)

#remove duplicate letters. Leave the first one

cabins <- train$Cabin
cabins <- str_split(cabins, " ", n = 2, simplify = TRUE)
cabins <- as.data.frame(cabins)
cabins <- cabins[,-2]
train$Cabin <- cabins


cabins <- test$Cabin
cabins <- str_split(cabins, " ", n = 2, simplify = TRUE)
cabins <- as.data.frame(cabins)
cabins <- cabins[,-2]
test$Cabin <- cabins



table(train$Cabin, train$Pclass)
table(test$Cabin, test$Pclass)

#Most of the third class passangers are at X. I'm gonna distribute first class A passangers to ABCDE, 2nd class to DEF, and leave third class at X
cabin_by_pclass <-  train %>% group_by(Pclass)
temp1st <- cabin_by_pclass %>% filter(Pclass == 1)
set.seed(4323)
temp1st[temp1st$Cabin == 'X','Cabin'] <- sample(c("A","B","C", "D", "E"), size = nrow(temp1st[temp1st$Cabin == 'X','Cabin']), replace = TRUE)
temp2st <- cabin_by_pclass %>% filter(Pclass == 2)
set.seed(4323)
temp2st[temp2st$Cabin == 'X','Cabin'] <- sample(c("D","E","F"), size = nrow(temp2st[temp2st$Cabin == 'X','Cabin']), replace = TRUE)
temp_1st_2nd <- rbind(temp1st, temp2st)
cabin_by_pclass <- cabin_by_pclass %>% rows_update(temp_1st_2nd, by = "PassengerId")
train <- cabin_by_pclass

cabin_by_pclass <-  test %>% group_by(Pclass)
temp1st <- cabin_by_pclass %>% filter(Pclass == 1)
set.seed(4323)
temp1st[temp1st$Cabin == 'X','Cabin'] <- sample(c("A","B","C", "D", "E"), size = nrow(temp1st[temp1st$Cabin == 'X','Cabin']), replace = TRUE)
temp2st <- cabin_by_pclass %>% filter(Pclass == 2)
set.seed(4323)
temp2st[temp2st$Cabin == 'X','Cabin'] <- sample(c("D","E","F"), size = nrow(temp2st[temp2st$Cabin == 'X','Cabin']), replace = TRUE)
temp_1st_2nd <- rbind(temp1st, temp2st)
cabin_by_pclass <- cabin_by_pclass %>% rows_update(temp_1st_2nd, by = "PassengerId")
test <- cabin_by_pclass



# need to confirm if I should sample from the train sample itself. Since test is smaller. We need similar distribution of Classes for test.
#sampling from sampling should give us that. Need to check. 

##################00000##########################
#Exploratory Data Analysis
table(train$Survived)
table(train$Sex)


hist(train$SibSp)
#Sibling / spouse number with them is very skewed as expected. Due to people having one spouse.
hist(train$Parch)
table(train$Parch)
#again very skewed. Most people don't have their parents or children with them.

hist(train$Pclass)
#most people are 3rd class passengers, then first then second.

hist(train$Fare)
#we see a lot of cheaper fare frequency, again a skewed distribution, with some extremes. 

#we can look at fare, age and class 

ggplot(data = train, aes(x = Age, y = Fare, color = factor(Pclass))) + geom_point(stat = "identity")

#First class prices vary a lot more than first and second classes. With couple extreme points.  looks like THere are more older people who were in 1st class. 

#table(train$Cabin)
#location could be important. We can decide if we just want to keep the first letters and disregard the numbers, then factorize.  
#train$Cabin <- removeNumbers(train$Cabin)

table(train$Embarked, train$Pclass)
#locations of where passangers embarked from based on class.




#Let's look at correlation table
x <- c(4,8,10,11)
corrplot::corrplot(cor(train[, -x]))


# Survival and Class is correlated, and age and fare and Sex.
# Parch

ggplot(data = train, aes(factor(Pclass), Age , color = Sex))+ geom_point(stat = "identity")+geom_boxplot()
#Age is older with class, and women are younger than male in all groups
table(train$Pclass, train$Sex)

ggplot(data = train, aes(y = Age, x = factor(Survived) , color = Sex))+ geom_point(stat = "identity")+geom_boxplot()
#There are more female survivors, median age of survivors is similar between sexes. But median age of dead is lower for women. 
table(train$Survived, train$Sex)


#let's look at cabin and pClass

table(train$Cabin, train$Survived)

#Let's look at Surnames and Tickets
train %>% group_by(Ticket, Surnames) %>% summarise(n =n())

#survival and family size

t <- as.data.frame.matrix(table(train$Survived, train$FamilySize))
t <- as.data.frame(t(t))
t <- rownames_to_column(t)
names(t) <- c("familysize","dead", "survived") 
t <- mutate(t, ratio = survived/dead)

ggplot(data = t, aes(x = familysize, y = ratio, fill = factor(familysize)))+geom_bar(stat ='identity')
##############Feature engineering##################

#Try family size  Sb/Spouse + Parent/Child Variable

train <- mutate(train, FamilySize= SibSp + Parch, .after = Parch)
train[train$FamilySize <=2, 'FamilySize' ] <-  0 #small
train[train$FamilySize >=3 & train$FamilySize <5 , 'FamilySize' ]  <- 1 #medium
train[train$FamilySize >=5, 'FamilySize' ]  <- 2 #large

train$FamilySize <- as.factor(train$FamilySize)
levels(train$FamilySize) <- c("small","medium","large")


test <- mutate(test, FamilySize= SibSp + Parch, .after = Parch)
test[test$FamilySize <=2, 'FamilySize' ] <-  0 #small
test[test$FamilySize >=3 & test$FamilySize <5 , 'FamilySize' ]  <- 1 #medium
test[test$FamilySize >=5, 'FamilySize' ]  <- 2 #large

test$FamilySize <- as.factor(test$FamilySize)
levels(test$FamilySize) <- c("small","medium","large")

#Is the passanger alone?
train <- mutate(train, IsAlone= SibSp + Parch)
train$IsAlone <- if_else(train$IsAlone == 0, "alone", "not alone")

test <- mutate(test, IsAlone= SibSp + Parch)
test$IsAlone <- if_else(test$IsAlone == 0, "alone", "not alone")

unique(train$Ticket)

#Tickets with letters

train$LetterTicket <- train$Ticket
train$LetterTicket <- ifelse(str_detect(train$LetterTicket, " "), "yes", "no")

test$LetterTicket <- test$Ticket
test$LetterTicket <- ifelse(str_detect(test$LetterTicket, " "), "yes", "no")

#only the letter containing tickets have a space.

#############################################################
#let's try a model.
#remove non numerical

num_train <- train %>% select(-c(FirstNames,Surnames, Ticket, sexFac))
num_test <- test %>% select(-c(FirstNames,Surnames, Ticket, sexFac))

num_train$Titles <- as.numeric(as.factor(num_train$Titles))
num_train$Embarked <- as.numeric(as.factor(num_train$Embarked))
num_train$Sex <- as.numeric(as.factor(num_train$Sex))


num_train$FamilySize <- as.numeric(as.factor(num_train$FamilySize))
num_train$IsAlone <- as.numeric(as.factor(num_train$IsAlone))
num_train$LetterTicket <- as.numeric(as.factor(num_train$LetterTicket))
num_train$Cabin <- as.numeric(as.factor(num_train$Cabin))


num_test$Titles <- as.numeric(as.factor(num_test$Titles))
num_test$Embarked <- as.numeric(as.factor(num_test$Embarked))
num_test$Sex <- as.numeric(as.factor(num_test$Sex))



#Start with logistic regression.


model_lm <- train(data = num_train, factor(Survived) ~., method = 'glm', family = "binomial", preProcess = c("center", "scale"))
pred_log <- predict(model_lm, num_train)
logistic_train_result <- confusionMatrix(pred_log, as.factor(train$Survived))

summary(model_lm)
#Sex, Sib/Parent, And Pclass seem significant predictors. 

#Logistic with AIC
model_lm_AIC <- train(data = num_train, factor(Survived) ~., method = 'glmStepAIC', family = "binomial", preProcess = c("center", "scale"))

pred_log_AIC <- predict(model_lm_AIC, num_train)
logisticAIC_train_result <- confusionMatrix(pred_log_AIC, as.factor(train$Survived))
#no change. 

#Decision Tree

model_DT <- train(data = num_train, factor(Survived) ~., method = 'rpart', preProcess = c("center", "scale"))
pred_DT <- predict(model_DT, num_train)
DT_train_result <- confusionMatrix(pred_DT, as.factor(train$Survived))

#Accuracy wise same, specificity wise worse model, sensitivity wise- a little bit better..


#random forest

model_RF <- train(data = num_train, factor(Survived) ~., method = 'rf', preProcess = c("center", "scale"))
pred_RF <- predict(model_RF, num_train)

pred_RF_Test <- predict(model_RF, num_test)
RF_train_result <- confusionMatrix(pred_RF, as.factor(num_train$Survived))


#check for var importance. 
#check for correlation 
#check with PCA
#reduce dimension
#pca_model <- prcomp(num_train[,-4],retx = TRUE, scale = TRUE, center = TRUE)

#much more accurate, specific and sensitive. 


pred_RF_Test <- as.data.frame(pred_RF_Test)
pred_RF_Test <- mutate(pred_RF_Test, PassengerId = test$PassengerId, .before = pred_RF_Test)
colnames(pred_RF_Test)[2] <-"Survived" 
write.csv(pred_RF_Test,"./submission3.csv", row.names = FALSE)
