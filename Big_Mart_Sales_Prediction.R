train <- read.csv("Train_UWu5bXk.csv")
test <- read.csv("Test_u94Q5KV.csv")

dim(train)

dim(test)

str(train)

## Check for missing values

table(is.na(train))

## Check for columns that have missing values
colSums(is.na(train))

## Item_Weight has 1463 values

summary(train)

summary(train$Outlet_Size)

## Graphical representation of variables

install.packages("ggplot2")
library(ggplot2)

ggplot(train, aes(x= Item_Visibility, y = Item_Outlet_Sales)) + geom_point(size = 2.5, color="navy") + xlab("Item Visibility") + ylab("Item Outlet Sales") + ggtitle("Item Visibility vs Item Outlet Sales")

ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales)) + geom_bar(stat = "identity", color = "purple") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))  + ggtitle("Outlets vs Total Sales") + theme_bw()

ggplot(train, aes(Item_Type, Item_Outlet_Sales)) + geom_bar( stat = "identity") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) + xlab("Item Type") + ylab("Item Outlet Sales")+ggtitle("Item Type vs Sales")

ggplot(train, aes(Item_Type, Item_MRP)) +geom_boxplot() +ggtitle("Box Plot") + theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) + xlab("Item Type") + ylab("Item MRP") + ggtitle("Item Type vs Item MRP")

ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales)) + geom_bar(stat = "identity", color = "purple")

test$Item_Outlet_Sales <-  1

combi <- rbind(train, test)

## Imputing missing values
## Item_Weight column

combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)
table(is.na(combi$Item_Weight))

combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility), combi$Item_Visibility)

## Correcting the factor levels

levels(combi$Outlet_Size)[1] <- "Other"

install.packages("plyr")
library(plyr)
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("LF" = "Low Fat", "reg" = "Regular"))
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))

table(combi$Item_Fat_Content)

################ Data Manipulation ####################################################################################

### Feature Engineering ######

library(dplyr)

a <- combi%>% group_by(Outlet_Identifier)%>% tally()

head(a)

names(a)[2] = "Outlet_Count"

combi <- full_join(a, combi, by = "Outlet_Identifier")

View(combi)

b <- combi%>% group_by(Item_Identifier)%>% tally()

b

names(b)[2] = "Item_Count"

combi = merge(b, combi,by = "Item_Identifier")

c <- combi%>% select(Outlet_Establishment_Year)%>% mutate(Outlet_Year = 2013 - combi$Outlet_Establishment_Year)

head(c)

combi <- full_join(c, combi)

q <- substr(combi$Item_Identifier,1,2)

q <- gsub("FD","Food",q)

q <- gsub("DR","Drinks",q)

q <- gsub("NC","Non-Consumable",q)

table(q)

combi$Item_Type_New <- q

## Label Encoding and One Hot Encoding ##

## Label Encoding
combi$Item_Fat_Content <- ifelse(combi$Item_Fat_Content == "Regular",1,0)

## Hot Encoding
sample <- select(combi, Outlet_Location_Type)

demo_sample <- data.frame(model.matrix(~.-1,sample))

head(demo_sample)

install.packages("dummies")
library(dummies)

combi <- dummy.data.frame(combi, names = c('Outlet_Size','Outlet_Location_Type','Outlet_Type', 'Item_Type_New'),  sep='_')

str(combi)

combi <- select(combi, -c(Item_Identifier, Outlet_Identifier, Item_Fat_Content, Outlet_Establishment_Year,Item_Type))

new_train <- combi[1:nrow(train),]
new_test <- combi[-(1:nrow(train)),]

linear_model <- lm(Item_Outlet_Sales ~ ., data = new_train)
summary(linear_model)

## R^2 value is 23.04 
cor(new_train)

cor(new_train$Outlet_Count, new_train$'Outlet_Type_Grocery Store')

train <- read.csv("Train_UWu5bXk.csv")
test <- read.csv("Test_u94Q5KV.csv")

test$Item_Outlet_Sales <- 1

combi <- rbind(train, test)

#impute missing value in Item_Weight

combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)

#impute 0 in item_visibility

combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility), combi$Item_Visibility)

#rename level in Outlet_Size

levels(combi$Outlet_Size)[1] <- "Other"

#rename levels of Item_Fat_Content

combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,c("LF" = "Low Fat", "reg" = "Regular"))

combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))

#create a new column 2013 - Year

combi$Year <- 2013 - combi$Outlet_Establishment_Year

#drop variables not required in modeling

library(dplyr)

combi <- select(combi, -c(Item_Identifier, Outlet_Identifier, Outlet_Establishment_Year))

#divide data set

new_train <- combi[1:nrow(train),]

new_test <- combi[-(1:nrow(train)),]

#linear regression

linear_model2 <- lm(Item_Outlet_Sales ~ ., data = new_train)

summary(linear_model2)  ## R-square value of56.37

## Diagnosing plots #########################
opar = par()
par(mfrow = c(2,2))
plot(linear_model2)

## Residuals vs Fitted values indicate a funnel shape curve which indicates heteroskedasticity which means there is unequal variance in the error terms

linear_model2 = lm(log(Item_Outlet_Sales) ~ ., data = new_train)

summary(linear_model2)

## R-square increased to 72.14 from 56.37

opar = par()
par(mfrow = c(2,2))
plot(linear_model2)
par(opar)

## To calculate RMSE use "Metrics" library

install.packages("Metrics")
library(Metrics)

rmse(new_train$Item_Outlet_Sales,exp(linear_model2$fitted.values))

#loading required libraries

library(rpart)
library(e1071)
library(rpart.plot)
library(caret)

## setting the tree control parameters

fitControl <- trainControl(method = "cv", number = 5)

## .cp is the complex parameter which is the trade off between model complexity and the accuracy on training set
## smaller value leads to bigger tree and leads to underfitting and higher value leads to smaller tree and underfitting

cartGrid <- expand.grid(.cp=(1:50)*0.01)

#decision tree

tree_model <- train(Item_Outlet_Sales ~ ., data = new_train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)

print(tree_model)

## optimal value .cp = 0.01 which has the least value of RMSE and highest R-square value

main_tree <- rpart(Item_Outlet_Sales ~ ., data = new_train, control = rpart.control(cp=0.01))

prp(main_tree)

## To calculate RMSE value
pre_score <- predict(main_tree, type = "vector")

rmse(new_train$Item_Outlet_Sales, pre_score)

## 1102.74 is much better than the value provided by Linear Regression

## Now building a Random Forest model

library(randomForest)

#set tuning parameters

control <- trainControl(method = "cv", number = 5)

#random forest model

## rf_model <- train(Item_Outlet_Sales ~ ., data = new_train, method = "parRF", trControl =  control, prox = TRUE, allowParallel = TRUE)

rf_model <- train(Item_Outlet_Sales ~ ., data = new_train, method = "rf", trControl =  control, prox = TRUE)
