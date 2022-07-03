#Loading the packages
library(tidyverse)
library(caret)
library(caTools)
library(rpart)
library(rpart.plot)
library(e1071)
library(randomForest)

str(Sales)
summary(Sales)
head(Sales)

sum(is.na(Sales))
sum(is.na(Sales$Item_Identifier))
sum(is.na(Sales$Outlet_Identifier))

#Converting item_type, outlet_size, outlet_location_type, outlet type and fat content to factor variables
Sales$Item_Type <- as.factor(Sales$Item_Type)
Sales$Outlet_Size <- as.factor(Sales$Outlet_Size)
Sales$Outlet_Location_Type <- as.factor(Sales$Outlet_Location_Type)
Sales$Outlet_Type <- as.factor(Sales$Outlet_Type)
Sales$Item_Fat_Content <- as.factor(Sales$Item_Fat_Content)

Sales$Item_Outlet_Sales <- round(Sales$Item_Outlet_Sales, 0)
Sales$Item_Fat_Content[Sales$Item_Fat_Content == 'reg'] <- "Regular"
Sales$Item_Fat_Content[Sales$Item_Fat_Content != "Regular"] <- "Low Fat"

#Replacing the NA values in Item_Weight with mean
Sales$Item_Weight[is.na(Sales$Item_Weight)] <- mean(Sales$Item_Weight, na.rm = TRUE)

#We will apply classification algorithms to impute the NA values in Outlet_Size column. In the training set we have observations without
#NA values while in the test set we will have observations with NA values
test <- Sales[is.na(Sales$Outlet_Size), ]
train <- Sales[complete.cases(Sales), ]

#Removing irrelevant variables from the train and test variables
train <- train[, -1]
train <- train[, -6]

test <- test[, -1]
test <- test[, -6]

#Random Forrest
rf <- randomForest(Outlet_Size ~ . , data = train, mtry = 3)
print(rf)

#Naive Bayes
nb <- naiveBayes(Outlet_Size ~ . , data = train)
summary(nb)
pred_nb <- predict(nb, train)
table(ActualValue = train$Outlet_Size, PredictedValue <- pred_nb)
(854+1911+2350)/6113
pred_size <- predict(nb, test)
head(pred_size)

#Replacing the NA values in outlet_size column with the values predicted by the naive bayes classifier
Sales$Outlet_Size[is.na(Sales$Outlet_Size)] <- pred_size
summary(Sales)
sum(is.na(Sales))

#Grouping the total sales by fat content
by_fat <- Sales %>%
  group_by(Item_Fat_Content) %>%
  summarise(Total_Sale <- sum(Item_Outlet_Sales))
colnames(by_fat) <- c('Item_Fat_Content', 'Total_Sales')
by_fat

#Grouping the total sales by outlet_size
by_size <- Sales %>%
  group_by(Outlet_Size) %>%
  summarise(Total_Sale <- sum(Item_Outlet_Sales))
colnames(by_size) <- c('Outlet_Size', 'Total_Sales')
by_size

#Grouping the total sales by outlet_location_type
by_location <- Sales %>%
  group_by(Outlet_Location_Type) %>%
  summarise(Total_Sale <- sum(Item_Outlet_Sales))
colnames(by_location) <- c('Outlet_Location_Type', 'Total_Sales')
by_location

#Grouping the total sales by outlet_type
by_type <- Sales %>%
  group_by(Outlet_Type) %>%
  summarise(Total_Sale <- sum(Item_Outlet_Sales))
colnames(by_type) <- c('Outlet_Type', 'Total_Sales')
by_type

#Plotting the bar graph for total sale by fat content
ggplot(data = by_fat, aes(x = Item_Fat_Content, y = Total_Sales)) +
  geom_bar(stat = 'identity', fill = 'red', colour = 'black') +
  geom_text(aes(label = Total_Sales), position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle('Total Sales by fat content') +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

#Plotting the bar graph for total sale by outlet_size
ggplot(data = by_size, aes(x = Outlet_Size, y = Total_Sales)) +
  geom_bar(stat = 'identity', fill = 'blue', colour = 'black') +
  geom_text(aes(label = Total_Sales), position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle('Total Sales by outlet size') +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

#Plotting the bar graph for total sale by outlet_location_type
ggplot(data = by_location, aes(x = Outlet_Location_Type, y = Total_Sales)) +
  geom_bar(stat = 'identity', fill = 'orange', colour = 'black') +
  geom_text(aes(label = Total_Sales), position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle('Total Sales by outlet type') +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

#Plotting the bar graph for total sale by outlet_type
ggplot(data = by_type, aes(x = Outlet_Type, y = Total_Sales)) +
  geom_bar(stat = 'identity', fill = 'grey', colour = 'black') +
  geom_text(aes(label = Total_Sales), position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle('Total Sales by fat outlet type') +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

#Splitting the dataset into training and testing data
spl <- sample.split(Sales$Item_Identifier, SplitRatio = 0.75)
train2 <- subset(Sales, spl == TRUE)
test2 <- subset(Sales, spl == FALSE)

#Removing item_identifier, outlet identifier and outlet establishment year from the training and testing dataset
train2 <- train2[, -1]
train2 <- train2[, -6]
train2 <- train2[, -6]

test2 <- test2[, -1]
test2 <- test2[, -6]
test2 <- test2[, -6]

#Run regression tree model on train set
dt <- rpart(Item_Outlet_Sales ~ . , data = train2, control = rpart.control(maxdepth = 4))

#Plot the decision Tree
rpart.plot(dt, box.palette="RdBu", digits = -3)

pred_dt <- predict(dt, test2, type = 'vector')
mse_dt <- mean((test2$Item_Outlet_Sales - pred_dt)^2)
mse_dt
rmse_dt <- sqrt(mse_dt)
rmse_dt

#test3 is the dataset for which the total sales is to be predicted
str(test3)

test3$Item_Fat_Content[test3$Item_Fat_Content == 'reg'] <- "Regular"
test3$Item_Fat_Content[test3$Item_Fat_Content != "Regular"] <- "Low Fat"

test3$Item_Type <- as.factor(test3$Item_Type)
test3$Outlet_Location_Type <- as.factor(test3$Outlet_Location_Type)
test3$Outlet_Type <- as.factor(test3$Outlet_Type)
test3$Outlet_Size <- as.factor(test3$Outlet_Size)
test3$Item_Fat_Content <- as.factor(test3$Item_Fat_Content)

summary(test3)
sum(is.na(test3))

#Replacing the NA values in Item_weight with the mean
test3$Item_Weight[is.na(test3$Item_Weight)] <- mean(test3$Item_Weight, na.rm = TRUE)

#We will use naive Bayes to predict the NA values in outlet_size column
#In the training set we will have values with non NA values and in testing set we will have values with NA values
training <- test3[complete.cases(test3), ]
testing <- test3[is.na(test3$Outlet_Size), ]

#Removing irrelevant variables from training and testing variable
training <- training[, -1]
training <- training[, -6:-7]

testing <- testing[, -1]
testing <- testing[, -6:-7]

#Applying Naive Bayes
nb2 <- naiveBayes(Outlet_Size ~ ., data = training)
summary(nb2)

pred_nb2 <- predict(nb2, training)
table(ActualValue = training$Outlet_Size, PredictedValue = pred_nb2)
(617+1242+1592)/4075

pred_size2 <- predict(nb2, testing)

#Replacing the NA values in outlet_size with those predicted by the Naive Bayes classifier
test3$Outlet_Size[is.na(test3$Outlet_Size)] <- pred_size2
sum(is.na(test3))

#Predicting the sales of products in test3 dataset
test3$Item_Outlet_Sales <- predict(dt, test3, type = 'vector')

submission_file <- test3[, c(1, 7, 12)]
write.csv(x = submission_file, file = "predictions.csv")