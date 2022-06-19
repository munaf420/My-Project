#Loading the packages
library(ggplot2)
library(dplyr)
library(caTools)
library(class)

str(loan_data)
head(loan_data)
summary(loan_data)

#Converting the data type from character to factor
loan_data$Gender <- as.factor(loan_data$Gender)
loan_data$Married <- as.factor(loan_data$Married)
loan_data$Loan_Status <- as.factor(loan_data$Loan_Status)
loan_data$Education <- as.factor(loan_data$Education)
loan_data$Property_Area <- as.factor(loan_data$Property_Area)
loan_data$Self_Employed <- as.factor(loan_data$Self_Employed)
loan_data$Dependents <- as.factor(loan_data$Dependents)
loan_data$Credit_History <- as.factor(loan_data$Credit_History)

#Searching for NA values
which(is.na(loan_data))
sum(is.na(loan_data))

sum(is.na(loan_data$Loan_ID))
sum(is.na(loan_data$Dependents))

#Replacing NA values of loan_amount and loan_amount_term with their respective mean
loan_data$LoanAmount[is.na(loan_data$LoanAmount)] <- mean(loan_data$LoanAmount, na.rm = TRUE)
loan_data$Loan_Amount_Term[is.na(loan_data$Loan_Amount_Term)] <- mean(loan_data$Loan_Amount_Term, na.rm = TRUE)

#Replacing NA values of gender, married , dependents, Credit_History and Self_Employed with their mode
loan_data$Gender[is.na(loan_data$Gender)] <- "Male"
loan_data$Married[is.na(loan_data$Married)] <- "Yes"
loan_data$Dependents[is.na(loan_data$Dependents)] <- 1
loan_data$Credit_History[is.na(loan_data$Credit_History)] <- 1
loan_data$Self_Employed[is.na(loan_data$Self_Employed)] <- "No"

#Converting the Y and N in loan_status column to 1 and 0 respectively
loan_data$Loan_Status <- ifelse(loan_data$Loan_Status == "Y", 1, 0)
loan_data$Loan_Status <- as.numeric(loan_data$Loan_Status)

#Grouping the percentage of people whose loans were approved by gender
by_gender <- loan_data %>%
  group_by(Gender) %>%
  summarize(percent <- (mean(Loan_Status))*100)
by_gender
colnames(by_gender) <- c('Gender', 'Percentage')

#Grouping the percentage of people whose loans were approved by marital status
by_ms <- loan_data %>%
  group_by(Married) %>%
  summarize(percent <- (mean(Loan_Status))*100)
colnames(by_ms) <- c('Married', 'Percentage')
by_ms

#Grouping the percentage of people whose loans were approved by no of dependents
by_dep <- loan_data %>%
  group_by(Dependents) %>%
  summarize(percent <- (mean(Loan_Status))*100)
colnames(by_dep) <- c('Dependents', 'Percentage')
by_dep

#Grouping the percentage of people whose loans were approved by education
by_education <- loan_data %>%
  group_by(Education) %>%
  summarize(percent <- (mean(Loan_Status))*100)
colnames(by_education) <- c('Education', 'Percentage')
by_education

#Grouping the percentage of people whose loans were approved by self employment status
by_se <- loan_data %>%
  group_by(Self_Employed) %>%
  summarize(percent <- (mean(Loan_Status))*100)
colnames(by_se) <- c('Self_Employed', 'Percentage')
by_se

#Grouping the percentage of people whose loans were approved by credit history
by_ch <- loan_data %>%
  group_by(Credit_History) %>%
  summarize(percent <- (mean(Loan_Status))*100)
colnames(by_ch) <- c('Credit_History', 'Percentage')
by_ch

#Plotting the bar graph for loan approval status by gender
ggplot(data = by_gender, aes(x = Gender, y = Percentage)) +
  geom_bar(stat = 'identity', fill = 'red', colour = 'black') +
  ylim(0, 100) +
  geom_text(aes(label = round(Percentage, 0)), position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle('Loan approval percentage by gender') +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

#Plotting the bar graph for loan approval status by marital status
ggplot(data = by_ms, aes(x = Married, y = Percentage)) +
  geom_bar(stat = 'identity', fill = 'blue', colour = 'black') +
  ylim(0, 100) +
  geom_text(aes(label = round(Percentage, 0)), position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle('Loan approval percentage by marital status') +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

#Plotting the bar graph for loan approval status by no of dependents
ggplot(data = by_dep, aes(x = Dependents, y = Percentage)) +
  geom_bar(stat = 'identity', fill = 'green', colour = 'black') +
  ylim(0, 100) +
  geom_text(aes(label = round(Percentage, 0)), position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle('Loan approval percentage by no of dependents') +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

#Plotting the bar graph for loan approval status by education
ggplot(data = by_education, aes(x = Education, y = Percentage)) +
  geom_bar(stat = 'identity', fill = 'purple', colour = 'black') +
  ylim(0, 100) +
  geom_text(aes(label = round(Percentage, 0)), position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle('Loan approval percentage by education') +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

#Plotting the bar graph for loan approval status by self employment status
ggplot(data = by_se, aes(x = Self_Employed, y = Percentage)) +
  geom_bar(stat = 'identity', fill = 'orange', colour = 'black') +
  ylim(0, 100) +
  geom_text(aes(label = round(Percentage, 0)), position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle('Loan approval percentage by self employment status') +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

#Plotting the bar graph for loan approval status by credit history
ggplot(data = by_ch, aes(x = Credit_History, y = Percentage)) +
  geom_bar(stat = 'identity', fill = 'grey', colour = 'black') +
  ylim(0, 100) +
  geom_text(aes(label = round(Percentage, 0)), position = position_dodge(width = 0.9), vjust = -0.5) +
  ggtitle('Loan approval percentage by credit history') +
  theme(plot.title = element_text(hjust = 0.5, size = 15))

#Dividing the dataset into training and testing data 
spl <- sample.split(loan_data$Loan_Status, SplitRatio = 0.75)
train <- subset(loan_data, spl == TRUE)
test <- subset(loan_data, spl == FALSE)

#Taking Income = Applicant Income + CoApplicantIncome
train$Income <- train$ApplicantIncome + train$CoapplicantIncome

#Loading the required packages
library(fastAdaboost)
library(ROCR)

#Running adaboost model
adaboost <- adaboost(Loan_Status ~., data = train, nIter = 1000)
pred_adaboost <- predict(adaboost, test)
summary(adaboost)
table(ActualValue = test$Loan_Status, PredictedValue = pred_adaboost$class)
(31+90)/154

#Finding the threshold for adaboost
ROCRPred_adaboost <- prediction(pred_adaboost$prob[, 2], test$Loan_Status)
ROCRPerf_adaboost <- performance(ROCRPred_adaboost, 'tpr', 'fpr')

#Plotting the ROC curve
plot(ROCRPerf_adaboost, colorize = TRUE, print.cutoffs.at = seq(0.1, by = 0.1))

#Taking the cutoff at 0.42 and making the confusion matrix
table(ActualValue = test$Loan_Status, PredictedValue = pred_adaboost$prob[, 2] > 0.42)
132/154

#Viewing the adaboost trees
t1 <- adaboost$trees[[1]]
plot(t1)
text(t1, pretty = 100)

#Taking Income as ApplicantIncome + CoApplicantIncome
loan_test$Income <- loan_test$ApplicantIncome + loan_test$CoapplicantIncome

#Removing loanID as it is an irrelevant variable
loan_test <- loan_test[, -1]

#Removing ApplicantIncome and CoApplicantIncome
loan_test <- loan_test[, -7:-8]

sum(is.na(loan_test))
summary(loan_test)
str(loan_test)

#Converting the data type from character to factor
loan_test$Gender <- as.factor(loan_test$Gender)
loan_test$Married <- as.factor(loan_test$Married)
loan_test$Education <- as.factor(loan_test$Education)
loan_test$Dependents <- as.factor(loan_test$Dependents)
loan_test$Self_Employed <- as.factor(loan_test$Self_Employed)
loan_test$Credit_History <- as.factor(loan_test$Credit_History)
loan_test$Property_Area <- as.factor(loan_test$Property_Area)

#Replacing NA values of loan_amount and loan_amount_term with their respective mean
loan_test$LoanAmount[is.na(loan_test$LoanAmount)] <- mean(loan_test$LoanAmount, na.rm = TRUE)
loan_test$Loan_Amount_Term[is.na(loan_test$Loan_Amount_Term)] <- mean(loan_test$Loan_Amount_Term, na.rm = TRUE)

#Replacing NA values of gender, dependents, Credit_History and Self_Employed with their mode
loan_test$Gender[is.na(loan_test$Gender)] <- "Male"
loan_test$Dependents[is.na(loan_test$Dependents)] <- 0
loan_test$Self_Employed[is.na(loan_test$Self_Employed)] <- "No"
loan_test$Credit_History[is.na(loan_test$Credit_History)] <- 1

#Making predictions on the dataset provided
prediction_adaboost <- predict(adaboost, loan_test)
head(pred_adaboost)

#Taking the cutoff probability 0.42
prediction_adaboost <- ifelse(prediction_adaboost$prob[, 2] > 0.42, "Y", "N")
head(prediction_adaboost)
loan_test$Loan_Status <- prediction_adaboost

#END