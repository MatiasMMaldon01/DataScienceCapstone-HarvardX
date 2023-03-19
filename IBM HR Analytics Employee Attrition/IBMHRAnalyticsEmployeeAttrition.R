# title: "IBM HR Analytics Employee Attrition - Capstone"
# author: "Martinez Maldonado Matías"

if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(tidyr)) install.packages("tidyr")
if(!require(caret)) install.packages("caret")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(purrr)) install.packages("purrr")
if(!require(cleandata)) install.packages("cleandata")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(pROC)) install.packages("pROC")
if(!require(randomForest)) install.packages("randomForest")
if(!require(gbm)) install.packages("gbm")

# Libraries we need
library(tidyverse)
library(caret)
library(purrr)
library(corrplot)
library(cleandata)
library(gridExtra)
library(pROC)
library(randomForest)
library(gbm)

#################################################################################################
                                         # Data Loading
#################################################################################################

# Data Loading with read.csv 
filename <- "WA_Fn-UseC_-HR-Employee-Attrition.csv"
employees <- read.csv(filename)

#################################################################################################
                                         # Data Exploration 
#################################################################################################
head(employees)

# Change "Yes" and "No" into numbers (1 and 0). To plot better.
employees$Over18 <- ifelse(employees$Over18 == "Y", 1, 0)
employees$OverTime <- ifelse(employees$OverTime == "Yes", 1, 0)

# Searching for missing values
sapply(employees, function(x) sum(is.na(x)))

# Counting the number of unique values of each feature
sapply(employees, function(x) n_distinct(x))

# Printing all numerical features
employees %>% mutate(Attrition = ifelse(Attrition== "Yes", 1, 0)) %>%
  keep(is.numeric) %>%                           # Keep only numeric columns
  gather() %>%                                   # Convert to key-value pairs
  ggplot(aes(value)) +                           # Convert to key-value pairs
  facet_wrap(~ key, ncol = 5, scales = "free") + # In separate panels
  geom_histogram(fill = "orange")                # as histogram

# Printing a separate plots for "Employee Count", "Over18" and "Standard Hours"  
plot(employees$EmployeeCount)
plot(employees$Over18)
plot(employees$StandardHours)

# Removing unnecessary features
employees <- employees %>% select(-c(EmployeeCount, EmployeeNumber, StandardHours, Over18))

# We created two different datasets, one with the employees who stay in the company 
# and the other with the employees who left the company.

left_employees <- employees %>% filter(Attrition == "Yes") %>% keep(is.numeric)
stay_employees <- employees %>% filter(Attrition == "No") %>% keep(is.numeric)

# Analizing some important statistics when the employee stay in the company,
data.frame(mean = sapply(stay_employees, function(x) mean(x, na.rm = TRUE)),
           sd = sapply(stay_employees, function(x) sd(x, na.rm = TRUE)),
           min = sapply(stay_employees, function(x) min(x, na.rm = TRUE)),
           max = sapply(stay_employees, function(x) max(x, na.rm = TRUE)))

# or left the company.
data.frame(mean = sapply(left_employees, function(x) mean(x, na.rm = TRUE)),
           sd = sapply(left_employees, function(x) sd(x, na.rm = TRUE)),
           min = sapply(left_employees, function(x) min(x, na.rm = TRUE)),
           max = sapply(left_employees, function(x) max(x, na.rm = TRUE)))

# We want to plot a correlation matrix
# First we create a matrix with all numeric variables correlations.
correlations <- employees %>% mutate(Attrition = ifelse(Attrition== "Yes", 1, 0)) %>% keep(is.numeric) %>% cor() %>% round(2)

# Then we use corrplot to print this matrix
corrplot(correlations, type = "upper", method = "color", tl.col = "black", 
         tl.cex = 0.75, cl.cex = 0.75)

# How much does the age of the workers affect their permanence in the company?
employees %>% ggplot(aes(Age, fill = Attrition)) +
  geom_bar() + labs(fill = "Attrition")

# We make a grid plot about some features and how they affect the  employee permanence in the company. 

# Job Level
plot1 <- employees %>% ggplot(aes(JobLevel, fill = Attrition)) + 
  geom_bar() + labs(fill = "Attrition")

# Marital Status
plot2 <- employees %>% ggplot(aes(MaritalStatus, fill = Attrition)) + 
  geom_bar()+ labs(fill = "Attrition")

# Job Role
plot3 <- employees %>% ggplot(aes(JobRole, fill = Attrition)) + geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1, size = 7)) +
  labs(fill = "Attrition")

# JobInvolvment
plot4 <- employees %>% ggplot(aes(JobInvolvement, fill = Attrition)) + 
  geom_bar() + labs(fill = "Attrition")

# Work-Life Balance
plot5 <- employees %>% ggplot(aes(WorkLifeBalance, fill = Attrition)) + 
  geom_bar() + labs(fill = "Attrition")

# Job Satisfaction
plot6 <- employees %>% ggplot(aes(JobSatisfaction, fill = Attrition)) + 
  geom_bar() + labs(fill = "Attrition")

# Make a plot list
plots <- list(plot1, plot2, plot3, plot4, plot5, plot6)

# Print a matrix of plots 
grid.arrange(grobs = plots)

# Printing Density plots DistanceFromHome, YearsWithCurrManager and TotalWorkingYears vs Attrition
plot7 <- employees %>% ggplot(aes(DistanceFromHome, fill = Attrition)) +
  geom_density(alpha = 0.3) + labs(fill = "Attrition")

plot8 <- employees %>% ggplot(aes(YearsWithCurrManager, fill = Attrition)) +
  geom_density(alpha = 0.3) + labs(fill = "Attrition")

plot9 <- employees %>% ggplot(aes(TotalWorkingYears, fill = Attrition)) +
  geom_density(alpha = 0.3) + labs(fill = "Attrition")

plots2 <- list(plot7, plot8, plot9)

grid.arrange(grobs = plots2)

# Making boxplots
# Monthly Income vs Gender
employees %>% ggplot(aes(MonthlyIncome, fill = Gender)) +
  geom_boxplot()

# Monthly Income vs JobRole
employees %>% ggplot(aes(MonthlyIncome, fill = JobRole)) +
  geom_boxplot()

# MonthlyIncome vs Overtime grouped by Attrition
employees %>% ggplot(aes(MonthlyIncome, fill = as.factor(OverTime))) +
  geom_density(alpha = 0.4)+
  facet_grid(~Attrition) + labs(fill = "Overtime Hours") +
  ggtitle("Attrition") + 
  theme(plot.title = element_text(hjust = 0.5))

#################################################################################################
                                        # Pre-Processing Data
#################################################################################################

# Binary encoding
# if employee is male then 1 otherwise 0.
employees$Gender <- ifelse(employees$Gender == "Male", 1, 0)

unique(employees$BusinessTravel)

# Creating an ordinal encoding function
ordinal_encode <- function(x){
  if(x == "Travel_Frequently"){
    return(2)
  }
  if(x == "Travel_Rarely"){
    return(1)
  }
  if(x == "Non-Travel"){
    return(0)
  }
}
# Changing values of "BusinessTravel" feature into numerical (0, 1, 2)
employees$BusinessTravel <- sapply(employees$BusinessTravel, ordinal_encode)

# One_Hot_Encoding "Department", "MaritalStatus", "JobRole" and "EducationField"
dummy <- employees %>% select(-Attrition) %>% dummyVars(" ~ .", data= .)
employees_dummie <- data.frame(predict(dummy, newdata = employees)) 

scale_values <- function(x){
  
  (x-min(x))/(max(x)-min(x))
  
}

# Normalizing Data
final_employees_df <- sapply(employees_dummie, scale_values) %>% as.data.frame()
final_employees_df$Attrition <- as.factor(employees$Attrition)

#################################################################################################
                                         # Creating the model
#################################################################################################

#Creating data partitioning into train and test sets.
set.seed(140323)
test_index <- createDataPartition(final_employees_df$Attrition, times = 1, p = 0.3, list = F)

train_set <- final_employees_df[-test_index,]
test_set <- final_employees_df[test_index,] 

# We need to know how many employees stayed at the company in our data set. (Probably our dataset is imbalanced)
left_employee <- employees %>% filter(Attrition == "Yes") %>% nrow()
left_employee
stayed_employee <- employees %>% filter(Attrition == "No") %>% nrow()
stayed_employee

# We predict all employees stay in the company
pred1 <- array("No", nrow(test_set)) %>% as.factor()
results <- data.frame(Method = "Naive Approach", Accuracy = confusionMatrix(pred1, as.factor(test_set$Attrition))$overall[["Accuracy"]], 
                      Sensitivity = 1, Specificity = 0, AUC = "0.5") 

# Creating a temporal variable that transform Attrition feature into 0 and 1.
tmp <- train_set %>% mutate(Attrition = as.factor(ifelse(Attrition == "Yes", 1, 0)))

# Logistic Regression
set.seed(4325)
glm_fit <- glm(Attrition ~ ., data = tmp, family = "binomial")
p_hat_logit <- predict(glm_fit, newdata = test_set)
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Yes", "No") %>% factor()
logit_cm <- table(y_hat_logit, test_set$Attrition)
new_row_logit <- list(Method = "Logistic Regression", Accuracy = confusionMatrix(y_hat_logit, as.factor(test_set$Attrition))$overall[["Accuracy"]], 
                      Sensitivity = sensitivity(logit_cm), Specificity = specificity(logit_cm), AUC = roc(as.numeric(test_set$Attrition), as.numeric(y_hat_logit))$auc)
results <- results %>% rbind(new_row_logit)

# Creating our trainControl variable 
ctrl <- trainControl(method = "cv",
                     number = 10,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

# The metric to compare the model is ROC
metric <- "ROC"

# Selecting our mtry value
mtry <- sqrt(ncol(train_set))
tunegrid <- expand.grid(.mtry=mtry)

# Random Forest Model
set.seed(123)
rf_fit <- train(Attrition~., 
                data=train_set, 
                method='rf', 
                metric= metric, 
                tuneGrid=tunegrid, 
                trControl=ctrl)
y_hat_rf <- predict(rf_fit, test_set) %>% factor()
rf_cm <- table(y_hat_rf, test_set$Attrition)
new_row_rf <- list(Method = "Random Forest", Accuracy = confusionMatrix(y_hat_rf, as.factor(test_set$Attrition))$overall[["Accuracy"]], 
                   Sensitivity = sensitivity(rf_cm), Specificity = specificity(rf_cm), AUC = roc(as.numeric(test_set$Attrition), as.numeric(y_hat_rf))$auc)
results <- results %>% rbind(new_row_rf)

# Knn Model
set.seed(34124)
knn_fit <- train(Attrition ~ ., 
                 train_set, 
                 method = "knn",
                 metric = metric,
                 tuneGrid = data.frame(k = c(3,5,7)),
                 trControl = ctrl)
y_hat_knn <- predict(knn_fit, test_set) %>% factor()
knn_cm <- table(y_hat_knn, test_set$Attrition)

new_row_knn <- list(Method = "K-Nearest Neighbor", Accuracy = confusionMatrix(y_hat_knn, as.factor(test_set$Attrition))$overall[["Accuracy"]], 
                    Sensitivity = sensitivity(knn_cm), Specificity = specificity(knn_cm), 
                    AUC = roc(as.numeric(test_set$Attrition),as.numeric(y_hat_knn))$auc)
results <- results %>% rbind(new_row_knn)

# GbM Model
set.seed(14)
gbm_fit <- train(Attrition ~., 
                 data = train_set, 
                 method = "gbm", 
                 verbose = FALSE, 
                 metric = metric, 
                 trControl = ctrl)
y_hat_gbm <- predict(gbm_fit, test_set) %>% factor()
gbm_cm <- table(y_hat_gbm, test_set$Attrition)
new_row_gbm <- list(Method = "GBM", Accuracy = confusionMatrix(y_hat_gbm, as.factor(test_set$Attrition))$overall[["Accuracy"]], 
                    Sensitivity = sensitivity(gbm_cm), Specificity = specificity(gbm_cm), 
                    AUC = roc(as.numeric(test_set$Attrition),as.numeric(y_hat_gbm))$auc)
results <- results %>% rbind(new_row_gbm)

# Setting the same seed that GBM model
ctrl$seeds <- gbm_fit$control$seeds

# Create a variable of penalized values
model_weights <- ifelse(train_set$Attrition == "No",
                        (1/table(train_set$Attrition)[1]) * 0.5,
                        (1/table(train_set$Attrition)[2]) * 0.5)

# Weighted GBM
gbm_weighted_fit <- train(Attrition ~ .,
                          data = train_set,
                          method = "gbm",
                          verbose = FALSE,
                          weights = model_weights,
                          metric = metric,
                          trControl = ctrl)

y_hat_gbm_weighted <- predict(gbm_weighted_fit, test_set) %>% factor()
gbm_weighted_cm <- table(y_hat_gbm_weighted, test_set$Attrition)
new_row_gbm_weighted <- list(Method = "GBM Weighted", Accuracy = confusionMatrix(y_hat_gbm_weighted, as.factor(test_set$Attrition))$overall[["Accuracy"]], 
                             Sensitivity = sensitivity(gbm_weighted_cm), Specificity = specificity(gbm_weighted_cm), 
                             AUC = roc(as.numeric(test_set$Attrition),as.numeric(y_hat_gbm_weighted))$auc)

gbm_table <- data.frame(new_row_gbm_weighted)

# GBM Under-Sampling
ctrl$sampling <- "down"

gbm_down_fit <- train(Attrition ~., 
                      data = train_set, 
                      method = "gbm", 
                      verbose = FALSE, 
                      metric = metric, 
                      trControl = ctrl)
y_hat_gbm_down <- predict(gbm_down_fit, test_set) %>% factor()
gbm_down_cm <- table(y_hat_gbm_down, test_set$Attrition)
new_row_gbm_down <- list(Method = "GBM Under-Sampling", Accuracy = confusionMatrix(y_hat_gbm_down, as.factor(test_set$Attrition))$overall[["Accuracy"]], 
                         Sensitivity = sensitivity(gbm_down_cm), Specificity = specificity(gbm_down_cm), 
                         AUC = roc(as.numeric(test_set$Attrition), as.numeric(y_hat_gbm_down))$auc)

gbm_table <- gbm_table %>% rbind(new_row_gbm_down)

# GBM Over-Sampling
ctrl$sampling <- "up"

gbm_up_fit <- train(Attrition ~., 
                    data = train_set, 
                    method = "gbm", 
                    verbose = FALSE, 
                    metric = metric, 
                    trControl = ctrl)
y_hat_gbm_up <- predict(gbm_up_fit, test_set) %>% factor()
gbm_up_cm <- table(y_hat_gbm_up, test_set$Attrition)
new_row_gbm_up <- list(Method = "GBM Over-Sampling", Accuracy = confusionMatrix(y_hat_gbm_up, as.factor(test_set$Attrition))$overall[["Accuracy"]], 
                       Sensitivity = sensitivity(gbm_up_cm), Specificity = specificity(gbm_up_cm), 
                       AUC = roc(as.numeric(test_set$Attrition), as.numeric(y_hat_gbm_up))$auc)

# Printing table with GBM algorithms 
gbm_table <- gbm_table %>% rbind(new_row_gbm_up)

gbm_table

# Printing table with all results
results <- results %>% rbind(new_row_gbm_down)

results

###############################################################################################

# Creating variables with ROC values for each algorithms.
roc_logit <- roc(as.numeric(test_set$Attrition), as.numeric(y_hat_logit))
roc_rf <- roc(as.numeric(test_set$Attrition), as.numeric(y_hat_rf))
roc_knn <- roc(as.numeric(test_set$Attrition),as.numeric(y_hat_knn))
roc_gbm <- roc(as.numeric(test_set$Attrition),as.numeric(y_hat_gbm))
roc_down_gbm <- roc(as.numeric(test_set$Attrition), as.numeric(y_hat_gbm_down))

# Plotting ROC curve for each algorithm
plot(roc_logit, print.thres = T, print.thres.cex = 0.8, main = "ROC Curves", sub ="GBM Under-Sampling in red", col = "salmon", cex.lab = 0.9)
plot(roc_rf, print.thres = T, print.thres.cex = 0.8, col = "darkolivegreen", add = T)
plot(roc_knn, print.thres = T, print.thres.cex = 0.8, col = "navyblue", add = T)
plot(roc_gbm, print.thres = T, print.thres.cex = 0.8, col = "chocolate3", add = T)
plot(roc_down_gbm, print.thres = T, print.thres.cex = 0.8, col = "red", add = T)

# Plotting variable importance of GBM Under-Sampling algorithm
ggplot(varImp(gbm_down_fit)) +
  geom_bar(stat = 'identity', fill = 'steelblue') 





