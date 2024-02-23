#################################################################################################
######################################## IMPORT LIBRARIES #######################################
#################################################################################################

library(MASS)
library(magrittr)
library(ggpubr)
library(dplyr)
library(car)
library(faraway)
library(corrplot)
library(caret)
library(biotools)
#################################################################################################
######################################### LOAD THE DATA #########################################
#################################################################################################

df <- read.csv("data.csv")

#################################################################################################
##################################### PRE-PROCESS THE DATA ######################################
#################################################################################################

########################################## DROP COLUMNS #########################################

drop_columns <- c("At.what.time.of.the.day.do.you.usually.study.outside.of.class.",
                  "Timestamp", "Email.address")

data <- df[ , !(names(df) %in% drop_columns)]

########################################### DROP ROWS ###########################################

drop_rows <- c(57, 80, 81, 82, 83, 84, 85, 86)
data <- data[-drop_rows,]

####################################### CHANGE COLUMN NAMES #####################################

new_column_names <- c("GPA","Performance_Type", "Ielts_Score", "Attendance", 
                      "Groupstudy_Likelyness", "Sleep_Duration", 
                      "Selfstudy_Duration", "Extracurricular_Duration", 
                      "Leisure_Duration", "Distance")

colnames(data) <- new_column_names 

##################################### CHANGE COLUMN DATA TYPE ###################################

# Columns to convert to numeric #
numeric_type_columns <- c("GPA","Ielts_Score", "Sleep_Duration", "Selfstudy_Duration", 
                          "Extracurricular_Duration", "Leisure_Duration", "Distance")

# Change data types of specified columns to numeric using mutate(across()) #
data <- data %>%
  mutate(across(all_of(numeric_type_columns), as.numeric))

# Character percentage columns into Numeric columns #
percentage_type_columns <- c("Attendance", "Groupstudy_Likelyness")

for (col in percentage_type_columns) {
  data[[col]] <- as.numeric(gsub("%", "", data[[col]])) / 100
}

# Character columns into Factor data type columns #
performance_levels <- c("High", "Low")

# Convert Performance_Type column to factor with levels "High" and "Low" #
data$Performance_Type <- factor(data$Performance_Type, levels = performance_levels)


##################################### CHECK TRANSFORMED DATA ####################################

# Check the final data types #
sapply(data, class)

# Check the levels of the Performance_Type column #
levels(data$Performance_Type)

# Check the few first data #
print(head(data))

#################################################################################################
################################## EXPLORE AND ANALYZE THE DATA #################################
#################################################################################################

################################## NORMAL DISTRIBUTION TESTING ##################################

# Perform Shapiro-Wilk test for each column and store results in a new data frame #
data.distribution <- data[ , !(names(data) %in% c("Performance_Type", "GPA"))]

shapiro_results <- data.frame(
  Variable = names(data.distribution),
  W = numeric(length(data.distribution)),
  p_value = numeric(length(data.distribution))
)

for (i in 1:ncol(data.distribution)) {
  shapiro_test_result <- shapiro.test(data.distribution[[i]])
  shapiro_results$W[i] <- shapiro_test_result$statistic
  shapiro_results$p_value[i] <- shapiro_test_result$p.value
}

# Get column names #
column_names <- names(data.distribution)

# Create density plots for each column #
par(mfrow=c(1, length(column_names)))  # Set up multiple plots in a row
for (col in column_names) {
  hist(data.distribution[[col]], main=paste("", col), xlab="Values")
}

################################### CORRELATION TESTING ###################################

# Extract predictor variables #
predictor_variables <- c("Ielts_Score", "Attendance", "Groupstudy_Likelyness",
                         "Sleep_Duration", "Selfstudy_Duration", 
                         "Extracurricular_Duration", "Leisure_Duration", "Distance")

correlation_matrix_spearman <- cor(data[,c(3:10)], method = "spearman")

# Round the corralation matrix to the 5th decimal place #
round(correlation_matrix_spearman, digits = 5)

# Plot corralation matrix #
corrplot(correlation_matrix_spearman,type = "lower",method = c("color"))

################################### COLLINEARITY TESTING ##################################

data.collinearity <- data[ , !(names(data) %in% c("Performance_Type"))]

# Build the regression model using the normalized data #
regression_model <- lm(GPA ~ ., data = data.collinearity)

# Calculate VIF values #
vif_values <- vif(regression_model)

# Print VIF values #
print(vif_values)

#################################################################################################
###################################### DISCRIMINANT ANALYSIS ####################################
#################################################################################################


########################################### LDA vs QDA ##########################################

########################################### 1st Method ##########################################

data.transformed <- data[ , !(names(data) %in% c("GPA"))]


partition_data <- function(data) {
  # Create an index-based for training data #
  training.samples <- data$Performance_Type %>%
    createDataPartition(p = 0.8, list = FALSE)
  
  # Split the data according to the index
  train.dt <- data[training.samples, ]
  test.dt <- data[-training.samples, ]
  
  # Estimate pre-processing parameters #
  preproc.param <- train.dt %>%
    preProcess(method = c("center", "scale"))
  
  # Transform the data using the estimated parameters #
  train.transformed <- preproc.param %>% predict(train.dt)
  test.transformed <- preproc.param %>% predict(test.dt)
  
  # Return both transformed dataset
  return(list(train = train.transformed, test = test.transformed))
}
  

testing_da_models <- function(data, n) {
  
  lda_accuracies <- numeric(n)
  qda_accuracies <- numeric(n)
  
  for (i in 1:n) {
    # Call the function and store the results #
    transformed_data <- partition_data(data)
    
    # Access the transformed dataset #
    train_data <- transformed_data$train
    test_data <- transformed_data$test
    
    
    lda_fit <- lda(Performance_Type ~ ., data = train_data)
    lda_predictions <- predict(lda_fit, newdata = test_data)
    lda_accuracies[i] <- mean(lda_predictions$class == test_data$Performance_Type)
    
    qda_fit <- qda(Performance_Type ~ ., data = train_data)
    qda_predictions <- predict(qda_fit, newdata = test_data)
    qda_accuracies[i] <- mean(qda_predictions$class == test_data$Performance_Type)
  }
  
  average_lda_accuracy <- mean(lda_accuracies)
  average_qda_accuracy <- mean(qda_accuracies)
  
  print(paste("Average LDA Accuracy (", n, "runs):", average_lda_accuracy))
  print(paste("Average QDA Accuracy (", n, "runs):", average_qda_accuracy))
  }

testing_da_models(data.transformed, 10000)

########################################### 2nd Method ##########################################

pairs(data[predictor_variables], col = "dodgerblue")

########################################### 3rd Method ##########################################

boxM_result <- boxM(data.transformed[,2:9], data.transformed[, "Performance_Type"])
boxM_result

boxM_result$cov$High
boxM_result$cov$Low

#################################################################################################
###################################### Experiment and Testing ###################################
#################################################################################################

####################################### Base Model Performance ##################################
data.transformed <- data[ , !(names(data) %in% c("GPA"))]

testing_qda_models <- function(data, n) {
  
  qda_accuracies <- numeric(n)
  
  for (i in 1:n) {
    # Call the function and store the results #
    transformed_data <- partition_data(data)
    
    # Access the transformed dataset #
    train_data <- transformed_data$train
    test_data <- transformed_data$test
    
    qda_fit <- qda(Performance_Type ~ ., data = train_data)
    qda_predictions <- predict(qda_fit, newdata = test_data)
    qda_accuracies[i] <- mean(qda_predictions$class == test_data$Performance_Type)
  }
  
  average_qda_accuracy <- mean(qda_accuracies)
  
  print(paste("Average QDA Accuracy (", n, "runs):", average_qda_accuracy))
}

testing_qda_models(data.transformed, 1)
testing_qda_models(data.transformed, 100)
testing_qda_models(data.transformed, 1000)
testing_qda_models(data.transformed, 10000)

################################## Optimize the Prediction Accuracy #############################

# Generate all dataframe with different prediction variable # 
data_combination <- function(data) {
  combined_df_list <- list() 
  for (i in 2:8) {
    df_list <- lapply(1:(ncol(combn(2:ncol(data), m = i))), function(y) {
      new_df <- cbind(Performance_Type = data[, 1], data[, combn(2:ncol(data.norm), m = i)[, y]])
      return(new_df)
    })
    combined_df_list <- c(combined_df_list, df_list)
  }
  return(combined_df_list)
}

# Find out the variables combination with highest prediction Accuracy #
testing_da_models_optimize <- function(data, n) {
  
  # Initialize variables
  dataset.list <- list()
  qda_highest_ave_acuraccy <- 0
  lda_highest_ave_acuraccy <- 0
  total_highest_ave_acuraccy <- 0
  rank <- 0
  
  # Define column names and their types #
  column_names <- c("Total var", "combination", "Average LDA", "Average QDA", "LDA + QDA")
  
  
  # Create an empty dataframe with initialized variables #
  variables_combination_list <- data.frame(
    Column2 = vector("numeric", length = 0),
    Column3 = vector("character", length = 0),
    Column4 = vector("numeric", length = 0),
    Column5 = vector("numeric", length = 0),
    Column6 = vector("numeric", length = 0)
  )
  
  # Rename the columns #
  colnames(variables_combination_list) <- column_names
  
  # Generate variable combination dataframes #
  dataset.list <- data_combination(data.norm)
  
  # Run the model of each dataframes #
  for(i in 1:length(dataset.list)){
    data.test <- dataset.list[i]
    data.test <- do.call(rbind, data.test)
    head(data.test)
    lda_accuracies <- numeric(n)
    qda_accuracies <- numeric(n)
    
    
    rank = rank + 1
    catergories <- ls(data.test[,c(2:ncol(data.test))])
    Total_var <- ncol(data.test) - 1
    
    for (i in 1:n) {
      # Call the function and store the results #
      transformed_data <- partition_data(data.test)
      
      # Access the transformed dataset #
      train_data <- transformed_data$train
      test_data <- transformed_data$test
      
      lda_fit <- lda(Performance_Type ~ ., data = train_data)
      lda_predictions <- predict(lda_fit, newdata = test_data)
      lda_accuracies[i] <- mean(lda_predictions$class == test_data$Performance_Type)
      
      qda_fit <- qda(Performance_Type ~ ., data = train_data)
      qda_predictions <- predict(qda_fit, newdata = test_data)
      qda_accuracies[i] <- mean(qda_predictions$class == test_data$Performance_Type)
      
    }
    average_lda_accuracy <- mean(lda_accuracies)
    average_qda_accuracy <- mean(qda_accuracies)
    total_accuracy <- average_lda_accuracy + average_qda_accuracy
    
    Var_combination <- paste(catergories, collapse = ", ")
    new_value <- list(Total_var, Var_combination ,average_lda_accuracy ,average_qda_accuracy , total_accuracy)
    variables_combination_list[nrow(variables_combination_list) + 1, ] <- new_value
    
    if (average_qda_accuracy + average_lda_accuracy > total_highest_ave_acuraccy   ) {
      qda_highest_ave_acuraccy <- average_qda_accuracy
      lda_highest_ave_acuraccy <- average_lda_accuracy
      total_highest_ave_acuraccy <- average_qda_accuracy + average_lda_accuracy
    }  
  }
  print(paste("qda highest ave acuraccy (", n, "runs):", qda_highest_ave_acuraccy))
  
  
  variables_combination_list <- variables_combination_list[order(variables_combination_list$`LDA + QDA`, decreasing = TRUE), ]
  Rank <- c(1:rank)
  variables_combination_list<- cbind(Rank, variables_combination_list)
  
  head(variables_combination_list)
  return(variables_combination_list)
}

ranking1  <- testing_da_models_optimize(data.norm, 500)
ranking2 <- testing_da_models_optimize(data.norm, 500)
ranking3  <- testing_da_models_optimize(data.norm, 1000)
ranking4  <- testing_da_models_optimize(data.norm, 1000)
ranking5 <- testing_da_models_optimize(data.norm, 5000)
ranking6  <- testing_da_models_optimize(data.norm, 10000)

knitr::kable(head(ranking,10), "pipe")