
# remove all objects from the current workspace.
rm(list = ls())
df <- read.csv("C:\\Users\\duong\\OneDrive\\Desktop\\QUAN\\Spring 2024\\Big Data Analytics\\SystemAdministrators.csv")



#============================ very basic exploration ========================
head(df) # check the headers
nrow(df) # number of rows in delay.df
df$isCompleted <- ifelse(df$Completed == "Yes", 1, 0)


#============================ logistic regression ==============================
summary(df$isCompleted)
lm.fit <- glm(isCompleted ~ Experience + Training, data = df, family = "binomial")
options(scipen=999) # remove scientific notation

summary(lm.fit, digits=5)

#============================ evaluation (covered in session 5) ==============================
pred <- predict(lm.fit,df)
# Assuming 'pred' contains the predicted probabilities from your logistic regression model

# Correctly calculate and compare predictions against actual outcomes
predictions <- ifelse(pred > 0.5, "Yes", "No")

# Use the confusionMatrix from the caret package (assuming 'df$Completed' contains the actual "Yes"/"No" outcomes)
library(caret)
confusionMatrix(factor(predictions), factor(df$Completed))

# If you want to map into "Positive" and "Negative" for a binary classification perspective
# where "Positive" = "Yes" (task completion) and "Negative" = "No" (task not completed)
# Make sure the actual outcomes are correctly represented in your comparison
confusionMatrix(factor(ifelse(pred > 0.5, "Positive", "Negative")), 
                factor(ifelse(df$Completed == "Yes", "Positive", "Negative")))






#### Figure 10.4
# Aggregate data to get the mean of isCompleted by Training
aggregated_data <- aggregate(isCompleted ~ Training, data=df, FUN=mean)

# Creating the bar chart
barplot(aggregated_data$isCompleted,
        xlab = "Training Level",
        ylab = "Average Completion Rate",
        names.arg = aggregated_data$Training,
        col = "blue", # Color of the bars
        main = "Average Task Completion by Training Level")


#### Figure 10.5
# install.packages("reshape") # Package installation is required for the first time
library(reshape)
library(ggplot2)
# create matrix for plot

matrix_data <- matrix(aggregated_data$isCompleted, nrow = length(aggregated_data$Training), byrow = TRUE)

# Name the rows or columns based on Training levels for clarity
rownames(matrix_data) <- aggregated_data$Training

# Creating the bar chart from the matrix
barplot(matrix_data,
        beside = TRUE, # Set to FALSE to stack bars instead of placing them beside each other
        xlab = "Training Level",
        ylab = "Average Completion Rate",
        col = "blue",
        legend.text = rownames(matrix_data),
        main = "Average Task Completion by Training Level")



