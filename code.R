#Loading the libraries
install.packages("gmodels")
library(gmodels)

#Opening the file
loan_data <- readRDS('./files/loan_data_ch1.rds')

#Examining the file
head(loan_data)

#DEA
tbl1 <- CrossTable(loan_data$loan_status)
tbl2 <- CrossTable(loan_data$grade, loan_data$loan_status, prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

#Plotting the results
hist_1 <- hist(loan_data$loan_amnt)
hist_1$breaks

hist_2 <- hist(loan_data$loan_amnt, breaks = 200, xlab = "Loan amount", 
               main = "Histogram of the loan amount")

#Creating a scatterplot to check on outliers
plot(loan_data$age, ylab = 'Age')

#Ploting the outliers
index_highage <- which(loan_data$age > 122)

#Deleting the outliers
new_data <- loan_data[-index_highage, ]

plot(loan_data$age, loan_data$annual_inc, xlab = "Age", ylab = "Annual income")

#Working with missing values
summary(loan_data$int_rate)

#Deleting the observations
na_index <- which(is.na(loan_data$int_rate))
loan_data_delrow_na <- loan_data[-na_index,]

#Delting the entire column
loan_data_delcol_na <- loan_data
loan_data_delcol_na$int_rate <- NULL

#Replacing missing with median
median_ir <- median(loan_data$int_rate, na.rm = TRUE)
loan_data_replace <- loan_data
loan_data_replace$int_rate[na_index] <- median_ir
summary(loan_data_replace$int_rate)

#Changing continuous to categorical
# Make the necessary replacements in the coarse classification example below 
# Make the necessary replacements in the coarse classification example below 
loan_data$ir_cat <- rep(NA, length(loan_data$int_rate))
loan_data$ir_cat[which(loan_data$int_rate <= 8)] <- "0-8"
loan_data$ir_cat[which(loan_data$int_rate > 8 & loan_data$int_rate <= 11)] <- "8-11"
loan_data$ir_cat[which(loan_data$int_rate > 11 & loan_data$int_rate <= 13.5)] <- "11-13.5"
loan_data$ir_cat[which(loan_data$int_rate > 13.5)] <- "13.5+"
loan_data$ir_cat[which(is.na(loan_data$int_rate))] <- "Missing"
loan_data$ir_cat <- as.factor(loan_data$ir_cat)

# Look at your new variable using plot()
plot(loan_data$ir_cat)

#Splitting the data into train and test datasets
# Set seed of 567
set.seed(567)
# Store row numbers for training set: index_train
index_train <- sample(1:nrow(loan_data), 2 / 3 * nrow(loan_data))
# Create training set: training_set
training_set <- loan_data[index_train, ]
# Create test set: test_set
test_set <- loan_data[-index_train, ]


# Create confusion matrix
conf_matrix <- table(test_set$loan_status, model_pred)