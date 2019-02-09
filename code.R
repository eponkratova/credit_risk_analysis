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