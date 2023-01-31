## ZAD 4 Asocijacijska analiza 
# requirements:
#install.packages("arules")
#install.packages("arulesViz")
library(arules)
library(arulesViz)

# loading the dataset:
data <- read.csv("diabetes.csv")
str(data)
data_subset <- data[,c( "BMI","BloodPressure", "Outcome")]


# binning the BMI and BloodPressure columns into "normal" or "high"
data_subset$BMI<-ifelse(data_subset$BMI<25,"normal","high")
data_subset$BloodPressure<-ifelse(data_subset$BloodPressure<120,"normal","high")

# converting the variables to factors
data_subset$BMI = as.factor(data_subset$BMI)
data_subset$BloodPressure = as.factor(data_subset$BloodPressure)
data_subset$Outcome = as.factor(data_subset$Outcome)

# displaying the structure of the data
str(data_subset)

# converting the data to transactions
transactions <- as(data_subset, "transactions")
print(transactions)
# perform association rule mining
rules <- apriori(transactions, parameter = list(supp = 0.5, conf = 0.9))

# sort rules by lift
rules <- sort(rules, by="lift")

# view the rules
inspect(rules)

# Plot the rules
plot(rules)

# Summary of the rules
summary(rules)

