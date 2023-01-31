## ZAD 1 AGREGACIJA 

# requirements:
# install.packages("dplyr")
library(dplyr)

# loading the dataset:
data <- read.csv("diabetes.csv")

# aggregate the dataset:
aggregated_data <- data %>% 
  group_by(Outcome) %>% 
  summarize(
    Total_Count = n(),
    Total_Pregnancies = sum(Pregnancies), 
    Max_Glucose = max(Glucose), 
    Max_Blood_Pressure = max(BloodPressure), 
    Average_Skin_Thickness = mean(SkinThickness), 
    Max_Insulin = max(Insulin), 
    Average_BMI = mean(BMI), 
    Average_Diabetes_Pedigree = mean(DiabetesPedigreeFunction),
    Average_Age = mean(Age)
  )

print(aggregated_data)
