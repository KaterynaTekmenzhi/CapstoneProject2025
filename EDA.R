data <- read.csv("C:/Users/Kateryna.Tekmenzhi/OneDrive - Bellevue College/data.csv", sep = ";")

str(data)
summary(data)
# 

# Check dimensions
dim(data)

# Check column names
names(data)

# View first few rows
head(data)

# First, let's convert the Target variable to a factor for classification
data$Target <- as.factor(data$Target)

# Examine the distribution of the target variable
table(data$Target)
prop.table(table(data$Target)) * 100  # Percentage distribution

# Create a visualization of target distribution
library(ggplot2)
ggplot(data, aes(x = Target, fill = Target)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  labs(title = "Distribution of Target Variable", x = "Outcome", y = "Count") +
  theme_minimal()

# Basic exploration of key variables
# Let's examine some numerical variables
numerical_vars <- c("Age.at.enrollment", "Previous.qualification..grade.", "Admission.grade",
                    "Curricular.units.1st.sem..approved.", "Curricular.units.2nd.sem..approved.")

# Create boxplots for numerical variables by target
for(var in numerical_vars) {
  p <- ggplot(data, aes_string(x = "Target", y = var, fill = "Target")) +
    geom_boxplot() +
    labs(title = paste("Distribution of", var, "by Target"), 
         x = "Outcome", y = var) +
    theme_minimal()
  print(p)
}

# Examine categorical variables
categorical_vars <- c("Marital.status", "Application.mode", "Course", 
                      "Daytime.evening.attendance.", "Previous.qualification",
                      "Gender", "Scholarship.holder")

# Create stacked bar plots for categorical variables
for(var in categorical_vars[1:3]) {  # Just the first 3 to start
  # Calculate proportions
  prop_data <- prop.table(table(data[[var]], data$Target), 1) * 100
  prop_data <- as.data.frame(prop_data)
  names(prop_data) <- c("Variable", "Target", "Percentage")
  
  # Create plot
  p <- ggplot(prop_data, aes(x = Variable, y = Percentage, fill = Target)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Proportion of Target by", var), 
         x = var, y = "Percentage") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}

# Check for missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))
missing_values[missing_values > 0]  # Display only variables with missing values

# Basic correlation analysis for numerical variables
library(corrplot)
numeric_data <- data[, sapply(data, is.numeric)]
correlation_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(correlation_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.cex = 0.7)

# Dimension 1: Academic Background
# Create a new categorical variable for admission grade ranges
data$Admission_grade_cat <- cut(data$Admission.grade, 
                                breaks = c(0, 110, 130, 150, 200),
                                labels = c("Low", "Medium", "High", "Very High"))

# Analyze dropout rates by admission grade category
admission_table <- table(data$Admission_grade_cat, data$Target)
prop.table(admission_table, 1) * 100  # Row percentages

# Visualize
ggplot(data, aes(x = Admission_grade_cat, fill = Target)) +
  geom_bar(position = "fill") +
  labs(title = "Dropout vs Graduate by Admission Grade Category", 
       x = "Admission Grade Category", y = "Proportion") +
  theme_minimal()

# Dimension 2: Demographics
# Age categories
data$Age_category <- cut(data$Age.at.enrollment, 
                         breaks = c(0, 20, 25, 30, 100),
                         labels = c("Under 20", "20-25", "25-30", "Over 30"))

# Analyze dropout rates by age category
age_table <- table(data$Age_category, data$Target)
prop.table(age_table, 1) * 100  # Row percentages

# Visualize
ggplot(data, aes(x = Age_category, fill = Target)) +
  geom_bar(position = "fill") +
  labs(title = "Dropout vs Graduate by Age Category", 
       x = "Age Category", y = "Proportion") +
  theme_minimal()

# Dimension 3: Economic Factors
# Analyze dropout rates by scholarship status
scholarship_table <- table(data$Scholarship.holder, data$Target)
prop.table(scholarship_table, 1) * 100  # Row percentages

# Visualize
ggplot(data, aes(x = factor(data$Scholarship.holder), fill = Target)) +
  geom_bar(position = "fill") +
  labs(title = "Dropout vs Graduate by Scholarship Status", 
       x = "Scholarship Holder (0=No, 1=Yes)", y = "Proportion") +
  theme_minimal()

# Dimension 4: Academic Performance
# Create a new variable for first semester performance
data$First_sem_performance <- ifelse(
  data$Curricular.units.1st.sem..enrolled. == 0, "No Enrollment",
  ifelse(data$Curricular.units.1st.sem..approved. / 
           data$Curricular.units.1st.sem..enrolled. >= 0.75, "High",
         ifelse(data$Curricular.units.1st.sem..approved. / 
                  data$Curricular.units.1st.sem..enrolled. >= 0.5, "Medium", "Low")))

# Handle division by zero
data$First_sem_performance[is.na(data$First_sem_performance)] <- "No Enrollment"

# Analyze dropout rates by first semester performance
performance_table <- table(data$First_sem_performance, data$Target)
prop.table(performance_table, 1) * 100  # Row percentages

# Visualize
ggplot(data, aes(x = First_sem_performance, fill = Target)) +
  geom_bar(position = "fill") +
  labs(title = "Dropout vs Graduate by First Semester Performance", 
       x = "First Semester Performance", y = "Proportion") +
  theme_minimal()