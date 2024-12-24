#loading the dataset
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(ggplot2)

Mental_health<- read.csv("C:/Users/Avinash/OneDrive/Desktop/Student Depression Dataset.csv")
View(Mental_health)

#View the first few rows of the dataset
View(head(Mental_health))
View(head(Mental_health,100))

#view the last few rows of the dataset
View(tail(Mental_health))

#check the structure of the dataset
str(Mental_health)
#summary  statistics of the dataset
print(summary(Mental_health))

# check for missing values
colSums(is.na(Mental_health))

# Remove duplicate rows, if any
Mental_health<-Mental_health %>% distinct()

#remove rows with missing values 
Mental_health<-na.omit(Mental_health)

str(Mental_health)

# Rename specific columns
#rename specific columns
Mental_health<-rename(Mental_health,
                      Age = Age,
                      Degree = Degree,)
print(Mental_health)

#  Distribution of categorical variables
table_distribution <- lapply(data[, categorical_cols], table)
print(table_distribution)

#Find the Average CGPA By study satisfaction
avg_cgpa__by_study_satisfaction<-Mental_health%>%
  group_by(Study.Satisfaction) %>%
  summarize(avg_cgpa=mean(cgpa,na.rm=TRUE)) %>%
  arrange(desc(avg_cgpa__by_study_satisfaction))

# View the result
View(head(avg_cgpa__by_study_satisfaction))

#Identify the Most Common Dietary habits
most_common_Dietary_Habits<-Mental_health%>%
count(Dietary_Habits) %>% 
arrange(desc(n)) %>%
slice(5)
print(most_common_Dietary_Habits)



#What are the top 10 most academic pressure?
top_10_academic_pressure<-Mental_health %>%
  select(Study.Satisfaction) %>%
  filter(!is.na(Study.Satisfaction), !is.na(Study.Satisfaction))%>%
  arrange(desc(Study.Satisfaction))
# View the result
View(top_10_academic_pressure)

#What are the top 10 study  hours?
top_10_study_hours<-Mental_health %>%
  select(Sleep.Duration) %>%
  filter(!is.na(Sleep.Duration), !is.na(Sleep.Duration))%>%
  arrange(Sleep.Duration)
# View the result
View(top_10_study_hours)

#Find the top 10 Cities with highest Academic pressure
top_10_city_name<-Mental_health%>%
  select(Academic.Pressure)%>%
  filter(!is.na(Academic.Pressure), !is.na(Academic.Pressure))%>%
  arrange(desc(Academic.Pressure))%>%
  slice(1:10)
#view the result
View(top_10_city_name)

#Find the Top 10 least cities
top_10_lowest_city<-Mental_health%>%
  select(city_name)%>%
  filter(!is.na(city), !is.na(city))%>%
  arrange(city)%>%
  slice(1:10)
#view the result
View(top_10_city)

#What is the correlation between CGPA and Academic pressure
correlation <- cor(Mental_health$CGPA, Mental_health$Academic.Pressure, method 
                   = "pearson")
print(correlation)

#what is the range of Study satisfaction and study hours
range_study_satisfaction <- range(Mental_health$Study.Satisfaction)
print(paste("Range of study satisfaction:", diff(range_study_satisfaction)))
range_study_hours <- range(Mental_health$study_hours)
print(paste("Range of Study hours:", diff(range_study_hours)))


#Analyze the impact of Family history 
Family_History_of_Mental_Illness<-Mental_health %>%
  group_by(Family.History.of.Mental.Illness)%>%
  summarize(avg_Depression=mean(Depression,na.rm = TRUE))
# View the result
View(Family_History_of_Mental_Illness_impact)


#univariate Analysis
# Distribution of a numerical variable(on CGPA)
ggplot(Mental_health, aes(x=CGPA)) +
  geom_histogram(binwidth=0.5, fill="blue", 
                 color="red") +
  labs(title="Distribution of Discount",
       x="CGPA", y="Count")

# Calculate the count of each CGPA
CGPA_counts <- Mental_health %>%
  count(CGPA) %>%
  arrange(desc(n)) %>%
  slice(1:5)  # Keep only the top 5 CGPA
# View the result
View(CGPA_counts)


#Bivariate Analysis
# Scatter plot between two numerical variables (Academic and satisfaction)
ggplot(Mental_health, aes(x=Academic.Pressure, y=Study.Satisfaction)) +
  geom_point(color="coral") +
  labs(title="Academic_Pressure vs Study_Satisfaction", x="Academic_pressure", y="Study_Satisfaction")

#Calculate the count and percentage of CGPA
CGPA_counts <- Mental_health %>%
  count(CGPA) %>%
  mutate(percentage = n / sum(n) * 100)
print(CGPA_counts)

# Create a pie chart with percentages
ggplot(CGPA_counts, aes(x = "", y = percentage, 
                                    fill = CGPA)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percentage,0), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Proportion of CGPA") +
  theme_void()

#Faceted plot to analyze multiple subsets
ggplot(Mental_health, aes(x = Age, y = Sleep.Duration)) +
  geom_point(aes(color = Age), alpha =0.7) +
  facet_wrap(~ Age) +
  labs(title = "Age vs. Sleep.Duration by Sleep_condition", 
       x = "Age", y = "Sleep.Duration") +
  theme_minimal()

# Box plots for categorical vs numerical variables

for (cat_col in colnames(categorical_features))
  {
  for (num_col in colnames(numerical_features))
    {
    ggplot(data, aes_string(x = cat_col, y = num_col)) + 
      geom_boxplot(fill = "green") + 
      ggtitle(paste("Boxplot of", num_col, "by", cat_col)) + 
      theme_minimal()
  }
}

#Bar plot
categorical_features <- select_if(data, is.character)
for (col in colnames(categorical_features))
  {
  
  ggplot(data, aes_string(x = col)) + 
    geom_bar(fill = "purple") + 
    ggtitle(paste("Bar chart of", col)) + 
    theme_minimal()
}





