# Report 2 - R Practice 
# Abhigna Ramamurthy 
getwd()
setwd('/Users/abhi/Desktop')

df0 <- read.csv(file = 'deaths.csv', stringsAsFactors=FALSE)
df0

# Structure of df0
str(df0)

# Summary of df0
summary(df0)

# column names of df0 
names(df0)

# check type of df0
typeof(df0)

# creating data frame of df0 for analysis
df1 <- as.data.frame(df0)
names(df1)

# total number of rows available in the original data 
nrow(df1)

# Renaming columns 
colnames(df1)[1] <- "Age"
colnames(df1)[2] <- "Ethnicity"
colnames(df1)[3] <- "Race"
colnames(df1)[4] <- "Sex"
colnames(df1)[5] <- "Country"
colnames(df1)[6] <- "Chronic_Condition"

# get top 5 rows of df1
head(df1)

tail(df1)
# converting sex, ethnicity and race to factor from character as it will be helpful to use group by analysis
df1$Sex = as.factor(df1$Sex)
df1$Ethnicity = as.factor(df1$Ethnicity)
df1$Race = as.factor(df1$Race)
df1
str(df1)


# Changing "Unknown" from sex, ethnicity, race and chronic_condition to NA
df1$Sex<-gsub("Unknown","NA",as.character(df1$Sex))
df1$Ethnicity<-gsub("Unknown","NA",as.character(df1$Ethnicity))
df1$Race<-gsub("Unknown","NA",as.character(df1$Race))
df1$Chronic_Condition<-gsub("Unknown","NA",as.character(df1$Chronic_Condition))

# converting "90+" data values of age to "90" as we can analyse 
df1$Age <- ifelse(df1$Age %in% c('90+'), '90', df1$Age)

# Convert age to numeric 
df1$Age <- as.integer(df1$Age)

# Analysis on each columns 
# Age analysis. 
# Would like to find number of people in the following age groups (Age groups of importance for group analysis later)
# 1-20, 21-40, 41-60, 61-89, 90(includes 90+)
ageGroup0To20 <- filter(df1, df1$Age < 21 & df1$Age > 0)
age1 <- nrow(ageGroup0To20)

ageGroup21To40 <- filter(df1, df1$Age > 20 & df1$Age < 41)
age2 <- nrow(ageGroup21To40)

ageGroup41To60 <- filter(df1, df1$Age > 40 & df1$Age < 61)
age3 <- nrow(ageGroup41To60)

ageGroup61To89 <- filter(df1, df1$Age > 60 & df1$Age < 90)
age4 <- nrow(ageGroup61To89)

ageGroup90AndAbove <- filter(df1, df1$Age == 90)
age5 <- nrow(ageGroup90AndAbove)

# Bar graph comparing different age groups 
library(ggplot2)
Age_Groups <- c('1-20','21-40', '41-60', '61-89', '90(includes 90+)')
Number_Of_Cases <- c(age1,age2,age3,age4,age5)

dfAge <- cbind.data.frame(Age_Groups,Number_Of_Cases)
dfAge

p<-ggplot(data=dfAge, aes(x=Age_Groups, y=Number_Of_Cases)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Number_Of_Cases), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  labs(title="Figure 1: Plot of number of cases based on age group")
p

# scatter plot of the same as above - 
scatterPlotAgeGroup <- ggplot(dfAge, aes(x=Age_Groups, y=Number_Of_Cases)) + 
  geom_point() + 
  labs(title="Figure 2: Scatter plot of number of cases based on age group")
scatterPlotAgeGroup

# Histogram of the abive age groups data 
hist <- ggplot(df1, aes(x=df1$Age)) + geom_histogram()+
  labs(title="Figure 7: Histogram of number of cases based on age group")
hist

##### Race Analysis
# Unique values of Race 
unique(df1[c("Race")])
library(tidyverse)

race_analysis <- df1 %>% group_by(Race)
race_analysis

## Asian Population
race_analysis_asian_pop <- filter(df1, df1$Race == 'Asian')
# No.of Asian Population
no_of_Asian <- nrow(race_analysis_asian_pop)

# No of chronic condition 
no_0f_Asian_Chronic_Condition_Yes <- nrow(filter(race_analysis_asian_pop, race_analysis_asian_pop$Chronic_Condition == "Yes"))
no_0f_Asian_Chronic_Condition_Yes
no_0f_Asian_Chronic_Condition_No <- nrow(filter(race_analysis_asian_pop, race_analysis_asian_pop$Chronic_Condition == "No"))
no_0f_Asian_Chronic_Condition_No

## White Population
race_analysis_white_pop <- filter(df1, df1$Race == 'White')
# No.of White Population
no_of_White <- nrow(race_analysis_white_pop)

# No of chronic condition 
no_0f_White_Chronic_Condition_Yes <- nrow(filter(race_analysis_white_pop, race_analysis_white_pop$Chronic_Condition == "Yes"))
no_0f_White_Chronic_Condition_Yes
no_0f_White_Chronic_Condition_No <- nrow(filter(race_analysis_white_pop, race_analysis_white_pop$Chronic_Condition == "No"))
no_0f_White_Chronic_Condition_No

## African-American/ Black
race_analysis_black_pop <- filter(df1, df1$Race == 'African-American/ Black')
no_of_Black <- nrow(race_analysis_black_pop)

# No of chronic condition 
no_0f_Black_Chronic_Condition_Yes <- nrow(filter(race_analysis_black_pop, race_analysis_black_pop$Chronic_Condition == "Yes"))
no_0f_Black_Chronic_Condition_Yes
no_0f_Black_Chronic_Condition_No <- nrow(filter(race_analysis_black_pop, race_analysis_black_pop$Chronic_Condition == "No"))
no_0f_Black_Chronic_Condition_No

## American Indian/ Alaska Native
race_analysis_indian_pop <- filter(df1, df1$Race == 'American Indian/ Alaska Native')
# No.of American Indian/ Alaska Native Population
no_of_Indian <- nrow(race_analysis_indian_pop)

# No of chronic condition 
no_0f_Indian_Chronic_Condition_Yes <- nrow(filter(race_analysis_indian_pop, race_analysis_indian_pop$Chronic_Condition == "Yes"))
no_0f_Indian_Chronic_Condition_Yes
no_0f_Indian_Chronic_Condition_No <- nrow(filter(race_analysis_indian_pop, race_analysis_indian_pop$Chronic_Condition == "No"))
no_0f_Indian_Chronic_Condition_No

## Other
race_analysis_other_pop <- filter(df1, df1$Race == 'Other')
no_of_Other <- nrow(race_analysis_other_pop)

# No of chronic condition 
no_0f_Other_Chronic_Condition_Yes <- nrow(filter(race_analysis_other_pop, race_analysis_other_pop$Chronic_Condition == "Yes"))
no_0f_Other_Chronic_Condition_Yes
no_0f_Other_Chronic_Condition_No <- nrow(filter(race_analysis_other_pop, race_analysis_other_pop$Chronic_Condition == "No"))
no_0f_Other_Chronic_Condition_No

## NA
race_analysis_na_pop <- filter(df1, df1$Race == 'NA')
no_of_NA <- nrow(race_analysis_na_pop)

# No of chronic condition 
no_0f_NA_Chronic_Condition_Yes <- nrow(filter(race_analysis_na_pop, race_analysis_na_pop$Chronic_Condition == "Yes"))
no_0f_NA_Chronic_Condition_Yes
no_0f_NA_Chronic_Condition_No <- nrow(filter(race_analysis_na_pop, race_analysis_na_pop$Chronic_Condition == "No"))
no_0f_NA_Chronic_Condition_No

# Plot of number of people with chronic conditions based on race 
Race <- c("White","African-American/ Black","American Indian/ Alaska Native","Asian","Other","NA")
Number_Of_Cases_By_Race_Group <- c(no_of_White,no_of_Black,no_of_Indian,no_of_Asian,no_of_Other,no_of_NA)


dfRace <- cbind.data.frame(Race,Number_Of_Cases_By_Race_Group)
dfRace

# Plots on number of cases based on race
p<-ggplot(data=dfRace, aes(x=Race, y=Number_Of_Cases_By_Race_Group)) +
  geom_bar(stat="identity", fill="green")+
  geom_text(aes(label=Number_Of_Cases_By_Race_Group), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  labs(title="Figure 3: Plot of number of cases based on race groups")
p
rlang::last_error()

# grouped bar chart of comparison between number of cases and no of cases with chronic condition 

Race <- c("White","Black","American Indian","Asian","Other","NA","White","Black","American Indian","Asian","Other","NA")
Number_Of_Cases <- c(no_of_White,no_of_Black,no_of_Indian,no_of_Asian,no_of_Other,no_of_NA, no_0f_White_Chronic_Condition_Yes,no_0f_Black_Chronic_Condition_Yes, no_0f_Indian_Chronic_Condition_Yes,no_0f_Asian_Chronic_Condition_Yes,no_0f_Other_Chronic_Condition_Yes,no_0f_NA_Chronic_Condition_Yes)
Indicator <- c("Total Cases","Total Cases","Total Cases","Total Cases","Total Cases","Total Cases","Number of chronic condition cases","Number of chronic condition cases","Number of chronic condition cases","Number of chronic condition cases","Number of chronic condition cases","Number of chronic condition cases")

dfRaceGroupedBar <- cbind.data.frame(Race,Number_Of_Cases,Indicator)
dfRaceGroupedBar

pGroupedBar<-ggplot(data=dfRaceGroupedBar, aes(x=Race, y=Number_Of_Cases,fill=Indicator)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=Number_Of_Cases), vjust=1.6, color="white", size=3.5, position = position_dodge(0.9),)+
  theme_minimal()+
  labs(title="Figure 4: Plot of number of cases and number of cases with chronic condition based on race groups")
pGroupedBar

##### End of Race Analysis. 

#### Ethnicity Analysis
str(df1)

# Unique values of Race 
unique(df1[c("Ethnicity")])

# Attempt to analyse each ethnic group number of cases based on gender. 

ethnicity_analysis <- df1 %>% group_by(Ethnicity)
ethnicity_analysis

## Non-Hispanic/ Latino Population
ethnicity_analysis_nonHispanic_pop <- filter(df1, df1$Ethnicity == 'Non-Hispanic/ Latino')
no_of_NonHispanic <- nrow(ethnicity_analysis_nonHispanic_pop)
no_of_NonHispanic

# No of male and female cases for Non-Hispanic/ Latino Population
no_0f_NonHispanic_Male <- nrow(filter(ethnicity_analysis_nonHispanic_pop, ethnicity_analysis_nonHispanic_pop$Sex == "Male"))
no_0f_NonHispanic_Male
no_0f_NonHispanic_Female <- nrow(filter(ethnicity_analysis_nonHispanic_pop, ethnicity_analysis_nonHispanic_pop$Sex == "Female"))
no_0f_NonHispanic_Female
no_0f_NonHispanic_Unknown <- nrow(filter(ethnicity_analysis_nonHispanic_pop, ethnicity_analysis_nonHispanic_pop$Sex == "NA"))
no_0f_NonHispanic_Unknown

## Hispanic/ Latino Population
ethnicity_analysis_Hispanic_pop <- filter(df1, df1$Ethnicity == 'Hispanic/ Latino')
no_of_Hispanic <- nrow(ethnicity_analysis_Hispanic_pop)
no_of_Hispanic

# No of male and female cases for Hispanic/ Latino Population
no_0f_Hispanic_Male <- nrow(filter(ethnicity_analysis_Hispanic_pop, ethnicity_analysis_Hispanic_pop$Sex == "Male"))
no_0f_Hispanic_Male
no_0f_Hispanic_Female <- nrow(filter(ethnicity_analysis_Hispanic_pop, ethnicity_analysis_Hispanic_pop$Sex == "Female"))
no_0f_Hispanic_Female
no_0f_Hispanic_Unknown <- nrow(filter(ethnicity_analysis_Hispanic_pop, ethnicity_analysis_Hispanic_pop$Sex == "NA"))
no_0f_Hispanic_Unknown

## Unknown ethnicity Population
ethnicity_analysis_unknown_pop <- filter(df1, df1$Ethnicity == 'NA')
no_of_NA <- nrow(ethnicity_analysis_unknown_pop)
no_of_NA

# No of male and female cases for Hispanic/ Latino Population
no_0f_NA_Male <- nrow(filter(ethnicity_analysis_unknown_pop, ethnicity_analysis_unknown_pop$Sex == "Male"))
no_0f_NA_Male
no_0f_NA_Female <- nrow(filter(ethnicity_analysis_unknown_pop, ethnicity_analysis_unknown_pop$Sex == "Female"))
no_0f_NA_Female
no_0f_NA_Unknown <- nrow(filter(ethnicity_analysis_unknown_pop, ethnicity_analysis_unknown_pop$Sex == "NA"))
no_0f_NA_Unknown

# grouped bar chart of comparison between number of cases for male/female/unknown sex based on Ethnicity

Ethnicity <- c("Non-Hispanic/ Latino","Non-Hispanic/ Latino","Non-Hispanic/ Latino","Hispanic/ Latino","Hispanic/ Latino","Hispanic/ Latino","Unknown","Unknown","Unknown")
Number_Of_Cases <- c(no_0f_NonHispanic_Male, no_0f_NonHispanic_Female,no_0f_NonHispanic_Unknown,no_0f_Hispanic_Male,no_0f_Hispanic_Female,no_0f_Hispanic_Unknown, no_0f_NA_Male, no_0f_NA_Female,no_0f_NA_Unknown)
Indicator <- c("Male","Female","Unknown","Male","Female","Unknown","Male","Female","Unknown")

dfEthnicityGroupedBar <- cbind.data.frame(Ethnicity,Number_Of_Cases,Indicator)
dfEthnicityGroupedBar

pGroupedBarEthnicity<-ggplot(data=dfEthnicityGroupedBar, aes(x=Ethnicity, y=Number_Of_Cases,fill=Indicator)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=Number_Of_Cases), vjust=1.6, color="white", size=3.5, position = position_dodge(0.9),)+
  theme_minimal()+
  labs(title="Figure 5: Plot of number of cases for different sex based on ethnicity")
pGroupedBarEthnicity

##### End of Ethnicity Analysis

#### Country Analysis 
# Unique values of Race 
unique(df1[c("Country")])

# Attempt to analyse each ethnic group number of cases based on gender. 

country_analysis <- df1 %>% group_by(Country)
country_analysis

library("plyr")

country_Freq <- count(country_analysis, 'Country')
names(country_Freq)
typeof(country_Freq)
country_Freq <- as.data.frame(country_Freq)
colnames(country_Freq)[2] <- "Number_of_Cases"
str(country_Freq)

# Writing Frequency table of number of cases in each countries. 
library("writexl")
write_xlsx(country_Freq,"/Users/abhi/Desktop/Country_table.xlsx")

# Plotting top 5 countries and their number of cases 
country_Freq_Top_5 <- country_Freq %>% group_by(Number_of_Cases)
country_Freq_Top_5 <- head(country_Freq_Top_5 %>% arrange(desc(Number_of_Cases)))
country_Freq_Top_5

p<-ggplot(data=country_Freq_Top_5, aes(x=Country, y=Number_of_Cases)) +
  geom_bar(stat="identity", fill="orange")+ 
  theme_minimal()+
  geom_text(aes(label=Number_of_Cases), vjust=1.6, color="black", size=3.5)+
  labs(title="Figure 6: Plot of top 5 number of cases by Country")+
  coord_flip()
p

#### What proportion of deaths are among Black or Hispanic population? 
cases_of_Black_Or_Hispanic <- filter(df1, df1$Ethnicity == 'Hispanic/ Latino' | df1$Race == 'African-American/ Black' )
cases_of_Black_Or_Hispanic
nrow(cases_of_Black_Or_Hispanic) # Total number of cases 8428

# total number of cases in the dataset 
nrow(df1) #21709

proportion_of_Black_Or_Hispaic <- nrow(cases_of_Black_Or_Hispanic)/nrow(df1)
proportion_of_Black_Or_Hispaic # 0.3882261 = 38.82% 

#### Descriptive summary table 
install.packages("psych")
library(psych)

df1 <- df1 %>%
  add_column(White_Indicator = ifelse(df1$Race %in% c('White'), 1, 0),
             .after = "Chronic_Condition") 
df1

str(df1)
df1Table <- describe(df1, na.rm = TRUE, skew = TRUE, ranges = TRUE)
df1Table
df1Table <- df1Table %>%
  add_column(columns = c("Age","Ethnicity","Race","Sex","Country","Chronic_Condition","White_Indicator"),
             .before = "n") 
write_xlsx(df1Table,"/Users/abhi/Desktop/OriginalDataSummaryData_table.xlsx")



