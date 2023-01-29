# download data from https://www.kaggle.com/datasets/wenruliu/adult-income-dataset?resource=download

mydata <- read.csv(file.choose(),sep = ',',header = T)
head(mydata)
attach(mydata)
library(tidyverse)
#explore the data
dim(mydata) # 48842 rows and 15 features
glimpse(mydata)

#check for missing variables in data
mydata %>% 
  select(everything()) %>% 
  filter(!complete.cases(.)) %>% 
  count() # count not complete rows --> no missing values are found
original_count <- nrow(mydata)
#data exploration in age and hours per week because some values are unreasonable to be very low or high
summary(mydata) # age 90 is found in age and 99 hours in hours per week
upper_limit_age = 67 # full retirement age in USA where the data was collected

h <- hist(hours.per.week, breaks = 15,col = "red", xlab = "hours per week", main = "Histogram with Normal curve")
xfit <- seq(min(hours.per.week), max(hours.per.week))
yfit <- dnorm(xfit, mean = mean(hours.per.week), sd = sd(hours.per.week))
yfit <- yfit*diff(h$mids[1:2]) * length(hours.per.week)
lines(xfit, yfit, col = "blue", lwd = 3 ) # the hours per week follows a normal dist --> outliers will be removed based on 99.8 CI
x1 <- mean(hours.per.week) + 2 * sd(hours.per.week)
x2 <- mean(hours.per.week) - 2 * sd(hours.per.week)

mydata <- mydata %>% 
  select(everything()) %>% 
  filter(hours.per.week >= x2 & hours.per.week <= x1 & age < upper_limit_age)
new_count <- nrow(mydata)
irrational_num <- original_count - new_count

#check for unique values in qualitative attributes
unique(workclass) # "?" is found in some instances
unique(education) # ordinal variable that needs to be transformed to factor
unique(marital.status)
unique(occupation) # "?" is found in some instances 
unique(relationship)
unique(race)
unique(gender)
unique(native.country) # "?" is found in some instances
unique(income) # ordinal variable that needs to be transformed to factor

#check the number of rows with "?" in values
nrow(mydata) - mydata %>% 
  select(everything()) %>% 
  filter(workclass!= "?" & occupation != "?" & native.country != "?") %>% 
  count() # --> 3620 rows of original count contain values of " ?" will be eliminated

mydata <- mydata %>% 
  select(everything()) %>% 
  filter(workclass != "?" & occupation != "?" & native.country != "?")

  
#re-code attributes values to more general value based on unique values investigation
mydata <- mydata %>% 
  select(everything()) %>% 
  
  mutate(workclass = recode(workclass,
                             "State-gov" = "gov",
                             "Private" = 'private',
                             "Self-emp-not-inc" = "self-emp",
                             "Federal-gov" ="gov",
                             "Local-gov" = "gov",
                             "Self-emp-inc" = "self-emp",
                             "Without-pay" = "volunteer",
                             "Never-worked" = "un-emp")) %>% 
  
  mutate(income = recode(income, "<=50K" = "low",
                         ">50K" = "high")) %>%

  mutate(education = recode(education,
                            "Bachelors" = "bachelors",
                            "HS-grad" = "high-school",
                            "11th" = "compulsory",
                            "Masters" = "masters",
                            "9th" = "compulsory",
                            "Some-college" = "bachelors",
                            "Assoc-acdm" = "associate",
                            "Assoc-voc" = "associate",
                            "7th-8th" = "compulsory",
                            "Doctorate" = "phd",
                            "Prof-school" = "prof-school",
                            "5th-6th" = "compulsory",
                            "10th" = "compulsory",
                            "1st-4th" = "compulsory",
                            "Preschool" = "compulsory",
                            "12th" = "compulsory" )) %>% 
  mutate(marital.status = recode(marital.status,
                                 "Never-married" = "single",
                                 "Married-civ-spouse" = "married",
                                 "Divorced" = "divorced",
                                 "Married-spouse-absent" = "married",
                                 "Separated" = "single",
                                 "Married-AF-spouse" = "married",
                                 "Widowed" = "single"
                                 )) %>% 
  view()

#drop relationship due to its low impact on analysis
mydata <- mydata %>% 
  select(everything(),-relationship)
#change variable types of char to factors
detach(mydata)
attach(mydata)
glimpse(mydata)
mydata$workclass <- factor(workclass)
mydata$education <- factor(education, c("phd","masters","prof-school","bachelors","associate","high-school","compulsory"),ordered = T)
mydata$marital.status <- factor(marital.status)
mydata$occupation <- factor(occupation)
mydata$race <- factor(race)
mydata$gender <- factor(gender)
mydata$native.country <- factor(native.country)
mydata$income <- factor(income,c("low","high"),ordered = T)

#check for duplicates // after re-coding since some classes are combined
as.data.frame(table(duplicated(mydata))) # --> 54 duplicates are found in data

mydata <- mydata %>% 
  select(everything()) %>% 
  distinct()
