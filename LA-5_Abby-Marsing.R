##Assignment Name:LA-5:  Examining and Managing Data
##Name:Abby Marsing
##Date:2 December 2022



##Installing the necessary packages
install.packages("tidyverse")
install.packages("magrittr")
install.packages("summarytools")
install.packages("rstatix")



##Setting the Current Working Directory to the Directory where the data is residing.
setwd("C:/R DATA SETS")


##Step 1
##Load the packages below into R. Then, read the HELP data set into R. This data set is saved as a .csv file.
##Packages to load:
##tidyverse
##magrittr
##summarytools
##rstatix
## Importing tidyverse,magrittr,summarytools and rstatix
library("tidyverse")
library("magrittr")
library("summarytools")
library("rstatix")



##Let us now read the hdata data set
##which has been read into a data frame called hdata.
hdata<-read.csv("HELP.csv")


## This Function is used to access column names directly without giving the dataframe name.
attach(hdata)
head(hdata)




##Step 2
##Examine the frequency distributions of the variables, sex and d1. Use these frequency distributions to answer the questions below.
##Examine the frequency distribution of the sex variable
hdata %>% 
  freq(sex) ##Examine freq dist of variable,sex



##Examine the frequency distribution of the d1 variable
hdata %>% 
  freq(d1) ##Examine freq dist of variable,d1



##a)How many patients in the study are female?
   ##The number of patient women involved in the study is 107.


##b)How many patients in the study have never been hospitalized for medical problems?
    ##Among the patients in the study,92 patients were never hospititalized for medical problems.


##c)What percentage of patients in the study have been hospitalized fewer than 5 times (i.e., 4 or fewer)?
##Hospitalization less than five times means only one to four times.(Do not count those who did not go to the hospital even once.)
##The percentage of patients who have been hospitalized less than 5 times is 62.91%
##Find the 1to 4 (120+92+36+37==285)
##The total number of patients 453.
##Percentage of patients who have been hospitalized less than 5 times=(285/453)**100
## %valid cum.->83.22-20.31==62.91%


##Step 3
##Now, subset the data to only include patients whose primary substance of abuse is cocaine and who are at least 40 years old. Then, examine the frequency distribution of age.

hdata %>% 
  filter(substance == "cocaine" & age >= 40)->df ##create new data frame df

##Then, examine the frequency distribution of age.
freq(df$age)

##a)How many patients are at least 40 years old whose primary substance of abuse is cocaine?
   ##27 patients.



##Step 4

##Examine a frequency table of sex based on this subset. Answer the questions below.
table(df$sex)
freq_table(df$sex)

##a)How many patients in the study are at least 40 years old and have cocaine listed as his/her primary abuse substance are female?
   ## 7 female patients.

##b)What percentage of patients who are at least 40 years old and have cocaine listed as his/her primary abuse substance are male?
   ## 20 male patients.




##Step 5
##Often, we want to look at descriptive statistics of a quantitative variable. 
##We can do this using the descr() function that is in the package, summarytools. 
##Let's examine descriptive statistics for the variable, e2b, which is the number of times in the past 6 months that the respondent entered a detox program.
##Examine the descriptive statistics for e2b and answer the questions below.
hdata %>% 
  descr(e2b) ## examine descriptive stats of variable,e2b

##a)What is the mean?
    ##mean is 2.50
##b)What is the median?
    ##median is 2.00
##c)What is the maximum?
    ##21.00


##Step 6
##Create a new variable called Mdays that is the mean of daysanysub and dayslink.
##Then, examine the descriptive statistics of the new variable, Mdays
hdata %>% 
  freq(daysanysub) # examine freq dist of daysanysub

hdata %>% 
  freq(dayslink) # examine freq dist of dayslink

hdata <- hdata %>% # including the <- saves the new variable back into the data frame
  mutate(Mdays = (daysanysub + dayslink)/2) # Mdays the two variables(mean =sum/2)

# Check that the new variable is correctly created
hdata %>% 
  freq(Mdays) # examine freq distribution of new variable, Mdays

#examine the descriptive statistics of the new variable "Mdays"
hdata %>% descr(Mdays)

## a)What is the mean of the variable, Mdays?
    ##159.26

## b)What is the minimum value of Mdays?
    ##7.00

## c)What is the maximum value of Mdays?
   ##358.50



##Step 7
#examine the frequency distribution of variable "cesd"
hdata %>% freq(cesd)

#categorize respondents who have 12 or lower depression scores into low(<=30) category and the rest into the high(>30) category.
hdata <- hdata %>% mutate(dcesd = case_when(cesd <= 30 ~ "low", cesd > 30 ~ "high"))
hdata

# Check that the new variable "dcesd" has been recoded correctly
hdata %>% freq(dcesd)

##Then, answer the following questions.

##a)How many respondents are in the low category of the new variable, dcesd?
    ##182 category

##b)How many respondents are in the high category of the new variable, dcesd?
    ##271 category


#bonus question

#let's subset the respondents in the low category and name it as subset2
subset2 <- hdata %>% filter(dcesd == "low")
subset2


#to get the mean age among respondents in low category, let's consider the descriptive statistics of the variable "age" in subset2
subset2 %>% descr(age)

#therefore, the mean age among respondents in low category is: 36.03

#let's subset the respondents who are in the high category and name it as subset3
subset3 <- hdata %>% filter(dcesd == "high")
subset3

#to get the mean age among respondents in the high category, let's consider the descriptive statistics of the variable "age" in subset3
subset3 %>% descr(age)

#therefore, the mean age among respondents in high category is: 35.40

##Bonus question (Bonus answer)
summary(hdata[hdata$dcesd=="high","age"])
summary(hdata[hdata$dcesd=="low","age"])

