##Assignment Name:LA-8: Examining Relationships
##Name:Abby Marsing
##Date:25 November 2022

##Installing the necessary packages
install.packages("tidyverse")
install.packages("summarytools")
install.packages("rstatix")

##Setting the Current Working Directory to the Directory where the data is residing.
setwd("C:/R DATA SETS")

##Step-1: Download and save the Utilities dataset into your working directory.
## Load the following packages in your current session of R:
## tidyverse
## summarytools
## rstatix

## Importing tidyverse ,summarytools and rstatix
library("tidyverse")
library("summarytools")
library("rstatix")

##Let us now read the utilities data set
utilities<-read.csv("utilities.csv")
## This Function is used to access column names directly without giving the dataframe name.
attach(utilities)

## Step-2: Create a new categorical variable called season.
## Have the variable equal winter if the bill was from December,January,or February.
## Have the variable equal spring if the bill was from March,April,or May.
## Have the variable equal summer if the bill was from June,July,or August.
## Have the variable equal fall if the bill was from september,octomber,or November.
## Then. create a frequency table of season.

##Creating a variable called season by taking the above rules into conseideration.
utilities$season <- utilities %>% mutate(season =ifelse(month %in% 3:5, "Spring",
                                                        ifelse(month %in% 6:8,"Summer",
                                                              ifelse(month %in% 9:11,"Fall",
                                                                "Winter"))))->new_utilities

#create new_utilities with season variable.
attach(new_utilities)#new_utilities data set attach                                                                                                                                                    
##create new season variable with entire data set in new_utilities. 


#2.a)Run a frequency distribution of season.
##Frequency table for season
freq(season)
tab=table(season)
tab
#graphical representation of the frequency table(frequency distribution).
barplot(tab)


##STEP 03
#Let's say we want to conduct a statistical test to determine whether respondents' total monthly bill differs by season. First, answer the questions below. Be sure to include your answers as comments in your R file.
#Which variables are involved in this statistical test? Which is the independent variable? Which is the dependent variable?
#3.a)
#In this statistical test,dependent and independent variables are involved.
#independent variable is the season.
#dependent variable is the total bill.


#3.b)
#Run a frequency distribution of total bill. Is this variable categorical or continuous?
freq(totalbill)
g=table(totalbill)
g
#graphical representation of the frequency table(frequency distribution).
hist(totalbill,xlab ="totalbill",ylab = "Frequency",main ="total 
bill Distribution",col="blue")
#Total bill variable is the continuous.


#3.c)
#Is the variable, season, continuous or categorical?
#season variable is the categorical.

#3.d)
#Check the chart in the Hint tab. Given your answers to the previous question, what statistical test should you use?
#Dependent variable is continuous and independent variable is categorical.
#The season variable has the more than two groups.
#The ANOVA test allows a comparison of more than two groups at the same time to determine whether a relationship exists between them.
#ANOVA should be used for statistical testing since the term variable has more than 2 groups.

#STEP-04
#Conduct the statistical test you selected in your answer to 3c and answer the following questions.
new_utilities %>% 
  anova_test(., totalbill ~ season)

#4.a)What is the test statistic and the probability value associated with that test statistic?
#a)F-value is test statistic=70.838
#a)p-value is =7.58e-26

#4.b)Based on the test statistic and the p-value, do respondents' total monthly bills differ by season?
#p-value is less than 0.05(P<0.05)then the relationship is significant.
#At least one total bill value also differ with season.


#Step 5
#Next, you will form a hypothesis about the difference in people's winter vs. 
#summer gas bills and conduct the appropriate statistical test to address your hypothesis.

#5.a)State your hypothesis. Do you think monthly gas bills will be different in the summer relative to winter? If so, why?
#H0:mean gas bill in summer-mean gas bill in winter=0
#H1: AT least one gas bill is different.
#yes monthly gas bill will be different summer relative to winter.
#Because energy consumption is  higher in winter than it is in summer.
#so  may be paying higher gas prices as  use more energy in the winter.

#5.b)Identify the dependent and independent variables that you will use to conduct this statistical test.
#Dependent variable is gas bill
#Independent variable are summer and winter.



#5.c)Determine whether your independent and dependent variables are categorical or continuous. State these clearly in your R file.
#categorical variable are summer and winter.
#continuous variable is gas bill.


#5.d)Select a statistical test; clearly state your answer in your R file.
#statistical test is t-test because dependent continuous and independent categorical variable.
#A t test is a statistical test that is used to compare the means of two groups. 
#that two groups are summer and winter.

#5.e)
#Write pseudocode that describes, in your own words, the process by which you will conduct your statistical test. 
#if(P-value<0.05){
# print("reject null hypothesis,model is significant"){
# else{
#   print("not reject null hypothesis,model is not significant")
# }   
#  }
#}
#if reject null hypothesis,there is a difference between summer gas bill and winter gas bill.    



#5.f)Write R code to conduct the statistical test and report the test statistic and associated p-value.
t_test(gasbill~season,data=new_utilities)
#test statistick is -19.9
#p-value is 5.16e-19

#5.g)Calculate the mean monthly gas bill in winter and summer.
summarise(group_by(new_utilities,season),mean(gasbill))


#5.h)Does the test support your hypothesis?
#p-value less than 0.05(p-value<0.05)
#p-value is 5.16e-19 (0.0000000000000000000516)
# so reject null hypothesis reject H0
# Support or reject null hypothesis? If the P-value is less, reject the null hypothesis.
#TEST SUPPORT HYPOTHESIS BECAUSE REJECT H0.

#Bonus Question
#Form a hypothesis about the difference in people's winter vs. summer electric bills and conduct the appropriate statistical test to address your hypothesis.
#a)State your hypothesis. Do you think monthly electric bills will be different in the summer relative to winter? If so, why?
#H0: mean elec bill in summer-mean elec bill in winter=0
#H1: AT least one elec bill is different.

#b)Write pseudocode that describes, in your own words, the process by which you will conduct your statistical test.
#if(P-value<0.05){
# print("reject null hypothesis,model is significant"){
# else{
#   print("not reject null hypothesis,model is not significant")
# }   
#  }
#}
#if reject null hypothesis,there is a difference between summer electricity bill and winter electricity. 


#c)Write R code to conduct the statistical test and report the test statistic and associated p-value.
t_test(elecbill~season,data=new_utilities)
#test statistic is (t)=0.745
#p-value is 0.46


#d)Does the test support your hypothesis?
#p-value greater than 0.05(p-value>0.05)
#p-value is 0.46
# so not- reject null hypothesis ,not-reject H0
# Support or reject null hypothesis? If the P-value is less, reject the null hypothesis.
#TEST NOT SUPPORT HYPOTHESIS BECAUSE NOT REJECT H0.



