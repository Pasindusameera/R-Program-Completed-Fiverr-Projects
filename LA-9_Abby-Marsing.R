##Assignment Name:LA-9:  Practice Data Analysis
##Name:Abby Marsing
##Date:08 December 2022
  
##Installing the necessary packages
install.packages("tidyverse")
install.packages("summarytools")
install.packages("rstatix")





##Setting the Current Working Directory to the Directory where the data is residing.
setwd("C:/R DATA SETS")

## Importing tidyverse ,summarytools and rstatix
library("tidyverse")
library("summarytools")
library("rstatix")

##Step 1
## Then, read the CPS data set into R.
##Let us now read the CPS data set
  df<-read.csv("CPS.csv")
  head(df)
## This Function is used to access column names directly without giving the dataframe name.
  attach(df)


##Step 2
##Find the mean wage earned per hour for males and females.
   df %>% group_by(sex) %>% summarise(mean(wage))
  
##a)What is the mean wage earned per hour for females?
   ##The average hourly wage for females earned 7.88 USD.
   
##b)What is the mean wage earned per hour for males?
   ##The average hourly wage for males earned 9.99 USD.

   
   
##Step 3
##Implement the appropriate statistical test to determine whether males have significantly higher wages earned per hour than females.
   df %>% 
     t_test(., wage ~ sex)

##a)State your hypothesis about whether males earn higher wages per hour than females.
   ##The null hypothesis (H0) is that the true difference between these wages means is zero.
  ## The alternate hypothesis (H1) is that males earn higher wages per hour than females
   ##(H1:males wages>females wages)

##b)Which statistical test will you use to address your hypothesis? Conduct the statistical test you have chosen.
   ## t-test
   ## wage = continuous , sex = categorical  with only 2 groups
   df %>% 
     t_test(., wage ~ sex)
   
##c)What is the test statistic and associated p-value?
  ## t value is test statistic
  ## t (test statistic) = -4.89, p < .001
  ## P-value is 0.00000137

##d)Does the test support or refute your hypothesis? Do males or females earn higher wages on average?
  ##yes test is support to hypothesis because reject H0(p-value is less than 0.05)   
  ##Accept alternative Hypothesis it means males earn higher wages average. 
   

##Step 4
##Implement the appropriate statistical test to determine whether there is a significant linear relationship between wages earned per hour and number of years of work experience.
   df %>% 
     cor_test(., wage, exper, method = "pearson") 
   
##a)State your hypothesis about the relationship between wages earned per hour and number of years of experience.
  ##The null hypothesis states that there is no correlation(relationship) between the variables.
  ##The alternative hypothesis states there is relationship have between the wages and experince.
   
##b)Which statistical test will you use to address your hypothesis? Conduct the statistical test you have chosen.
   ## Pearson's correlation
   ## wages earn per hour = continuous , years of experince = continuous 
   df %>% 
     cor_test(., wage,exper, method = "pearson") 
   # Pearson's r = 0.087, p < .001
   
##c)What is the test statistic and associated p-value?
   ##	Pearson's correlation coefficient	r=0.087
   ## P-value is 0.0443 (p<0.05)
   
##d)Does the test support or refute your hypothesis? What is the relationship, if any, between wages earned per hour and number of years of experience?  
  ##yes, test  support hypothesis because reject H0 and accept H1
   ## r value is 0.087 ,this is having very weak positive relationship
  ## IF Correlation Coefficient = 0: No relationship.But (r=0.087)
   
     
##Step 5 
##Implement the appropriate statistical test to determine whether job satisfaction varies significantly among the different job sectors.
   tbl <- table(df$satisfaction,df$sector)
   tbl  %>% 
     chisq_test(., satisfaction ~ sector) 
   
##a)State your hypothesis about whether job satisfaction varies significantly among job sectors.
  # Null:  job satisfaction and  job sectors are independent.
  # Alternate:  job satisfaction and  job sectors are not independent.
   
   
##b)Which statistical test will you use to address your hypothesis? Conduct the statistical test you have chosen. 
   # Chi-squared test
   # job satisfaction = categorical dependent variable (DV), job sector = categorical independent variable (IV).
   tbl <- table(df$satisfaction,df$sector)
   tbl  %>% 
     chisq_test(., satisfaction ~ sector) 
   # Chi-squared value (test statistic) = 3.65, p = .819 
   
   
##c)What is the test statistic and associated p-value?
   ## Chi-squared value (test statistic) = 3.65
   ## p = .819
    
   
##d)Does the test support or refute your hypothesis? Does job satisfaction vary among different job sectors?  
   ## Test not support to hypothesis test,P-value>0.05
   ## not reject H0,accept Null hypothesis.
   ## job satisfaction and  job sectors are independent.
   
   
##Bonus Question   
##Implement the appropriate statistical test to determine whether wages vary by job sector.
   df %>% 
    anova_test(., wage ~ sector) 
   
##a)State your hypothesis about whether wages and job sector are related.
   ##hypothesis (H0) of ANOVA is that there is no difference among wages mean and job sector.
    
   
##b)Which statistical test will you use to address your hypothesis? Conduct the statistical test you have chosen.
   ## ANOVA
   ## Wages = continuous DV, job sector = categorical (more than 2 groups)
   df %>% 
     anova_test(., wage ~ sector) 
   # F (test statistic) = 16.796, p < .001
    
   
##c)What is the test statistic and associated p-value?
   ## F (test statistic) = 16.796, p-value=4.64e-20 
   
   
##d)Does the test support or refute your hypothesis? Do wages vary among different job sectors?
  ##yes,test support hypothesis because p < 0.001
   ##Reject null hypothesis and accept alternative hypothesis.
   ##There is difference among wages mean and job sector.
   
        