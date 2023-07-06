

##Installing the necessary packages
install.packages("tidyverse")
install.packages("ggplot")
install.packages("summarytools")
install.packages("dplyr")

##Setting the Current Working Directory to the Directory where the data is residing.
setwd("C:/R DATA SETS")

##Step-1: Download and save the Utilities dataset into your working directory.
## Load the following packages in your current session of R:
## tidyverse
## summarytools

## Importing tidyverse and summarytools
library("tidyverse")
library("summarytools")
library("dplyr")

##Let us now read the utilities data set
utilities<-read.csv("utilities.csv")
## This Function is used to access column names directly without giving the dataframe name.
attach(utilities)

## Step-2: Following chapter Comm 3710 on ggplot which will be easy for me solve the next questions.

## step-3:Make a histogram to display the distribution of customers' total monthly bill.

## a. Use the histogram you created and describe the distribution. Is the distribution skewed or symmetric? If skewed, is the distribution skewed positively or negatively?
## b. At what cost do most of the total monthly bills fall? Provide a range between which the total monthly bills fall

##3a.
utilities %>%
  ggplot(aes(x=totalbill)) +
  geom_histogram()

## WE can see that histogram is positively skewed(right skewed) and this is because the total bill may have

##3b.
## As per the histrogram we can see that most of the total bills are falling in the range of 80-100 dollars


##Step-4:Make a scatter plot to display gas bill by month.

## a.Based on your graph, which month do you think has the highest average gas bill?
## b.UseR to calculate the average gas bill for the months of January and December. Which is higher?

##4a.
utilities %>%
  ggplot(aes(x=month, y= gasbill)) +
  geom_point()
## 4a.According to the graph I think December is the highest mean because its data is higher and closer than the others.
## Filering the data for both january and december which will be easy for me to calculate the average
jan_data<-filter(utilities,month==1)
dec_data<-filter(utilities,month==12)

##Having a look at the summary statistcs for both December and december
avggasbill_jan<-summary(jan_data$gasbill)
print(avggasbill_jan)
avggasbill_dec<-summary(dec_data$gasbill)
print(avggasbill_dec)

##Observations:
##From the below output we can see that the averall Gasbill is high for the month of january
##Average Gas bill of January-180.38
##Average Gas bill of December-179.3
##The january mean is higher than the december mean of gas bills.


##Step-5:Make a scatter plot to display electric bill by month.
## a.Based on your graph, which month do you think has the highest average electric bill?
## b.Calculate the average electric bill for the months of September and December. Which is higher?

##5a.Plotting a scatterplot to see elctricity bill by month.
utilities %>%
  ggplot(aes(x=month, y= elecbill)) +
  geom_point()

##5a.According to the graph,I think September is the month with the highest mean electricn bill value because the data is very close and has ahigh value range.

##Filtering the data for both september and december which will be easy for me to calculate the average
sept_data<-filter(utilities,month==9)
dec_data<-filter(utilities,month==12)

##Having a look at the summary statistcs for both september and december
avgelecbill_sept<-summary(sept_data$elecbill)
print(avgelecbill_sept)
avgelecbill_dec<-summary(dec_data$elecbill)
print(avgelecbill_dec)

##Observations:
##From the below output we can see that the average electric bill high for the month of september at
##Average Electric Bill of September - 99.16
##Average Electric Bill of December - 87.21
##The September mean is higher than the december mean of electric bills.

##Step-6. Make a scatter plot to display the relationship between gas usage and gas bill. Place gas usage on the x-axis and monthly gas bill on the y-axis.
#a.Does there appear to be a relationship between gas usage and gas bill? If so, describe the relationship (e.g., does a relationship exist? Is it linear? Is there a positive or negative relationship between the variables?).
##If so,describe the relationship(e.g.,does a relationship exist?Is it linear?Is there a positive or negative relationship)

utilities %>%
  ggplot(aes(x=ccf, y= gasbill)) +
  geom_point()
##From the scatterplot we can that there is linear relationship between ccf(Gas usage)and Gas bill
##As the gas usage is increasing the gas bill is also increasing
##Gas usage is directly propotional to gas bill.

##Step 7
##Make a scatter plot to display the relationship between electricity usage and electric bill.
##a.Does there appear to be a relationship between electricity usage and electric bill? If so, describe the relationship.

utilities %>%
  ggplot(aes(x=kwh, y= elecbill)) +
  geom_point()

## Observation:
## From the scatterplot we can that there is linear relationship between kwh(electric usage)and electric bill
## We can also see that there is a positive correlation between electric usage and electric bill.
## As the electric usage is increasing the electric bill is also increasing.
## Electric usage is directly propotional to electric bill.


## Step-8: Create a new categorical variable called season.
## Have the variable equal winter if the bill was from December,January,or February.
## Have the variable equal spring if the bill was from March,April,or May.
## Have the variable equal summer if the bill was from June,July,or August.
## Have the variable equal fall if the bill was from september,octomber,or November.
## Then. create a frequency table of season.
## a.How many bills in the data set are from fall?
## b.How many bills in the data set are from summer?                                                                                                     


##Creating a variable called season by taking the above rules into conseideration.
utilities$season <- utilities %>% mutate(season =ifelse(month %in% 3:5, "Spring",
                                                        ifelse(month %in% 6:8,"Summer",                                                          ifelse(month %in% 9:11,"Fall",
                                                           "Winter"))))->new_utilities

##create new season variable with entire data set in new_utilities. 
##Frequency table for season
table(new_utilities$season)

## a.From the below frequency table we can see that there are 29 bills from Fall.
## b.From the below frequency table we can see that there and 30 bills are from Summer.

#Bonus Question
#Make a bar chart using the geom_col() to display the count or proportion of donors by season.
#First, make a new variable, DonorStatus. Set DonorStatus equal to 1 if the billee donated money to Operation Fuel and 0 otherwise. Check that you have created the DonorStatus variable correctly.
#Then, use ggplot() to create your graph.
#In your own words, describe the graph. Does donation vary by season?

donorstatus=ifelse(new_utilities$donate=="yes",1,0)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
donorstatus
tab=table(new_utilities$season,donorstatus)
tab
new_utilities %>%
  ggplot(aes(x=season, y=donorstatus)) +
  geom_col()

#According to the graph winter is the season with the smallest donations.
#According to the graph highest donations are given during the summer.
#People tend not to donate during winter.
#Donations in fall,Summer and Spring do not show much difference.

