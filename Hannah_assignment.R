## Name: Hannah Wangui
## Assignment: 1
## Date: 29th February, 2020

## You are provided with the Financial Inclusion in Africa dataset, 
## which is hosted on the Zindi platform. https://zindi.africa/competitions/financial-inclusion-in-africa.
## Use the dataset to answer the questions below.

## NB: Please make your work neat by including spaces in the code e.g
## my_df <- df %>% 
##          filter(Age == 35)


##1. At the beginning of the training, we learnt how to tell a story from data.
## List down 5 or more questions that we can ask from the dataset.

##a.The number of people in a certain country eg Kenya who have a bank account.
##b.Where are majority of the people with a bank account in a specific counrty located.
##c.The number of people with bank accounts who have cellphone access and those who do not have cellphone access.
##d.Where are the majority of people with bank accounts and have cellphone acces located.
##e.Identify the household size of individuals with bank accounts,cellphone acces and located in urban areas.


##2. Generate a dataset that only contains observations from Kenya. We will 
## use this dataset to answer subsequent questions. Call the dataset Kenya_df.

###0.loading packages
library(tidyverse)

###1.reading in the data
my_df <- read.csv("C:/Users/user/Downloads/Train_v2.csv")
View(Train_v2)
Kenya_df <- my_df %>%filter(country == "Kenya")


##3. Drop the "unique_id" and "year" variables from the dataset.
Kenya_df1 <- Kenya_df %>%
          select(- uniqueid , - year)

##4. Generate a variable called "household_cat" that re-categorizes the "household
## size" variable into 5 groups. Group 1 (1-5), Group 2 (6-10), Group 3 (11-15),
## Group 4 (>15).

Kenya_df2 <- Kenya_df %>%
          mutate(household_cat = if_else(household_size >= 1 & household_size <= 5 ,"Group 1",
                                         if_else(household_size >= 6 & household_size <= 10 ,"Group 2",
                                                 if_else(household_size >= 11 & household_size <= 15 ,"Group 3","Group 4" ))))
                                                                             

##5. Generate a variable called "age_cat" that categorizes the "age" variable into three buckets.
## Use limits of your own choice.

##5.1finding the minimum and the maximum age
min(Train_v2$age_of_respondent)

max(Train_v2$age_of_respondent)

##5.2Generating a new variable  called age_cat that meets the below conditions
##16-45 : Young
##46-70 : Middle
##71-100 : Old

Kenya_df3 <- Kenya_df %>% 
  mutate( age_cat = if_else(age_of_respondent >=16 & age_of_respondent <=45,"Young",
                              if_else(age_of_respondent >=46 & age_of_respondent <=70,"Middle", "Old")))


##6. Generate a variable called "relationship_with_head2", that combines the 
## "other relative" and "Other non-relatives" options of the "relationship_with_head"
## variable into a single option called "other".

###6.1to know the exact format of a variables
unique(as.character(Kenya_df$relationship_with_head))

###6.2Generatingg new variable
relationship_with_head2 <- Kenya_df %>%
                           filter(relationship_with_head == "Other relative"  & relationship_with_head == "Other non-relative" ) 
relationship_with_head2 == "other"


###to know the exact format of a variables
unique(as.character(Kenya_df$job_type))
unique(as.character(Kenya_df$gender_of_respondent))
unique(as.character(Kenya_df$marital_status))
unique(as.character(Kenya_df$cellphone_access))
unique(as.character(Kenya_df$bank_account))

##7. Generate a dataset that contains self employed ladies who have never been married,
## they have no cellphone access, but they have bank accounts. Call this dataset "empowered".
empowered<-Kenya_df %>%
  filter(job_type =="Self employed" & gender_of_respondent == "Female" & marital_status == "Single/Never Married" & cellphone_access == "No" & bank_account == "Yes" )
empowered