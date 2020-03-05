##Name: Hannah Wangui
##Date: 26/02/2020
##Description:

##0. Loading Packages
library(tidyverse)

##1.Read in dataset
library(readr)
wafanyikazi <- read.csv("C:/Users/user/Downloads/wafanyikazi.csv")
   View(wafanyikazi)
 
##Creating a small dataset by picking certain variables
df <- wafanyikazi %>% 
      select(Sid, Age, Gender)


## to drop certain variables
df1 <- wafanyikazi %>% 
      select(-Sid)

##Selecting variables with a certain pattern
#selecting varibles that contains e

df2 <- wafanyikazi %>% 
  select(contains("e"))

#selecting variables that starts with a
df3 <- wafanyikazi %>% 
  select(starts_with("A"))

### mutate(): Used for generating new variables
df4 <- wafanyikazi %>% 
        mutate(prop_leavedays= Leave_Days/365)

###Generating a new variable caaled income_usd
df5<-wafanyikazi %>% 
     mutate(income_usd= Income*20)


###ifelse(a,b,c)
## a what if the condition is true ("Low")
## b what if the condition is false (" High")

df6 <- wafanyikazi %>% 
      mutate(Income_category= if_else(Income <= 500, "Low", "High"))

df7<-wafanyikazi %>% 
     mutate(age_category = if_else(Age<=40,"young","old"))


##finding the minimum and the maximum age 
min(wafanyikazi$Age)

max(wafanyikazi$Age)

##18-25 : Young
##26-35 : Middle
##36-50 : Old
if_else(a,bifesle(a,b,c))

##generating a new variable caale age group using the if else condition
df8 <- wafanyikazi %>% 
      mutate( age_group = if_else(Age >=18 & Age <=25,"Young",
                            if_else(Age >=26 & Age <=35,"Middle", "Old")))

###generating a new variable caale income group using the if else condition
df9<-wafanyikazi %>% 
     mutate(income_group = if_else(Income>=1000 & Income<=4000,"low",
                                   if_else(Income>=4001 & Income<=6000,"medium","high")))

###finding the minimum and the maximum leave days
min(wafanyikazi$Leave_Days)

max(wafanyikazi$Leave_Days)

####generating a new variable using the if condition
df10<-wafanyikazi %>% 
      mutate(Leave_Days_group = if_else(Leave_Days>=5 & Leave_Days<=0,"sickoff",
                                        if_else(Leave_Days>=6 & Leave_Days <=10, "compassionate",
                                                if_else(Leave_Days>=11 & Leave_Days <=15, "dayoff",
                                                        if_else(Leave_Days>= 16 & Leave_Days <=20, "utilized", "maternity")))))
###finding the minimum and the maximum income
min(wafanyikazi$Income)

max(wafanyikazi$Income)


####generating a new variable called jinsia
df11 <- wafanyikazi %>% 
  mutate(Jinsia= if_else(Gender=="Male", "mume", "mke"))

## if you want to denote equality use == nonequality !=
## & and | or

###to know the exact format of a variable
 unique(as.character(wafanyikazi$Gender))
 
 ###to filter female variables
 Female_df <- wafanyikazi %>% 
              filter(Gender== "Female")
 
 ##to filter age less than or equal to 50
yearsdf<- wafanyikazi %>% 
         filter(Age<=50)

###to filter or keep females 50 years and below
BeloFem <- wafanyikazi %>% 
  filter(Gender=="Female"& Age<= 50)

###to keep females or below 50 
BeloFem2 <- wafanyikazi %>% 
  filter(Gender=="Female" | Age<= 50)

###to know the exact format of a variable
unique(as.character(wafanyikazi$Department))
unique(as.character(wafanyikazi$Role))


##to keep employees in the finance department with senior roles eraning 5000 and above
Emp <- wafanyikazi %>% 
  filter(Department=="Finance" & Role=="Senior" & Income >= 5000)


#grep::looks for a string variable in order to condense it into one
xM <- "i love eve"
grepl("love", xM ,ignore.case = TRUE)

xM <- "i love eve"
grep("love", xM ,ignore.case = TRUE, value = T)


##looking for counties that starts with N where for evey county starting with n is true and not is false
counties <-c("mombasa", "kakamega", "Kiambu", "nairobi", "nyeri", "muranga")
grep("^n", counties ,ignore.case = TRUE, value = TRUE)

##looking for counties that ends with a where for evey county ending with a is true and not is false
counties <-c("mombasa", "kakamega", "Kiambu", "nairobi", "nyeri", "muranga")
grep("a$", counties ,ignore.case = TRUE, value = TRUE)


###generating a dataset called Ncounties if the county starts with n is okay otherwise not okay
df20 <- wafanyikazi %>%
  mutate(Ncounties = ifelse(County %in% grep("^n", County, ignore.case= TRUE, value = TRUE), "okay", "Not okay"))
 
###generating a dataset called Ncounties if the county ends with n is okay otherwise not okay 
df21 <- wafanyikazi %>%
  mutate(Ncounties = ifelse(County %in% grep("a$", County, ignore.case= TRUE, value = TRUE), "okay", "Not okay"))


df23 <-wafanyikazi %>% filter(Department %in% grep("analyst", Department, ignore.case = T,value = T))
df23

df25<-wafanyikazi %>% filter(Department =="Research Analyst")
df25
##arranging in ascending order
df26 <- wafanyikazi %>% arrange(Age)


##generating a single varible
df24<-wafanyikazi %>% 
  transmute(income_usd= Income*20)

##arranging in ascending order
df26 <- wafanyikazi %>% arrange(Age)

####arranging in ascending order
df26 <- wafanyikazi %>% arrange (desc(Age))

##arranging county in ascending order and age in descending order

df27<- wafanyikazi %>% arrange(desc(Age),County)

df28<-wafanyikazi %>% arrange(County,desc(Age))
## group by() summarize()