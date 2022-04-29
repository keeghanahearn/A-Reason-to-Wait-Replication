#==============================================================================
#  A Reason to Waiit Replication: Comparison Graph and Main Resuls
#==============================================================================
# Keeghan Ahearn
# First Version 4/29/2022.

#==============================================================================
#   1. Settings, packages, and options
#==============================================================================

# Clear the working space
rm(list = ls())

# Set working directory 

setwd("C:/Users/kahea/Desktop/Desktop Files/Homework/2022/Winter/A Reason to Wait Replication/")


library(stargazer)
library(sandwich)
library(car)
library(plyr)
library(data.table)
library(bit64)
library(doBy)
library(AER)
library(lmtest)
library(ggplot2)
library(gdata)
library(plm)
library(ivpack)
library(tidyverse)

# turn off scientific notation except for big numbers
options(scipen = 9)

# function to calculate robust standard errors for regression 
cse <- function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}



#==============================================================================
#   2. Data section
#==============================================================================

#ACS_master <- fread('A Reason to Wait.csv', header=T, sep=',')
#save(ACS_master, file = "ACS_master.RData")

load("ACS_master.RData")

#create subsample for female hispanic teens
General <- subset(ACS_master, YEAR>2004 & YEAR<2016 & AGE>14 & AGE<21 & SEX==2 & HISPAN==1 & CNTRY==840 & YRIMMIG<2008)

#non-Citizen 
non_Citizen <- subset(ACS_master, YEAR>2004 & YEAR<2018 & AGE>14 & AGE<21 & HISPAN==1 & CITIZEN==3) 

#Citizen
Citizen <- subset(ACS_master, YEAR>2004 & YEAR<2018 & AGE>14 & AGE<21 & HISPAN==1 & CITIZEN==2)


##Might have to recode the fertitlity stuff
#any children
General$CHILDREN <- car::recode(General$NCHILD, "1:9=1; else=0")
#number of children in the household

#children born within last year
General$NEWCHILD <- car::recode(General$FERTYR, "1=0; 2=1")


#non-citizen dummy
#no citzenship*in highschool/rececent graduate
General$NONCITIZEN <- car::recode(General$CITIZEN, "3=1; else=0")


#post treatment 2012 onwards
General$POSTTREAT <- car::recode(General$YEAR, "2012:2017=1; else=0")

#School
General$SCHOOL <- car::recode(General$SCHOOL, "2=1; 1=0")
General$EDUCD <- car::recode(General$EDUCD, "62:116=1; else=0")
General$SCHOOLGED <- car::recode(General$SCHOOL|General$EDUCD, '1=1; 0=0')


##indicator for elgibility status
#basically just pull from general sample; plus noncitizens and school
General$ELIG <- car::recode(General$NONCITIZEN & General$SCHOOLGED, '1=1; 0=0')


#indicator for having any number of children for citizen or non citizen
non_Citizen$CHILDREN <- car::recode(non_Citizen$NCHILD, "1:9=1; else=0")
Citizen$CHILDREN <- car::recode(Citizen$NCHILD, '1:9=1; else=0')

#prepping for graph
##percentage of non citizens that had children
non_Citizen <- non_Citizen %>% 
  select(YEAR,CHILDREN) %>% 
  group_by(YEAR, CHILDREN==0) %>% 
  summarise(total=n()) %>% 
  mutate(percent=total/sum(total))

non_Citizen$`CHILDREN == 0` <- as.integer(non_Citizen$`CHILDREN == 0`)

non_Citizen <-  select(non_Citizen,c('YEAR','CHILDREN == 0','percent'))
names(non_Citizen)[2] <- 'CHILDREN'
non_Citizen <- subset(non_Citizen, CHILDREN == 1)
  

##percentage of citizens that had children
Citizen <- Citizen %>% 
  select(YEAR,CHILDREN) %>% 
  group_by(YEAR, CHILDREN==0) %>% 
  summarise(total=n()) %>% 
  mutate(percent=total/sum(total))
                        
Citizen$`CHILDREN == 0` <- as.integer(Citizen$`CHILDREN == 0`)

Citizen <-  select(Citizen,c('YEAR','CHILDREN == 0','percent'))
names(Citizen)[2] <- 'CHILDREN'
Citizen <- subset(Citizen, CHILDREN == 1)


#Analysis

#plot of citizens vs non-citizens
ggplot()+
  geom_line(data=non_Citizen, mapping=aes(x=YEAR,y=percent), color="blue")+
  geom_point(data=non_Citizen, mapping=aes(x=YEAR,y=percent), color="blue")+
  geom_line(data=Citizen, mapping=aes(x=YEAR,y=percent), color="red")+
  geom_point(data=Citizen, mapping=aes(x=YEAR,y=percent), color='red')+
  labs(x="Year",y="Share of 15-20 Year Olds With No Children",title="Likelihood of Having No Children Between Citizens and Noncitizens")+
  scale_x_continuous(breaks = c(2005, 2007, 2009, 2011, 2013, 2015, 2017))


# This is the basic D-in-D model (no controls)
reg0=lm(CHILDREN~ELIG+POSTTREAT+I(ELIG*POSTTREAT), data=General) #Impact on Any Children
reg1=lm(NEWCHILD~ELIG+POSTTREAT+I(ELIG*POSTTREAT), data=General) #Impact on Birth in the Last Year
reg2=lm(NCHILD~ELIG+POSTTREAT+I(ELIG*POSTTREAT), data=General) #Impact on Number of Children

stargazer(reg0, reg1, reg2,   
          se=list(cse(reg0),cse(reg1),cse(reg2)), 
          title="D-in-D Model with No Controls", type="text", 
          column.labels=c("Any Children", "New Child", "Number of Children"), 
          df=FALSE, digits=4)

#D-in-D model with controls
reg3=lm(CHILDREN~ELIG+POSTTREAT+I(ELIG*POSTTREAT)+AGE+YRIMMIG, data=General)
reg4=lm(NEWCHILD~ELIG+POSTTREAT+I(ELIG*POSTTREAT)+AGE+YRIMMIG, data=General)
reg5=lm(NCHILD~ELIG+POSTTREAT+I(ELIG*POSTTREAT)+AGE+YRIMMIG, data=General)


stargazer(reg3, reg4, reg5,   
          se=list(cse(reg3),cse(reg4),cse(reg5)), 
          title="D-in-D Model with Age and Year of Immigration Controls", type="text", 
          column.labels=c("Any Children", "New Child", "Number of Children"), 
          df=FALSE, digits=4)
