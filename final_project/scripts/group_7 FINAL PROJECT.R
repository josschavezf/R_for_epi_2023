
# FINAL PROJECT GROUP 7 (YASHVEE,KRUPA,BRIANA)

#1- YASHVEE

library(tidyverse)
vanco <- read.csv("data/vancomycin-resistant-enterococci-bloodstream-infections-in-california-hospitals.csv")
vanco
vanco <- read.csv("data/vancomycin-resistant-enterococci-vre-bloodstream-infections-in-california-hospitals.csv")
view(vanco)

###### CLEANING DATA SET
V1 <- vanco[c(1,2,5,8,9,10,11,12,13,14,15)]
V1

view(V1)

V1 <- V1%>%
  rename(Facility_Name = Facility_Name1, ID = FACID1,)
V1

## Visualizing 
V2 <- ggplot(V1, aes(factor(Hospital_Type),Cases))+
  geom_boxplot()
V2
## Statistical Modeling to find association between Hospital type and cases
V2L <- glm(factor(Hospital_Type)~ Cases , data= vanco, family=binomial)
V2L
summary(V2L)
confint(V2L)

## VISUALIZING two variables- Hospital type and Case Mix Index 
V3 <- ggplot(vanco, aes(factor(Hospital_Type),Case_Mix_Index))+
  geom_boxplot()
V3
# Statistical modeling to visualize association between Hospital type and Case Mix Index
V4 <-  glm(factor(Hospital_Type)~ Case_Mix_Index , data= vanco, family=binomial)
V4
summary(V4)
confint(V4)

# 2- CHRISTIAN BRIANA

#library(tidyverse)

vre <- read.csv('vancomycin-resistant-enterococci-vre-bloodstream-infections-in-california-hospitals.csv')
vre <- read.csv("data/vancomycin-resistant-enterococci-vre-bloodstream-infections-in-california-hospitals.csv")
#Number of Cases per County
count_cases<- vre %>%
  group_by(County) %>%
  count(Cases) 

count_cases

ggplot(count_cases,
       aes(x = County,
           y = Cases,
           fill = County)) +
  coord_flip()+
  geom_col(position = position_dodge()) +
  labs(title = "Cases per County",
       x = "County",
       y = "Cases",
       fill = "County") +
  
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45))

#Number of Patient_Stay per County

options(scipen=999) 

count_cases<- vre %>%
  group_by(County) %>%


#3- KRUPA PATEL
  library(readr)
install.packages("readr")
vancomycin <- read_csv("vancomycin.csv")
View(vancomycin)

vancomycin <- read.csv("data/vancomycin-resistant-enterococci-vre-bloodstream-infections-in-california-hospitals.csv")
#####Cleaning data######## 

library(tidyverse)
colnames(vancomycin)
sapply(vancomycin, class)

vancomycin[vancomycin == ""] <- NA
vancomycin



V1 <- vancomycin[c("Hospital_Type" ,"Facility_Name1","FACID1", "County", "Cases",
                   "Patient_Days","Incidence_Rate" ,"X95._Confidence_Interval_Lower_Limit",
                   "X95._Confidence_Interval_Upper_Limit" ,"Compared_To_Pooled_Mean_Rate",
                   "Case_Mix_Index")]
V1
View(V1)

library(ggpubr)
install.packages("ggpubr")

library(magrittr)
install.packages("magrittr")

library(dplyr)
install.packages("dplyr")
V1 <- V1 %>%
  rename("Facility_Name" = "Facility_Name1", "ID" = "FACID1" )


View(V1)


###### Visualizing########

colnames(V1)

library(tidyverse)
install.packages("tidyverse")
#
count_cases <- V1%>%
  group_by(County)%>%
  count(Cases)
count_cases

library(patchwork)
install.packages("patchwork")

ggplot(count_cases,
       aes(x=County, y= Cases))+
  geom_col(position = position_dodge())+
  theme(axis.text = element_text(angle = 90))
#
count_days <- V1%>%
  group_by(Patient_Days,County)%>%
  count(Cases)
count_days

ggplot(count_days, aes(y=Patient_Days, x= County,color = Cases))+
  
  geom_point()+
  theme(axis.text = element_text(angle = 90))+
  scale_color_continuous()+
  labs(title = "Number of patient days per county",
       x = "County",
       y = "Patient Days ",
       fill = factor("Cases"))
#
count_hospital <- V1%>%
  group_by(Hospital_Type)%>%
  count(Cases)
count_hospital 

ggplot(count_hospital, aes(Hospital_Type, Cases))+
  geom_boxplot()

########## statistical modelling ##########

library(modelr)
install.packages("modelr")

#patient days ,county , cases

ggplot(count_days, aes(y=Patient_Days, x= County ,color = Cases))+
  geom_point()+
  coord_polar()+
  theme(axis.text = element_text(angle = 90))+
  scale_color_continuous()+
  labs(title = "Number of patient days per county",
       x = "County",
       y = "Patient Days ",
       fill = factor("Cases"))

fit1 <- lm(Patient_Days~  County,data = V1)
summary(fit1)
coef(fit1)
exp(coef(fit1))

# county and cases

ggplot(count_cases,
       aes(x=County, y= Cases))+
  geom_col(position = position_dodge())+
  theme(axis.text = element_text(angle = 90))

fit2 <- lm(Cases ~  County,data = V1)
summary(fit2)
coef(fit2)
exp(coef(fit2))

#Variable that shows association: patient days and cases

count_Pdays <- V1%>%
  group_by(Patient_Days)%>%
  count(Cases)
count_Pdays

ggplot(count_days, aes(x=Patient_Days, y= Cases ))+
  geom_point()+coord_polar()+
  
  scale_color_continuous()+
  labs(title = "Number of patient days per county",
       y = "Cases",
       x = "Patient Days " )

fit3 <- lm(Patient_Days~ Cases,data = V1)
summary(fit3)
coef(fit3)
exp(coef(fit3))

#SOME VARIABLE SHOWS ASSOCIATION - COM,LTAC,MTAC Hosp type with cases
count_hospital <- V1%>%
  group_by(Hospital_Type)%>%
  count(Cases)
count_hospital 

ggplot(count_hospital, aes(Hospital_Type, Cases, color = n))+
  geom_boxplot()

fit4 <- lm (Cases ~ Hospital_Type, data = V1)
summary(fit4)
coef(fit4)
exp(coef(fit4))
  

  
