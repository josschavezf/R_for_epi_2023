library(tidyverse)
install.packages("gridExtra")
library(gridExtra)
install.packages("ggpubr")
library(ggpubr)
install.packages("patchwork")
library(patchwork)

###############

clabsi <-read_csv("data/central-line-associated-bloodstream-infections-clabsi-in-california-hospitals.csv")

glimpse(clabsi)

#data here is unclean


#removing unwanted data to get clear datasheet

main_data <- clabsi %>% 
  select(-Facility_Name2, -Facility_Name3, -FACID2, -FACID3)

glimpse(main_data)
view(main_data)



#renaming all variables

main_data <- main_data %>%
  rename(hospital = Facility_Name1,
         id = FACID1,
         county = County,
         ci_upper_limit = X95.Confidence_Interval_Upper_Limit,
         ci_lower_limit = X95.Confidence_Interval_Low_Limit,
         statistical_interpretation = Statistical_Interpretation,
         observed_infections = Observed_infections,
         predicted_infections = Predicted_infections,
         central_line_days = Central_line_days,
         sir = SIR)

view(main_data)


#number of hospital in each county

by_county <- main_data %>% 
  select(hospital, county) %>%
  count(county) %>% 
  arrange(desc(n))
view(by_county)
##maximum of 81 hospitals in LOS ANGELES county f/b 28 in ORANGE county and 18 in SAN DIEGO
## 39 counties have less than 5 hospitals


#maximum observed infections by county

by_infection <- main_data %>% 
  select(hospital, county, observed_infections,central_line_days) %>% 
  arrange(desc(observed_infections))
view(by_infection)

##TOP 3 MOST NUMBER OF INFECTIOUS HOSPITALS IN CALIFORNIA STATE
##Ronald Reagan UCLA Medical Center, Los Angeles--LA --138
##City of Hope Helford Clinical Research Hospital, Duarte--LA -- 121
##UCSF Medical Center, San Francisco-- 97
## 94 Hospitals in among 342 hospitals have zero observed infections
## 275 Hospitals have shown less than 10 observed infections between _____ 
## ONLY John D Klarich Memorial Hospital, CSP-Corcoran in Kings county 
###in the entire state of California has no central lines inserted
##UCSF Medical Center, San Francisco has maximum (70876) days for inserted Central lines
###f/b (70333) Ronald Reagan UCLA Medical Center, Los Angeles and 
#### (61839) Stanford Hospital, Stanford


#maximum predicted infections by county

by_predicted <- main_data %>%
  select(hospital, county, predicted_infections,central_line_days) %>% 
  arrange(desc(predicted_infections))
view(by_predicted)

##Ronald Reagan UCLA Medical Center, Los Angeles--155.08
###f/b UCSF Medical Center, San Francisco--147.37
## 57 hospitals have between zero to 1 predicted infections


#plotting observed infections vs central line days-------------------------------1

# Error: you missed a c() after face = italic
a <- ggplot(by_infection,
       aes(x= observed_infections,
           y= central_line_days,
           color= county))+
  geom_point()+
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = c("italic", "bold")))+ 
  theme(legend.position = "",
        legend.text.align = NULL)+
  labs(title ="Number of Infections Observed vs Number of Central line Days
       in the State of California",
       x= "Observed Infections",
       y= "Days of CVC Line",
       color= "County")
a
##maximum number of observed infections among all counties is between zero and 10


#plotting predicted infections vs central line days-------------------------------2
b <- ggplot(by_predicted,
       aes(x= predicted_infections,
           y= central_line_days,
           color= county))+
  geom_point()+
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = c("italic", "bold")))+
  theme(legend.position = "left",
        legend.text.align = NULL)+
  labs(title ="Number of Infections Predicted vs Number of Central line Days
       in the State of California",
       x= "Predicted Infections",
       y= "Days of CVC Line",
       color= "County")
  
b


ggarrange(a, b, ncol= 1, labels= "AUTO",legend = "none")


##from graph it is evident that most infections are in the range of zero to 50
## however, few counties have more than 60 infections


#Statistical Interpretation of central line infections in all counties among different hospitals----3

by_si <- main_data %>% 
  select(hospital, county, statistical_interpretation) %>%
  group_by(county) %>% 
  count(statistical_interpretation)

by_name <- unique(sort(main_data$county))
by_name

by_order <- c("Too few to calculate", "No difference", "Low", "High")



ggplot(by_si,
       aes(statistical_interpretation, county,
           size= n))+
  geom_point()+
  theme(axis.text.x= element_text(angle = 90),
        plot.title = element_text(hjust = 0.5, face = c("italic", "bold")))+
  scale_y_discrete(limits= rev(by_name))+
  scale_x_discrete(limits= by_order)+
  labs(title = "Statistical Interpretation of Central line infections 
       in each county of California State",
       x = "Statistical Interpretation",
       y= "",
       size= "Number of Hospitals")

##too few to calculate more in LA and Orange county
##no difference highest in LA
##Low also maximum in LA
##High SI in 4 counties


#number of observed infection in Orange County in each hospital-------------------4

orange <- main_data %>% 
  group_by(hospital, county) %>% 
  filter(county == "Orange")


ggplot(orange,
       aes(x= observed_infections,
           y= hospital,
           fill= county))+
  geom_col()+
  scale_x_continuous(limits = c(0,50))+
  labs(title = "Observed Infections in Orange County in each Hospital",
       x = "Number Observed Infections",
       y = "Hospital Name",
       fill = NULL,
       legend = NULL)
##


#density total observed infections in the entire California are too few>high>no diff>low-----5
ggplot(main_data,
       aes(x= observed_infections,
           fill = statistical_interpretation))+
  geom_density(alpha= 0.4)+
  scale_x_continuous(breaks=c(1,2,3,4,5,10,15,20,25,30,35,40,45,50),limits = c(0,50))+
  labs(title = "Density of Observed Infections 
       in the state of California in all 342 Hospitals",
       x = "Number Observed Infections",
       y = "Density",
       fill = "Statistical Interpretation",
       legend = NULL)


#comparing density of infections in 3 counties--------------------------6


by_county_type <- main_data %>% 
  filter(county == "Orange" |county == "San Bernardino"| county == "Riverside")
view(by_county_type)


p1 <- ggplot(by_county_type,
       aes(x= observed_infections, fill = county))+
  geom_density(alpha = 0.4)+
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50),limits = c(0,50))+
  labs(title = "Density of Central line Observed Infections in 3 Counties",
       x= "Number of Observed infections",
       y= "Density",
       fill= "County")+
  theme_linedraw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold.italic"))
          
p1

##comment

p2 <- ggplot(by_county_type,
             aes(x= predicted_infections, fill = county))+
  geom_density(alpha = 0.4)+
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50),limits = c(0,50))+
  labs(title = "Density of Central line Predicted Infections in 3 Counties",
       x= "Number of Predicted Infections",
       y= "Density",
       fill= "County")+
  theme_linedraw() +
  theme(plot.title = element_text(hjust = 1, face = "bold.italic"))
p2


p1/p2
ggarrange(p1, p2, ncol= 1, labels= "AUTO",legend = "right")
##density is reducing but number is increasing in riverside


#plotting northern and southern California counties and comparing their observed infections

southern_cali <- main_data %>%  
  filter(county=="Los Angeles"|county=="San Bernardino"|county=="Orange"|county=="Riverside"|county=="San Diego"|county=="Ventura"
         |county=="Santa Barbara"|county=="San Luis Obispo"|county=="Imperial"|county=="Kern")



northern_cali <- main_data %>% 
  filter(county=="Alameda"|county=="Alameda-Contra Costa"|county=="Amador"|county=="Butte"|county=="Calaveras"|county=="Colusa"
         |county=="Contra Costa"|county=="Del Norte"|county=="El Dorado"|county=="Fresno"|county=="Glenn"|county=="Humboldt"
         |county=="Inyo"|county=="Kings"|county=="Kings-Fresno"|county=="Lake"|county=="Lassen"|county=="Madera"|county=="Marin"
         |county=="Mariposa"|county=="Mendocino"|county=="Merced"|county=="Modoc"
         |county=="Mono"|county=="Monterey"|county=="Napa"|county=="Nevada"|county=="Placer"|county=="Plumas"|county=="Sacramento"
         |county=="San Benito"|county=="San Francisco"|county=="San Joaquin"|county=="San Mateo"
         |county=="Santa Clara"|county=="Santa Cruz"|county=="Shasta"|county=="Siskiyou"|county=="Solano"
         |county=="Sonoma"|county=="Stanislaus-San Joaquin"|county=="Stanislaus"|county=="Sutter"|county=="Sutter-Yuba"
         |county=="Tehama"|county=="Trinity"|county=="Tulare"|county=="Tuolumne"|county=="Yolo")

count(northern_cali,county)   

count(southern_cali,county)

by_name <- unique(sort(northern_cali$county))
view(by_name)


###
plotA <- ggplot(northern_cali,
                aes(x=county,y=observed_infections))+
  geom_col(fill="red") +
  scale_y_continuous(limits= c(0,900),breaks = c(0,50,100,150,
                                                 200,250,300,350,
                                                 400,450,500,550,600,
                                                 650,700,750,800,850,900)) +
  labs(title= "NORTHERN COUNTIES VS OBSERVED INFECTIONS",
       x= "Name of County",
       y= "Number of Observed Infections")+
  theme_minimal()+
  theme(axis.text.x= element_text(angle = 90),
        plot.title = element_text(hjust = 0.5, face = "bold.italic"))
  


plotA
##

plotB <- ggplot(southern_cali,
                aes(x=county,y=observed_infections))+
  geom_col(fill="blue",width = 0.25)+
  scale_y_continuous(limits= c(0,900),breaks = c(0,50,100,150,200,250,
                                                 300,350,400,450,500,
                                                 550,600,650,700,
                                                 750,800,850,900))+
  labs(title= "SOUTHERN COUNTIES VS OBSERVED INFECTIONS",
       x= "Name of County",
       y= "Number of Observed Infections")+
  theme_minimal()+
  theme(axis.text.x= element_text(angle = 90),
        plot.title = element_text(hjust = 0.5, face = "bold.italic"))

plotB


plotA+plotB

ggarrange(plotA, plotB, ncol= 2, labels= "AUTO",legend = "none")
#linear model for finding association between observed infections and central line days

model_one <- lm(observed_infections~central_line_days,data = main_data)
summary(model_one)
coef(model_one)
exp(coef(model_one))
#############################################
#Call:
#lm(formula = observed_infections ~ central_line_days, data = main_data)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-48.888  -2.171   0.921   2.160  69.463 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       -2.189e+00  5.685e-01  -3.851 0.000141 ***
#  central_line_days  1.204e-03  4.277e-05  28.149  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 8.525 on 340 degrees of freedom
#Multiple R-squared:  0.6997,	Adjusted R-squared:  0.6989 
#F-statistic: 792.4 on 1 and 340 DF,  p-value: < 2.2e-16

#> coef(model_one)
#(Intercept) central_line_days 
#-2.189294307       0.001203886 
#> exp(coef(model_one))
#(Intercept) central_line_days 
#0.1119958         1.0012046 
##############################################################



##############THANK YOU##################
