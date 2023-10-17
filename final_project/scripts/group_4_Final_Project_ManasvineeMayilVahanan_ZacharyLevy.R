############################################
############ FINAL PROJECT #################
############################################

### BY: Manasvinee Mayil Vahanan & Zachary Levy ###


########## Load Tidyverse and Read CSV file ###########

library(tidyverse)
hospital_staffing_working_hours <- read.csv("data/hospital-staffing-working_hours_2009-2013.csv")

############################################
############ DATA CLEANING #################
############################################


########## Change the Class of Date Variables and Add a Variable for Length of time (n_days) ###########
hospital_staffing_working_hours_date_fix <- hospital_staffing_working_hours %>% 
  mutate(Begin.Date = as.Date(Begin.Date, format = "%m/%d/%Y"),
         End.Date = as.Date(End.Date, format = "%m/%d/%Y"),
         n_days = as.numeric(End.Date-Begin.Date))

###### Create a Dataset without Statewide Values ########
county <- hospital_staffing_working_hours_date_fix %>%
  filter(County.Name != "Statewide")

##### Create a Dataset with only Statewide Values #####
statewide_results <- hospital_staffing_working_hours_date_fix %>%
  filter(County.Name == "Statewide")

#### Remove variables from Statewide_Results that are not useful - (Facility.Number, Facility.Name, Dates, County.Name, Type.of.Control, n_days) ####
statewide_results <- statewide_results %>% 
  select(Year, Hours.Type, Productive.Hours, Productive.Hours.per.Adjusted.Patient.Day)


############################################
########### DATA EXPLORING #################
############################################

######## Exploring for General Data Features.  ############

### 37,604 Observations of 10 Variables ####
#Year - 5 years - 2009 - 2013
unique(hospital_staffing_working_hours$Year)
#Facility.Number - 465 different facilities
unique(county$Facility.Number)
#Facility.Name - 485 different facility names
unique(county$Facility.Name)
#County.Name - 57 different counties (including "statewide")
unique(hospital_staffing_working_hours$County.Name)
#Type.of.Control - 7 different types of control (including "", the type of control given to statewide observations)
unique(hospital_staffing_working_hours$Type.of.Control)
#Hours.Type - 17 different Hours.Type
unique(hospital_staffing_working_hours$Hours.Type)


############################################
####### DATA EXPLORATION / GRAPHING ########
############################################

#### Visualize State-wide Results ####
ggplot(data = statewide_results, aes(x = Hours.Type, y =Productive.Hours.per.Adjusted.Patient.Day, fill = Year)) +
  geom_col() +
  labs(title = "Productive Hours by Hour Type (Statewide)",
       x = "Hours Type", y = "Productive Hours per Adjusted Patient Day",
       color = "Year") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90))

#### Visualize Productive Hours by County, Hour type, and Year ####
## 1. Create object to graph Productive Hours by County, Hour type, and Year #
adjusted <- county %>% 
  group_by(Year, Hours.Type, County.Name) %>% 
  summarise(productive_hours_adjusted = Productive.Hours.per.Adjusted.Patient.Day)

## 2. plot variables using object created in 1.#
ggplot(adjusted,
       aes(x = County.Name,
           y = productive_hours_adjusted,
           color = Hours.Type)) + geom_point() + labs(title = "Adjusted Productive Hours Per Year",
                                                      x = "County", y = "Productive Hours Per Adjusted Patient Day",
                                                      color = "Hour Types") + facet_grid(rows = vars(Year)) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90))

#### Visualize Productive Hours per Adjusted Patient Day by Year and Type of Control 
## 1. Create object to graph Productive Hours per Adjusted Patient Day by Year and Type of Control #
type_of_control_county <- county %>%
  group_by(Facility.Number, Facility.Name, Type.of.Control, Year) %>%
  summarise(hours_prod = sum(Productive.Hours.per.Adjusted.Patient.Day)) %>% filter(Type.of.Control != "State")
## 2. Plot Variables using object created in 1. #
ggplot(data = type_of_control_county, 
       aes(x = Year, y = hours_prod, fill = Type.of.Control)) +
  geom_col() +
  labs(title = "Productive Hours vs. Type of Control", 
       x = "Year" , 
       y = "Sum of Productive Hours per Adjusted Patient Day", 
       fill = "Type of Control") + theme_bw()


##### Visualize Productive Hours per Adjusted Patient Day by Year and Hour Type ########
## 1. Create object to graph Productive Hours per Adjusted Patient Day by Year and Hour Type #
hours_type_by_county <- county %>%
  group_by(Facility.Number, Facility.Name, Type.of.Control, County.Name, Hours.Type, Year) %>%
  summarise(hours_tot = sum(Productive.Hours.per.Adjusted.Patient.Day))
## 2. Group object by variables we want to graph #
hours_type_by_county_graph <- hours_type_by_county %>%
  group_by(Year, County.Name, Hours.Type) %>%
  summarise(hours_total = sum(hours_tot, na.rm = T))
## 3. Plot Variables using object made in 2. #
ggplot(data = hours_type_by_county_graph, aes(x = Year, y = hours_total, fill = Hours.Type)) + 
  geom_bar(stat = "identity", position = "stack") + labs(title = "Total Productive Hours by Hour Type", x = "Year", y = "Sum of Productive Hours per Adjusted Patient Day", fill = "Hour Type") +
  theme_bw()
## 4. Visualize using column graphs instead of Bar ##
ggplot(hours_type_by_county_graph,
       aes(x = Hours.Type, 
           y = hours_total,
           fill = Year)) + 
  geom_col() + labs(title = "Total Productive Hours vs Hour Types and Year",
                    x = "", y = "Sum of Productive Hours per Adjusted Patient Day",
                    color = "Hour Types") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90))

###### Visualize Number of Facilities per County #####
### 1. Create object that adds a variable to count the number of facilities in each county #
county_name_n_facility <- county %>% 
  group_by(County.Name) %>% 
  summarise(n_facility = n_distinct(Facility.Number)) 
## 2. Plot Variables using object made in 1. #
ggplot(county_name_n_facility, aes(x = County.Name, y = n_facility)) +
  geom_col() + labs(title = "Number of Facilities per County", x = "Facility Name", y = "Amount of Facilities" ) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90))



############################################
############ STATISTICAL TESTING ###########
############################################

regression <- lm(Productive.Hours.per.Adjusted.Patient.Day~Year+as.factor(County.Name), data = county)
coef(regression)
summary(regression)
coef(regression)
exp(coef(regression))

anova_testing_results <- aov(Productive.Hours.per.Adjusted.Patient.Day ~ Hours.Type, county)
summary(anova_testing_results)


