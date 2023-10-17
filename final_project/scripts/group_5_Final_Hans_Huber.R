### Hans Huber Final Project Script

library(tidyverse)
library(modelr)
library(ggplot2)
library(dplyr)

### file imported using import file option in console 

bed_trends <- read_csv("~/Desktop/R Course/Final Project/bed_trends.csv")
bed_trends <- read_csv("data/licensed-bed-classification-and-designations-trends.csv")
### creating a new column that finds the length of stay for each hospital unit

bed_trends <- bed_trends %>% 
  rename(Census_Day = "Census Day",
         Facility_Name = "Facility Name",
         Licensed_Bed_Classification = "Licensed Bed Classification",
         Licensed_Bed_Day = "Licensed Bed Day",
         License_Bed_Designation = "License Bed Designation",
         Transfer = "Intra Hospital Transfer from Critical Care")

LOScomplete <- mutate(bed_trends, LOS = Census_Day / (Discharges + Transfer))

### Isolating LOS data for ICU beds only

LOSICU <- filter(LOScomplete, License_Bed_Designation == "Intensive Care" 
                 | License_Bed_Designation == "Coronary Care" 
                 | License_Bed_Designation == "Intensive Care Newborn Nursery")

### omitting NA and infinite values 
LOSICU_filtered <- na.omit(LOSICU)
LOSICU_filtered <- LOSICU_filtered[!is.infinite(LOSICU_filtered$LOS),]

by_year <- group_by(LOSICU_filtered, Year)
summarise(by_year, mean(LOS))

Plot1 <- ggplot(LOSICU_filtered, aes(x = "", LOS, group = 1)) +
  facet_wrap(vars(Year), nrow = 1) +
  theme_bw() +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim=c(0, 20)) +
  labs(title = "Average ICU Length of Stay", x = "Year", y = "Length of Stay in Days")


Plot1


### Summary statistics by year: 

group_by(LOSICU_filtered, Year) %>%
  summarise(
    count = n(),
    mean = mean(LOS),
    sd = sd(LOS)
  )

### ANOVA to test for differences in year:

class(by_year$Year) = "Numeric"

by_year <- group_by(by_year, Year)

anovatest <- aov(LOS ~ Year, data = by_year)

summary(anovatest)

linearModel <- lm(formula = LOS ~ Year, data = by_year)
summary(linearModel)


