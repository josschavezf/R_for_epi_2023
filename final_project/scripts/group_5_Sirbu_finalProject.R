library(tidyverse)
library(modelr)
library(dplyr)

fnlpjt <- read_csv("finalproject.csv")
fnlpjt <- read_csv("data/licensed-bed-classification-and-designations-trends.csv")
View(fnlpjt)

#Arranging by year and hospital ID
fnlpjt <- arrange(fnlpjt, Year, OSHPD_ID)

fnlpjt <- fnlpjt %>% 
  rename(License_Bed_Designation = "License Bed Designation",
         Census_Day = "Census Day")

## Excluding null values
fnlpjt <- filter(fnlpjt, 
License_Bed_Designation == "Medical/Surgical Acute (includes GYN/DOU)" 
& Discharges > 0 
& Census_Day >0)

#Creating new Length of Stay (LOS) variable
fnlpjt <- mutate(fnlpjt, LOS = Census_Day/Discharges)

#Mean LOS with data as is
fnlpjt <- group_by(fnlpjt, Year)
summarise(fnlpjt, mean(LOS))

#Box plot for mean LOS with data as is
bxplot <- ggplot(fnlpjt, aes(x = "", LOS, group = 1)) +
  geom_boxplot() +
  facet_wrap(vars(Year), nrow=1) + 
  labs(title = "Length of stay per year", x = "Year", y = "Length of Stay (Days)")
bxplot

#Removing outliers per year
Y2005 <- filter(fnlpjt, Year == 2005)
Y2005 <- filter(Y2005, LOS < ((quantile(Y2005$LOS,0.75)-quantile(Y2005$LOS,0.25))*1.5+quantile(Y2005$LOS,0.75)), 
                LOS > (quantile(Y2005$LOS,0.25) - (quantile(Y2005$LOS,0.75)-quantile(Y2005$LOS,0.25))*1.5))

Y2006 <- filter(fnlpjt, Year == 2006)
Y2006 <- filter(Y2006, LOS < ((quantile(Y2006$LOS,0.75)-quantile(Y2006$LOS,0.25))*1.5+quantile(Y2006$LOS,0.75)), 
                LOS > (quantile(Y2006$LOS,0.25) - (quantile(Y2006$LOS,0.75)-quantile(Y2006$LOS,0.25))*1.5))

Y2007 <- filter(fnlpjt, Year == 2007)
Y2007 <- filter(Y2007, LOS < ((quantile(Y2007$LOS,0.75)-quantile(Y2007$LOS,0.25))*1.5+quantile(Y2007$LOS,0.75)), 
                LOS > (quantile(Y2007$LOS,0.25) - (quantile(Y2007$LOS,0.75)-quantile(Y2007$LOS,0.25))*1.5))

Y2008 <- filter(fnlpjt, Year == 2008)
Y2008 <- filter(Y2008, LOS < ((quantile(Y2008$LOS,0.75)-quantile(Y2008$LOS,0.25))*1.5+quantile(Y2008$LOS,0.75)), 
                LOS > (quantile(Y2008$LOS,0.25) - (quantile(Y2008$LOS,0.75)-quantile(Y2008$LOS,0.25))*1.5))

Y2009 <- filter(fnlpjt, Year == 2009)
Y2009 <- filter(Y2009, LOS < ((quantile(Y2009$LOS,0.75)-quantile(Y2009$LOS,0.25))*1.5+quantile(Y2009$LOS,0.75)), 
                LOS > (quantile(Y2009$LOS,0.25) - (quantile(Y2009$LOS,0.75)-quantile(Y2009$LOS,0.25))*1.5))
Y2010 <- filter(fnlpjt, Year == 2010)
Y2010 <- filter(Y2010, LOS < ((quantile(Y2010$LOS,0.75)-quantile(Y2010$LOS,0.25))*1.5+quantile(Y2010$LOS,0.75)), 
                LOS > (quantile(Y2010$LOS,0.25) - (quantile(Y2010$LOS,0.75)-quantile(Y2010$LOS,0.25))*1.5))

Y2011 <- filter(fnlpjt, Year == 2011)
Y2011 <- filter(Y2011, LOS < ((quantile(Y2011$LOS,0.75)-quantile(Y2011$LOS,0.25))*1.5+quantile(Y2011$LOS,0.75)), 
                LOS > (quantile(Y2011$LOS,0.25) - (quantile(Y2011$LOS,0.75)-quantile(Y2011$LOS,0.25))*1.5))

Y2012 <- filter(fnlpjt, Year == 2012)
Y2012 <- filter(Y2012, LOS < ((quantile(Y2012$LOS,0.75)-quantile(Y2012$LOS,0.25))*1.5+quantile(Y2012$LOS,0.75)), 
                LOS > (quantile(Y2012$LOS,0.25) - (quantile(Y2012$LOS,0.75)-quantile(Y2012$LOS,0.25))*1.5))

Y2013 <- filter(fnlpjt, Year == 2013)
Y2013 <- filter(Y2013, LOS < ((quantile(Y2013$LOS,0.75)-quantile(Y2013$LOS,0.25))*1.5+quantile(Y2013$LOS,0.75)), 
                LOS > (quantile(Y2013$LOS,0.25) - (quantile(Y2013$LOS,0.75)-quantile(Y2013$LOS,0.25))*1.5))

Y2014 <- filter(fnlpjt, Year == 2014)
Y2014 <- filter(Y2014, LOS < ((quantile(Y2014$LOS,0.75)-quantile(Y2014$LOS,0.25))*1.5+quantile(Y2014$LOS,0.75)), 
                LOS > (quantile(Y2014$LOS,0.25) - (quantile(Y2014$LOS,0.75)-quantile(Y2014$LOS,0.25))*1.5))

Y2015 <- filter(fnlpjt, Year == 2015)
Y2015 <- filter(Y2015, LOS < ((quantile(Y2015$LOS,0.75)-quantile(Y2015$LOS,0.25))*1.5+quantile(Y2015$LOS,0.75)), 
                LOS > (quantile(Y2015$LOS,0.25) - (quantile(Y2015$LOS,0.75)-quantile(Y2015$LOS,0.25))*1.5))

#Combine all years again
fnlpjt_noout <- rbind(Y2005, Y2006, Y2007, Y2008, Y2009, Y2010, Y2011, Y2012, Y2013, Y2014, Y2015)


bxplot_noout <- ggplot(fnlpjt_noout, aes(x = "", LOS, group = 1)) +
  geom_boxplot() +
  facet_wrap(vars(Year), nrow=1) + 
  labs(title = "Length of stay per year", x = "Year", y = "Length of Stay (Days)")
bxplot_noout

fnlpjt_noout_grouped <- group_by(fnlpjt_noout, Year)
summarise(fnlpjt_noout_grouped, mean(LOS))

t.test(Y2005$LOS, Y2015$LOS)





