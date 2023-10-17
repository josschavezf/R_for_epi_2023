#Group 3: Keerthana Byreddy, Bushra, Raj
#dataset: hiv-aids-annual-report

library (tidyverse)
library(dplyr)
library(ggplot2)
annual_report <- read_csv("data/hiv-aids-annual-report.csv")
View(annual_report)
tibble (annual_report)
summary (annual_report$Age)

# Replace 99999.0 values with NA
annual_report[annual_report == 99999.0] <- NA

# Check for missing values
colSums(is.na(annual_report))
#HIV diagnosis rate-5, % linked to care within 3 months-1522,
#AIDS diagnoses-1, AIDS diagnosis rate-6, PLWDHI prevalence-19, % viral suppression-155,
#Deaths-2, HIV-related death rate-1201, Non-HIV-related death rate-1201

# Rename the column names
annual_report <- annual_report %>% rename(HIV_death_rate = `HIV-related death rate`) 
annual_report <- annual_report %>% rename(HIV_diagnoses = `HIV diagnoses`)
annual_report <- annual_report %>% rename(HIV_diagnosis_rate = `HIV diagnosis rate`)
annual_report <- annual_report %>% rename(AIDS_diagnosis = `AIDS diagnoses`)
view (annual_report)

# Check for outliers
ggplot(annual_report, aes(x = 1, y = HIV_diagnoses)) +
  geom_boxplot() +
  labs(x = "", y = "HIV Diagnoses")


#age is character but hasn't been converted since it is a range

#VISUALIZATION

#Stacked bar plot showing the distribution of HIV diagnoses by Borough, Race/Ethnicity, and Gender
ggplot(annual_report, aes(x = Borough, y = HIV_diagnoses, fill = Race)) +
  geom_bar(stat = "identity") +
  labs(x = "Boroughs", y = "HIV diagnoses", title = "HIV Diagnoses by Borough, Race/Ethnicity, and Gender") +
  scale_fill_manual(values = c("blue", "black", "brown", "orange", "gray", "yellow", "green", "pink")) +
  facet_grid(. ~ Gender) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#HIV Diagnoses by Borough and Gender -ALL
annual_report_all <- filter(annual_report, Age == "All", Borough == "All")
ggplot(annual_report_all, aes(x = Borough, y = HIV_diagnoses, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0, 3500), breaks = seq(0, 3500, 500)) +
  labs(x = "Boroughs", y = "HIV diagnoses", title = "HIV Diagnoses by Borough(All) and Gender") +
  scale_fill_manual(values = c("pink", "red", "blue","purple")) +
  theme(panel.grid.major.y = element_line(color = "#c0c0c0", linetype = "dashed"))

#HIV Diagnoses by all Age and Gender -ALL
ggplot(annual_report_all, aes(x = Age, y = HIV_diagnoses, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0, 3500), breaks = seq(0, 3500, 500)) +
  labs(x = "Age", y = "HIV diagnoses", title = "HIV Diagnoses by Age(All age groups) and Gender") +
  scale_fill_manual(values = c("brown", "orange", "black","red")) +
  theme(panel.grid.major.y = element_line(color = "#c0c0c0", linetype = "dashed"))

#HIV Diagnoses by Borough and Gender
annual_report_others <- filter(annual_report, Age != "All", Borough != "All")
ggplot(annual_report_others, aes(x = Borough, y = HIV_diagnoses, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50)) +
  labs(x = "Boroughs", y = "HIV diagnoses", title = "HIV Diagnoses by Borough and Gender") +
  scale_fill_manual(values = c("pink", "brown")) +
  theme(panel.grid.major.y = element_line(color = "#c0c0c0", linetype = "dashed"))
##can add " , alpha = 0.7 " after dodge for transparency

#HIV Diagnoses by Age and Gender
ggplot(annual_report_others, aes(x = Age, y = HIV_diagnoses, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50)) +
  labs(x = "Age", y = "HIV diagnoses", title = "HIV Diagnoses by Age and Gender") +
  scale_fill_manual(values = c("black", "blue")) +
  theme(panel.grid.major.y = element_line(color = "#c0c0c0", linetype = "dashed"))

#HIV Diagnoses Rate vs. HIV Death Rate
ggplot(annual_report, aes(x = Year)) +
  geom_bar(aes(y = HIV_diagnoses, fill = "Diagnoses Rate"), stat = "identity", width = 0.6, position = position_dodge(0.9), na.rm = TRUE) +
  geom_bar(aes(y = HIV_death_rate, fill = "Death Rate"), stat = "identity", width = 0.6, position = position_dodge(0.9), na.rm = TRUE) +
  scale_y_continuous(limits = c(0, 650), breaks = seq(0, 650, 50)) +
  labs(x = "Year", y = "Rate per 100,000 population", title = "HIV Diagnoses Rate vs. HIV Death Rate", fill="") +
  scale_fill_manual(values = c("#4C72B0", "#C44E52")) +
  theme(panel.grid.major.y = element_line(color = "#c0c0c0", linetype = "dashed"))
#na.rm to remove the missing values

#HIV Diagnosis vs AIDS Diagnosis
ggplot(annual_report, aes(x = Year)) +
  geom_bar(aes(y = HIV_diagnoses, fill = "HIV Diagnosis"), stat = "identity", width = 0.6, position = position_dodge(0.9), na.rm = TRUE) +
  geom_bar(aes(y = AIDS_diagnosis, fill = "AIDS Diagnosis"), stat = "identity", width = 0.6, position = position_dodge(0.9), na.rm = TRUE) +
  scale_y_continuous(limits = c(0, 650), breaks = seq(0, 650, 50)) +
  labs(x = "Year", y = "Rate per 100,000 population", title = "HIV Diagnoses vs. AIDS Diagnosis", fill="") +
  scale_fill_manual(values = c("black", "brown")) +
  theme(panel.grid.major.y = element_line(color = "#c0c0c0", linetype = "dashed"))

#Linear regression model - HIV diagnosis vs Borough
model_borough <- lm(HIV_diagnoses ~ Borough, data = annual_report)
summary(model_borough)
coef(model_borough)
exp(coef(model_borough))

#Residuals:
#Min      1Q  Median      3Q     Max 
#-446.62  -18.87  -12.52   -1.77 2930.38 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            448.62      10.07   44.53   <2e-16 ***
#  BoroughBronx          -426.75      10.71  -39.85   <2e-16 ***
#  BoroughBrooklyn       -428.30      10.50  -40.78   <2e-16 ***
#  BoroughManhattan      -428.16      10.54  -40.62   <2e-16 ***
#  BoroughQueens         -435.10      10.54  -41.28   <2e-16 ***
#  BoroughStaten Island  -445.84      11.07  -40.27   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 114.9 on 5999 degrees of freedom
#Multiple R-squared:  0.2317,	Adjusted R-squared:  0.2311 
#F-statistic: 361.9 on 5 and 5999 DF,  p-value: < 2.2e-16

#> coef(model_borough)
#(Intercept)         BoroughBronx      BoroughBrooklyn     BoroughManhattan        BoroughQueens 
#448.6154            -426.7474            -428.3007            -428.1616            -435.0997 
#BoroughStaten Island 
#-445.8442 
#> exp(coef(model_borough))
#(Intercept)         BoroughBronx      BoroughBrooklyn     BoroughManhattan        BoroughQueens 
#6.779318e+194        4.634103e-186        9.803059e-187        1.126664e-186        1.092900e-189 
#BoroughStaten Island 
#2.356844e-194 

#Linear regression model - HIV Diagnosis vs Gender
model <- lm(HIV_diagnoses ~ Gender, data = annual_report)
summary(model)
coef(model)
exp(coef(model))

#Residuals:
#  Min     1Q Median     3Q    Max 
#-165.7  -27.4   -9.0   -5.7 3213.3 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        165.729      8.223   20.15   <2e-16 ***
#  GenderFemale      -156.756      8.559  -18.32   <2e-16 ***
#  GenderMale        -133.350      8.559  -15.58   <2e-16 ***
#  GenderTransgender -119.729     57.562   -2.08   0.0376 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 127.4 on 6001 degrees of freedom
#Multiple R-squared:  0.05473,	Adjusted R-squared:  0.05426 
#F-statistic: 115.8 on 3 and 6001 DF,  p-value: < 2.2e-16

#> coef(model)
#(Intercept)      GenderFemale        GenderMale GenderTransgender 
#165.7292         -156.7556         -133.3503         -119.7292 
#> exp(coef(model))
#(Intercept)      GenderFemale        GenderMale GenderTransgender 
#9.446318e+71      8.354630e-69      1.220900e-58      1.005272e-52 


