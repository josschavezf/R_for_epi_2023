# load packages
library(tidyverse) # data cleaning
library(ggpubr) # arrange plots
library(patchwork) # combine plots
library(modelr) # modelling
library(dplyr)  

MRS <- read_csv("methicillin-resistant-staphylococcus-aureus-mrsa-bloodstream-infections-bsi-in-california-hospitals.csv")

################################################################################

# n.b. Methicillin-resistant Staphylococcus aureus (MRSA) bloodstream infections (BSI)
# Dataset reports number of MRSA BSI reported by California hospitals in the specified reporting year.

### Explore your dataset, and evaluate if the data requires some cleaning or formatting modifications.
### Look for possible relationships between the variables or whether data show some patterns related to 
### each categorical or numerical variable. 
### Perform the corresponding statistical tests to evaluate the distribution of your data 
### and the significant differences between groups in your data.


### Prepare an R script showing the process of cleaning, visualizing, and statistical modeling. 
View(MRS) 

#### Cleaning ####

# Renamed variables Facility_ID1, Facility_Name1, Hospital_Onset_Cases to shortent variable names.
MRS <- MRS %>% rename(ID = Facility_ID1, Facility = Facility_Name1, MRS_Incidence = Hospital_Onset_Cases)
names(MRS)

### OUTPUT ###
# [1] "ID"                    "Facility"              "County"                "MRS_Incidence"        
# [5] "Predicted_Cases"       "Patient_Days"          "SIR"                   "SIR_CI_95_Lower_Limit"
# [9] "SIR_CI_95_Upper_Limit" "Comparison"       

# Created variable for MRS BSI Incidence per 10000 patient-days.
MRS <- mutate(MRS, MRS_per_10000 = (MRS_Incidence/Patient_Days)*10000)
view(MRS)

# Compute mean SIR for each County
mean_SIR = MRS %>%
  drop_na("SIR") %>%  # remove rows where SIR == "NA"
  group_by(County) %>%
  summarise_at(vars(SIR), list(mean_SIR = mean))

view(mean_SIR)


################################################################################
#### Visualizations ####

#Edit1

# Plot Incidence by County.
ggplot(MRS,
       aes(y = County,
           x = MRS_per_10000,
           fill = Comparison)) +
  geom_col() + 
  labs(title = "MRS BSI Incidence by County",      
       x = "Incidence per 10,000 patient-days",
       y = "County")

# Plot mean SIR by County to better represent 
ggplot(mean_SIR,
       aes(y = County,
           x = mean_SIR)) +
  geom_col() + 
  labs(title = "Standardized Infection Ratio by County",      
       x = "Mean Standardized Infection Ratio",
       y = "County")

# tiff("plot1.tiff")
#des
# dev.off()




## Not sure where I was going with this lol
???    MRS_observed <- MRS %>% 
  group_by(County) %>% 
  count(MRS_Incidence) 

???   MRS_predicted <- MRS %>% 
  group_by(County) %>% 
  count(Predicted_Cases) 



################################################################################       
#####

# Compare MRSA BSI incidence between counties
aov_county <- aov(MRS_Incidence ~ County, MRS)
summary(aov_county)

# Df      Sum Sq     Mean Sq     F value   Pr(>F)  
# County       55       553        10.05        0.817    0.817
# Residuals   306       3766       12.31                   

# The ANOVA indicates that the difference in MRSA BSI Incidence does not
# differ between counties. However, the comparing the raw counts may 
# produce biased results because it does not account for differences in population
# size between counties and the time patients are observed for. Therefore,
# the analysis was repeated comparing MRS incidence/10,000 patient-days:

MRS <- mutate(MRS, MRS_per_10000 = (MRS_Incidence/Patient_Days)*10000)
aov_county <- aov(MRS_per_10000 ~ County, MRS)
summary(aov_county)

# Df     Sum Sq    Mean Sq    F value   Pr(>F)
# County        55    15.94     0.2899      1.153     0.229
# Residuals     302    75.94    0.2515  

# The ANOVA comparing MRS Incidence per 10,000 patient-days is also non-significant;
# however, it is trending closer towards significance and represents a less biased
# estimate of the incidence rate among counties. This suggests the incidence rate 
# does not differ between counties.


############

# Compare the distribution of predicted and observed MRSA BSI cases
t.test(MRS$MRS_Incidence, MRS$Predicted_Cases)

### OUTPUT ###   
# mean MRS_Incidence = 2.116022 
# mean Predicted_Cases = 2.460769 

# t = -1.433, df = 689.98, p-value = 0.1523
# 95% CI (-0.8170903  0.1275961)

# The t-test is nonsignificant, indicating that the mean number of observed cases
# across facilities and the mean number of predicted cases do not differ. 
# mean MRS_Incidence = 2.116022 
# mean Predicted_Cases = 2.460769 


#### Models ####

## Is there a relationship between the number of hospital onset cases and patient days:
model2 <- lm(Patient_Days ~  MRS_Incidence, data = MRS)
summary(model2)

## Answer: p-value is less than 0.05 indicating that hospital onset cases is significant. 
coef(model2)
exp(coef(model2))

## Calculate a p-value for R2: p-value is 2.2e-16. 


################################################################################
### Prepare a presentation with the following elements: 

###  1) Description of your dataset 
###  (where it comes from, how many observations have, what type of variables contains). 

MRS %>% 
  count(ID) %>% 
  summarise(n = n())

#   There are 362 observations in the dataset.
#
#   The dataset contains the variables facility name, county, observed MRSA BSI incidence, 
#   predicted MRSA BSI incidence, and patient-days. The dataset also contains the facility 
#   ID number, the SIR metric (with 95% CI), and test result comparing the individual hospitals'
#   SIR with the national baseline.
#
#   The variables Facility and County are discrete, categorical variables. MRSA BSI Incidence 
#   and patient days are discrete, count variables. The SIR is a metric for representing the
#   ratio of observed to predicted number of cases of MRSA BSI.


###  2) Describe the analysis process, you don’t need to show the code but you must mention 
###  which packages you used, whether your data was clean or not, which variables are you comparing, etc. 

# packages
library(tidyverse) # data cleaning
library(ggpubr)    # arrange plots
library(patchwork) # combine plots
library(modelr)    # modelling
library(dplyr)     #

#   The dataset was clean. The only changes to the dataset were the variable names and 
#   the creation of new variables for MRS BSI incidence per 10,000 person-days and 
#   mean SIR by County. 


### 3) Show and discuss your results (plots and statistical tests), 
###  what can you conclude from the data? What clinical or biological interpretation do they have?

# Above.
#
#
#






