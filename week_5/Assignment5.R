### Introduction to Epidemiology Data Analysis with R. 

### Introduction to coding with R. Part III.

# Assignment week 4: 


## Let's create a list with immunization records from a patient

patient_1 <- list(name = "Jane",
                  last_name = "Doe",
                  year = c(2010,2011,2012,2013,2014),
                  vaccine = c("Hepatitis B", "Rotavirus","Tetanus","Influenza","Varicella"))

### 1) Use brackets to extract the year and vaccines applied


### 2) Use $ to extract the vaccines component


### 3) How would you extract the "Influenza" vaccine? 


## Let's create a data frame

vaccines <- data.frame(year = c(2010,2011,2012,2013,2014),
                       vaccine = c("Hepatitis B", "Rotavirus","Tetanus","Influenza","Varicella"),
                       doses_applied = c(205, 216, 192, 187, 209))

### 4) Extract the column of vaccines and dosis applied


### 5) Add a new row with the Measles vaccine that was applied on year 2015 using 200 doses





my_matrix <- matrix(1:30, ncol = 6)

### 6) How would you select columns 2 to 4 with all their rows?




