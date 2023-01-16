### Introduction to Epidemiology Data Analysis with R. 

### Introduction to coding with R. Part I.

# Assignment week 2: 

## 1) Which of the following are good variable names?

a) Patient$age
b) cause_of_disease
c) median
d) DEMOGRAPHICINFORMATION
e) incidenceRate


## Let's create some vectors

a <- 70
b <- "hello"
c <- TRUE

### 2) What is the class of vector a?


### 3) What is the class of vector c?


## Store the result of the following code in a new vector
d <- c(a,b,c)

### 4) What is the resulting class of the new vector?


## Let's create a vector that contains the weight of three patients:
patients_weight <- c(198, 165, 150)

### 5) How do you get the number of patients in the vector?


### 6) How would you get the weight of patient 2?


### 7) How would you get the weight of patients 2 and 3?


## Let's add the patient's name to the vector

patients_weight <- c("John" = 198, "Andrea" = 165, "Will" = 150)

### 8) Use patients' names to get John and Will’s weight 


### 9) Use a logical evaluation to select patients with 
### a weight greater than 155 pounds


## Let's create a matrix

my_matrix <- matrix(1:30, ncol = 6)

### 10) How would you select columns 3 to 5 with all their rows?




