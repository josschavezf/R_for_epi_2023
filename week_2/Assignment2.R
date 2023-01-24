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
class(a)

### 3) What is the class of vector c?
class(c)

## Store the result of the following code in a new vector
d <- c(a,b,c)

### 4) What is the resulting class of the new vector?
class(d)

## Let's create a vector that contains the weight of three patients:
patients_weight <- c(198, 165, 150)

### 5) How do you get the number of patients in the vector?
length(patients_weight)

### 6) How would you get the weight of patient 2?
patients_weight[2]

### 7) How would you get the weight of patients 2 and 3?
patients_weight[2:3]
patients_weight[c(2,3)]
patients_weight[seq(from = 2, to = 3)]
