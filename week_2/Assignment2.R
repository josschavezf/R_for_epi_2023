### Introduction to Epidemiology Data Analysis with R

# Assignment 2: 

## Let's create some vectors

a <- 70
b <- "hello"
c <- TRUE

### What is the class of vector a?
class(a)

### What is the class of vector b?
class(b)

### What is the class of vector c?
class(c)

## Store the result of the following code in a new vector
d <- c(a,b,c)

### What is the resulting class of your new vector?
class(d)
    
## Let's create a vector that contains the weight in kg from three patients:

patients_weight <- c(90, 75, 68)

### How do you get the number of patients in the vector?
length(patients_weight)

### How would you get the weight from patient 2?
patients_weight[2]

### How would you extract the weight from patients 2 and 3?
patients_weight[2:3]
patients_weight[c(2,3)]

npatients <- c(2,3)
patients_weight[npatients]

## Let's add the patient's name to the vector

patients_weight <- c("John" = 90, "Andrea" = 75, "Will" = 68)

### Use the patient's name to get the weight from Andrea 
patients_weight["Andrea"]

### Use the patient names to get the weight from John and Will 
patients_weight[c("John", "Will")]

patients_name <- c("John", "Will")
patients_weight[patients_name]

### Use logical evaluation to select which patients have 
### a weight bigger than 70 kg
patients_weight[patients_weight > 70]



