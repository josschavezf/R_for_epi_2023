# Week 4

# Link https://josschavezf.github.io/R_for_epi_2023/week_4/slides.html

fruits <- list(name = "apple", 
               quantity = 5,
               properties = c("small", "red", "rounded"))

fruits

View(fruits)

fruits[1]

fruits[[1]]

class(fruits[1])
class(fruits[[1]])

fruits

fruits[[2]]

fruits[[3]][2]

fruits[[3:2]]

result <- fruits[1:2]
class(fruits[1:2])

result[[1]]

fruits[-2]

fruits <- fruits[-2]

fruits

fruits[[1]]

class(fruits$name)

fruits$name[1]

fruits$quantity

fruits$properties[3]
fruits$properties[2]

edible_shell <- c(TRUE, FALSE, TRUE)
edible_shell

fruits[[4]] <- edible_shell
fruits

fruits$hard_shell <- c(FALSE, FALSE, FALSE)
fruits

patient_1 <- list(name = "John",
                  last_name = "Doe",
                  weight = 170,
                  glucose = c(85, 90, 87, 89, 91))
patient_1

patient_1[c(1,2)]
patient_1[1:2]

patient_1[[1]]

patient_1$name
patient_1$last_name

patient_1[[4]][3:5]
patient_1[[4]][c(3,4,5)]
patient_1$glucose[3:5]
patient_1$glucose[c(3,4,5)]

patient_1 <- patient_1[-3]
patient_1

patient_1$blood_pressure <- c(120,80)

patient_1

fruits <- data.frame(name = c("apples", "berries", 
                              "mangos", "bananas"),
                     number = c(1,10,7,2),
                     edible_shell = c(TRUE, TRUE,
                                      FALSE, FALSE))

fruits

View(fruits)

nrow(fruits)
ncol(fruits)

dim(fruits)

rownames(fruits) <- c("row_1", "row_2", "row_3", "row_4")
rownames(fruits)
fruits

colnames(fruits) <- c("Name", "Number", "Edible_shell")
colnames(fruits)

colnames(fruits)[1] <- "name"
colnames(fruits)

colnames(fruits)[2:3] <- c("number", "edible_shell")
colnames(fruits)
fruits

patients <- data.frame(first_name = c("Ava", "Noah",
                                      "Olivia"),
                       last_name = c("Smith",
                                     "Johnson",
                                     "Williams"),
                       age = c(65,20,47),
                       co_morbidity = c(TRUE, FALSE, FALSE))
patients

dim(patients)
nrow(patients)
ncol(patients)

rownames(patients)
colnames(patients)

#matrix_name[rows,cols]
#df[rows,cols]

fruits

fruits[1,2]
fruits[3,1]

fruits[c(1,2),2]
fruits[1:2,2]

fruits[3,2:3]
fruits[3,c(2,3)]

fruits[3,c(1,3)]

patients[c(1,2),c(2,3)]
patients[1:2, 2:3]

# all rows from column 2
fruits[,2]
fruits[1:4,2]

# all
fruits[2,]

patients[3,]

patients[,3]


fruits
fruits$name
fruits$number

class(fruits$name)
fruits$name[3]

patients$last_name
patients$age
patients$age[2]

cbind(fruits,
      "hard_shell" = c(FALSE, FALSE, FALSE, FALSE))

fruits <- cbind(fruits,
                "hard_shell" = c(FALSE, FALSE, 
                                 FALSE, FALSE))

fruits$hard_shell <- c(FALSE, FALSE, FALSE, FALSE)

fruits

rbind(fruits,
      c("coconut", 3, FALSE, TRUE))

fruits <- rbind(fruits,
                c("coconut", 3, FALSE, TRUE))
fruits

rownames(fruits)
fruits[5,]
fruits["5",]

fruits$hard_shell

patients <- cbind(patients, 
                  "glucose" = c(100,125,108))
patients

patients$glucose <- c(100,125,108)

patients

patients <- rbind(patients,
                  c("Elza_sister", "Rechtman", 27, FALSE, 90))
patients



