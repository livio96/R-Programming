# Accessing Help in R
?max # Shows the help documentation for the max function
?tidyverse # Shows the documentation for the tidyverse package
??"max" # Returns documentation associated with a given input
str(my_df) # Returns the structure and information of a given object
class(my_df) # Returns the class of a given object

# Using Packages in R
install.packages("tidyverse") # Lets you install new packages (e.g., tidyverse package)
library(tidyverse) # Lets you load and use packages (e.g., tidyverse package)

# The Working Directory
getwd() # Returns your current working directory
setwd("C://file/path") # Changes your current working directory to a desired file path

# Operators in R
# Arithmetic Operators
a + b # Sums two variables
a - b # Subtracts two variables
a * b # Multiply two variables
a / b # Divide two variables
a ^ b # Exponentiation of a variable
a %% b # The remainder of a variable
a %/% b # Integer division of variables

# Relational Operators
a == b # Tests for equality
a != b # Tests for inequality
a > b # Tests for greater than
a < b # Tests for smaller than
a >= b # Tests for greater or equal than
a <= b # Tests for smaller or equal than

# Logical Operators
!a # Logical NOT
a & b # Element-wise Logical AND
a && b # Logical AND
a | b # Element-wise Logical OR
a || b # Logical OR

# Assignment Operators
x <- 1 # Assigns a variable to x
x = 1 # Assigns a variable to x

# Other Operators
a %in% b # Identifies whether an element belongs to a vector
my_df$col # Allows you to access objects stored within an object
a %>% b # Part of magrittr package, it’s used to pass objects to functions

# Getting Started with Vectors in R
# Vectors are one-dimension arrays that can hold numeric data, character data, or logical data.
my_vec <- c(1, 2, 3) # Creates a numeric vector
my_vec <- c("a", "b", "c") # Creates a character vector
my_vec <- c(TRUE, FALSE, TRUE) # Creates a logical vector
length(my_vec) # Returns the length of a vector
my_vec[1] # Returns the first element of a vector
my_vec[-1] # Returns all elements except the first one
my_vec[c(1, 3)] # Returns the first and third elements of a vector
my_vec[my_vec > 1] # Returns the elements of a vector that are greater than 1
sum(my_vec) # Returns the sum of a vector
mean(my_vec) # Returns the mean of a vector
sd(my_vec) # Returns the standard deviation of a vector
sort(my_vec) # Returns a sorted vector
rev(my_vec) # Returns a reversed vector
unique(my_vec) # Returns a vector with only unique elements
append(my_vec, 4) # Adds an element to the end of a vector
insert(my_vec, 2, 0) # Inserts an element at a specified position of a vector
remove(my_vec, 2) # Removes an element at a specified position of a vector
replace(my_vec, 2, 0) # Replaces an element at a specified position of a vector
