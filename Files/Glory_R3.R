#Colin Glory 11/3/23
#Assignment 3
------------------------------------------------------------------------------------------
#Loading library tidyverse
library(tidyverse)
library(ggplot2)
library(dplyr)
library(magrittr)
library(lubridate) # Easier way to work with dates (easier than base R)
options(scipen=999)
------------------------------------------------------------------------------------------
# Basic table declarations & renaming some columns
  
#Creating table assignment from csv. Use the code "skip=#" to skip a # amount of lines
bee_data <- read_csv("C:/Users/cglor/Downloads/HoneyBees.csv", col_names = TRUE)

#Takes a look at columns & some of its data
glimpse(bee_data)
#Specifically looks at the column names, it is time to change them
names(bee_data)

#Change the column names
names(bee_data)[2] <- 'num_col' 
names(bee_data)[3] <- 'yield_per_col' 
names(bee_data)[4] <- 'total_prod' 
names(bee_data)[6] <- 'price_per_lb' 
names(bee_data)[7] <- 'prod_value'
names(bee_data)[10] <- 'region' 
------------------------------------------------------------------------------------------
#Taking a look at the data and handling NA entries
glimpse(bee_data)
summary(bee_data)
bee_data<- na.omit(bee_data)
------------------------------------------------------------------------------------------

# Exploring the data

#Graph Section #1
#Let's take a look the correlation between the number of colonies & price per lb
  
ggplot(data = bee_data) + 
  geom_point(mapping = aes(x = num_col, y = price_per_lb))

#After graphing you would be able to see that most of the data revolves around the far left of the graph.
#There seems to be a higher price per pound when the number of colonies is lower. 

#Let's get a cleaner look at the data. Since a high majority of the data fell below th 50,000 range, I decided to take a look at just that data

col_price <- bee_data

#This will allow us to only see the data below or equal to 50000 colonies

col_price %<>% 
  filter(num_col <= 50000)

#Here is the graph. Looks much better and proves my previous point even further. The less the amount of colonies, the higher the price per lb
ggplot(data = col_price) + 
  geom_point(mapping = aes(x = num_col, y = price_per_lb))

# The data that is temporarily removed wouldn't necessarily be outliers, im sure some of it is, but it could be considered good data while also being out of the current scope
 
# Graph Section #2

# Let's take a look at nCLOTHIANIDIN via a histogram. It refers to the amount in kg of CLOTHIANIDIN applied.

hist(bee_data$nCLOTHIANIDIN, main = "Histogram of Clothianidin", xlab = "Clothianidin")

# Looking at the results, you can see the majority of the Clothianidin applied was in lower doses was more frequent than higher doses.

# Taking a further look, we have outliers that reside past the 100,000 mark for the substance applied.
# Clothianidin when added in smaller doses, seems to correlate to a higher production level. Let's look further

ggplot(data = bee_data, aes(x = nCLOTHIANIDIN, y = total_prod)) +
  geom_point() + 
  theme_classic() +
  labs(
    title = "Relationship between nCLOTHIANIDIN and Total Production",
    x = "nCLOTHIANIDIN",
    y = "Total Production"
  )

# Same process as before

cloth_prod <- bee_data
cloth_prod %<>% 
  filter(nCLOTHIANIDIN <= 50000)

ggplot(data = cloth_prod, aes(x = nCLOTHIANIDIN, y = total_prod)) +
  geom_point() + 
  theme_classic() +
  labs(
    title = "Relationship between nCLOTHIANIDIN and Total Production",
    x = "nCLOTHIANIDIN",
    y = "Total Production"
  )

# The data is still heavily cluttered in the corner. There seems to be a lot of entries with 0's & a high production count, so let's look at the data without them

cloth_prod %<>% 
  filter(nCLOTHIANIDIN > 0 &  nCLOTHIANIDIN <= 20000)

ggplot(data = cloth_prod, aes(x = nCLOTHIANIDIN, y = total_prod)) +
  geom_point() + 
  theme_classic() +
  labs(
    title = "Relationship between nCLOTHIANIDIN and Total Production",
    x = "nCLOTHIANIDIN",
    y = "Total Production"
  )

# Most of the results are below the 20,000,000 mark on the y axis. I'd personally say most of the higher production numbers correlate to lower Clothianidin used, but there is a valid argument with this data that it doesn't matter.

# Graph 3 Section

#Here I tried to use the factor() function for StateName, but it ended up looking a bit clunky. 
data_factor <- c(bee_data$StateName)
factor_data <- factor(data_factor)
levels(factor_data)
barplot(table(factor_data),
        col = "skyblue",
        main = "Custom Bar Plot",
        xlab = "Categories",
        ylab = "Frequency",
        border = "black",
        names.arg = levels(factor_data),
        cex.names = 0.8,
        las = 2  # Rotate labels vertically
)

# Graph Section 4

# Let's take a look at nCLOTHIANIDIN, there is a max value around
# 278K kg, while most values reside around 0-50k kg.
hist(bee_data$nCLOTHIANIDIN, main = "Histogram of Colonies", xlab = "Colonies", breaks=30)

# As you can see here, 
ggplot(bee_data, aes(x = nCLOTHIANIDIN)) +
  geom_boxplot() + # Create a box and whiskers graph
  labs(title = "Box And Whiskers Chart of CLOTHIANIDIN applied",
       x = "nCLOTHIANIDIN") +
  theme_classic() # Removes the background grey and grid


# Graph Section 5

# Here I show a way to make a quick and effortless plot. I took the years as the x and used nTHIAMETHOXAM as the y.
# Using the plot() function will create the quick graph to get a general idea of what the data looks like.

plot(bee_data$year,bee_data$nTHIAMETHOXAM)