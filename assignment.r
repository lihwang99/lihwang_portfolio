# Assignment 3: Using Data
#
# Before you get started:
# - Set your working directory to "source file location" using the Session menu
# - Run the following line of code to delete all variables in your workspace
#     (This will make it easier to test your script)
rm(list = ls())


### Built in R Data ###########################################################

# In this section, you'll work with the variable `Titanic`, a data set which is
# built into the R environment.
# This data set actually loads in a format called a *table*
# See https://cran.r-project.org/web/packages/data.table/data.table.pdf
# Use the `is.data.frame()` function to test if it is a table.
is.data.frame(Titanic) # FALSE

# Create a variable `titanic_df` by converting `Titanic` into a data frame;
# you can use the `data.frame()` function or `as.data.frame()`
# Hint: Be sure to **not** treat strings as factors!
titanic_df <- data.frame(Titanic, stringsAsFactors = FALSE)

# It's important to understand the _meaning_ of each column before analyzing it
# Using comments below, describe what information is stored in each column
# For categorical variables, list all possible values
# Class: What class they were on the ship. 1st, 2nd, 3rd or Crew.
# Sex: Their biological gender. Male or Female.
# Age: How old they were. Child or Adult.
# Survived: Whether or not they survived the Titanic sinking. Yes or No.
# Freq: Frequency of occurence on the Titanic.


# Create a variable `children` that is a data frame containing only the rows
# from `titanic_df` with information about children on the Titanic
# Hints:
# - Filter rows using a vector of boolean values (like vector filtering)
# - See chapter 10.2.3
children <- titanic_df[titanic_df$Age == "Child", ]

# Create a variable `num_children` that is the total number of children.
# Hint: Remember the `sum()` function!
num_children <- sum(children$Freq)

# Create a variable `most_lost` that is the *row* from `titanic_df` with the
# largest absolute number of losses (people who did not survive)
# You can use multiple lines of code if you find that helpful
# to create this variable
# Hint: Filter for those who did not survive, then look for the row
all_lost <- titanic_df[titanic_df$Survived == "No", ]
number_lost <- max(all_lost$Freq)

most_lost <- all_lost[all_lost$Freq == number_lost, ]

# Define a function called `survival_rate()` that takes in two arguments which
# must be in *the following order*:
# - a ticket class (e.g., "1st", "2nd"), and
# - the dataframe itself (it's good practice to explicitly pass in data frames)
#
# This function should return a sentence that states the *survival rate*
# (# survived / # in group) of adult men and "women and children" in that
# ticketing class.
# It should read (for example):
# >"Of Crew class, 87% of women and children survived and 22% of men survived."
#
# This is a complicated function! We recommend the following approach:
# - Filter for all rows representing the given ticketing class and save the
#   new data frame to a variable
# - Using this data frame, filter for all rows representing Adult Males
# - Find the total number of men and total number of male survivors to
#   calculate the survival rate
# - Likewise, use the data frame to filter for all Children and Adult Females
# - Perform the above calculation for this group as well
#
# Other approaches are also acceptable, please comment to explain what you do!
survival_rate <- function(class, dataframe) {
  classs <- dataframe[dataframe$Class == class, ]
  males <- classs[classs$Sex == "Male" & classs$Age ==
    "Adult", ]
  total_males <- sum(males$Freq)
  male_survivors <- males[males$"Survived" == "Yes", ]
  total_male_survivors <- sum(male_survivors$Freq)
  male_survival_rte <- total_male_survivors / total_males
  wac <- sum(classs$Freq) - total_males
  wac_survivors <- 
    sum(classs[classs$Survived == "Yes", "Freq"]) - total_male_survivors
  wac_survival_rte <- wac_survivors / wac
  male_survival_rate <- paste0(round(male_survival_rte * 100), "%")
  wac_survival_rate <- paste0(round(wac_survival_rte * 100), "%")
  paste(
    "Of", class, "class,", wac_survival_rate, 
    "of women and children survived and",
    male_survival_rate, "of men survived."
  )
}


# Create variables `first_survived`, `second_survived`, `third_survived` and
# `crew_survived` by passing each class and the `titanic_df` data frame
# to your `survival_rate` function
first_survived <- survival_rate("1st", titanic_df)
second_survived <- survival_rate("2nd", titanic_df)
third_survived <- survival_rate("3rd", titanic_df)
crew_survived <- survival_rate("Crew", titanic_df)

# What notable differences do you observe in the survival rates across classes?
# Note at least 2 observations.
# I noticed that the lower the class, the general lower rates of survival. I
# also noticed that the crew class was an exception and had a survival rate
# close to 1st and 2nd class.


# What notable differences do you observe in the survival rates between the
# women and children versus the men in each group?
# Note at least 2 observations.
# I noticed that generally, the survival rate of women and children was
# generally much higher than that of men. I also noticed that 2nd class seemed
# to be an extreme case, as only 8% versus 89% of men to women and children
# survived.


### Reading in Data ###########################################################

# In this section, you'll work with .csv data of life expectancy by country
# First, download the csv file of `Life Expectancy` data from GapMinder:
# https://www.gapminder.org/data/
# You should save the .csv file into your `data` directory

# Before getting started, explore the GapMinder website to better understand
# the *original* source of the data (e.g., who calculated these estimates)
# Place a brief summary of the each data source here (e.g., 1 - 2 sentences
# per data source)
# Mattias Lindgren - Mattias Lindgren made several estimates regarding life
# expectancy after traumatic historical events in a variety of sources for
# time period 1800-1970.
# IHME - Using 1990-2017 data from an IHME study, GapMinder used annual
# estimates from research data for 1970-2016.
# UN - GapMinder used forecasts from "World Population Prospects 2019", which
# includes annual data indicators on world populations.


# Using the `read.csv` function, read the life_expectancy_years.csv file into
# a variable called `life_exp`. Make sure not to read strings as factors
life_exp <- read.csv("life_expectancy_years.csv", stringsAsFactors = FALSE)

# Write a function `get_col_mean()` that takes a column name and a data frame
# and returns the mean of that column. Make sure to properly handle NA values
# Hint: `mean()` takes in an argument called `na.rm`
get_col_mean <- function(column, dataframe) {
  mean <- mean(dataframe[[column]], na.rm = TRUE)
  return(mean)
}

# Create a list `col_means` that has the mean value of each column in the
# data frame (except the `Country` column). You should use your function above.
# Hint: Use an `*apply` function (lapply, sapply, etc.)
years <- life_exp[-1]
col_means <- lapply(colnames(years)[1:ncol(years)], get_col_mean,
                    dataframe = years)

# Create a variable `avg_diff` that is the difference in average country life
# expectancy between 1800 and 2018
avg_diff <- get_col_mean("X2018", life_exp) - get_col_mean("X1800", life_exp)

# Create a column `life_exp$change` that is the change in life
# expectancy from 2000 to 2018. Increases in life expectancy should
# be *positive*
life_exp$change <- life_exp$X2018 - life_exp$X2000

# Create a variable `most_improved` that is the *name* of the country
# with the largest gain in life expectancy. Make sure to filter NA values
# Hint: `max()` takes in an argument called `na.rm`
largest_gain <- life_exp[life_exp$change == max(life_exp$change, na.rm = TRUE),
                         "country"]
most_improved <- largest_gain[!is.na(largest_gain)]

# Create a variable `num_small_gain` that has the *number* of countries
# whose life expectance has improved less than 1 year between 2000 and 2018
# Make sure to filter NA values
# Hint: Lookup `is.na()`
num_small_gain <- length(life_exp$country[life_exp$change < 1 & !is.na(life_exp$change)])


# Write a function `country_change()` that takes in a country's name,
# two years as numbers (not strings), and the `life_exp` data frame
# Parameters should be written *in the a"bove order*
# It should return the phrase:
# "Between YEAR1 and YEAR2, the life expectancy in COUNTRY went DIRECTION by
# SOME_YEARS years".
# Make sure to properly indictate the DIRECTION as "up" or "down"
# Hint: Use an if/else statement to help compute DIRECTION
country_change <- function(country_name, year1, year2, dataframe) {
  year_1 <- paste0("X", year1)
  year_2 <- paste0("X", year2)
  y1 <- dataframe[dataframe$country == country_name, year_1]
  y2 <- dataframe[dataframe$country == country_name, year_2]
  diff <- y2 - y1
  if (diff > 0) {
    direction <- "up"
  }
  else if (diff < 0) {
    direction <- "down"
  }
  difference <- abs(diff)
  paste(
    "Between", year1, "and", year2, ", the life expectancy in",
    country_name, "went",
    direction, "by", difference, "years."
  )
}

# Using your `country_change()` function, create a variable `sweden_change`
# that is the change in life expectancy from 1960 to 1990 in Sweden
sweden_change <- country_change("Sweden", 1960, 1990, life_exp)

# Write a function `compare_change()` that takes in two country names and your
# `life_exp` data frame as parameters, and returns a sentence that describes
# their change in life expectancy from 2000 to 2018 (the `change` column)
# For example, if you passed the values "China", and "Bolivia" to you function,
# It would return this:
# "The country with the bigger change in life expectancy was China (gain=6.9),
#  whose life expectancy grew by 0.6 years more than Bolivia's (gain=6.3)."
# Make sure to round your numbers to one digit (though only after calculations)
# Hint: Use an if/else statement to paste the countries in the correct order
compare_change <- function(country_1, country_2, dataframe) {
  country_1_change <- dataframe[dataframe$country == country_1, "change"]
  country_2_change <- dataframe[dataframe$country == country_2, "change"]
  country_diff <- country_1_change - country_2_change
  if (country_diff > 0) {
    bigger_country <- country_1
    bigger_gain <- round(country_1_change, digits = 1)
    diff <- round(country_diff, digits = 1)
    smaller_country <- country_2
    smaller_gain <- round(country_2_change, digits = 1)
  }
  else if (country_diff < 0) {
    bigger_country <- country_2
    bigger_gain <- round(country_2_change, digits = 1)
    diff <- round(abs(country_diff), digits = 1)
    smaller_country <- country_1
    smaller_gain <- round(country_1_change, digits = 1)
  }
  paste(
    "The country with the bigger change in life expectancy was",
    bigger_country, "(gain=", bigger_gain, "), whose life expectancy grew by ",
    diff, "years more than", smaller_country, " (gain=", smaller_gain, ")."
  )
}

# Using your `bigger_change()` function, create a variable `usa_or_france`
# that describes who had a larger gain in life expectancy (the U.S. or France)
usa_or_france <- compare_change("United States", "France", life_exp)

# Write your `life_exp` data.frame to a new .csv file to your
# data/ directory with the filename `life_exp_with_change.csv`.
# Make sure not to write row names.
write.csv(life_exp, "data\\life_exp_with_change.csv", row.names = FALSE)
