library(tidyverse)
# to see dataframe
View(msleep)
# to see summary of variable, data type, row info
glimpse(msleep)
# summary of first 6 rows
head(msleep)
# to see the data type of the variable name in the dataframe msleep
class(msleep$name)
# number of variables in dataframe
length(msleep)
# counts how many observations or rows there are for the name variable
length(msleep$name)
# to get a list of all of the variables
names(msleep)
# to see the unique factors in the category or column vore
unique(msleep$vore)
# creating a variable called missing that holds all of the rows that have
#missing data
missing <- !complete.cases(msleep)
msleep[missing, ]

# Describing data
View(msleep)

# Range / spread
min(msleep$awake)
max(msleep$awake)
range(msleep$awake)
IQR(msleep$awake)

#Centrality
mean(msleep$awake)
median(msleep$awake)

# To get a summary of the statistical descriptions
summary(msleep$awake)
# if you just want a summary for 2 variables
msleep %>%
  select(awake, sleep_total) %>%
  summary()

# Summarize your data
msleep %>%
  # drop missing values from vore
  drop_na(vore) %>%
  # group all of the vore types
  group_by(vore) %>%
  # Create new variable or column headings
  summarise(Lower = min(sleep_total),
            Average = mean(sleep_total),
            Upper = max(sleep_total),
            Difference =
              max(sleep_total)-min(sleep_total)) %>%
  arrange(Average) %>%
  View()

# Create tables
# will count the number of observation types in a column
table(msleep$vore)

# Create tables
# will count the number of observation types in a column
table(msleep$vore)
# to see how many of the vores are Rodents and h.m. Primates
msleep %>%
  # select the vore and order columns
  select(vore, order) %>%
  # filter the order column by Rodentia and Primates
  filter(order %in% c("Rodentia", "Primates")) %>%
  table()

