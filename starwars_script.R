#call on tidyverse packages
library(tidyverse)

# View the data frame from original data, from SWAPI, the Star Wars API, 
#https://swapi.py4e.com/, has been revised to reflect additional research into 
#gender and sex determinations of characters.
View(starwars)
?starwars

# filter by height and mass and view
starwars %>%
  filter(height > 150 & mass < 200) %>%
  mutate(height_in_meters = height/100) %>%
  select(height_in_meters, mass) %>%
  arrange(mass) %>%
  #View()
  plot()

# Cleaning and selecting data
starwars %>%
  select(name, height, mass)

# Can select columns that end with 'color'
starwars %>%
  select(ends_with("color"))

# Change the variable or column order
starwars %>%
select(name, mass, height, everything())

# Change the variable or column order
starwars %>%
  select(name, mass, height, everything()) %>%
  head()

#Change the variable name
starwars %>%
  rename("characters" = "name") %>%
  head()

#Change the variable type
# To see what kind of variable hair_color is (it is a character variable)
class(starwars$hair_color)
# to assign it or let R know it is a categorical character or string
starwars$hair_color <- as.factor(starwars$hair_color)
class(starwars$hair_color)

# Another way to change the type is using function mutate, it means to change
# or write over. Here change hair_color back to a character type from factor
starwars %>%
  mutate(hair_color = as.character(hair_color)) %>%
  glimpse()

#Changing factor levels, factor automatically are sorted in alphabetical order,
#but sometimes you don't want them to do that like months of the year
# Create a new data frame and assign it to starwars
df <- starwars
# Make the sex variable a factor
df$sex <- as.factor(df$sex)
#check to see if changed to factor
class(df$sex)
# Ask for the levels (df$sex)
levels(df$sex)
# these will automatically be in alphabetical order
# if you want to change the default order, you need to mutate the factors
df <- df %>%
  mutate(sex = factor(sex, levels = c("male", "female", "hermaphroditic", "none"
  )))
levels(df$sex)

# Filter rows
starwars %>%
  # select or highlight the columns that you want
  select(mass, sex) %>%
  # filter out the rows you want by a condition to be met
  filter(mass < 55 & sex == "male")

# To change the names of row observations, ex. change male to man
starwars %>%
  select(sex) %>%
  mutate(sex = recode(sex, "male" = "man", "female" = "woman"))

# Dealing with missing data ( mean will return NA if there is missing data)
mean(starwars$height, na.rm = TRUE)

#Dealing with duplicates
Names <- c("Peter", "John", "Andrew", "Peter")
Age <- c(22, 33, 44, 22)
friends <- data.frame(Names, Age)
friends %>% 
  distinct()

#Create or change a variablstarware (mutate)
starwars %>%
  mutate(height_m = height/100) %>%
  select(name, height, height_m)

#Conditional change (if_else)
starwars %>%
  mutate(height_m = height/100) %>%
  select(name, height, height_m) %>%
  mutate(tallness =
           if_else(height_m < 1,
                   "short",
                   "tall"))

# Reshape data with Pivot wider
library(gapminder)
view(gapminder)

data <- select(gapminder, country, year, lifeExp)
View(data)
# will consolidate the countries into one row, makes the years columns and 
# puts the life expectacy under the years
wide_data <- data %>%
  pivot_wider(names_from = year, values_from = lifeExp)
View(wide_data)

# to do the reverse of above
#reshape data with Pivot longer
long_data <- wide_data %>%
  pivot_longer(2:13,
               names_to = "year",
               values_to = "lifeExp"
  )
View(long_data)

#Visualize data
############################

plot(pressure)

# Bar plots
# ggplot is the function but ggplot2 is the library
# is in tidyverse
ggplot(data = starwars,
       #aes means estetics like mapping on an x and y access
       mapping = aes(x = gender)) + geom_bar()

#Histograms
starwars %>%
  drop_na(height) %>%
  ggplot(mapping = aes(x = height)) +
  geom_histogram()

# this is the same as above
starwars %>%
  drop_na(height) %>%
  ggplot(aes(height)) +
  geom_histogram()

# Box plots
# graphical representation of the distribution of numerical
#data through five key summary statistics. These statistics
#are the minimum, first quartile (Q1), median (or second
#quartile, Q2), third quartile (Q3), and maximum.
starwars %>%
  drop_na(height) %>%
  ggplot(mapping = aes(x = height)) +
  geom_boxplot(fill = "steelblue") +
  theme_bw() +
  labs(title = "Boxplot of height",
       x = "Heght of character")

# Density Plots
# the probability of an observation at any particular value
starwars %>%
  drop_na(height) %>%
  filter(sex %in% c("male", "female")) %>%
  ggplot(mapping = aes(x = height,
                       color =sex,
                       fill = sex)) +
  # alpha is how dark the coloring is
  geom_density(alpha = .2)+
  theme_bw()

# Scatter plots
starwars %>%
  filter(mass < 200) %>%
  # x axis is height, y is mass, colored by sex
  ggplot(aes(height, mass, color = sex)) +
  # geom_point is the scatter plot
  geom_point(size = 5, alpha = 0.5)+
  theme_minimal()
labs(title = "Height and mass by sex")

# Smoothed model
starwars %>%
  filter(mass < 200) %>%
  ggplot(aes(height, mass, color = sex)) +
  geom_point(size = 3, alpha = 0.8)+
  # adds a smooth linear model to plot
  geom_smooth()+
  # creates a different box for each of the sexes
  facet_wrap(~sex) +
  theme_bw()
labs(title = "Height and mass by sex")

# P-value:
# running a T test gets you the P-value
# start with a null hypothesis, that there is no difference
#btwn two sets of data, Ex. life expectancy in africa and
# europe. If there was no difference, how likely would it be
# you would get a sample with the difference that you got
# when you plotted the two next to each other.
library(gapminder)
View(gapminder)
#t_test_plot
# can't get above to plot

gapminder %>%
  filter(continent %in% c("Africa", "Europe")) %>%
  t.test(lifeExp ~ continent, data = .,
         altrnative = "two.sided",
         paired = FALSE)
# returns data:  lifeExp by continent
#t = -49.551, df = 981.2, p-value < 2.2e-16

# ANOVA
#ANOVA_plot, can't get to plot? 

gapminder %>%
  filter(year == 2007) %>%
  filter(continent %in% c("Americas", "Europe", "Asia")) %>%
  aov(lifeExp ~ continent, data = .) %>%
  summary()
#            Df Sum Sq Mean Sq F value   Pr(>F)    
#continent    2  755.6   377.8   11.63 3.42e-05 ***
  #Residuals   85 2760.3    32.5
# the Pr(>F) is p value

# TukeyHSD()
gapminder %>%
  filter(year == 2007) %>%
  filter(continent %in% c("Americas", "Europe", "Asia")) %>%
  aov(lifeExp ~ continent, data = .) %>%
  TukeyHSD() %>%
  plot()

# Chi Squared, categorical data
chi_plot

head(iris)
head(iris)
flowers <- iris %>%
  mutate(size = cut(Sepal.Length,
                    breaks = 3,
                    labels = c("Small", "Medium", "Large"))) %>%
  select(Species, size)
View(flowers)

#Chi Squared goodness of fit test
# the null hypothesis is that there are equal proportions of
# small, medium, large sepal length, the alternative is that
# they are not equal,
flowers %>%
  select(size) %>%
  table() %>%
  chisq.test()

# Are we seeing the number of each species be the same for
#small, medium and large?
# it will show that one variable is dependent on another,
# this species is ususally small etc
# Chi squared test of independence
flowers %>%
  table() %>%
chisq.test()

#Linear Model
head(cars, 10)
cars %>%
  lm(dist ~ speed, data = .) %>%
  summary()