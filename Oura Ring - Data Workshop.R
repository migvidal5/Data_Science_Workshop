library(tidyverse)
library(psych)



df <- read_csv('Oura Ring Data/oura_2019_trends.csv')

# head() will show the first 6 rows in the dataframe
head(df)

# What if we only wanted to see the first 3 rows?

head(df, n = 3)

# tail() will show the last 5 rows in the dataframe

tail(df)

# Notice that there are NaN (Not a Number) values in the dataframe. Let's find out how many rows have NaN values.
# By using .isna() each value in the dataframe is tested to determine if it is a NaN value.
# The result is a boolean showing True or False results. True means the value is NaN.

is.na(df)

# Counting the number of True booleans would be tedious if done by hand.
# Luckily colSums does the counting for us.

colSums(is.na(df))

# Lets drop all rows with NaN values

drop_na(df)

# Lets make sure all NaN values have been dropped

colSums(is.na(df))

# The reason the NaN values are still there is because the dataframe was never updated
# In order to update the dataframe the equal sign must be used.

df <- drop_na(df)

colSums(is.na(df))

# Now lets see what each column looks like by using describe() from the psych library

describe(df)

# Now lets visualize these results shown above with whisker plots
# First, the date column needs to be dropped. Let's create a new dataframe without th date column.

keep <- c('average_rhr', 'sleep_score', 'activity_score', 'readiness_score')

df1 <- df[keep]

boxplot(df1)

# Notice one column name is not included in the x-axis of the visual. That is due to the name being too long.
# Let's create a new viz, with shorter labels in the x-axis.

boxplot(df1, names=c('average rhr', 'sleep', 'activity', 'readiness'))

# Now lets put better aesthetics on the boxplots using ggplot2
# ggplot2 requires the dataframe to be condensed, limiting the number of columns
# This is done with melt()



library(reshape2)
data_mod <- melt(df, id.vars='date', 
                  measure.vars=c('average_rhr', 'sleep_score', 'activity_score', 'readiness_score'))

# Have a look at the new dataframe created
# creating a plot
p <- ggplot(data_mod) +
  geom_boxplot(aes(x=variable, y=value, color=variable))

# printing the plot
print(p)

