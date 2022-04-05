library(tidyverse)
library(psych)



df <- read_csv('Oura_Ring _Sample.csv')

# How do you find the sum of duplicate data for just one column?

duplicated(df$activity_score)

sum(duplicated(df$activity_score))

# How do you get the summary statistics for the data frame?

describe(df)


# Merge two data frames based on the column that is in both sets.

df1 <- read_csv('Dummy Data 1.csv')

df2 <- read_csv('Dumm Data 2.csv')


df_merge <- merge(df1,df2)

df_merge1 <- merge(df1, df2, by = 'Name')

df_merge2 <- merge(df1, df2, by = c('Date', 'Name'))

# Now lets visualize these results shown above with whisker plots
# First, the date column needs to be dropped. Let's create a new dataframe without th date column.

keep <- c('average_rhr', 'sleep_score', 'activity_score', 'readiness_score')

df_new <- df[keep]

boxplot(df_new)

# Notice one column name is not included in the x-axis of the visual. That is due to the name being too long.
# Let's create a new viz, with shorter labels in the x-axis.

boxplot(df_new, names=c('average rhr', 'sleep', 'activity', 'readiness'))

#Distribution Plot with one variable

ggplot(df, aes(x=average_rhr)) +
  geom_histogram()

# Change binwidth

ggplot(df, aes(x=average_rhr)) +
  geom_histogram(binwidth = .2)


# Change number of bins

ggplot(df, aes(x=average_rhr)) +
  geom_histogram(bins = 5)

#Change color and add title

ggplot(df, aes(x=c('average_rhr', 'sleep_score'))) +
  geom_histogram(color = 'red', fill = 'lightblue') +
  ggtitle('This is a new title') +
  xlab('this is now the x axis') +
  ylab('new title')

#Scatter Plot

ggplot(df, aes(x = sleep_score, y = readiness_score)) +
  geom_point() +
  ggtitle('This is a new title') +
  xlab('this is now the x axis') +
  ylab('new title')

# Melt the Data

library(reshape2)
data_mod <- melt(df, id.vars='date', 
                 measure.vars=c('average_rhr', 'sleep_score', 'activity_score', 'readiness_score'))

# Distribution Plot

ggplot(data_mod, aes(x=value, fill = variable)) +
  geom_histogram()

# Box Plot/Whisker Plot
ggplot(data_mod, aes(x=variable, y=value, color=variable)) +
  geom_boxplot()

R.version
