library(tidyverse)
library(psych)



df <- read_csv('Oura_Ring _Sample.csv')
sapply(df,class)
# How do you find the sum of duplicate data for just one column?

duplicated(df$activity_score)

sum(duplicated(df$activity_score))

# How do you get the summary statistics for the data frame?

describe(df)

#Create a barchart

library(reshape2)
data_mod <- melt(df, id.vars='date', 
                 measure.vars=c('average_rhr', 'sleep_score', 'activity_score', 'readiness_score'))

ggplot(data_mod, aes(x=variable, y = value, fill = variable)) +
  geom_bar(stat = "summary",
           fun = "mean")

#Create subplots
#Install griExtra package

library(gridExtra)

p1 <- ggplot(df, aes(x=average_rhr)) +
  geom_histogram(color = 'red', fill = 'lightblue')

p2 <- ggplot(df, aes(x=sleep_score)) +
  geom_histogram()

p3 <- ggplot(df, aes(x=activity_score)) +
  geom_histogram()

p4 <- ggplot(df, aes(x=readiness_score)) +
  geom_histogram()

grid.arrange(p1, p2, p3, p4, nrow = 2)


# Box Plot/Whisker Plot
ggplot(data_mod, aes(x=variable, y=value, color=variable)) +
  geom_boxplot()

dim(df)



Q <- quantile(df$average_rhr, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(df$average_rhr, na.rm = TRUE)

up <- Q[2]+1.5*iqr # Upper Range  
low <- Q[1]-1.5*iqr # Lower Range

eliminated<- subset(df, df$average_rhr > (Q[1] - 1.5*iqr) & df$average_rhr < (Q[2]+1.5*iqr))

dim(eliminated)

#Melt data
data_mod2 <- melt(eliminated, id.vars='date', 
                 measure.vars=c('average_rhr', 'sleep_score', 'activity_score', 'readiness_score'))

ggplot(data_mod2, aes(x=variable, y=value, color=variable)) +
  geom_boxplot()

#Correlation

keep <- c('average_rhr', 'sleep_score', 'activity_score', 'readiness_score')

df_new <- eliminated[keep]

corr <- cor(df_new, use = "complete.obs") #ignores NaN values

corr1 <- cor(df_new)

melted_df <- melt(corr)

ggplot(data = melted_df, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

#Adding r coefficient to correlation matrix

ggplot(data = melted_df, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "white")


#Adding regression line to scatter plot

ggplot(data = df_new, aes(x= average_rhr, y = readiness_score)) +
  geom_point() +
  geom_smooth(method = lm)


