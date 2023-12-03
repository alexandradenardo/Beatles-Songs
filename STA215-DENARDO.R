## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################

# EXAMINE QUANT_VAR1
table(data$Spotify_Streams) 
mean(data$Spotify_Streams) 
sd(data$Spotify_Streams)
summary(data$Spotify_Streams)
describe(data$Spotify_Streams)

table(data$Frequency_of_Love) 
mean(data$Frequency_of_Love) 
sd(data$Frequency_of_Love)
summary(data$Frequency_of_Love)
describe(data$Frequency_of_Love)

table(data$Chart_Popularity)
summary(data$Chart_Popularity)
describe(data$Chart_Popularity)

table(data$Lead_Singer)

table(data$Theme)
##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################

table(data$Chart_Popularity, data$Lead_Singer)
table(data$Chart_Popularity, data$Theme)
table(data$Lead_Singer, data$Theme)
##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################

chisq.test(table(data$Chart_Popularity, data$Lead_Singer))
chisq.test(table(data$Chart_Popularity, data$Theme))
chisq.test(table(data$Lead_Singer, data$Theme))
chisq.test(table(data$Theme, data$Lead_Singer))
##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
anova <- aov(Spotify_Streams ~ Lead_Singer, data = data)
summary(anova)
anova <- aov(Spotify_Streams ~ Theme, data = data)
summary(anova)
anova <- aov(Spotify_Streams ~ Chart_Popularity, data = data)
summary(anova)
anova <- aov(Frequency_of_Love ~ Lead_Singer, data = data)
summary(anova)
anova <- aov(Frequency_of_Love ~ Theme, data = data)
summary(anova)
anova <- aov(Frequency_of_Love ~ Chart_Popularity, data = data)
summary(anova)
plot(data$Frequency_of_Love, data$Chart_Popularity)
##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################

cor(data$Frequency_of_Love, data$Spotify_Streams)
plot(data$Frequency_of_Love, data$Spotify_Streams)
##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
linear_relationship <- lm(Frequency_of_Love ~ Spotify_Streams, data = data)
summary(linear_relationship)

##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################

plot(data$Frequency_of_Love, data$Spotify_Streams) 
abline(linear_relationship, col = "red")
abline(h = 51.56271, col = "green")
abline(v = 4.491525, col = "green")
eliminated<- subset(warpbreaks, warpbreaks$breaks > (Q[1] - 1.5*iqr) & warpbreaks$breaks < (Q[2]+1.5*iqr))
plot(data$Spotify_Streams, data$Frequency_of_Love)
abline(linear_relationship, col = "red")
abline(h = 4.491525, col = "green")
abline(v = 51.56271, col = "green")

##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
plot(data$Spotify_Streams, residuals(linear_relationship))
abline(v=51.56271, col = "green")
mean(residuals(linear_relationship))
abline(h=5.802503e-17, col = "green")
abline(linear_relationship, col = "red")