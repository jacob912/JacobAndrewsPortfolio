### DATA ANALYSIS ASSIGNMENT ###
### Group: Anistyn Grant, Sara Macintosh, Sarah Hitchcock, Jacob Andrews ###

rm(list=ls())
setwd("/Users/jonandrews/Downloads/POLI209")
library(dplyr)
library(tidyr)
library(ggplot2)

data <- read.csv("POLI209SurveyData.csv")
data.numerical <- read.csv("POLI209DataNumerical.csv")

clean.data <- data[-c(1:2),-c(1:13, 16:17)]
clean.data.num <- data.numerical[-c(1:2),-c(1:13, 16:17)]

unique(clean.data.num$Location)
clean.data.num$in_state[clean.data.num$Location == "North Carolina"] <- 1
clean.data.num$in_state[clean.data.num$Location == "NC"] <- 1
clean.data.num$in_state[clean.data.num$Location == "North Carolina "] <- 1
clean.data.num$in_state[clean.data.num$Location == "North carolina"] <- 1
clean.data.num$in_state[clean.data.num$Location == "NORTH CAROLINA"] <- 1
clean.data.num$in_state[clean.data.num$Location == "nc"] <- 1
clean.data.num$in_state[clean.data.num$Location == "MC"] <- 1
clean.data.num$in_state[is.na(clean.data.num$in_state) == TRUE] <- 0

clean.data.num$College.Voters[clean.data.num$College.Voters == 2] <- 0

clean.data.num$College.Voters <- as.numeric(clean.data.num$College.Voters)

### Looking at Lat and Long to determine if people were actually in state or not ###
clean.data.num[38, 14] = 1
clean.data.num[39, 14] = 1
clean.data.num[40, 14] = 1
clean.data.num[45, 14] = 1
clean.data.num[49, 14] = 1
clean.data.num[52, 14] = 1
clean.data.num[54, 14] = 1
clean.data.num[58, 14] = 1
clean.data.num[64, 14] = 1
clean.data.num[72, 14] = 1
clean.data.num[73, 14] = 1

### HYPOTHESES #1: Out of state students vote less than in state students ###
### Independent Variable: in_state ###
### Dependent Variable: College.voters ###

### Measure of Central Tendency: Mean ###
mean(clean.data.num$in_state, na.rm = T)
mean(clean.data.num$College.Voters, na.rm = T)

### Measure of Dispersion: Variance ###
var(clean.data.num$in_state, na.rm = T)
var(clean.data.num$College.Voters, na.rm = T)

### Descriptive Statistics Table ###
#datasummary(Heading("In State")*in_state + Heading("Voting")*College.Voters ~ (Mean + SD + Median), data = clean.data.num)

### Statistical test: T-Test ###
?t.test # Pulls up package documentation for t.test()
t.test(clean.data.num$College.Voters[clean.data.num$in_state == 1], clean.data.num$College.Voters[clean.data.num$in_state == 0])

### Description of results: ###
# From the T-Test that we used for our hypothesis #1, we got a p-value of .105, which isn't in the .05 range to say it is definitely a significant relationship, but it is close. From this, we can say there is some evidence to say that out of state students vote less than in state students. The mean values that we got from the T-Test told us that 81.5% of people who reported in-state voted and that 62.5% of people who reported out-of-state voted. 

### HYPOTHESES #2: Out of state students are more likely to use mail-in voting. ###
### Independent Variable: in_state ###
### Dependent Variable: mail_in ###
clean.data.num$mail_in[clean.data.num$Voting.in.College == 2] <- 0
clean.data.num$mail_in[clean.data.num$Voting.in.College == 1] <- 0
clean.data.num$mail_in[clean.data.num$Voting.in.College == 3] <- 1
### Measure of Central Tendency: Mean ###
mean(clean.data.num$in_state, na.rm = T)
mean(clean.data.num$mail_in, na.rm = T)

### Measure of Dispersion: Variance ###
var(clean.data.num$in_state, na.rm = T)
var(clean.data.num$mail_in, na.rm = T)

### Descriptive Statistics Table ###
#datasummary(Heading("In State")*in_state + Heading("Mail in")*mail_in ~ (Mean + SD + Median), data = clean.data.num)

### Statistical test: T-Test ###
?t.test # Pulls up package documentation for t.test()
t.test(clean.data.num$mail_in[clean.data.num$in_state == 1], clean.data.num$mail_in[clean.data.num$in_state == 0])


### Description of results: ###
# From the T-Test that we used for Hypothesis #2, we got the p-value result of .3687, which is significantly higher than the threshold of .05. This means that we cannot support the argument that out-of-state students use mail-in voting more than in-state students. The mean values that we got from the T-Test told us that 20.45% of people who reported in-state reported using mail-in voting and that 33.33% of people who reported out-of-state reported mail-in voting.

newdata <- clean.data %>% 
  filter(College.Voters!="")

table(newdata$College.Voters)
ggplot(subset(newdata, !is.na(College.Voters)), aes(x=as.factor(College.Voters))) + 
  geom_bar(fill = "sky blue", width = 1, color = "black") + 
  labs(title = "Voting Status of Respondents", 
       x = "Did They Vote?",
       y = "Frequency") +
  theme_bw()  + 
  theme(plot.title = element_text(hjust = 0.5), # Center title 
        axis.text.x = element_text(size = 8)) # Change size of x-axis text

table(clean.data.num$in_state, na.rm = T)
ggplot(subset(clean.data.num, !is.na(in_state)), aes(x=as.factor(in_state))) + 
  geom_bar(fill = "sky blue", width = 1, color = "black") + 
  labs(title = "Resident Status of Respondents", 
       x = "Resident Status",
       y = "Frequency") + scale_x_discrete(labels = c("Out of State", "In State")) +
  theme_bw()  + 
  theme(plot.title = element_text(hjust = 0.5), # Center title 
        axis.text.x = element_text(size = 8)) # Change size of x-axis text

newdata2 <- clean.data %>% 
  filter(Voting.in.College!="")

table(clean.data$Voting.in.College)

ggplot(subset(newdata2, !is.na(Voting.in.College)), aes(x=as.factor(Voting.in.College))) + 
  geom_bar(fill = "sky blue", width = 1, color = "black") + 
  labs(title = "Voting Methods of Respondents", 
       x = "Voting Method",
       y = "Frequency") +
  theme_bw()  + 
  theme(plot.title = element_text(hjust = 0.5), # Center title 
        axis.text.x = element_text(size = 8)) # Change size of x-axis 

newdata3 <- clean.data %>% 
  filter(Not.Voting!="Other", Not.Voting != "")

table(clean.data$Not.Voting)

ggplot(subset(newdata3, !is.na(Not.Voting)), aes(x=as.factor(Not.Voting))) + 
  geom_bar(fill = "sky blue", width = 1, color = "black") + 
  labs(title = "Why Did Respondents Not Vote?", 
       x = "Reason",
       y = "Frequency") + scale_x_discrete(labels = c("All 3", "No Trans + Time", "Transportation", "Out-Of-State", "Time")) +
  theme_bw()  + 
  theme(plot.title = element_text(hjust = 0.5), # Center title 
        axis.text.x = element_text(size = 8)) # Change size of x-axis 