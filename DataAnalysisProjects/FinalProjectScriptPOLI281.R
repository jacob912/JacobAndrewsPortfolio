rm(list=ls())
setwd("~/POLI281")
library(ggplot2)
library(dplyr)
ces20 <- read.csv("ces20.csv", stringsAsFactors = TRUE)

#Part II
table(ces20$wait)
prop.table(table(ces20$wait))
mean(ces20$wait)
median(ces20$wait)

ces20$more10 <- ""
ces20$more10[ces20$wait < 3] <- FALSE
ces20$more10[ces20$wait >= 3 & ces20$wait != 6] <- TRUE
ces20$more10[ces20$wait == 6] <- NA

#Part III
ces20$more10int <- NA
ces20$more10int[ces20$more10 == "TRUE"] <- 1
ces20$more10int[ces20$more10 == "FALSE"] <- 0
ces20$more10int[is.na(ces20$more10)] <- NA

perc_more10 <- ces20 %>%
  group_by(state, region) %>%
  summarize(perc = mean(more10int, na.rm = TRUE))

perc_more10 <- perc_more10 %>%
  arrange(desc(perc))

perc_more10$state <- factor(perc_more10$state, levels = perc_more10$state)

perc_more10_p <- ggplot(perc_more10, aes(x=state, y= perc)) + 
  geom_col() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))

#Part IV
perc_more10region_p <- ggplot(perc_more10, aes(x=state, y= perc, fill = region)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90))

# Part V

ces20$conserv_vote <- NA
ces20$conserv_vote[ces20$vote2020==1] <- "conservative"
ces20$conserv_vote[ces20$vote2020==2] <- "liberal"
ces20$conserv_vote[ces20$vote2020==3] <- "other"
ces20$conserv_vote[ces20$vote2020==4] <- "other"
ces20$conserv_vote[ces20$vote2020==5] <- "other"

ces20$more10 <- NA
ces20$more10[ces20$wait < 3] <- FALSE
ces20$more10[ces20$wait >= 3 & ces20$wait != 6] <- TRUE
ces20$more10[ces20$wait == 6] <- NA

ces20$vote2020_cl[ces20$vote2020==1] <- "conservative"
ces20$vote2020_cl[ces20$vote2020==2] <- "liberal"
ces20$vote2020_cl[ces20$vote2020==3] <- "other"
ces20$vote2020_cl[ces20$vote2020==4] <- "other"
ces20$vote2020_cl[ces20$vote2020==5] <- "other"

wait_by_cl <- ces20 %>%
  group_by(vote2020_cl) %>%
  summarise(prop=mean(more10, na.rm = TRUE))
wait_by_cl

pt_5_bargraph <- ggplot(wait_by_cl, aes(x=vote2020_cl, y=prop)) + geom_col() + theme_bw()
pt_5_bargraph


#Part VI

ces20$race5 <- NA
ces20$race5[ces20$race == 1] <- "White"
ces20$race5[ces20$race == 2] <- "Black"
ces20$race5[ces20$race == 3] <- "Hispanic"
ces20$race5[ces20$race == 4] <- "Asian"
ces20$race5[ces20$race == 5] <- "Other"
ces20$race5[ces20$race == 6] <- "Other"
ces20$race5[ces20$race == 7] <- "Other"
ces20$race5[ces20$race == 8] <- "Other"

ces20$more10 <- NA
ces20$more10[ces20$wait < 3] <- FALSE
ces20$more10[ces20$wait >= 3 & ces20$wait != 6] <- TRUE
ces20$more10[ces20$wait == 6] <- NA


bargraph_wait_race <- ces20 %>%
  group_by(race5) %>%
  summarise(prop=mean(more10, na.rm = TRUE))

pt_6_bargraph <- ggplot(bargraph_wait_race, aes(x=race5, y=prop)) + geom_col() + theme_bw()


# Part VII

ces20$faminc4 <- NA
ces20$faminc4[ces20$faminc <= 4] <- "Lower Income"
ces20$faminc4[ces20$faminc >= 5 & ces20$faminc <= 8] <- "Middle Income"
ces20$faminc4[ces20$faminc >= 9 & ces20$faminc <= 12] <- "Upper Middle Income"

ces20$faminc4[ces20$faminc >= 13 & ces20$faminc <= 16] <- "High Income"

ces20$faminc4[ces20$faminc == 97] <- NA

prop.table(table(ces20$faminc4))

bargraph_wait_income <- ces20 %>%
  group_by(faminc4) %>%
  summarise(prop=mean(more10, na.rm = TRUE))

pt_7_bargraph <- ggplot(bargraph_wait_income, aes(x=faminc4, y=prop)) + geom_col() + theme_bw()

#Part VIII

bargraph_raceinc <- ces20 %>%
  group_by(race5, faminc4) %>%
  summarise(prop=mean(more10, na.rm = TRUE))

pt_8_bargraph <- ggplot(bargraph_raceinc, aes(x=race5, y=prop)) + geom_col() + theme_bw() + facet_wrap(~faminc4)

#Part IX

ces20$income_county[ces20$income_county > 150] <- NA

hist_countyinc <- ggplot(ces20, aes(x=income_county)) + geom_histogram()

ces20$density <- (ces20$county_pop / ces20$land_area ) / 1000

ces20$black <- 0
ces20$black[ces20$race5 == "Black"] <- 1

ces20$hispanic <- 0
ces20$hispanic[ces20$race5 == "Hispanic"] <- 1

ces20$asian <- 0
ces20$asian[ces20$race5 == "Asian"] <- 1

ces20$other <- 0
ces20$other[ces20$race5 == "Other"] <- 1

ces20$wait_reg <- ces20$wait
ces20$wait_reg[ces20$wait_reg == 6] <- NA

ces20$faminc_reg <- ces20$faminc
ces20$faminc_reg[ces20$faminc_reg == 97] <- NA

model1 <- lm(wait_reg ~ black + hispanic + asian + other, data =
               ces20)
model2 <- lm(wait_reg ~ black + hispanic + asian + other +
               faminc_reg + income_county + density, data = ces20)
summary(model1)
summary(model2)