rm(list=ls())

setwd("/Users/jonandrews/Downloads/UNCBB")

library(dplyr)
library(ggplot2)
library(tidyverse)

seager.data <- read.csv("Kyle_Seager_Data.csv", stringsAsFactors = F)

RHP_BPIP <- seager.data %>%
  filter(p_throws == "R", type == "X")
RHP_Hits <- seager.data %>%
  filter(p_throws == "R", type == "X", events == "single"| events == "double"| events == "triple"| events == "home_run")
RHP_EOPA <- seager.data %>%
  filter(p_throws == "R", events != "")
RHP_BADF <- RHP_EOPA %>%
  filter(events != "walk", events != "hit_by_pitch", events != "sac_fly")
RHP_BABIP_DF <- seager.data %>%
  filter(p_throws == "R", events != "home_run", events != "strikeout", events != "", events != "strikeout_double_play", events != "walk")

LHP_BPIP <- seager.data %>%
  filter(p_throws == "L", type == "X")
LHP_Hits <- seager.data %>%
  filter(p_throws == "L", type == "X", events == "single"| events == "double"| events == "triple"| events == "home_run")
LHP_EOPA <- seager.data %>%
  filter(p_throws == "L", events != "")
LHP_BADF <- LHP_EOPA %>%
  filter(events != "walk", events != "hit_by_pitch", events != "sac_fly")
LHP_BABIP_DF <- seager.data %>%
  filter(p_throws == "L", events != "home_run", events != "strikeout", events != "", events != "strikeout_double_play", events != "walk")

BABIP_RHP = sum(RHP_BABIP_DF$events == "single"| RHP_BABIP_DF$events == "double"| RHP_BABIP_DF$events == "triple")/nrow(RHP_BABIP_DF)
BABIP_LHP = sum(LHP_BABIP_DF$events == "single"| LHP_BABIP_DF$events == "double"| LHP_BABIP_DF$events == "triple")/nrow(LHP_BABIP_DF)

SLG_RHP = (sum(RHP_BPIP$events == "single") + 2*sum(RHP_BPIP$events == "double") + 3*sum(RHP_BPIP$events == "triple") + 4*sum(RHP_BPIP$events == "home_run"))/nrow(RHP_BPIP)
SLG_LHP = (sum(LHP_BPIP$events == "single") + 2*sum(LHP_BPIP$events == "double") + 3*sum(LHP_BPIP$events == "triple") + 4*sum(LHP_BPIP$events == "home_run"))/nrow(LHP_BPIP)

OBP_RHP = (sum(RHP_EOPA$events == "single"| RHP_EOPA$events == "double"| RHP_EOPA$events == "triple"| RHP_EOPA$events == "home_run"|RHP_EOPA$events == "walk"|RHP_EOPA$events == "hit_by_pitch"))/nrow(RHP_EOPA)
OBP_LHP = (sum(LHP_EOPA$events == "single"| LHP_EOPA$events == "double"| LHP_EOPA$events == "triple"| LHP_EOPA$events == "home_run"|LHP_EOPA$events == "walk"|LHP_EOPA$events == "hit_by_pitch"))/nrow(LHP_EOPA)

BA_RHP = (sum(RHP_BADF$events == "single"| RHP_BADF$events == "double"| RHP_BADF$events == "triple"| RHP_BADF$events == "home_run")/nrow(RHP_BADF))
BA_LHP = (sum(LHP_BADF$events == "single"| LHP_BADF$events == "double"| LHP_BADF$events == "triple"| LHP_BADF$events == "home_run")/nrow(LHP_BADF))

EVLO_RHP <- mean(RHP_BPIP$launch_speed, na.rm=TRUE)
EVLO_LHP <- mean(LHP_BPIP$launch_speed, na.rm=TRUE)

LANG_RHP <- mean(RHP_BPIP$launch_angle, na.rm=TRUE)
LANG_LHP <- mean(LHP_BPIP$launch_angle, na.rm=TRUE)

EstBA_RHP <- mean(RHP_BPIP$estimated_ba_using_speedangle, na.rm =TRUE)
EstBA_LHP <- mean(LHP_BPIP$estimated_ba_using_speedangle, na.rm =TRUE)

RHP_walks <- sum(RHP_EOPA$events == "walk")
LHP_walks <- sum(LHP_EOPA$events == "walk")

RHP_SO <- sum(RHP_EOPA$events == "strikeout")
LHP_SO <- sum(LHP_EOPA$events == "strikeout")

RHP_PA <- nrow(RHP_BADF)
LHP_PA <- nrow(LHP_BADF)

RHP_Hits$events = factor(RHP_Hits$events, levels = c('single', 'double', 'triple', 'home_run'))
LHP_Hits$events = factor(LHP_Hits$events, levels = c('single', 'double', 'triple', 'home_run'))

barBPIP_RHP <- ggplot(RHP_Hits, aes(x=events, fill=events)) +
  geom_bar() + 
  labs(title = "Hits Against RHP",
       y = "Count", x = "Type of Hit")
barBPIP_RHP
barBPIP_LHP <- ggplot(LHP_Hits, aes(x=events, fill=events)) +
  geom_bar() + 
  labs(title = "Hits Against LHP",
       y = "Count", x = "Type of Hit")
barBPIP_LHP
