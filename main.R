library(tidyverse)
library(lubridate)
library(magrittr)

# What's the most important factors to define the necessity of mental treatment?
# Is it possible to build any ML model based on those feature to evaluate the risk of mental health problems?

df = read.csv(file=here::here("data/survey.csv"))
df %>% str(max.level=1)

df = df %>% mutate(Timestamp = as_datetime(Timestamp))

df$mental_health_interview %>% levels()
df$comments %>% levels()

df$obs_consequence %>% levels()

treatment_state = df %>% 
    group_by(state, treatment) %>%
    summarise(n = n())






