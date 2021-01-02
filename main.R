library(tidyverse)
library(lubridate)
library(magrittr)
library(stringdist)

# What's the most important factors to define the necessity of mental treatment?
# Is it possible to build any ML model based on those feature to evaluate the risk of mental health problems?

#
# so Target variable is `treatment`
#

df = read.csv(file=here::here("data/survey.csv"))
df %>% str(max.level=1)

df = df %>% mutate(Timestamp = as_datetime(Timestamp))

df$mental_health_interview %>% levels()
df$comments %>% levels()

df$obs_consequence %>% levels()

df$treatment  %>% levels()

{ # TODO: transform gender to Male/Female Format
    df$Gender  %>%  levels()
    gender_table = df %>% 
        mutate(Gender = tolower(Gender)) %>% 
        group_by(Gender) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n))


    normalize_gender <- function(input){
#         input = "Female"
        input = tolower(input)
        gender_type = c("male", "female", "confusion")

        if(all(input == gender_type[1])) return(gender_type[1])
        if(all(input == gender_type[2])) return(gender_type[2])

        male_word_pool      = c("male", "man", "guy", "m")
        female_word_pool    = c("female", "woman", "girl", "f")
        confusing_word_pool = c("trans", "agender", "fluid", "queer", "binary", "na", "all", "p", "unsure")

        male_value   = min(stringdist::stringdist(input, male_word_pool,       "jaccard"))
        female_value = min(stringdist::stringdist(input, female_word_pool,     "jaccard"))
        confusion_value  = min(stringdist::stringdist(input, confusing_word_pool,  "jaccard"))

        i = which.min(c(male_value, female_value, confusion_value))

        return(gender_type[i])

    }

    df = df %>% 
        mutate(Gender = map(Gender, normalize_gender) %>% unlist()) %>% 
        mutate(Gender = unlist   (Gender),
               Gender = as.factor(Gender)) 

}


treatment_state = df %>% 
    group_by(state, treatment) %>%
    summarise(n = n())

# 0 Evaluate the number of NAs
df %>% glimpse()
df %>% nrow()
df %>% 
    summarise_all(~ sum(is.na(.)))

#
# It's better to get rid of feature `comments` as more than 85 % (1095/1259)of comments are NA







