{
    library(tidyverse)
    library(lubridate)
    library(magrittr)
    library(stringdist)
    library(recipes)
    options(max.print=10000)

    # What's the most important factors to define the necessity of mental treatment?
    # Is it possible to build any ML model based on those feature to evaluate the risk of mental health problems?

    #
    # so Target variable is `treatment`
    #

    df = read.csv(file=here::here("data/survey.csv"))
}
df %>% str(max.level=1)

{
    df = df %>% 
        rename_all(function(.name){ .name %>% tolower() }) %>% 
        mutate(timestamp = as_datetime(timestamp)) 
}

df$mental_health_interview %>% levels()
df$comments %>% levels()

df$obs_consequence %>% levels()

df$treatment  %>% levels()

df$gender  %>%  levels()
gender_table = df %>% 
    mutate(gender = tolower(gender)) %>% 
    group_by(gender) %>% 
    summarise(n = n()) %>% 
    arrange(desc(n))

{ 
    # TODO: transform gender to Male/Female Format


    normalize_gender <- function(input){
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
        mutate(gender = map(gender, normalize_gender),
               gender = unlist   (gender),
               gender = as.factor(gender)) 

}

{ # TODO: remove outlier entries by age

    df %>% 
        filter(age > 0) %>% 
        filter(age < 100) %>% 
        group_by(age) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n)) %>% 
        ggplot(aes(age, n)) +
        geom_point() +
        geom_line()

    df %<>% 
        filter(age > 17) %>% 
        filter(age < 100)  

}

age = df %>% 
    group_by(age) %>% 
    summarise(n = n()) %>% 
    arrange(desc(age))  
df %>% nrow()

df$age

df$gender  %>% levels()

df %>% 
    select(state) %>% 
    distinct()

treatment_state = df %>% 
    group_by(state, treatment) %>%
    summarise(n = n()) %>% 
    arrange(desc(n))

# 1 Evaluate the number of NAs and Impute ================================================
df %>% glimpse()
df %>% nrow()
df %>% summarise_all(~ sum(is.na(.)))

df %>% 
    group_by(country) %>% 
    summarise(total   = n(),
              missing = sum(is.na(state))) %>% 
    arrange(desc(total))

df$work_interfere  %>% levels()

df %>% 
    group_by(work_interfere, treatment) %>% 
    summarise(n = n())

#
# It's better to get rid of feature `comments` as more than 85 % (1095/1259)of comments are NA
# and impute feature `work_interfere` and `state` but the last one only for USA for the rest countries 
# we will use "None"
#

{
    df = df %>% select(-comments)
    df = df %>% 
        pmap(function(country, state, ...){
                 if(country != "United States") state = "None"

                 return(data.frame(country, state, ...))
          }) %>% 
    bind_rows()  %>% 
    mutate(state = as.character(state)) %>% 
    mutate(state = as.factor   (state))
}

df$state  %>% levels() # now you can see "None"



{
    check_NA <- function(value) ifelse(is.na(value), "yes", "no")

    df = df %>% 
        mutate(
               isNAstate          = check_NA(state),
               isNAself_employed  = check_NA(self_employed),
               isNAwork_interfere = check_NA(work_interfere),
               ) %>% 
    mutate(
           isNAstate          = as.factor(isNAstate),
           isNAself_employed  = as.factor(isNAself_employed),
           isNAwork_interfere = as.factor(isNAwork_interfere),

    )
}


df %>% glimpse()

{
    impute_recipe  <- df %>% 
        recipe(treatment ~ .) %>% 
        step_knnimpute(all_predictors(), neighbors = 3)

    imputed_df = impute_recipe %>% 
        prep() %>% 
        juice()

    imputed_df %>% glimpse()
    imputed_df %>% 
        filter(isNAstate == "yes") %>% 
        head()


    imputed_df %>% summarise_all(~ sum(is.na(.)))

    df = imputed_df
}
df %>% glimpse()

# 2 What are features the most important? ================================================
# Here I will use Boruta algorithm and correlation matrix
library(Boruta)

boruta.df <- Boruta(treatment ~ ., data=df, doTrace=2, maxRuns=300)
print(boruta.df)

{
    par(mar = c(10, 3, 3, 3))
    plot(boruta.df, xlab = "", xaxt = "n")
    grid (NULL,NULL, lty = 6, col = "cornsilk2") 
    lz <- lapply(1:ncol(boruta.df$ImpHistory), function(i) boruta.df$ImpHistory[is.finite(boruta.df$ImpHistory[,i]),i])
    names(lz) <- colnames(boruta.df$ImpHistory)
    Labels    <- sort(sapply(lz, median))
    axis(side = 1, las = 2, labels = names(Labels), at = 1:ncol(boruta.df$ImpHistory), cex.axis = 0.7)
    mtext("Variable Importance", side = 3, line = 1, cex = 1.2)
}

{ # TODO: use only not rejected features
    rejected = boruta.df$finalDecision == "Rejected"
    names_to_select = boruta.df$finalDecision[!rejected] %>%
        unlist() %>% 
        names()

    tdf = df %>% 
        select(treatment, all_of(names_to_select))

}


tdf = df

install.packages("ggcorrplot")
library(ggcorrplot)
model.matrix(~0+., data=tdf %>% select(-country)) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", 
             lab=FALSE, lab_size=1, tl.cex=7,
  tl.srt=45)

model.matrix(~0+., data=tdf %>% select(country)) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="upper", 
             lab=FALSE, lab_size=1, tl.cex=7,
  tl.srt=45)

# 3 Train the models





