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

tdf %>% str(max.level=2)

df = tdf

# install.packages("ggcorrplot")
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

# 3 Train the models (RF and SVM) ========================================================
library(tidymodels)

df %>% str(max.level=2)

df %<>% mutate(treatment = as.character(treatment))

set.seed(0)
df_split <- initial_split(df, prop=4/5)
df_train <- training(df_split)
df_test  <- testing (df_split)

df_cv <- vfold_cv(df_train, strata=treatment, v=5)

#
# Random Forest
#

{ # random forest model
    rf_recipe <- df %>% 
        recipe(treatment ~ .)

    rf_model <- rand_forest() %>% 
        set_args(mtry = tune()) %>% 
        set_engine("ranger", importance = "impurity") %>% 
        set_mode("classification")

    rf_workflow <- workflow() %>% 
        add_recipe(rf_recipe) %>% 
        add_model(rf_model)

    rf_grid  <- expand.grid(mtry=2:10)

    rf_tune_results <- rf_workflow %>% 
        tune_grid(resamples = df_cv,
                  grid      = rf_grid,
                  control   = control_grid(save_pred = TRUE, verbose = TRUE),
                  metrics   = metric_set(accuracy, recall, roc_auc))

}


{ # plot the penalty plot
    rf_plot  <- 
        rf_tune_results %>% 
        collect_metrics() %>% 
#         filter(.metric == "accuracy") %>% 
        ggplot(aes(x = mtry, y = mean)) +
        geom_point() +
        geom_line() +
        facet_wrap(~.metric) +
#         ylab("AUC the ROC Curve") +
        theme_bw()

    rf_plot

}


{ # choosing the best parameter and building the final model
    param_final  <- rf_tune_results %>% 
        select_best(metric = "roc_auc")

    rf_fit  <- rf_workflow %>% 
        finalize_workflow(param_final) %>% 
        last_fit(df_split)

}

{ # calculate AUC
    roc_obj = rf_fit %>% 
        collect_predictions() %>% 
        pROC::roc(treatment, .pred_Yes)
    auc_metric = pROC::auc(roc_obj)[[1]]


}

{ # draw ROC
    rf_auc <- rf_fit %>% 
        collect_predictions() %>% 
        roc_curve(treatment, .pred_No) %>% 
        mutate(model = "Random Forest")
    rf_roc_plot <- autoplot(rf_auc) + 
        ggtitle(paste0(c("RF model: AUC", round(auc_metric, 3)), collapse=" "))

    rf_roc_plot
}

{ # validation
    test_performance <- rf_fit %>% 
        collect_metrics(); test_performance

    test_predictions <- rf_fit %>% 
        collect_predictions(); test_predictions

    test_predictions %>% conf_mat(truth = treatment, estimate = .pred_class)

    test_predictions %>% 
        ggplot() +
        geom_density(aes(x = .pred_Yes, fill = treatment), alpha=0.5) +
        theme_bw()



}


# 
# SVM
# 

{ # svm model
    svm_recipe <- df %>% 
        recipe(treatment ~ .)  %>% 
        step_zv(all_predictors()) #%>% 
#         step_lincomb(all_numeric())


    svm_model <-
        svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
        set_mode("classification") %>%
        set_engine("kernlab")

    svm_workflow <- workflow() %>% 
        add_recipe(svm_recipe) %>% 
        add_model(svm_model)

    ctrl <- control_grid(verbose = TRUE, save_pred = TRUE)

    svm_tune_results <- svm_workflow %>% 
      tune_grid(resamples = df_cv,
        metrics = metric_set(accuracy, roc_auc),
        control = ctrl
      )

}

{ # plot the penalty plot
    svm_plot_cost  <- 
        svm_tune_results %>% 
        collect_metrics() %>% 
#         filter(.metric == "roc_auc") %>% 
        ggplot(aes(x = cost, y = mean)) +
        geom_point() +
        geom_line() +
        facet_wrap(~.metric) +
#         ylab("AUC the ROC Curve") +
        theme_bw() +
        ggtitle("Tuning regarding to Cost")


    svm_plot_sigma  <- 
        svm_tune_results %>% 
        collect_metrics() %>% 
#         filter(.metric == "roc_auc") %>% 
        ggplot(aes(x = rbf_sigma, y = mean)) +
        geom_point() +
        geom_line() +
        facet_wrap(~.metric) +
#         ylab("AUC the ROC Curve") +
        theme_bw() +
        ggtitle("Tuning regarding to Sigma")

    gridExtra::grid.arrange(svm_plot_cost, svm_plot_sigma, nrow = 2)

}

{ # choosing the best parameter and building the final model
    param_final  <- svm_tune_results %>% 
        select_best(metric = "roc_auc")

    svm_fit  <- svm_workflow %>% 
        finalize_workflow(param_final) %>% 
        last_fit(df_split)

}

{ # calculate AUC
    install.packages("pROC")
    roc_obj = svm_tune_results %>% 
        collect_predictions(parameters = param_final)  %>% 
        pROC::roc(treatment, .pred_Yes)
    auc_metric = pROC::auc(roc_obj)[[1]]


}

{ # draw ROC
    svm_auc <- svm_fit %>% 
        collect_predictions() %>% 
        roc_curve(treatment, .pred_No) %>% 
        mutate(model = "Random Forest")
    roc_plot <- autoplot(svm_auc) +
        ggtitle(paste0(c("SVM model: AUC", round(auc_metric, 3)), collapse=" "))

    roc_plot 
}

{ # validation
    test_performance <- svm_fit %>% 
        collect_metrics(); test_performance

    test_predictions <- svm_fit %>% 
        collect_predictions(); test_predictions

    test_predictions %>% conf_mat(truth = treatment, estimate = .pred_class)

    test_predictions %>% 
        ggplot() +
        geom_density(aes(x = .pred_Yes, fill = treatment), alpha=0.5) +
        theme_bw()



}


{ # Testing
    treatment_fit <- fit(rf_workflow, data=df_train)

    predicted = predict(treatment_fit, df_test)
    accuracy = sum(df_test$treatment == predicted)/nrow(predicted)
    accuracy

    real = tibble(treatment = df_test$treatment) %>% 
        mutate(treatment = as.factor(treatment))

    c_mat = bind_cols(real, predicted) %>% 
        conf_mat(truth = treatment, estimate = .pred_class)

    acc = conf_mat


    library(caret) 
    confusionMatrix(c_mat)
    conf_matrix = matrix(c_mat$table, nrow = 2)
    acc = sum(diag(conf_matrix) / sum(conf_matrix))
    apply(conf_matrix, 2, sum)
    diag(conf_matrix) / apply(conf_matrix, 2, sum) / 2
    pre = sum(diag(conf_matrix) / apply(conf_matrix, 2, sum)) / 2
    diag(conf_matrix)
}

