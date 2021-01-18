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
        step_impute_knn(all_predictors(), neighbors = 3)

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

    { # Calculate stats for features
        imp.history = boruta.df$ImpHistory
        lz <- lapply(1:ncol(imp.history), 
                     function(i) imp.history[is.finite(imp.history[,i]),i])
        names(lz) <- colnames(boruta.df$ImpHistory)
        Labels    <- sort(sapply(lz, median))
        rm(imp.history)
    }

    
    tosave = TRUE
    { # Plot feature importance whisker plot
        if(tosave){
#             png(filename="pics/feature_importance.png", width=1280,height=720)
            svg(filename="pics/feature_importance.svg", width=16, height=9)

            ticks_cex = 2
            title_cex = 3
        }else{

            ticks_cex = 1.0
            title_cex = 1.5
        }

        par(mar = c(20, 4, 5, 4))
        plot(boruta.df, xlab = "", xaxt = "n", ylab = "", yaxt = "n")
        grid(NULL, NULL, lty = 7, col = "gray") 


        axis(side = 1, labels = FALSE, at = 1:ncol(boruta.df$ImpHistory))
        axis(side = 2, las = 2, mgp = c(3, 0.75, 0), cex.axis = ticks_cex)

        text(x = 1:ncol(boruta.df$ImpHistory),
             y = par("usr")[3] - 5,
             labels = names(Labels),
             xpd = NA,
             srt = 45,
             adj = 1.0,
             cex = ticks_cex)
        mtext("Variable Importance", side = 3, line = 1, cex = title_cex)

        if(tosave) dev.off()
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
{ # Draw correlation matrix

    model.matrix(~0+., data=df %>%  select(-country)) %>% 
      cor(use="pairwise.complete.obs") %>% 
      ggcorrplot(show.diag = F, type="full", 
                 lab=FALSE, lab_size=1, tl.cex=8, tl.srt=45,
      ) +
    ggtitle("Correlation Matrix") +
    theme(plot.title = element_text(size = 20, hjust = 0.5))
    if(tosave)ggsave("pics/correlation_matrix_without_county.svg")

}

# 3 Train the models (RF and SVM) ========================================================
library(tidymodels)

df %>% str(max.level=2)

df %<>% mutate(treatment = as.character(treatment))

{ # split on training and testing
    bound <- floor((nrow(df)/5)*4)         #define % of training and test set

    df <- df[sample(nrow(df)), ]           #sample rows 
    df.train <- df[1:bound, ]              #get training set
    df.test <- df[(bound+1):nrow(df), ]    #get test set

    df.train %>% 
        group_by(treatment) %>% 
        summarise(n = n(),
                  n.prop = n/nrow(df.train))
    df.test %>%
        group_by(treatment) %>% 
        summarise(n = n(),
                  n.prop = n/nrow(df.test))

}

set.seed(0)
df_split <- initial_split(df.train, prop=4/5)
df_train <- training(df_split)
df_valid <- testing (df_split)

df_cv <- vfold_cv(df_train, strata=treatment, v=5)

#
# Random Forest
#

{
    { # random forest model
        rf_recipe <- df.train %>% 
            recipe(treatment ~ .)

        m = df.train %>% ncol() %>% sqrt() %>% floor()
        rf_model <- rand_forest() %>% 
            set_args(mtry = m, trees = tune(), min_n = tune()) %>% 
            set_engine("ranger", importance = "impurity") %>% 
            set_mode("classification")

        rf_workflow <- workflow() %>% 
            add_recipe(rf_recipe) %>% 
            add_model(rf_model)

        # the number of trees will be tuned from 125 to 500.
        # the minimum number of data points in a node 
        # will be tuned from 2 to 8.
        rf_grid <- grid_regular(trees(range = c(125, 500)),
                                min_n(range = c(2, 8)),
                                levels = 6)

        doParallel::registerDoParallel()
        rf_tune_results <- rf_workflow %>% 
            tune_grid(resamples = df_cv,
                      grid      = rf_grid,
                      control   = control_grid(save_pred = TRUE, verbose = TRUE),
                      metrics   = metric_set(bal_accuracy, recall, roc_auc))

    }


    { # plot the penalty plot
        tosave = TRUE
        plot_title = paste("Random Forest penalty plot", 
                           collapse=" ")
        rf_plot_trees  <- 
            rf_tune_results %>% 
            collect_metrics() %>% 
            ggplot(aes(x = trees, y = mean)) +
            geom_point() +
            geom_line() +
            facet_wrap(~.metric) +
            theme_bw() +
            ggtitle(plot_title) +
            theme(plot.title = element_text(size = 20, hjust = 0.5))

        rf_plot_min_n  <- 
            rf_tune_results %>% 
            collect_metrics() %>% 
            ggplot(aes(x = min_n, y = mean)) +
            geom_point() +
            geom_line() +
            facet_wrap(~.metric) +
            theme_bw()

        rf_penalty_plot = gridExtra::grid.arrange(rf_plot_trees, rf_plot_min_n, nrow = 2) 
        rf_penalty_plot # just to show the plot
        plot_path = glue::glue("pics/", "random_forest_penalty_plot.svg")
        if(tosave) ggsave(plot_path, plot=rf_penalty_plot)

    }


    { # choosing the best parameter and building the final model
        param_final  <- rf_tune_results %>% 
            select_best(metric = "roc_auc")

        rf_fit  <- rf_workflow %>% 
            finalize_workflow(param_final) %>% 
            last_fit(df_split)

    }

    { # ROC and AUC
    { # calculate AUC
        roc_obj = rf_fit %>% 
            collect_predictions() %>% 
            pROC::roc(treatment, .pred_Yes)
        auc_metric = pROC::auc(roc_obj)[[1]]

    }

    { # draw ROC
        rf_auc <- rf_fit %>% collect_predictions() %>% 
            roc_curve(treatment, .pred_No) %>% 
            mutate(model = "Random Forest")

        plot_title = paste("Random Forest: AUC", 
                           round(auc_metric, 3),
                           collapse=" ")

        rf_roc_plot <- autoplot(rf_auc) + 
            ggtitle(plot_title) + 
            theme(plot.title = element_text(size = 20, hjust = 0.5))

        rf_roc_plot
        plot_path = glue::glue("pics/", 
                               "random_forest_roc_plot.svg")
        if(tosave) ggsave(plot_path, plot=rf_roc_plot)
    }

    }

    { # Draw Distribution 
        validation_predictions <- rf_fit %>% collect_predictions()

        plot_title = paste("Random Forest, Validation Distribution", 
                           collapse=" ")

        rf_val_dist = validation_predictions %>% 
            ggplot() +
            geom_density(aes(x = .pred_Yes, fill = treatment), alpha=0.5) +
            theme_bw() +
            ggtitle(plot_title) + 
            theme(plot.title = element_text(size = 20, hjust = 0.5))
        rf_val_dist

        plot_path = glue::glue("pics/", 
                               "random_forest_validation_distribution.svg")
        if(tosave) ggsave(plot_path, plot=rf_val_dist)


    }

    { # Validation Metrics
        rf_conf_mat      = validation_predictions %>% conf_mat    (truth = treatment, estimate = .pred_class)

        rf_recall        = validation_predictions %>% recall      (truth = treatment, estimate = .pred_class, event_level="second")
        rf_accuracy      = validation_predictions %>% accuracy    (truth = treatment, estimate = .pred_class)
        rf_fbal_accuracy = validation_predictions %>% bal_accuracy(truth = treatment, estimate = .pred_class)
        rf_kap           = validation_predictions %>% kap         (truth = treatment, estimate = .pred_class)

        rf_metrics = bind_rows(rf_recall       ,
                               rf_accuracy     ,
                               rf_fbal_accuracy,
                               rf_kap          
        )
    }

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

