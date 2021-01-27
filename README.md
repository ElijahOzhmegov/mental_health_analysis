# Mental Health Analysis in Tech

source: https://osmihelp.org/research
final report: [R report](https://htmlpreview.github.io/?https://raw.githubusercontent.com/ElijahOzhmegov/mental_health_analysis/master/main.html)


Original data from OSMI dataset are categorical features - answers on the questions.
among these features we chose feature called “treatment” as a target variable. That means if there was a treatment for a mental health condition.

We ask themself a few simple questions: 
* What is the most important factors to define the necessity of mental treatment? (it would be great to ask only relevant questions)
* Is it possible to build any ML model based on those features to evaluate the risk of mental health problems? (all features categorical)
* What quality would be of a model if it is possible? (no signs that it is feasible to build a model with a decent balanced accuracy)

Data preprocessing step includes such actions as gender normalization (transformation to Male/Female/Other format)

Data cleaning step includes: 
* removing age outliers (for certain entries value could be negative or more than 100)
* checking missingness and removing features that are impossible to impute (only comments feature)

Data Imputation 
* marking missing values and then imputing with knn approach where k = 3

Feature selection step includes two steps:
* removing unimportant variables with Boruta algorithm
* And removing highly correlated features
Modeling step includes 4 steps:
1. Model choice
2. HPO
2. Validation 
2. Testing

Originally, there was no need to consider something besides ML, so 3 ML models were considered:
* RF
* SVM
* Logistic Regression

Unfortunately, it wasn't feasible to apply Logistic regression as all variable are categorical (yes, one hot encoding was applied).
So further we optimized and validated RF and SVM. As there were a few bold evidence that SVM lacks generalization we proceeded with RF. (rbf SVM)

You can see the testing result of our Random Forest Model.
Metrics shows us a moderate quality model.

ALL THE DETAILS AND EVEN MORE CAN BE FOUND IN OUR REPORT!

