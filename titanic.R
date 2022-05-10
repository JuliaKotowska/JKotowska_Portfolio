rm(list=ls())
setwd("C:/Users/julko/Desktop/Dokumenty/R/IRD-master/data")
train <- read.csv('titanic_train.csv')
test <- read.csv('titanic_test.csv')
library(dplyr)

#data exploration
dim(train)
head(train)
sapply(train, typeof)
summary(train)
survival_rate = sum(train$Survived)/nrow(train)

#in Cabin there are some blank spaces that are not considered as NA's by R, so they should be first filled with NA
train_new <- train
train_new[train_new==""|train_new==" "]<-NA

test_new <- test
test_new[test_new ==""| test_new==" "]<-NA


braki_danych_train <- data.frame("is.null" = apply(train_new,2,function(x) any(is.na(x))),
                           'how_many' = colSums(is.na(train_new)),
                           'mean' = colMeans(is.na(train_new))*100)
braki_danych_train

braki_danych_test <- data.frame("is.null" = apply(test_new,2,function(x) any(is.na(x))),
                                 'how_many' = colSums(is.na(test_new)),
                                 'mean' = colMeans(is.na(test_new))*100)
braki_danych_test

#NA's in age, cabin and embarked column
#skewness
library(e1071)
train_numeric <- train[,sapply(train,is.numeric)]
sapply(train_numeric,skewness)
par(mfrow = c(3,2))
hist(train_numeric$Survived)
hist(train_numeric$Pclass)
hist(train_numeric$Age)
hist(train_numeric$SibSp)
hist(train_numeric$Parch)
hist(train_numeric$Fare)

#setting factor variables: survived, pclass, sex, embarked
train_new$Survived<- as.factor(train_new$Survived)
train_new$Pclass<- as.factor(train_new$Pclass)
train_new$Sex<- as.factor(train_new$Sex)
train_new$Embarked<- as.factor(train_new$Embarked)

test_new$Pclass<- as.factor(test_new$Pclass)
test_new$Sex<- as.factor(test_new$Sex)
test_new$Embarked<- as.factor(test_new$Embarked)

glimpse(train_new)

#data visualization
library(ggplot2)

#how many people survived?
ggplot(train_new, aes(x = Survived)) + geom_bar() + geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white")
table(train_new$Survived)
prop.table(table(train_new$Survived))

#Survivng by sex
ggplot(train_new) + geom_bar(aes(x = Sex, fill = Survived))+
  theme_minimal()+xlab("")
#Surviving by class
ggplot(train_new)+geom_bar(aes(Pclass, fill = Survived))+theme_minimal()+xlab("")
by_class <- train_new%>%group_by(Pclass)%>%summarise(survived = sum(Survived == 1), Not_survived = sum(Survived==0), N = n(), not_survived_prop = Not_survived/N)
#Survivng by sex and class combined
ggplot(train_new) + geom_bar(aes(x = Sex, fill = Survived))+ facet_wrap(~Pclass)
by_class_sex <- train_new%>%group_by(Pclass, Sex)%>%summarise(survived = sum(Survived == 1), Not_survived = sum(Survived==0), N = n(), not_survived_prop = Not_survived/N)
#Survivng by where a person was embarked
ggplot(train_new) + geom_bar(aes (x= Embarked, fill = Survived))
by_emb <- train_new%>%group_by(Embarked)%>%summarise(survived = sum(Survived == 1), Not_survived = sum(Survived==0), N = n(), not_survived_prop = Not_survived/N)
#no significant pattern seen
#age classes
age_grouped <- cut(train_new$Age, c(0,18,30,40,50,60,70,80), labels = c("(0-18>","(18-30>","(30-40>","(40-50>","(50-60>","(60-70>","(70-80>"))
age_grouped_sum<-train_new%>%mutate(category = age_grouped)%>%group_by(category)%>%summarise(survived = sum(Survived == 1), Not_survived = sum(Survived==0), N = n(), not_survived_prop = Not_survived/N)
age_grouped_sum2<-train_new%>%mutate(category = age_grouped)%>%group_by(category, Survived)%>%summarise(N = n())
#Survivng by age groups, including NA's values
ggplot(age_grouped_sum2)+ geom_bar(aes(x = category,y =  N, fill = Survived), stat = "identity",
                                                  position = "stack")+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+xlab("")
ggplot(train_new, aes(x = Age, fill = Survived))+theme_bw()+facet_wrap(Sex~Pclass)+geom_density(alpha = 0.4)+labs(x = "Age",
                                                                                                                  y = "Survived",
                                                                                                                  title = "Survivng stats by age, sex and class (NA's excluded)")

#Fare
summary(train_new$Fare)
head(train_new$Fare)

ggplot(train_new, aes(x = Pclass,y = Fare))+ geom_boxplot()
#Survival vs the amount of fare
ggplot(train_new, aes(x = Survived, y = Fare)) + geom_boxplot()
ggplot(train_new, aes( x = Fare, fill = Survived)) + geom_histogram(bins = 25)+ylab("N")

#Mean fare value by survival and class 
by_fare <- train_new%>%group_by(Pclass)%>%summarise(survived = sum(Survived == 1), Not_survived = sum(Survived==0), N = n(), mean_fare = mean(Fare))

#Mean fare value by age groups
by_age_fare <- train_new%>%mutate(category = age_grouped)%>%group_by(category)%>%summarise(survived = sum(Survived == 1), Not_survived = sum(Survived==0), N = n(), mean_fare = mean(Fare))
ggplot(by_age_fare)+ geom_bar(aes(x = category, y = mean_fare), stat = 'identity', fill = 'darkblue')

#Mean fare by sex and class
by_sex_fare <-train_new%>%group_by(Pclass, Sex)%>%summarise(survived = sum(Survived == 1), Not_survived = sum(Survived==0), N = n(), mean_fare = mean(Fare))
by_sex_fare2 <-train_new%>%group_by(Pclass, Sex, Survived)%>%summarise(N = n(), mean_fare = mean(Fare))
#Do class, sex and mean fare influence survival? 
ggplot(by_sex_fare2) + geom_bar(aes(x = Sex,, y=mean_fare, fill = Survived), stat = 'identity', position = 'dodge')+ facet_wrap(~Pclass) 
#there is no clear pattern

#Mean fare by point of departure
by_emb_fare <- train_new%>%group_by(Embarked)%>%summarise(survived = sum(Survived == 1), Not_survived = sum(Survived==0), N = n(), mean_fare = mean(Fare))

#Scatter plot of Age vs Fare colored by Sex, faceted by Survived
ggplot(train_new, aes(x = Age, y = Fare, col = Sex))+geom_point()+facet_grid(~Survived)
#for higher vaues of fare it can be seen that most of those who survived were women whereas those who parished were men
#moreover it can be seen that generally in the group of survivors there is more women

#Class by point of departure
by_class_emb <- train_new%>%group_by(Embarked, Pclass)%>%summarise(N = n())


#Creation of a new column representing family size of a passenger
train_new <- train_new%>%mutate(fam_size = 1+Parch+SibSp)

#is family size and survival linked?
ggplot(train_new, aes(x = Survived, y = fam_size))+geom_boxplot()
hist(train_new$fam_size)
ggplot(train_new, aes(fam_size, fill = Survived))+geom_bar()
by_fam_size <- train_new%>%group_by(fam_size)%>%summarise(survived = sum(Survived == 1), Not_survived = sum(Survived==0), N = n(),not_survived_prop = Not_survived/N)%>%arrange(desc(not_survived_prop))
ggplot(by_fam_size, aes(fam_size, not_survived_prop))+geom_bar(stat = 'identity')
#clearly, large family sizes such as 11 people performed worse than others, but overall there is no strong pattern seen


#Dealing with NA's
#1 Embarked
ggplot(train_new) + geom_bar(aes (x= Embarked, y = Fare), stat = 'identity')
by_emb2 <- train_new%>%group_by(Embarked, Sex)%>%filter(is.na(Embarked))%>%summarise(survived = sum(Survived == 1), Not_survived = sum(Survived==0), N = n(), fare = Fare, mean_age = mean(Age), family = fam_size)
#so we deal with two women who survived, traveled alone, were middle aged and paid quite high price for first class tickets
#as it is known already, the highest part of first class was embarked in C
#so the two NA's will be filled with C point
train_new$Embarked[is.na(train_new$Embarked)]<-"C"
table(train_new$Embarked)

#2 Cabin
#the variable expresses cabin number of each passenger (but sme passengers have more than one cabin attached)
#the variable doesnt give any valuable information, so it wont be taken into account during analysis
train_new2 <- subset(train_new, select = -c(Cabin)) 
train_new <- train_new2
rm(train_new2)

#3 Age
#Examining missing values of age
train_new$missing_age <- ifelse(is.na(train_new$Age),1,0)
by_missing_age <- group_by(train_new, Pclass, fam_size, Sex)%>%filter(missing_age==1)%>%summarise(survived = sum(Survived == 1), Not_survived = sum(Survived==0), N = n(), not_survived_prop = Not_survived/N)
by_missing_age2 <- group_by(train_new, Pclass, Survived)%>%filter(missing_age==1)%>%summarise(N = n())
ggplot(by_missing_age2, aes(x = Pclass, y = N))+geom_bar(aes(fill = Survived), stat = 'identity')+geom_text(aes(label = N))
#there is no clear pattern seen in the missing values of age, so the fact of missigness should not influence the rest of the data
#random missingness can be replaced with median/mean value of the variable
median_age <- median(train_new$Age, na.rm = TRUE)
#NA's in Age will be filled with median value = 28
#train_new$Age[is.na(train_new$Age)]<- median_age

#second way of filling NA's in age
#we need to build an OLS model wthich will explain Age
#to do so outliers cannot be taken into model
ggplot(train_new, aes(x = Age))+geom_boxplot()
boxplot.stats(train_new$Age)
upper_whisker <- boxplot.stats(train_new$Age)$stats[5] #above age of 65 there are outliers 

age_model = lm(Age~Pclass+Sex+SibSp+Parch+Fare+Embarked, data = train_new[train_new$Age<upper_whisker,])
age_predictions <- predict(age_model, newdata = train_new[is.na(train_new$Age), c("Pclass","Sex","SibSp","Parch","Fare","Embarked")])
age_predictions[age_predictions<18]
train_new[is.na(train_new$Age), 'Age']<-age_predictions
train_new[train_new$Age<0,] #all kids from Sage family got negative values for age
#we will check the median age for girls in the first age group
median_age_girls<-(train_new%>%mutate(category = age_grouped)%>%group_by(category, Sex)%>%filter(Sex == 'female', category == '(0-18>')%>%summarise(survived = sum(Survived == 1), Not_survived = sum(Survived==0), N = n(), not_survived_prop = Not_survived/N, median_age = median(Age)))$median_age
#for girls from Sage family age will be filled with median value 13.5
median_age_boys<-(train_new%>%mutate(category = age_grouped)%>%group_by(category, Sex)%>%filter(Sex == 'male', category == '(0-18>')%>%summarise(survived = sum(Survived == 1), Not_survived = sum(Survived==0), N = n(), not_survived_prop = Not_survived/N, median_age = median(Age)))$median_age
#same for boys - 11 yr old
train_new[c(160, 181, 202,325, 793, 847, 864),'Age']<- c(11,13.5,11,11,13.5,11,13.5)


#cleaning test set
test_new2 <- subset(test_new, select = -c(Cabin)) 
test_new <- test_new2
rm(test_new2)

ggplot(test_new, aes(x = Age))+geom_boxplot()
boxplot.stats(test_new$Age)
upper_whisker2 <- boxplot.stats(test_new$Age)$stats[5] #above age of 64 there are outliers 

age_model2 = lm(Age~Pclass+Sex+SibSp+Parch+Fare+Embarked, data = test_new[test_new$Age<upper_whisker2,])
age_predictions2 <- predict(age_model2, newdata = test_new[is.na(test_new$Age), c("Pclass","Sex","SibSp","Parch","Fare","Embarked")])
age_predictions2[age_predictions2<18]
test_new[is.na(test_new$Age), 'Age']<-age_predictions2

test_new[is.na(test_new$Fare),]
by_fare_test <- test_new%>%group_by(Pclass)%>%summarise(N = n(), median_fare = median(Fare, na.rm = TRUE))
by_sex_fare_test <-test_new%>%group_by(Pclass, Sex)%>%summarise(N = n(), median_fare = median(Fare, na.rm = TRUE))
#the median value of fare for a man from third class is 7.9, so the missing value will be replaced with this one
test_new[is.na(test_new$Fare),'Fare']<- by_sex_fare_test[6,4]


#Information value IV of factor variables
library(InformationValue)

IV(train_new$Pclass, train_new$Survived, valueOfGood = 1)
IV(train_new$Sex, train_new$Survived, valueOfGood = 1)
IV(train_new$Embarked, train_new$Survived, valueOfGood = 1)
#all facotr variables are highly predictive 


##################### DECISION TREE ###############################
#normalization of numeric variables: Age, Fare
train_new$Age<-scale(train_new$Age)
train_new$Fare <- scale(train_new$Fare)

# Second
library(tidymodels)
library(parsnip)
library(rpart.plot)
library(rsample)
library(yardstick)
library(tune)
library(dials)

train_new$fam_size<-as.factor(train_new$fam_size)
# generate a specification of a model before fitting 
tree_spec <- decision_tree()%>%
  set_engine('rpart')%>%
  set_mode('classification')
#changing family size into facotr variable
#splitting train set into two to train and test the model
dataset_split <- initial_split(train_new, prop = 0.75, strata = Survived)
# Proportion of '1' outcomes in the training data
counts_train <- table(training(dataset_split)$Survived)
prop_yes_train <- counts_train[2]/ sum(counts_train)
# Proportion of '1' outcomes in the testing data
counts_test <- table(testing(dataset_split)$Survived)
prop_yes_test <- counts_test[2]/ sum(counts_test)
#so the proportion in two sets is almost the same,we can now proceed to next steps

# fitting the model
model_fit <- tree_spec%>%
  fit(Survived~Pclass+Sex+Age+fam_size+Fare+Embarked, data = training(dataset_split))
model_fit$fit%>%rpart.plot(roundint = FALSE)
#making predictions
predictions <- predict(model_fit, testing(dataset_split))
train_new_train = training(dataset_split)
train_new_test = testing(dataset_split)
predictions_check <- predictions%>%mutate(true_class = train_new_test$Survived)
head(predictions_check)
confus_matrix <- conf_mat(predictions_check, estimate = .pred_class, truth = true_class)
confus_matrix
#accuracy
acc = (confus_matrix$table[1,1]+confus_matrix$table[2,2])/(confus_matrix$table[1,1]+confus_matrix$table[2,2]+confus_matrix$table[1,2]+confus_matrix$table[2,1])
# or 
accuracy(predictions_check, estimate = .pred_class,truth = true_class)
#accuracy of the model nearly 80%

#metrics based on confusion matrix
cm_metrics <- function(confusion_matrix){
  Accuracy = (confusion_matrix$table[1,1]+confusion_matrix$table[2,2])/(confusion_matrix$table[1,1]+confusion_matrix$table[2,2]+confusion_matrix$table[1,2]+confusion_matrix$table[2,1])
  Precision = confusion_matrix$table[1,1]/(confusion_matrix$table[1,1]+confusion_matrix$table[2,1])
  Recall = confusion_matrix$table[1,1]/(confusion_matrix$table[1,1]+confusion_matrix$table[1,2])
  False_positive_rate = confusion_matrix$table[2,1]/(confusion_matrix$table[2,1]+confusion_matrix$table[2,2])
  Specificity = confusion_matrix$table[2,2]/(confusion_matrix$table[2,2]+confusion_matrix$table[2,1])
  lista = list(Accuracy, Precision, Recall, False_positive_rate, Specificity)
  return(data.frame(Metrics = c("Accuracy", "Precision", "Recall", "False_positive_rate", "Specificity"), Value = c(Accuracy, Precision, Recall, False_positive_rate, Specificity))) 
}
cm_metrics(confus_matrix)

#cross validation
set.seed(123)
cv_folds <- vfold_cv(train_new_train, v = 10)
fit_formula = Survived~Pclass+Sex+Age+fam_size+Fare+Embarked
#fitting folds to the specification
fits_cv <- fit_resamples(tree_spec, fit_formula, resamples = cv_folds, metrics = metric_set(accuracy))
#collecting errors
errors <- collect_metrics(fits_cv, summarize = FALSE)
# Plot an error histogram
ggplot(errors, aes(y = .estimate, fill = .metric)) +
  geom_histogram(bins = 20)
collect_metrics(fits_cv, summarize = TRUE)


#3 Hyperparameters will be tuned
#creating a specification of a model with hyperparameters left to be tuned later
tune_spec <- decision_tree(tree_depth = tune(), cost_complexity = tune())%>%
  set_mode("classification")%>%
  set_engine('rpart')
#creating a regular grid
tree_grid <- grid_regular(parameters(tune_spec), levels = 2)
cv_folds2 <- vfold_cv(train_new_train, v = 10) #10 folds cross validation
results_tuned <- tune_grid(tune_spec, fit_formula,resamples = cv_folds2, grid = tree_grid, metrics = metric_set(accuracy))
autoplot(results_tuned)
#selecting parameters that perform best
best_param <- select_best(results_tuned)
#finalization of selection
final_param <- finalize_model(tune_spec, best_param)
final_param
#training the final model
final_model <- final_param%>%fit(formula = fit_formula, data = train_new_train)
final_model

final_model$fit%>%rpart.plot()
predictions_tuned <- predict(final_model,train_new_test)
predictions_check_tuned <- predictions_tuned%>%mutate(true_class = train_new_test$Survived)
confus_matrix_tuned <- conf_mat(predictions_check_tuned, estimate = .pred_class, truth = true_class)
cm_metrics(confus_matrix_tuned)
cm_metrics(confus_matrix)


#now the procedure using random grid
tree_grid_random <- grid_random(parameters(tune_spec), size = 5)
cv_folds2 <- vfold_cv(train_new_train, v = 10) #10 folds cross validation
results_tuned_random <- tune_grid(tune_spec, fit_formula,resamples = cv_folds2, grid = tree_grid_random, metrics = metric_set(accuracy))
autoplot(results_tuned_random)
#selecting parameters that perform best
best_param_random <- select_best(results_tuned_random)
#finalization of selection
final_param_random <- finalize_model(tune_spec, best_param_random)
final_param_random
#training the final model
final_model_random <- final_param_random%>%fit(formula = fit_formula, data = train_new_train)
final_model_random

final_model_random$fit%>%rpart.plot()
predictions_tuned_random <- predict(final_model_random,train_new_test)
predictions_check_tuned_random <- predictions_tuned_random%>%mutate(true_class = train_new_test$Survived)
confus_matrix_tuned_random <- conf_mat(predictions_check_tuned_random, estimate = .pred_class, truth = true_class)
cm_metrics(confus_matrix_tuned)
cm_metrics(confus_matrix)
cm_metrics(confus_matrix_tuned_random)


#boosted tree
install.packages("xgboost")
library(xgboost)
boost_spec <- boost_tree(
  trees = 500,
  learn_rate = tune(),
  tree_depth = tune(),
  sample_size = tune()
)%>%
  set_mode("classification")%>%
  set_engine('xgboost')

#regular grid
grid_regular_boost <- grid_regular(hardhat::extract_parameter_set_dials(boost_spec), levels = 3)
boost_results <- tune_grid(boost_spec, fit_formula, resamples = vfold_cv(train_new_train, v = 5), grid = grid_regular_boost, 
                           metrics = metric_set(roc_auc) )
autoplot(boost_results)

best_param_boost <- select_best(boost_results)
final_param_boost <- finalize_model(boost_spec, best_param_boost)

final_model_boost <- final_param_boost%>%fit(formula = fit_formula, data = train_new_train)

predictions_boost <- predict(final_model_boost, train_new_test)
predictions_boost_check <- predictions_boost%>%mutate(true_class = train_new_test$Survived)
confus_matrix_boost <- conf_mat(predictions_boost_check, estimate = .pred_class, truth = true_class)
cm_metrics(confus_matrix_boost)
#best so far

#random forest
install.packages('ranger')
library(ranger)
forest_spec <- rand_forest(mtry = tune(), trees = tune(), min_n = tune())%>%set_mode("classification")%>%set_engine('ranger')
#regular grid
grid_regular_forest <- grid_regular(mtry(c(1,ncol(train_new_train)-1)), min_n(), trees(), levels = 3)
forest_results <- tune_grid(forest_spec, fit_formula, resamples = vfold_cv(train_new_train, v = 5), grid = grid_regular_forest,
                            metrics = metric_set(roc_auc))
autoplot(forest_results)

best_param_forest <- select_best(forest_results)
final_param_forest <- finalize_model(forest_spec, best_param_forest)

final_model_forest <- final_param_forest%>%fit(formula = fit_formula, data = train_new_train)

predictions_forest <- predict(final_model_forest, train_new_test)
predictions_forest_check <- predictions_forest%>%mutate(true_class = train_new_test$Survived)
confus_matrix_forest <- conf_mat(predictions_forest_check, estimate = .pred_class, truth = true_class)
cm_metrics(confus_matrix_forest)




#predictions in test data set
test_new <- test_new%>%mutate(fam_size = 1+Parch+SibSp)
test_new$fam_size<-as.factor(test_new$fam_size)
test_new$Age <- scale(test_new$Age)
test_new$Fare <- scale(test_new$Fare)
predictions <- predict(final_model_forest, test_new)
summary_survival = data.frame(PassengerId = test_new$PassengerId, Survived = predictions$.pred_class)
write.csv(summary_survival, file = "kaggle_titanic1.csv", row.names = FALSE)










