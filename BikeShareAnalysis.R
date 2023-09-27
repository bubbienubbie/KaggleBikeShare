library(recipes)

bike <- vroom("C:/Users/isaac/Documents/Fall 2023 Real/KaggleBikeShare/train.csv")

myCleanData <- bike %>%
  mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  select(season, weather, temp, windspeed, datetime, humidity, count)

# my_recipe <- recipe(rFormula, data=myDataSet) %>% # Set model formula and d2
#   step_mutate(newVar=var1*var2) %>% #Create a new variable3
#   step_poly(var, degree=2) %>% #Create polynomial expansion of var4
#   step_date(timestamp, features="dow") %>% # gets day of week5
#   step_time(timestamp, features=c("hour", "minute")) %>%
# step_dummy(all_nominal_predictors()) %>% #create dummy variables7
#   step_zv(all_predictors()) %>% #removes zero-variance predictors8
#   step_corr(variables, threshold=0.5) %>% # removes > than .5 corr9
#   step_rm(var) %>% #removes a variables10
#   step_select(var, -var2) #selects columns11
# prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using myDataS12
# bake(prepped_recipe, new_data=AnotherDataSet)


myCleanData$count <- log(myCleanData$count)

my_recipe <- recipe(count ~ ., data=myCleanData) %>%
  step_time(datetime, features=c("hour", "minute")) %>%
  step_num2factor(weather, levels=c("clear","clouds/mist","light_precip","heavy_precip")) %>%
  step_num2factor(season, levels=c("spring","summer","fall","winter")) %>%
  step_dummy(season, weather) %>% #make dummy variables
  step_rm(datetime)
prepped_recipe <- prep(my_recipe)
bake(prepped_recipe, new_data=myCleanData)

baked_recipe <- prep(my_recipe, myCleanData) %>%
  juice()


library(tidymodels)

my_mod <- linear_reg() %>% #Type of model3
  set_engine("lm") # Engine = What R function to use

bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model) %>%
  fit(data = myCleanData) # Fit the workflow9

test <- vroom("C:/Users/isaac/Documents/Fall 2023 Real/KaggleBikeShare/test.csv")
CleanTestData <- test %>%
  mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  select(season, weather, temp, windspeed, humidity, datetime)

bike_predictions <- predict(bike_workflow,
                            new_data=CleanTestData)

bike_predictions[bike_predictions<0]=0
Lin_Pred <- data.frame(CleanTestData$datetime, bike_predictions)
colnames(Lin_Pred) <- c("datetime", "count")

Lin_Pred$count <- exp(Lin_Pred$count)

library(lubridate)
Lin_Pred$datetime <- format(as_datetime(Lin_Pred$datetime), "%Y-%m-%d %H:%M:%S")
vroom_write(Lin_Pred, file="PolyLin_Prediction.csv", delim=",")





#PoissonRegression
install.packages("poissonreg")
library(poissonreg)

pois_mod <- poisson_reg() %>% #Type of model3
  set_engine("glm") # GLM = generalized linear model4

bike_pois_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(pois_mod) %>%
  fit(data = myCleanData)

bike_predictions <- predict(bike_pois_workflow,
                            new_data=CleanTestData) # Use fit to predict
bike_predictions[bike_predictions<0]=0
Pois_Pred <- data.frame(CleanTestData$datetime, bike_predictions)
colnames(Pois_Pred) <- c("datetime", "count")

Pois_Pred$datetime <- format(as_datetime(Pois_Pred$datetime), "%Y-%m-%d %H:%M:%S")
vroom_write(Pois_Pred, file="Pois_Predictions.csv", delim=",")






#Penalized Regression
install.packages("glmnet")

## Penalized regression model
preg_model <- linear_reg(penalty=0.1, mixture=0.5) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R
preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model) %>%
  fit(data=myCleanData)
bike_predictions <- predict(preg_wf, new_data=CleanTestData)


bike_predictions[bike_predictions<0]=0
Pen_Pred <- data.frame(CleanTestData$datetime, bike_predictions)
colnames(Pen_Pred) <- c("datetime", "count")

Pen_Pred$count <- exp(Pen_Pred$count)

Pen_Pred$datetime <- format(as_datetime(Pen_Pred$datetime), "%Y-%m-%d %H:%M:%S")
vroom_write(Pen_Pred, file="PolyPen_Predictions.csv", delim=",")





#Tuning
library(tidymodels)
library(poissonreg) #if you want to do penalized, poisson regression

## Penalized regression model
preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R

## Set Workflow
preg_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(preg_model)

## Grid of values to tune over
tuning_grid <- grid_regular(penalty(),
                            mixture(),
                            levels = 5) ## L^2 total tuning possibilities

## Split data for CV19
folds <- vfold_cv(myCleanData, v = 5, repeats=5)

## Run the CV
CV_results <- preg_wf %>%
tune_grid(resamples=folds,
          grid=tuning_grid,
          metrics=metric_set(rmse)) #Or leave metrics NULL

## Plot Results (example)
collect_metrics(CV_results) %>% # Gathers metrics into DF
  filter(.metric=="rmse") %>%
ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
geom_line()

## Find Best Tuning Parameters
bestTune <- CV_results %>%
select_best("rmse")

## Finalize the Workflow & fit it
final_wf <-
  preg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=myCleanData)

## Predict
final_pred <- predict(final_wf, new_data = CleanTestData)


final_pred[final_pred<0]=0
My_Pred <- data.frame(CleanTestData$datetime, final_pred)
colnames(My_Pred) <- c("datetime", "count")

My_Pred$count <- exp(My_Pred$count)

My_Pred$datetime <- format(as_datetime(My_Pred$datetime), "%Y-%m-%d %H:%M:%S")
vroom_write(My_Pred, file="Tun_Predictions.csv", delim=",")




#Regression Trees
install.packages("rpart")
library(tidymodels)

tree_mod <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(),
                        min_n=tune()) %>% #Type of model
  set_engine("rpart") %>% # Engine = What R function to use
  set_mode("regression")

## Create a workflow with model & recipe
tree_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(tree_mod)

## Set up grid of tuning value
tuning_grid <- grid_regular(tree_depth(),
                            cost_complexity(),
                            min_n(),
                            levels = 3)

## Set up K-fold CV
folds <- vfold_cv(myCleanData, v = 2, repeats=3)


## Find best tuning parameters
CV_results <- tree_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(rmse)) #Or leave metrics NULL

bestTune <- CV_results %>%
  select_best("rmse")


## Finalize workflow and predict
final_wf <-
  tree_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=myCleanData)

final_pred <- predict(final_wf, new_data = CleanTestData)


final_pred[final_pred<0]=0
My_Pred <- data.frame(CleanTestData$datetime, final_pred)
colnames(My_Pred) <- c("datetime", "count")

My_Pred$count <- exp(My_Pred$count)

My_Pred$datetime <- format(as_datetime(My_Pred$datetime), "%Y-%m-%d %H:%M:%S")
vroom_write(My_Pred, file="Tree_Predictions.csv", delim=",")
