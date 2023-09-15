library(recipes)

bike <- vroom("C:/Users/isaac/KaggleBikeShare/train.csv")

myCleanData <- bike %>%
filter(bike$weather !=4) %>% 
  select(season, weather, temp, windspeed, datetime, count)

# my_recipe <- recipe(rFormula, data=myDataSet) %>% # Set model formula and d2
#   step_mutate(newVar=var1*var2) %>% #Create a new variable3
#   step_poly(var, degree=2) %>% #Create polynomial expansion of var4
#   step_date(timestamp, features="dow") %>% # gets day of week5
#   step_time(timestamp, features=c("hour", "minute")) %>%6
# step_dummy(all_nominal_predictors()) %>% #create dummy variables7
#   step_zv(all_predictors()) %>% #removes zero-variance predictors8
#   step_corr(variables, threshold=0.5) %>% # removes > than .5 corr9
#   step_rm(var) %>% #removes a variables10
#   step_select(var, -var2) #selects columns11
# prepped_recipe <- prep(my_recipe) # Sets up the preprocessing using myDataS12
# bake(prepped_recipe, new_data=AnotherDataSet)


my_recipe <- recipe(count ~ ., data=myCleanData) %>%
  step_time(datetime, features=c("hour", "minute")) %>%
  step_num2factor(weather, levels=c("clear","clouds/mist","light_precip","heavy_precip")) %>%
  step_num2factor(season, levels=c("spring","summer","fall","winter"))
prepped_recipe <- prep(my_recipe)
bake(prepped_recipe, new_data=myCleanData)

