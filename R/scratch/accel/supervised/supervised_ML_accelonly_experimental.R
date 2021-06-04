library(tidyverse)
library(caret) # for computing cross-validation
library(ranger)
library(vip)

# read data ----
cow_accel_file <- list.files("R/scratch/accel/rf_2018/", pattern="Cow 7", full.names = TRUE)
cow_accel_raw <- readxl::read_excel(cow_accel_file)
# prep data ----
clean_accel_data <- function(data){
  # before using, prep the data to format: datetime, X, Y, Z, Behavior
  data %>% 
    dplyr::mutate(
      elapsed = lubridate::seconds(datetime - dplyr::lag(datetime)),
      elapsed = as.numeric(gsub("S", "", elapsed)),
      elapsed = replace_na(elapsed, 0),
      XY.Z = X*Y+Z,  
      XZ = X*Z,
      X.Y.Z = X+Y+Z,
      norm_accel = sqrt(X^2+Y^2+Z^2),
      tiltX = asin(X/norm_accel),
      tiltY = asin(Y/norm_accel),
      tiltZ = acos(Z/norm_accel),
      pitch = atan(X/(Y^2+Z^2)),
      roll = atan(Y/(X^2+Z^2)),
      yaw = atan(Z /(X^2+Y^2)),
      jerkX = (X-dplyr::lag(X))/elapsed, #meters/sec^3
      jerkY = (Y -dplyr::lag(Y))/elapsed,
      jerkZ = (Z-dplyr::lag(Z))/elapsed,
      tvelX = (cumsum(X) - dplyr::first(X))*elapsed, #meters/sec
      tvelY = (cumsum(Y) - dplyr::first(Y))*elapsed,
      tvelZ = (cumsum(Z) - dplyr::first(Z))*elapsed,
      tdispX = cumsum(tvelX)*elapsed, #meters
      tdispY = cumsum(tvelY)*elapsed,
      tdispZ = cumsum(tvelZ)*elapsed
    ) %>% 
    mutate(across(starts_with("jerk"), function(x){ replace_na(x,0)}) ) %>% 
    filter(abs(elapsed) < 60, !is.infinite(jerkX))
}


cow_accel <- cow_accel_raw %>% 
  dplyr::select( datetime = Time, X = X_mean, Y = Y_mean, Z = Z_mean, 
                 Rest, Graze, Walk
  ) %>%
  unite(Behavior, c(Rest, Graze, Walk), remove = TRUE, na.rm=TRUE) %>%
  dplyr::mutate(
    Behavior = as.factor(Behavior)) %>% 
  clean_accel_data()
  
   

## NOTE: the data includes X+Y+Z, 
#        which is a linear dependence, so X.Y.Z should be omitted
findLinearCombos(cow_accel %>%                  
                   select(-c(datetime, Behavior))  )

# Define training control
set.seed(123) 

train.control <- trainControl(method = "cv", number = 5)

# Train models using Sprinkle's variable set ----
cow_accel_sprinkle <- cow_accel %>% 
  select(X,Y,Z,XY.Z, XZ, Behavior)

# QDA (as Sprinkle did)
model_qda_sprinkle <- train(Behavior ~ ., 
                   data = cow_accel_sprinkle, method = "qda",
                   trControl = train.control)
print(model_qda_sprinkle)
confusionMatrix(model_qda_sprinkle)

# RF (with Sprinkle's data)
rf_tune_grid_sprinkle <- expand.grid( mtry = seq(1,5),
                             splitrule = "gini",
                             min.node.size = seq(1,10,1))

model_rf_sprinkle <- train(Behavior ~., 
                  data = cow_accel_sprinkle, 
                  method = "ranger",
                  trControl = train.control, 
                  tuneGrid = rf_tune_grid_sprinkle,
                  num.trees = 50 )
print(model_rf_sprinkle)
confusionMatrix(model_rf_sprinkle)

# Train models using Expanded variable set ----
cow_accel_expanded <- cow_accel %>% 
  select(-c(datetime, elapsed, X.Y.Z, XY.Z, XZ, X.Y.Z) )

# QDA (as Sprinkle did)
model_qda_expanded <- train(Behavior ~ ., 
                            data = cow_accel_expanded, method = "qda",
                            trControl = train.control)
print(model_qda_expanded)
confusionMatrix(model_qda_expanded)

# RF (with expanded's data)
rf_tune_grid_expanded <- expand.grid( mtry = seq(1,ncol(cow_accel_expanded)-1),
                                      splitrule = "gini",
                                      min.node.size = seq(1,10,1))

model_rf_expanded <- train(Behavior ~., 
                           data = cow_accel_expanded, 
                           method = "ranger",
                           trControl = train.control, 
                           tuneGrid = rf_tune_grid_expanded,
                           importance = "impurity",
                           num.trees = 50 )
print(model_rf_expanded)
confusionMatrix(model_rf_expanded)

### inspect variable importance ----
vip(model_rf_expanded, geom="point", num_features = 20) + theme_minimal()

vi_rf_expanded <- vi(model_rf_expanded)
vars_rf_keep <- vi_rf_expanded %>% 
  filter(Importance >= 50) %>% 
  pull(Variable)

# RF (with expanded data, smaller variable set)
cow_accel_parsy <- cow_accel %>% 
  select(all_of(vars_rf_keep), Behavior)
  
rf_tune_grid_parsy <- expand.grid( mtry = seq(1,ncol(cow_accel_parsy)-1),
                                      splitrule = "gini",
                                      min.node.size = seq(1,10,1))

model_rf_parsy <- train(Behavior ~., 
                           data = cow_accel_parsy, 
                           method = "ranger",
                           trControl = train.control, 
                           tuneGrid = rf_tune_grid_parsy,
                           importance = "impurity",
                           num.trees = 50 )
print(model_rf_parsy)
confusionMatrix(model_rf_parsy)

## test the model predictions on another data set ----
cow_accel_file2 <- list.files("R/scratch/accel/rf_2018/", pattern="Cow 12", full.names = TRUE)
cow_accel_raw2 <- readxl::read_excel(cow_accel_file2)

cow_accel2 <- cow_accel_raw2 %>% 
  select(datetime = Time...2, 
         Behavior = Observed, 
         X = `Avg X`,
         Y = `Avg Y`,
         Z = `Avg z`
         ) %>% 
  mutate(Behavior = as.factor(Behavior)) %>%
  clean_accel_data()

cow_accel2_pred <- cow_accel2 %>% 
  modelr::add_predictions(model_rf_parsy)

confusionMatrix(cow_accel2_pred$pred, cow_accel2_pred$Behavior)

## retrain the RF model on BOTH data sets ----
cow_accel_parsy2 <- cow_accel %>%
  bind_rows(cow_accel2) %>%
  select(all_of(vars_rf_keep), Behavior)

rf_tune_grid_parsy2 <- expand.grid( mtry = seq(1,ncol(cow_accel_parsy2)-1),
                                   splitrule = "gini",
                                   min.node.size = seq(1,10,1))

model_rf_parsy2 <- train(Behavior ~., 
                        data = cow_accel_parsy2, 
                        method = "ranger",
                        trControl = train.control, 
                        tuneGrid = rf_tune_grid_parsy2,
                        importance = "impurity",
                        num.trees = 50 )
print(model_rf_parsy2)
confusionMatrix(model_rf_parsy2)

## - test RF model on a NEW data set
cow_accel_file3 <- list.files("R/scratch/accel/rf_2018/", pattern="All Cows", full.names = TRUE)
cow_accel_raw3 <- readxl::read_excel(cow_accel_file3)

## TO DO: CLEAN THE TEST DATA SET, COMPARE PREDICTIONS TO OBSERVED USING PARSY model
# cow_accel3 <- ...
