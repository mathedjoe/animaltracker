library(tidyverse)
library(caret)
library(xgboost)

load("R/scratch/accel/supervised/cow_accel.rda")

train.control <- trainControl(method = "cv", number = 5)

cow_accel_sprinkle <- cow_accel %>% 
  select(X,Y,Z,XY.Z, XZ, Behavior)

model_gboost_sprinkle <- train(Behavior ~ ., 
                            data = cow_accel_sprinkle, method = "xgbTree",
                            trControl = train.control)

confusionMatrix(model_gboost_sprinkle)

varImp(model_gboost_sprinkle)
vip(model_gboost_sprinkle, geom="point", num_features = 20) + theme_minimal()

model_gboost_all <- train(Behavior ~ ., 
                      data = cow_accel %>% 
                        select(-c(datetime, COWID, elapsed)), method = "xgbTree",
                      trControl = train.control)

confusionMatrix(model_gboost_all)

