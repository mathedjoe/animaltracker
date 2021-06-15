library(tidyverse)
library(caret)
library(xgboost)
library(modelr)
library(vip)

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

# Randomly select half of cow IDs for training
set.seed(1812)

train_ids <- sample(unique(cow_accel$COWID), ceiling(length(unique(cow_accel$COWID))/2), replace = FALSE)
accel_train <- cow_accel %>% dplyr::filter(COWID %in% train_ids)
accel_test <- cow_accel %>% dplyr::filter(!(COWID %in% train_ids))

accel_sprinkle_train <- accel_train %>% select(X, Y, Z, XY.Z, XZ, Behavior)

gboost_sprinkle_train <- train(Behavior ~ ., 
                               data = accel_sprinkle_train, method = "xgbTree",
                               trControl = train.control)

save(gboost_sprinkle_train, file = "R/scratch/accel/supervised/gboost_sprinkle_train.rda", compress = "xz")

confusionMatrix(gboost_sprinkle_train) # 89.42% train accuracy

accel_sprinkle_test <- accel_test %>% modelr::add_predictions(gboost_sprinkle_train)
confusionMatrix(accel_sprinkle_test$pred, accel_sprinkle_test$Behavior) # 46.82% test accuracy

varImp(gboost_sprinkle_train) 
vip(gboost_sprinkle_train, geom="point", num_features = 20) + theme_minimal() # XZ is most important

gboost_all_train <- train(Behavior ~ ., 
                          data = accel_train %>% 
                            select(-c(datetime, COWID, elapsed)), method = "xgbTree",
                          trControl = train.control)

save(gboost_all_train, file = "R/scratch/accel/supervised/gboost_all_train.rda", compress = "xz")
confusionMatrix(gboost_all_train) # 99.88% train accuracy

accel_all_test <- accel_test %>% modelr::add_predictions(gboost_all_train)
confusionMatrix(accel_all_test$pred, accel_all_test$Behavior) # 52.93% test accuracy



