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
############## 
# TRAIN USING DATA FROM SOME COWS
#############
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
load("R/scratch/accel/supervised/gboost_all_train.rda")
confusionMatrix(gboost_all_train) # 99.88% train accuracy

############## 
# TRAIN USING DATA FROM ALL COWS
#############
cow_accel2 <- cow_accel %>% 
  mutate(rowid = 1:nrow(.))

accel_train2 <-  cow_accel2 %>%
  group_by(COWID) %>% 
  sample_frac(.5) %>% ungroup() 

accel_test2 <- cow_accel2 %>% 
  filter(!rowid %in% accel_train2$rowid)

gboost_all_train2 <- train(Behavior ~ ., 
                          data = accel_train2 %>% 
                            dplyr::select(-c(datetime, COWID, elapsed, rowid)), method = "xgbTree",
                          trControl = train.control)

save(gboost_all_train2, file = "R/scratch/accel/supervised/gboost_all_train2.rda", compress = "xz")
# load("R/scratch/accel/supervised/gboost_all_train2.rda")
confusionMatrix(gboost_all_train2) #

accel_all_test2 <- accel_test2 %>% modelr::add_predictions(gboost_all_train2)
confusionMatrix(accel_all_test2$pred, accel_test2$Behavior) #

varImp(gboost_all_train2) 
vip(gboost_all_train2, geom="point", num_features = 20) + theme_minimal() # XZ is most important

############## 
# TRAIN USING DATA FROM ALL COWS, NO AUTOCORRELATED TIME SERIES VARIABLES
#############
cow_accel3 <- cow_accel %>% 
  mutate(rowid = 1:nrow(.)) %>% 
  dplyr::select(rowid, Behavior, COWID, X, Y, Z, norm_accel:yaw)

accel_train3 <-  cow_accel3 %>%
  group_by(COWID) %>% 
  sample_frac(.5) %>% ungroup() 

accel_test3 <- cow_accel3 %>% 
  filter(!rowid %in% accel_train3$rowid)

gboost_all_train3 <- train(Behavior ~ ., 
                           data = accel_train3 %>% 
                             dplyr::select(-c( COWID, rowid)), method = "xgbTree",
                           trControl = train.control)

save(gboost_all_train3, file = "R/scratch/accel/supervised/gboost_all_train3.rda", compress = "xz")
# load("R/scratch/accel/supervised/gboost_all_train3.rda")
confusionMatrix(gboost_all_train3) #

accel_all_test3 <- accel_test3 %>% modelr::add_predictions(gboost_all_train3)
confusionMatrix(accel_all_test3$pred, accel_test3$Behavior) #

varImp(gboost_all_train3) 
vip(gboost_all_train3, geom="point", num_features = 20) + theme_minimal() # XZ is most important

### SAME SAMPLE, TRAINING WITH SPRINKLES VARIABLES
accel_sprinkle_train2 <- cow_accel %>% 
  mutate(rowid = 1:nrow(.)) %>%
  filter(rowid %in% accel_train3$rowid) %>% 
  dplyr::select(X, Y, Z, XY.Z, XZ, Behavior)

accel_sprinkle_test2 <- cow_accel %>% 
  mutate(rowid = 1:nrow(.)) %>%
  filter(! rowid %in% accel_train3$rowid)  %>% 
  dplyr::select(X, Y, Z, XY.Z, XZ, Behavior)


gboost_sprinkle_train2 <- train(Behavior ~ ., 
                               data = accel_sprinkle_train2, method = "xgbTree",
                               trControl = train.control)

save(gboost_sprinkle_train2, file = "R/scratch/accel/supervised/gboost_sprinkle_train2.rda", compress = "xz")

confusionMatrix(gboost_sprinkle_train2) 

accel_sprinkle_test2 <- accel_sprinkle_test2 %>% modelr::add_predictions(gboost_sprinkle_train2)
confusionMatrix(accel_sprinkle_test2$pred, accel_sprinkle_test2$Behavior) 

varImp(gboost_sprinkle_train2) 
vip(gboost_sprinkle_train2, geom="point", num_features = 20) + theme_minimal() 
