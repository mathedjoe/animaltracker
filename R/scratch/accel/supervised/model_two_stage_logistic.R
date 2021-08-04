library(tidyverse)
library(caret)
load("R/scratch/accel/supervised/cow_accel.rda")
load("R/scratch/accel/supervised/model_rf_parsy.rda")

cowdf <- cow_accel %>% 
  mutate(isResting = 1*(Behavior == "R"),
         isGrazing = 1*(Behavior == "G"))

vars_predict <- caret::predictors(model_rf_parsy)

logrest_formula <- paste("isResting ~ ",paste(vars_predict, collapse="+"),sep = "")

model_logrest <- glm(logrest_formula, data = cowdf, family = "binomial" ) 
summary(model_logrest)

cowdf$is_predict_moving <- 1*(predict(model_logrest, type = "response") < .5)

log_move_formula <- paste("isGrazing ~ ",paste(vars_predict, collapse="+"),sep = "")
model_logmove <- glm(log_move_formula, data = cowdf %>% filter(is_predict_moving == 1), family = "binomial" ) 
summary(model_logmove)
cowdf$is_predict_grazing <- 0
cowdf$is_predict_grazing[cowdf$is_predict_moving == 1] <- 
  1*(predict(model_logmove, type = "response") > .5)

cowdf$predict_Behavior <- ifelse(cowdf$is_predict_moving == 0, "R", 
                                 ifelse(cowdf$is_predict_grazing == 1, "G", 
                                 "W")) %>% as.factor
caret::confusionMatrix(cowdf$predict_Behavior, cowdf$Behavior)
