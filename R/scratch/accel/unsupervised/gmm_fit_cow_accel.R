library(mclust)

load("R/scratch/accel/supervised/model_rf_parsy.rda")
load("R/scratch/accel/supervised/cow_accel.rda")

cow_df <- cow_accel %>% 
  # select(all_of(predictors(model_rf_parsy)), Behavior) %>% 
  mutate(across(where(is.numeric), 
                .fns = function(x){case_when(
                  abs(x)<10^(-4) ~ 10^(-4), 
                  abs(x)>10^(5) ~ 10^5*x/abs(x),
                  TRUE ~ x
                )}))
  
predx <- cow_df %>% 
  select(-c(datetime, Behavior, COWID)) 

behavior <- cow_df$Behavior

BIC <- mclustBIC(predx)
plot(BIC)
summary(BIC)

mod1 <- Mclust(predx, x = BIC, G= 3)
summary(mod1, parameters = TRUE)

# plot(mod1, what = "classification")
table(mod1$classification, behavior)
caret::confusionMatrix(factor(mod1$classification, 
                              labels = c("G", "W", "R") ), behavior)

