library(animaltracker)
library(h2o)
library(tidyverse)

demo <- animaltracker::demo_unfiltered %>% 
  dplyr::mutate(across(where(is.character), as.factor)) %>% # convert all character columns to factors 
  dplyr::filter(Date == "2017-12-14")

set.seed(1812)

# Get train indices
train_idx <- sample(1:nrow(demo), round(0.8*nrow(demo))) 

# Training set
demo_train <- demo[train_idx,] 
# Test set
demo_test <- demo[-train_idx,]

colnames(demo)

h2o.shutdown()

# turn on h2o cluster
h2o.init(enable_assertions = FALSE)

# Convert training set into h2o format, remove correlated features
features <- as.h2o(demo_train %>% dplyr::select(-c(Order, Index, Date, Time, DateTime)))

# hyperparameter search space
# dense autoencoders use a "mirrored" 
hyper_grid <- list(hidden = list(
  c(2), # latent space dimension
  c(3), 
  c(8, 2, 8), # 8-node dense encoder, 3D latent space, 8-node dense decoder 
  c(16, 2, 16), 
  c(32, 2, 32),
  c(64, 2, 64),
  c(8, 3, 8),
  c(16, 3, 16),
  c(32, 3, 32),
  c(64, 3, 64),
  c(16, 8, 2, 8, 16),
  c(32, 16, 2, 16, 32),
  c(64, 32, 2, 32, 64),
  c(16, 8, 3, 8, 16),
  c(32, 16, 3, 16, 32),
  c(64, 32, 3, 32, 64)
))

# grid search hyperparameter space
ae_grid <- h2o.grid(
  algorithm = 'deeplearning',
  x = seq_along(features),
  training_frame = features,
  grid_id = 'autoencoder_grid',
  autoencoder = TRUE,
  activation = 'Tanh',
  hyper_params = hyper_grid,
  sparse = TRUE,
  ignore_const_cols = FALSE,
  seed = 1812
)

# get results
h2o.getGrid('autoencoder_grid', sort_by = 'mse', decreasing = FALSE)

# pick the model with the lowest MSE
best_model_id <- ae_grid@model_ids[[7]]
best_model <- h2o.getModel(best_model_id)

# view reconstruction MSE for training data
(reconstruction_errors <- h2o.anomaly(best_model, features))

train_results <- dplyr::bind_cols(demo_train, as.data.frame(reconstruction_errors)) %>% 
  arrange(Reconstruction.MSE)

tail(train_results, 10)

test_features <- as.h2o(demo_test %>% dplyr::select(-c(Order, Index, Date, Time, DateTime)))

test_results <- dplyr::bind_cols(demo_test, as.data.frame(h2o.anomaly(best_model, test_features))) %>% 
  arrange(Reconstruction.MSE)

tail(test_results, 10)
