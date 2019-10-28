## ATTEMPT TO APPLY ML TO CLASSIFY "FILTERED DATA"

library(mlr)
library(dplyr)
library(parallelMap)
library(parallel)

load("data/demo_comparison.rda")

df<- demo_comparison %>%
  select(time = TimeDiff,
         lat = Latitude.x,
         lon = Longitude.x,
         dist = Distance.y,
         rate = Rate.y,
         course = Course.y,
         elev = Elevation.x,
         slope = Slope.x,
         drop =Dropped.x) %>%
  mutate(drop = factor(drop))

### PREPROCESSING 

# Median imputation
df_imp <- mlr::impute(df, target = "drop", classes = list(numeric = imputeMedian()) ) 

# Standardize features
df_imp <- normalizeFeatures(df_imp$data, target = "drop", method = "standardize")

# Train-test split: 2/3 - 1/3
set.seed(0)

train_rows <- sample(1:nrow(df_imp), floor(nrow(df_imp)*2/3))
test_rows <- setdiff(1:nrow(df_imp), train_rows)


#setDT(train)
#setDT(test)
#library(data.table)
#library(mlr)

#setDT(train)[,.N/nrow(train),drop]
#setDT(test)[,.N/nrow(test),drop]

# clean target in test data set
# test$drop = NA

#create a task
task <- mlr::makeClassifTask(data = df_imp, target = "drop", positive = "1")


#create learner
#bag <- makeLearner("classif.rpart",predict.type = "response")
#bag.lrn <- makeBaggingWrapper(learner = bag, bw.iters = 100, bw.replace = TRUE)

#set 5 fold cross validation
#rdesc <- makeResampleDesc("CV",iters=5L)

#parallelStartSocket(cpus = detectCores())


#r <- mlr::resample(learner = bag.lrn
                   #,task = traintask
                   #,resampling = rdesc
                   #,measures = list(tpr,fpr,fnr,fpr,acc)
                   #,show.info = T)


#make randomForest learner
rf.lrn <- makeLearner("classif.randomForest")
rf.lrn$par.vals <- list(ntree = 100L,
                        importance=TRUE)

r <- resample(learner = rf.lrn
              ,task = traintask
              ,resampling = rdesc
              ,measures = list(tpr,fpr,fnr,fpr,acc)
              ,show.info = T)


getParamSet(rf.lrn)

#set parameter space
params <- makeParamSet(
  makeIntegerParam("mtry",lower = 2,upper = 10),
  makeIntegerParam("nodesize",lower = 10,upper = 50)
)

#set validation strategy
rdesc <- makeResampleDesc("CV",iters=5L)

#set optimization technique
ctrl <- makeTuneControlRandom(maxit = 5L)

#start tuning
tune <- tuneParams(learner = rf.lrn
                   ,task = traintask
                   ,resampling = rdesc
                   ,measures = list(acc)
                   ,par.set = params
                   ,control = ctrl
                   ,show.info = T)

#train model
fmodel <- train(rf.lrn, task, subset = train_rows)
getLearnerModel(fmodel)

#predict on test data
fpmodel <- predict(fmodel, task, subset = test_rows)
fpmodel

parallelStop()

calculateROCMeasures(fpmodel)

# Try logistic regression

traintask <- mlr::makeClassifTask(data = df_imp[train_rows,], target = "drop", positive = "1")

logistic.learner <- makeLearner("classif.logreg",predict.type = "response")

# 5-fold CV
cv.logistic <- crossval(learner = logistic.learner, task = traintask, iters = 5, stratify = TRUE, measures = acc)

fmodel_logistic <- train(logistic.learner, task, subset = train_rows)

fpmodel_logistic <- predict(fmodel_logistic, task, subset = test_rows)

calculateROCMeasures(fpmodel_logistic)

# High Type 1 error.
