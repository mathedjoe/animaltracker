## ATTEMPT TO APPLY ML TO CLASSIFY "FILTERED DATA"

load("data/demo_comparison.rda")
require(dplyr)
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
  
library(mlr)
df_imp <- mlr::impute(df, target = "drop", classes = list(numeric = imputeMedian()) )

df_imp <- df_imp$data %>%
  mutate_if(is.numeric, scale)

train_rows <- sample(1:nrow(df_imp), floor(nrow(df_imp)*2/3))
test_rows <- setdiff(1:nrow(df_imp), train_rows)

train <- df_imp[train_rows,]
test <- df_imp[test_rows,]

setDT(train)
setDT(test)
library(data.table)
library(mlr)
setDT(train)[,.N/nrow(train),drop]

setDT(test)[,.N/nrow(test),drop]

# clean target in test data set
test[,drop:=0]


#create a task
traintask <- mlr::makeClassifTask(data = as.data.frame(train), target = "drop")
testtask <- mlr::makeClassifTask(data = as.data.frame(test), target = "drop")


#create learner
bag <- makeLearner("classif.rpart",predict.type = "response")
bag.lrn <- makeBaggingWrapper(learner = bag, bw.iters = 100, bw.replace = TRUE)

#set 5 fold cross validation
rdesc <- makeResampleDesc("CV",iters=5L)

#set parallel backend (Windows)
library(parallelMap)
library(parallel)
parallelStartSocket(cpus = detectCores())


r <- mlr::resample(learner = bag.lrn
              ,task = traintask
              ,resampling = rdesc
              ,measures = list(tpr,fpr,fnr,fpr,acc)
              ,show.info = T)


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
fmodel <- train(rf.lrn,traintask)
getLearnerModel(fmodel)

#predict on test data
fpmodel <- predict(fmodel, testtask)

parallelStop()

table(fpmodel$data$response,df_imp$drop[test_rows] )
