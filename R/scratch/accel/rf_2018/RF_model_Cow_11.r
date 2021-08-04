
df=read.csv("Cow_11_2017_RF_vs2.csv",header=T)

library(randomForest)

names(df)


###########run models with G or NG
All_Data_RF=randomForest(G_or_NG ~ +Avg.X+ Avg.Y+ Avg.z+ X.Y...Z+    
                      LN.X.Y..Z+X.Z+LN.X.Z+X.Y.Z+LN.X.Y.Z+LN.Z,
                     ntree=10000,data=df)
All_Data_RF
plot(All_Data_RF)
varImpPlot(All_Data_RF)
partialPlot(All_Data_RF,pred.data = df,x.var = "X.Y...Z",main="X.Y...Z",which.class = "G")

unique(df$Time.1)
df$Time.1=as.character(df$Time.1)
df_Oct=subset(df,Time.1=="10/30/2017")

Nov_3_RF=randomForest(G_or_NG ~ +Avg.X+ Avg.Y+ Avg.z+ X.Y...Z+    
                           LN.X.Y..Z+X.Z+LN.X.Z+X.Y.Z+LN.X.Y.Z+LN.Z,
                         ntree=10000,data=df_Oct)
Nov_3_RF
plot(Nov_3_RF)
varImpPlot(Nov_3_RF)
partialPlot(Nov_3_RF,pred.data = df,x.var = "X.Y...Z",main="X.Y...Z",which.class = "G")

unique(df$Time.1)
df$Time.1=as.character(df$Time.1)
df_Nov=subset(df,Time.1!="11/20/2017")

Nov_RF=randomForest(G_or_NG ~ +Avg.X+ Avg.Y+ Avg.z+ X.Y...Z+LN.X.Y..Z+X.Z+LN.X.Z+X.Y.Z+LN.X.Y.Z+LN.Z,
                       ntree=10000,data=df_Nov)
Nov_RF
plot(Nov_RF)
varImpPlot(Nov_RF)


###########run models with R, W, G
All_Data_RF=randomForest(Observed ~ +Avg.X+ Avg.Y+ Avg.z+ X.Y...Z+    
                           LN.X.Y..Z+X.Z+LN.X.Z+X.Y.Z+LN.X.Y.Z+LN.Z,
                         ntree=10000,data=df)
All_Data_RF
plot(All_Data_RF)
varImpPlot(All_Data_RF)
partialPlot(All_Data_RF,pred.data = df,x.var = "X.Y...Z",main="X.Y...Z",which.class = "G")

unique(df$Time.1)
df$Time.1=as.character(df$Time.1)
df_Oct=subset(df,Time.1=="10/30/2017")

unique(df_Oct$Observed)

Nov_3_RF=randomForest(Observed ~ +Avg.X+ Avg.Y+ Avg.z+ X.Y...Z+    
                         LN.X.Y..Z+X.Z+LN.X.Z+X.Y.Z+LN.X.Y.Z+LN.Z,
                       ntree=10000,data=df_Oct)
Nov_3_RF
plot(Nov_3_RF)
varImpPlot(Nov_3_RF)


unique(df$Time.1)
df$Time.1=as.character(df$Time.1)
df_Nov=subset(df,Time.1!="11/20/2017")

Nov_RF=randomForest(Observed ~ +Avg.X+ Avg.Y+ Avg.z+ X.Y...Z+LN.X.Y..Z+X.Z+LN.X.Z+X.Y.Z+LN.X.Y.Z+LN.Z,
                    ntree=10000,data=df_Nov)
Nov_RF
plot(Nov_RF)
varImpPlot(Nov_RF)