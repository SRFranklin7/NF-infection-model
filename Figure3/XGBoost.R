library(xgboost)
library(caret)
library(readxl)
library(pROC)
library(gtools)
library(tidyverse)
library(doParallel)
library(SHAPforxgboost)

df <- read_excel("Aim1_XGBoost_Baseline_01072025.xlsx", sheet = "UniCLR_Final")
df <- df[-c(1,3)]

df$Group <- factor(df$Group, labels = c("InfectiousNF", "SterileNF"))

# Create
best.tunes <- c()
VarImp <- list()
all.test.response <- all.test.predictor <- test_aucs <- c()
all.cv.response <- all.cv.predictor <- cv_aucs <- c()
indivpred <- list()
indivresp <- list()
mean_shap <- list()
shap_longer <- data.frame()


set.seed(1) #1
SeedList <- sample(1:10000, 100, replace=FALSE)
#Extract duplicated variables
SeedList[duplicated(SeedList)]
#set.seed(4050)

start <- Sys.time()
for (i in 1:100) { # Change back to 100 after test
  print(i)
  set.seed(SeedList[i])
  inTraining <- createDataPartition(df$Group, p = .80, list = FALSE)
  training <- df[ inTraining,]
  testing  <- df[-inTraining,]
  preProcValues <- preProcess(training, method = "range")
  trainTransformed <- predict(preProcValues, training)
  testTransformed <- predict(preProcValues, testing)
  
  grid <-  expand.grid(nrounds=c(250,500,750),
                       gamma=c(0),
                       eta=c(0.25,0.5,0.75),
                       max_depth=c(6), 
                       colsample_bytree= c(0.2,0.4,0.6), 
                       min_child_weight=c(2,4,6), 
                       subsample=c(0.225,0.35,0.475)
  )
  
  
  cv <- trainControl(method="repeatedcv",
                     verboseIter = TRUE,
                     repeats = 10,
                     number=5,  #### Probably gotta change this to 7? look into changing this?
                     returnResamp="final",
                     classProbs=TRUE,
                     #summaryFunction=twoClassSummary,
                     indexFinal=NULL,
                     savePredictions = TRUE)
  set.seed(SeedList[i])
  xgboost <- train(Group ~ .,
                   data=trainTransformed,
                   method = "xgbTree",
                   trControl = cv,
                   metric = "Accuracy",  ## originally they had it as ROC
                   tuneGrid = grid,
                   verbose = TRUE)
  
  #xgboost <- train(Group ~ .,
  #                 data=trainTransformed,
  #                 method = "xgbTree",
  #                 trControl = trainControl(method = "cv", number = 5),
  #                 verbose = TRUE)
  
  # Best mtry parameter
  best.tune <- xgboost$bestTune
  # Save the best mtry parameter for each model
  best.tunes <- c(best.tunes, best.tune)
  
  # Mean AUC value over repeats of the best cost parameter during training
  cv_auc <- getTrainPerf(xgboost)$TrainAccuracy
  # Print the cv mean AUC
  print(max(xgboost$results[,"Accuracy"]))
  
  
  # Predict on the test set and get predicted probabilities
  rpartProbs <- predict(xgboost, testTransformed, type="prob")
  # Test AUC calculation
  test_auc <- roc(ifelse(testTransformed$Group == "InfectiousNF", 1, 0), rpartProbs[[2]])$auc
  # Save all the test AUCs over iterations in test_aucs
  test_aucs <- c(test_aucs, test_auc)
  
  # Cross-validation mean AUC value
  # Save all the cv AUCs over iterations in cv_aucs
  cv_aucs <- c(cv_aucs, cv_auc)
  # Save the test set labels in all.test.response. Labels converted to 0 for normal and 1 for cancer
  all.test.response <- c(all.test.response, ifelse(testTransformed$Group == "InfectiousNF", 1, 0))
  # Save the test set predicted probabilities of highest class in all.test.predictor
  all.test.predictor <- c(all.test.predictor, rpartProbs[[2]])
  
  indivresp[[i]] <- ifelse(testTransformed$Group == "InfectiousNF", 1, 0)
  indivpred[[i]] <- rpartProbs[[2]]
  
  gbmImp <- varImp(xgboost, scale = F)$importance
  gbmImp <- tibble::rownames_to_column(gbmImp, "Names")
  VarImp[[i]] <- gbmImp
  
  
  # SHAP
  shap_values <- shap.values(xgb_model = xgboost$finalModel, X_train = as.matrix(trainTransformed[,-1]))
  # The ranked features by mean |SHAP|
  vals <- shap_values$mean_shap_score
  mean_shap[[i]] <- vals
  
  shap_long <- shap.prep(xgb_model = xgboost$finalModel, X_train = as.matrix(trainTransformed[,-1]))
  shap_longer <- rbind(shap_longer, shap_long)
  
}

end <- Sys.time()
end - start

save.image(file = "Aim1_UniCLRFinal_SHAP_02222025.RData")

paramdf <- matrix(unlist(best.tunes), ncol=7, byrow = T)
colnames(paramdf) <- c("nrounds", "max_depth",
                       "eta", "gamma",
                       "colsample_bytree", "min_child_weight",
                       "subsample")
paramdf <- cbind(paramdf, test_aucs)
write.csv(paramdf, file = "ParamsList_Aim1.csv")

######################
### Plot Combo ROC ###
######################

# Get the ROC of both test and cv from all the iterations
test_roc <- roc(all.test.response, all.test.predictor, auc=TRUE, ci=TRUE)

plot(c(1,0),c(0,1),
     type='l',
     lty=3,
     xlim=c(1.01,0), ylim=c(-0.01,1.01),
     xaxs='i', yaxs='i',
     ylab='', xlab='')
# Plot Test ROC in red line
plot(test_roc,
     col='black',
     lwd=2,
     add=T,
     lty=1)
# Compute the CI of the AUC
auc.ci <- ci.auc(test_roc)
ci.sp.obj <- ci.sp(test_roc, sensitivities = seq(0,1,.01), boot.n=100)
plot(ci.sp.obj, type = "shape", col = adjustcolor("grey", alpha = 0.5))

# Label the axes
mtext(side=2,
      text="Sensitivity",
      line=2.5,
      cex=1)
mtext(side=1,
      text="Specificity",
      line=2.5,
      cex=1)
# Add legends for both lines
legend(x=0.7,y=0.2,
       legend=(sprintf('Test - AUC: %.3g, CI: %.3g', test_roc$auc, (auc.ci[3]-auc.ci[2]))),
       bty='n',
       xjust=0,
       lty=c(1,1),
       col='black',
       text.col='black')



##################
### MEDIAN ROC ###
##################

#find location of median AUC value in test_aucs
value <- match(median(test_aucs), test_aucs)


plot(c(1,0),c(0,1),
     type='l',
     lty=3,
     xlim=c(1.01,0), ylim=c(-0.01,1.01),
     xaxs='i', yaxs='i',
     ylab='', xlab='')

for(i in 1:100){
  iter_roc <- roc(unlist(indivresp[i]), unlist(indivpred[i]), auc=TRUE, ci=TRUE)
  plot(iter_roc,
       col='grey',
       lwd=2,
       add=T,
       lty=1)
}
MedianROC <- roc(unlist(indivresp[value]), unlist(indivpred[value]), auc=TRUE, ci=TRUE)
plot(MedianROC,
     col='black',
     lwd=2,
     add=T,
     lty=1)
auc.ci <- ci.auc(MedianROC)
legend(x=0.7,y=0.2,
       legend=(sprintf('Median - AUC: %.3g, CI: %.3g', MedianROC$auc, (auc.ci[3]-auc.ci[2]))),
       bty='n',
       xjust=0,
       lty=c(1,1),
       col='black',
       text.col='black')
# Label the axes
mtext(side=2,
      text="Sensitivity",
      line=2.5,
      cex=1)
mtext(side=1,
      text="Specificity",
      line=2.5,
      cex=1)


###########################
### Plot AUC in Boxplot ###
###########################

#Boxplot showing the Test AUCs for each model
test_aucs1 <- as.data.frame(test_aucs)
test_aucs1$cat <- rep("AUC", each = length(test_aucs1))

ggplot(test_aucs1, aes(y=test_aucs, x=cat))+
  geom_boxplot()+
  theme_bw()+
  geom_dotplot(binaxis = 'y',
               stackdir = 'center',
               dotsize = 1)+
  theme(axis.title.x = element_blank())

##################################
### Plot parameter performance ###
##################################

trellis.par.set(caretTheme())
plot(xgboost)


#######################################
### Save Variable Importance Scores ###
#######################################

sorted_lists <- lapply(VarImp, function(df){
  df[order(df$Names),]
})

merged_df <- Reduce(function(x,y) merge(x,y, by = "Names", all=T), sorted_lists)
merged_df<- merged_df %>% remove_rownames %>% column_to_rownames(var="Names")

#Add new column for the average across all models
merged_df$Means <- apply(merged_df, 1, mean)

#add new column for count of non-zero importance scores
merged_df$counts <- rowSums(merged_df !=0)

write.csv(merged_df, file = "ImpScores_Aim1_UniCLRFinal.csv")

######################
## Plotting Var Imp ##
######################

# Counts

mdf <- merged_df[merged_df$counts !=0,]
mdf <- mdf[order(mdf$counts, decreasing = T),]

par(mar=c(5,10,3,3)) #bottom, left, top, right
barplot(mdf$counts,
        main = "Variables with Non-Zero Importance Scores",
        xlab = "# of Models",
        names.arg = rownames(mdf),
        horiz = T,
        las=2,
        cex.names = 0.75)


# Averages 

avdf<- merged_df[merged_df$Means !=0,]
avdf <- mdf[order(mdf$Means, decreasing = T),]

par(mar=c(5,12,3,3)) #bottom, left, top, right
barplot(avdf$Means,
        main = "Variables with Non-Zero \n Importance Scores",
        xlab = "Average Importance across Models",
        names.arg = rownames(avdf),
        horiz = T,
        las=2,
        cex.names = 0.75)

## SHAP Analysis ##
library(SHAPforxgboost)

# Aggregate SHAP values
aggregated_shap <- Reduce("+", lapply(shap_values_list, function(x) x$shap_score))
aggregated_shap <- aggregated_shap / length(shap_values_list)

# Summary plot
shap.plot.summary()

