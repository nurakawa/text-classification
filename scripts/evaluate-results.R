# =====================================================================================
# title: evaluate-results.R
# author: Nura Kawa
# summary: collects results to be displated in report
# =====================================================================================

# =====================================================================================
# Setup
# =====================================================================================

# set working directory
setwd("C:/Users/Nura/Desktop/text-classification/scripts/")

# =====================================================================================
# load results
# =====================================================================================

glmnet_models <- integer(4)

# LASSO-penalized logistic regresion
for(i in 1:4)
{
  load(paste0("../data/model-N-",i,".RData"))
  glmnet_models[i]  = model_accuracy
}


# Term Stemming
load("../data/stemmed-model-accuracies.RData")

# SVM models
svm_models <- integer(4)

# LASSO-penalized logistic regresion
for(i in 1:4)
{
  load(paste0("../data/SVM-model-N-",i,".RData"))
  svm_models[i]  = model_accuracy
}

results <- data.frame("GLMNET" = glmnet_models,
           "GLMNET+stemming" = c(model_accuracies$glmnet_stemmed_models[1],NA,NA,NA),
           "SVM" = svm_models*100,
           "SVM+stemming" = c(model_accuracies$glmnet_stemmed_models[4],NA,NA,NA))



print(results)

