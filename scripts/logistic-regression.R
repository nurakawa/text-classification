# =====================================================================================
# title: classify-n-gram.R
# author: Nura Kawa
# summary: fits linear classifier to objects from n-grams.R
# =====================================================================================

# =====================================================================================
# load library
# =====================================================================================
library(glmnet)

# =====================================================================================
# classification
# =====================================================================================

for(N in 1:4)
{
  # load data created in n-grams.R
  load(paste0("../data/n-gram/dtms-ngrams-N-",N, ".RData"))
  #t
  print(paste("Currently, N is", N))
  NFOLDS = 4
  t1 = Sys.time()
  
  y_train[y_train == 0] = -1
  
  glmnet_classifier = cv.glmnet(x = dtm_train,
                                y = y_train,
                                family = 'multinomial',
                                
                                # L1 penalty: LASSO
                                alpha = 1,
                                
                                # 5-fold cross-validation
                                nfolds = NFOLDS,
                                
                                # high value is less accurate, but has faster training
                                thresh = 1e-3,
                                
                                # again lower number of iterations for faster training
                                maxit = 1e3)
  
  print(difftime(Sys.time(), t1, units = 'min'))
  
  
  
  # plot(glmnet_classifier)
  
  preds = predict(glmnet_classifier,
                  newx = as.matrix(dtm_test),
                  type = "class")
  
  print(preds)
  
  
  print(paste0("Your Model Accuracy is ", round( ((sum(preds == y_test)/length(y_test))*100), 4), "%"))
  
  model_accuracy <- round( ((sum(preds == y_test)/length(y_test))*100), 4)
  
  
  f_name <- paste0("../data/glmnet-models/non-penalized/model-N-",N,".RData")
  
  
  
  save(preds,
       model_accuracy,
       glmnet_classifier,
       file = f_name)
  
  rm(f_name)
}


 
