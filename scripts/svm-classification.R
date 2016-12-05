# =====================================================================================
# title: ml-classification.R
# author: Nura Kawa
# summary: fits linear classifier to objects from n-grams.R
# =====================================================================================

# =====================================================================================
# Setup
# =====================================================================================
my_dir <- "C:/Users/Nura/Desktop/text-classification/"
setwd(paste0(my_dir, "scripts/"))

# =====================================================================================
# Loading Library
# =====================================================================================

library(e1071)
library(SparseM)

# =====================================================================================
# Support Vector Machine Loop
# =====================================================================================

# Global Environment variables

n_class = 5 #number of classes

is_val <- function(x, i) (x == i)+0 #creates y for each class


timr <- Sys.time()

for(N in 1:4)
{
  # load data
  load(paste0("../data/n-gram/dtms-ngrams-N-",N, ".RData"))
  
  print(paste("N=", N))
  
  which_class <- matrix(NA, 
                        nrow = nrow(dtm_train), 
                        ncol = n_class)
  
  for(i in 1:n_class){which_class[,i] = is_val(y_train, i)}
  which_class <- as.data.frame(which_class)
  colnames(which_class) <- paste0("y_", 1:5)
  
  
  # create matrix to hold predictions
  svm_predictions <- matrix(NA, 
                            nrow = nrow(dtm_test), 
                            ncol = n_class)
  svm_predictions <- as.data.frame(svm_predictions)
  colnames(svm_predictions) <- paste0("class_", 1:5)
  
  
  for(i in 1:n_class)
  {
    
    print(paste("Iteration", i))
    
    # create your data frame for SVM
    svm_df <- cbind(as.data.frame(dtm_train), which_class[,i])
    # set the column name for use of svm()
    colnames(svm_df)[ncol(svm_df)] <- "y_train"
    
    # fit model
    svm_model <- svm(y_train ~ .,
                     data = svm_df,
                     scale = FALSE,
                     kernel = "radial")
    
    print(paste("Fit SVM model, Iteration", i))
    
    # make predictions
    svm_preds <- predict(svm_model,
                         dtm_test, 
                         type = "class", 
                         probability = FALSE)
    
    print(paste("Calculated SVM predictions, Iteration", i))
    
    # store predictions in the svm_predictions matrix
    svm_predictions[,i] <- svm_preds
  }
  
  
  # Make predictions
  
  class_preds <- integer(nrow(svm_predictions))
  for(i in 1:nrow(svm_predictions))
  {
    class_preds[i] = which.max(svm_predictions[i,])
  }
  
  confusion_matrix <- table(class_preds, y_test)
  
  model_accuracy <- sum(class_preds == y_test)/length(class_preds)

  print(confusion_matrix)
  
  print(paste0("Your Model Accuracy for SVM for N=",N, " is ", round(model_accuracy*100, 4), "%"))
  
  f_name <- paste0("../data/svm-models/SVM-model-N-",N,".RData")
  
  save(svm_df,
       svm_model,
       class_preds,
       model_accuracy,
       file = f_name
       )
  
  print(paste("Saved", f_name))
  
  rm(f_name)
}

total_runtime <- difftime(Sys.time(), timr)
print(total_runtime)


# =====================================================================================
# Naive Bayes
# =====================================================================================

# the problem I encounter is too many variables. Use LASSO coefficients to select
# variables.

# sum(!(coef(lasso)[[1]] == 0)) is 131. So we have 131 coefficients that are nonzero

# which coefficients are larger than 0
# coef(lasso)[[1]][which(coef(lasso)[[1]] != 0)]
# coef_names <- rownames(coef(lasso)[[1]])
# 
# lasso_selected_coefs <- data.frame("coef" = coef_names[which(coef(lasso)[[1]] != 0)][-1],
#                                    "value" = coef(lasso)[[1]][which(coef(lasso)[[1]] != 0)][-1])
# 
# 
# important_coefs <- which(colnames(dtm_train) %in% lasso_selected_coefs$coef)
# 
# 
# df <- cbind(dtm_train, y_train)
# df <- as.data.frame(df)
# df$y_train <- factor(df$y_train)
# 
# nb <- naiveBayes(y_train~.,
#                  data = df)
# 
# timr <- proc.time()
# nb_preds <- predict(nb, 
#                     dtm_test
#                     , type = "class")
# 
# proc.time() - timr
# 
# table(nb_preds, y_test)
# sum(nb_preds == y_test)/length(y_test)
# 
