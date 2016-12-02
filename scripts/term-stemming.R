# =====================================================================================
# title: term-stemming.R
# author: Nura Kawa
# summary: applies stemming of terms and fits GLM and SVM classifiers
# =====================================================================================

# Using Stanford NLP's feature selection guideline
library(class)
library(e1071)
library(glmnet)

N = 1
load(paste0("../data/dtms-ngrams-N-",N, ".RData"))

glmnet_models <- integer(5)
svm_models <- integer(5)

glmnet_models
svm_models

for(n_char in 0:5)
{
  print(paste("n_char = ", n_char))
  
  
  stem_dtm <- function(dtm, n_char)
  {
    #dtm is a document term matrix
    #nchar is minimum number of characters in a predictor - 1
    stem <- function(word)
    {
      common_endings <- c("*ed$", "*ing$", "*s$", "*es$", 
                          "*ly$", "*ary$", "*self$", "*ful$", 
                          "*less$","*ment$", "*er$", "*ance$",
                          "*al$", "*ent$", "*sion$", "*tion$",
                          "*ance$", "*or$", "*ive$", "*ise$")
      
      # remove common endings
      for(i in 1:length(common_endings)){word <- sub(common_endings[i], "", word)}
      
      return(word)
    }
    
    predictors <- colnames(dtm)
    stemmed_predictors <- stem(colnames(dtm))
    duplicated_terms <- stemmed_predictors[duplicated(stemmed_predictors, 
                                                      incomparables = FALSE)]
    duplicated_terms <- unique(duplicated_terms[nchar(duplicated_terms) > n_char])
    
    stemmed_dtm <- matrix(NA, 
                          nrow = nrow(dtm), 
                          ncol=length(duplicated_terms))
    
    for(i in 1:length(duplicated_terms))
    {
      # find columns of duplicated terms
      duplicated_columns <- grep(duplicated_terms[i], predictors)
      
      # add them
      replacement_column <- rowSums(dtm[,duplicated_columns])
      
      # add the column to a replacement matrix
      stemmed_dtm[,i] <- replacement_column
      
      
    }
    
    print("Made DTM")
    colnames(stemmed_dtm) <- duplicated_terms
    
    #making it binary
    stemmed_dtm <- (stemmed_dtm > 0)+0
    
    return(stemmed_dtm)
  }
  
  stemmed_dtm_train <- stem_dtm(dtm_train, n_char)
  
  stemmed_dtm_test <- stem_dtm(dtm_test, n_char)
  
  
  #dim(stemmed_dtm_train)
  #dim(stemmed_dtm_test)
  
  NFOLDS=5
  glmnet_classifier = cv.glmnet(x = stemmed_dtm_train,
                                y = y_train,
                                family = 'multinomial',
                                
                                # L1 penalty
                                alpha = 1,
                                
                                # 5-fold cross-validation
                                nfolds = NFOLDS,
                                
                                # high value is less accurate, but has faster training
                                thresh = 1e-3,
                                
                                # again lower number of iterations for faster training
                                maxit = 1e3)
  
  
  glmnet_preds = predict(glmnet_classifier,
                         newx = as.matrix(stemmed_dtm_test),
                         type = "class")
  
  
  glmnet_model_accuracy <- round( ((sum(glmnet_preds == y_test)/length(y_test))*100), 4)
  
  glmnet_models[n_char+1] <- glmnet_model_accuracy
  
  print(paste0("For nchar =",n_char, "Your GLMNET Model Accuracy is", glmnet_model_accuracy, "%"))
  
  n_class =5
  is_val <- function(x, i) (x == i)+0 #creates y for each class
  
  which_class <- matrix(NA, 
                        nrow = nrow(stemmed_dtm_train), 
                        ncol = n_class)
  
  for(i in 1:n_class){which_class[,i] = is_val(y_train, i)}
  which_class <- as.data.frame(which_class)
  colnames(which_class) <- paste0("y_", 1:5)
  
  
  # create matrix to hold predictions
  svm_predictions <- matrix(NA, 
                            nrow = nrow(stemmed_dtm_test), 
                            ncol = n_class)
  svm_predictions <- as.data.frame(svm_predictions)
  colnames(svm_predictions) <- paste0("class_", 1:5)
  
  
  for(i in 1:n_class)
  {
    
    print(paste("Iteration", i))
    
    # create your data frame for SVM
    svm_df <- cbind(as.data.frame(stemmed_dtm_train), which_class[,i])
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
                         stemmed_dtm_test, 
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
  
  svm_model_accuracy <- sum(class_preds == y_test)/length(class_preds)
  
  svm_models[n_char+1] <- svm_model_accuracy
  
  print(confusion_matrix)
  
  print(paste0("Your Model Accuracy for SVM for N=",N, " is ", round(svm_model_accuracy*100, 4), "%"))
  
  
}




model_accuracies <- data.frame("n_char" = paste0("n_char_",0:5),
                               "glmnet_stemmed_models"= glmnet_models,
                               "svm_stemmed_models" = svm_models)

save(model_accuracies,
     file="../data/stemmed-model-accuracies.RData")