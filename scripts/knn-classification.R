# =====================================================================================
# Setup
# =====================================================================================
my_dir <- "C:/Users/Nura/Desktop/text-classification/"
setwd(paste0(my_dir, "scripts/"))

N=1

# load data
load(paste0("../data/dtms-ngrams-N-",N, ".RData"))

library(class)


# =====================================================================================
# KNN Classification
# =====================================================================================


# create your data frame for SVM
ols_df <- cbind(as.data.frame(dtm_train), as.data.frame(y_train))
# set the column name for use of svm()
colnames(ols_df)[ncol(ols_df)] <- "y_train"


knn_predictions <- matrix(NA, nrow=length(y_test), ncol = 10)
runtimes <- list(list())

for(k in 1:10)
{
  timr <- Sys.time()
  
  print(paste("k=", k))
  knn_classifier <- knn(train = dtm_train,
                        test = dtm_test, #A vector will be a row vector. Try t()?
                        cl = y_train,
                        k = k)
  print(paste("Fit classifier for k=", k))
  knn_predictions[,k] = knn_classifier
  
  runtimes[[k]] <- difftime(Sys.time(), timr)
  
  print(runtimes[[k]])
  rm(knn_classifier)

}


accuracies <- integer(10)
for(i in 1:10){accuracies[i] = sum(knn_predictions[,i] == y_test)/length(y_test)}


