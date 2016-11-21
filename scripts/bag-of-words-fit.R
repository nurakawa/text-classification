# =====================================================================================
# title: text-classification/bag-of-words-fit.R
# author: Nura Kawa
# packages used: {tm}, {stringr}, {glmnet}
# description: fit bag-of-words model
# =====================================================================================

# load glmnet
library(glmnet)

# load data
load("../data/dtm-items.RData")

# glmnet classifier with L1 penalty
NFOLDS = 5 # number of cross validation folds

t1 = Sys.time() # start timer

print(t1)

glmnet_classifier = cv.glmnet(x = dtm_train, 
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

print(difftime(Sys.time(), t1, units = 'min'))

plot(glmnet_classifier)

preds = predict(glmnet_classifier, 
                newx = dtm_test,
                type = "class")

sum(preds == y_test)/length(y_test)

table(preds, y_test)

print("EOF")
# =====================================================================================
