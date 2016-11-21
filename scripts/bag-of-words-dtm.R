# =====================================================================================
# title: text-classification/bag-of-words.R
# author: Nura Kawa
# packages used: {tm}, {stringr}
# description: pre-processes data for bag-of-words model
# =====================================================================================

# =====================================================================================
# downloading data
# =====================================================================================

load("../data/bbc_matrix.RData")

# =====================================================================================
# split into train/test
# =====================================================================================
set.seed(1)
selected_columns <- sample(1:nrow(bbc_matrix), floor(0.7*nrow(bbc_matrix)))

# data for training
x_train <- bbc_matrix[selected_columns,1]

# data for testing
x_test <- bbc_matrix[-selected_columns,1]

# classes for training
y_train <- factor(bbc_matrix[selected_columns,2]) 

# classes for testing
y_test <- factor(bbc_matrix[-selected_columns,2])

# =====================================================================================
# making a Document Term Matrix
# =====================================================================================

# download libraries
library(tm)
library(stringr)


make_dtm <- function(docs)
{
  
  # remove punctuation
  docs <- sapply(docs, function(x){x <- gsub("[[:punct:]]", " ", x)})
  
  # make everything lower-case
  docs <- sapply(docs, tolower)
  
  # remove english stop-words
  
  rm_stopwords <- function(x)
  {
    x <- strsplit(x, " ")[[1]]
    x <- x[!(x %in% stopwords(kind = "en"))]
    x <- x[!(x == "")]
    x <- paste(x, collapse = " ")
  }
  
  docs <- sapply(docs, rm_stopwords)
  
  # stem words - remove the -ing or -ed endings to keep root of word
  docs <- stemDocument(docs)
  
  # remove numbers
  docs <- removeNumbers(docs)
  
  # create a corpus from our vectors
  corpus <- Corpus(VectorSource(docs))
  
  # create DocumentTermMatrix
  dtm <- DocumentTermMatrix(corpus)
  
  # return a DocumentTermMatrix
  return(dtm)
  
}


# =====================================================================================
# make dtm and term frequency plots
# =====================================================================================

dtm_train <- make_dtm(x_train)
dtm_test <- make_dtm(x_test) #this is not the final product for prediction.

# change to class Matrix
dtm_train <- as.matrix(dtm_train)
dtm_test <- as.matrix(dtm_test)


freq_train <- sort(colSums(dtm_train), decreasing = T)

pdf("../images/barplot-freq-train.pdf")
barplot(freq_train[1:30],
        las = 2,
        cex.names = 0.8,
        cex.axis = 0.8,
        col = "peachpuff3",
        main = "Most Frequent Terms: Training Data")
dev.off()

freq_test <- sort(colSums(dtm_test), decreasing = T)

pdf("../images/barplot-freq-test.pdf")
barplot(freq_test[1:30],
        las = 2,
        cex.names = 0.8,
        cex.axis = 0.8,
        col = "peachpuff3",
        main = "Most Frequent Terms: Testing Data")
dev.off()


# =====================================================================================
# make dtm for test data to be fit with glmnet model
# =====================================================================================
output_dtm <- matrix(NA, nrow=nrow(dtm_test), ncol=ncol(dtm_train))

create_pred_dtm <- function(dtm_train, dtm_test)
{
  overlap <- which(colnames(dtm_train) %in% colnames(dtm_test))
  
  included <- colnames(dtm_test)[overlap]
  excluded <- colnames(dtm_test)[-(overlap)]
  
  included <- na.omit(included)
  excluded <- na.omit(excluded)
  
  for(i in 1:ncol(dtm_train))
  {
    selected_column <- colnames(dtm_train)[i]
    #print(paste("selected column:", selected_column))
    if((selected_column) %in% included)
    {
      replacement_column <- which(colnames(dtm_test) == selected_column) 
      print(paste("replacement column:", replacement_column))
      output_dtm[,i] = dtm_test[,replacement_column]
    }
    else
    {
      #print("replacement: NA")
      output_dtm[,i] = rep(0, nrow(output_dtm))
    }
    
  }
  return(output_dtm)
}


print("Creating Document Term Matrix for Testing Data")
timr <- Sys.time()
new_output_dtm <- create_pred_dtm(dtm_train, dtm_test)
print(difftime(Sys.time(), t1, units = 'min'))

colnames(new_output_dtm) <- colnames(dtm_train)

# renaming to original name
dtm_test <- new_output_dtm

# =====================================================================================
# save dtms
# =====================================================================================

save(dtm_train,
     y_train,
     y_test,
     dtm_test, 
     file = "../data/dtm-items.RData")

print("EOF")
# =====================================================================================