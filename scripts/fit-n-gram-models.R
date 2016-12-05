# =====================================================================================
# title: n-grams.R
# author: Nura Kawa
# summary: pre-processes data for n-gram models (including n=1/bag-of-words)
#          outputs items for classification in .RData form
# =====================================================================================

# =====================================================================================
# downloading data
# =====================================================================================
load("../data/bbc_matrix.RData")

# =====================================================================================
# downloading libraries
# =====================================================================================
library(tau)
library(tm)
library(stringr)
library(Matrix)
library(glmnet)

# =====================================================================================
# split into training/testing
# =====================================================================================

# extract text from bbc_matrix
txt <- bbc_matrix[,1] 

# set random seed
set.seed(100)

# randomly select training columns
selected_columns <- sample(1:nrow(bbc_matrix), floor(0.7*nrow(bbc_matrix)))

# --------------------------------------------------------------------------------------------------
# select data for DTM and for response
# --------------------------------------------------------------------------------------------------
dtm_train <-txt[selected_columns] 
dtm_test <- txt[-selected_columns]
y_train <- factor(bbc_matrix[selected_columns,2]) 
y_test <- factor(bbc_matrix[-selected_columns,2])

# =====================================================================================
# pre-process data: create corpus
# =====================================================================================

clean_text <- function(docs)
{
  # remove punctuation
  #docs <- sapply(docs, function(x){x <- gsub("[[:punct:]]", " ", x)})
  
  # make everything lower-case
  docs <- sapply(docs, tolower)
  
  #remove english stop-words
  rm_stopwords <- function(x)
  {
    x <- strsplit(x, " ")[[1]]
    x <- x[!(x %in% stopwords(kind = "en"))]
    x <- x[!(x == "")]
    x <- paste(x, collapse = " ")
  }
  
  docs <- sapply(docs, rm_stopwords)
  
  #stem words - remove the -ing or -ed endings to keep root of word
  docs <- stemDocument(docs)
  
  # remove numbers
  docs <- removeNumbers(docs)
  
  return(docs)
}

pre_process <- function(docs)
{
  docs <- unlist(lapply(docs, clean_text))
  #txt <- unlist(lapply(txt, clean_text))
  cp_txt <- character(length(docs))
  for(i in 1:length(docs)){cp_txt[i] <- docs[[i]]} 
  docs <- cp_txt
  
  corpus <- Corpus(VectorSource(docs))
  return(corpus)
}

# --------------------------------------------------------------------------------------------------
# create corpus for training and testing
# --------------------------------------------------------------------------------------------------
corpus_train <-pre_process(dtm_train)
corpus_test <- pre_process(dtm_test)


# =====================================================================================
# Building N-Gram Model
# =====================================================================================

# Here we use N from 1(bag-of-words model) to 4

timr <- proc.time() #timing loop

for(N in 1:4)
{
  print((paste0("Beginning N-Gram Model for N = ", N)))
  
  # =====================================================================================
  # tokenize n-grams
  # =====================================================================================
  
  tokenize_ngrams <- function(x, n=N) #x is a corpus, n is number of words
  {
    # uses the function textcn in {tau}
    c <- character(length(x))
    for(i in 1:length(x)){c[i] = (as.character(x[[i]]))}
    c <- paste(c, collapse = "")
    return(names(textcnt(c, n, method = "string", tolower = TRUE)))
  }
  
  create_dtm <- function(corpus_train)
  {
    #weight_function <- function(x){weightTfIdf(x, normalize = FALSE)}
    dtm_train <- DocumentTermMatrix(corpus_train,
                                    control=list(tokenize=tokenize_ngrams,
                                                 wordLengths = c(1, Inf)))
    
    #attributes(dtm_train)
    
    mat_train <- sparseMatrix(i=dtm_train$i, j=dtm_train$j, x=dtm_train$v,
                              dims=c(dtm_train$nrow, dtm_train$ncol))
    colnames(mat_train) <- dtm_train$dimnames[[2]]
    
    # keeping only > 1
    mat_train <- mat_train[,colSums(mat_train) > 1]
    mat_train <- as.matrix(mat_train)
    return(mat_train)
  }
  
  dtm_train <- create_dtm(corpus_train)
  dtm_test <- create_dtm(corpus_test)
  
  # getting the n-grams
  
  n_grams_train <- sort(colSums(dtm_train), decreasing = TRUE)
  n_grams_test <- sort(colSums(dtm_test), decreasing = TRUE)
  
  # getting the overlapping terms
  
  overlap <- n_grams_test[(names(n_grams_test) %in% names(n_grams_train))]
  overlap <- names(overlap)
  overlap <- overlap[nchar(overlap)>2]
  
  if(N > 1)
  {
    too_short <- integer(length(overlap))
    for(i in 1:length(overlap))
    {
      too_short[i] <- (sum(c(1, 2) %in% nchar(strsplit(overlap[i]," ")[[1]])) > 0)
    }
    
    overlap <- overlap[!(too_short)]
  }
  
  
  dtm_train <- dtm_train[,which(colnames(dtm_train) %in% overlap)]
  dtm_test <- dtm_test[,which(colnames(dtm_test) %in% overlap)]
  
  # --------------------------------------------------------------------------------------------------
  # Export Items to .Rdata object
  # --------------------------------------------------------------------------------------------------
  f_name <- paste0("../data/n-gram/dtms-ngrams-N-", N, ".RData")
  save(overlap,y_train, y_test, dtm_train, dtm_test, file = f_name)
  print(paste("Saved File:", f_name))
}

proc.time() - timr




# --------------------------------------------------------------------------------------------------
# Later will return to Tf-Idf weighting experimenting
# --------------------------------------------------------------------------------------------------

  

# # =====================================================================================
# # Tf-Idf weighting
# # =====================================================================================
# 
# tfidf=function(mat){
#   tf <- mat
#   id=function(col){sum(!col==0)}
#   idf <- log(nrow(mat)/apply(mat, 2, id))
#   tfidf <- mat
#   for(word in names(idf)){tfidf[,word] <- tf[,word] * idf[word]}
#   return(tfidf)
# }
# 
# barplot(sort(colSums(dtm_train),decreasing = T)[1:50], las=2, cex.names = 0.7)
# weighting_train <- tfidf(dtm_train)
# weighting_test <- tfidf(dtm_test)
# 
# # weighting matrix by TF-IDF: multiply your DTM by the transpose of its TF-IDF weights matrix
# dtm_train <- dtm_train %*% t(weighting_train)
# 
# dtm_test <- dtm_test %*% t(weighting_test)
# 


