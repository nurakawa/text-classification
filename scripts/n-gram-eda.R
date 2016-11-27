# =====================================================================================
# title: n-gram-eda.R
# author: Nura Kawa
# summary: save barplots and wordclouds of term frequency for N=1:4
# =====================================================================================

# =====================================================================================
# downloading libraries
# =====================================================================================
library(wordcloud)


# =====================================================================================
# global environment
# =====================================================================================
categories <- c("business", 
                "entertainment", 
                "politics", 
                "sports", 
                "tech")

# function to select category
find_category <- function(x, c){which(x == c)}


# =====================================================================================
# loop
# =====================================================================================


for(N in 1:4)
{
  print(paste0("Currently, N = ", N))
  
  # load data created in n-grams.R
  load(paste0("../data/dtms-ngrams-N-",N, ".RData"))
  
  # calculate overall term frequency
  most_frequent_terms <- sort(colSums(rbind(dtm_train, dtm_test)), 
                              decreasing = TRUE)
  
  # barplot of overall term frequency
  f_name <- paste0("../images/n-gram/n=", N, "/barplot.pdf")
  
  pdf(f_name)
  barplot(most_frequent_terms[1:50],
          cex.axis = 0.8,
          cex.names = 0.8,
          border = "grey",
          las = 2,
          main = paste0("Most Frequent Terms, N =", N))
  dev.off()
  rm(f_name)
  
  # barplot per category
  for(i in 1:5)
  {
    freq_terms <- sort(colSums(dtm_train[find_category(y_train, i),]), 
                       decreasing = TRUE)
    
    f_name <- paste0("../images/n-gram/n=", N, "/barplot-", categories[i],".pdf")
    pdf(f_name)
    barplot(freq_terms[1:70],
            cex.axis = 0.8,
            cex.names = 0.8,
            border = "grey",
            col = "grey",
            las = 2,
            main = paste0("Most Frequent Terms: ", categories[i], ", N = ", N))
    dev.off()
    rm(f_name)
  }
  
  # wordcloud per category
  for(i in 1:5)
  {
    f_name <- paste0("../images/n-gram/n=", N, "/wordcloud-", categories[i], ".pdf")
    
    freq_terms <- sort(colSums(dtm_train[find_category(y_train, i),]), decreasing = TRUE)
    
    pdf(f_name)
    wordcloud(names(freq_terms)[7:60], 
              freq_terms[7:60], 
              scale=c(2, .2), 
              random.order = FALSE,
              random.color = FALSE,
              colors = colors()[65:77])
    dev.off()
    rm(f_name)
  }
}










