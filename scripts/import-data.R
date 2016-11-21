# ==================================================================================================
# title:    Initial Data Preparation
# author:   Nura Kawa
# summary:  This script loads bbc data and prepares it for analysis. Outputs bbc_matrix.RData
# ==================================================================================================

# current working directory

# names of folders and number of files

folders <- c("business", "entertainment","politics","sport","tech")
number_of_files <- c(510,386,417,511,401)

# generating names of files

article_numbers = c(paste0("00", 1:9), paste0("0", 10:99), 100:511) 
all_file_names <- paste0(folders,"/", rep(article_numbers, each=5),".txt")

# re-order files
order <- c(grep("business", all_file_names),
           grep("entertainment", all_file_names),
           grep("politics", all_file_names),
           grep("sport", all_file_names),
           grep("tech", all_file_names))

all_file_names <- all_file_names[order]
downloaded_file <- character(length(all_file_names))

# loop to read in files

for(i in 1:length(all_file_names))
{
  file_to_download <- paste0("../data/raw-data/",all_file_names[i])
  if(!(file.exists(file_to_download)))
  {
    print("File not found!")
    downloaded_file[i] = NA
    next
  }
  testing <- readLines(file_to_download)
  downloaded_file[i] = paste(testing, sep = "", collapse = "")
}

downloaded_file <- na.omit(downloaded_file) #removing the NAs
class(downloaded_file)

category <- rep(folders, number_of_files)
bbc_matrix <- cbind(downloaded_file, category)
colnames(bbc_matrix) <- c("file", "category")
bbc_matrix[,2] <- factor(bbc_matrix[,2])

# save the bbc_matrix
save(bbc_matrix, file = "../data/bbc_matrix.RData")
