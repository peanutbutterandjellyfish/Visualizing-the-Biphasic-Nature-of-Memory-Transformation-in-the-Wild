# install packages - R version 4.2.3 (2023-03-15 ucrt)
install.packages("dplyr") 
install.packages("stringr")
install.packages("udpipe")
install.packages("flextable")
install.packages("here")
# install klippy for copy-to-clipboard button in code chunks
install.packages("remotes")


# load packages
library(dplyr)
library(stringr)
library(udpipe)
library(flextable)


# Clear Workspace
rm(list = ls())

# Set Workspace
setwd("C:/Users/TinnerF/Dropbox/RCode/MemoryEngrams")

# Import Data
df <- data.table::fread("LIWC_Output.csv", encoding = "UTF-8") 
df <- df %>% filter(review.provided == 1 & !is.na(text)) 
#df <- df[sample(nrow(df), size = 305073, replace = TRUE), ]


# Convert Character Date to Date
df$reviewDate <- lubridate::dmy(df$reviewDate)
df$reviewMonth <- as.numeric(substr(df$reviewDate, start = 6, stop = 7))

# download language model
m_ger   <- udpipe::udpipe_download_model(language = "german-gsd")
# load model 
m_ger <- udpipe_load_model(file = "german-gsd-ud-2.5-191206.udpipe")


library(udpipe)
library(data.table)
library(parallel)
df_annotate <- df %>% select(review_day, text, reviewID)
ud_model <- udpipe_download_model(language = "german-gsd")
ud_model <- udpipe_load_model(ud_model$file_model)

annotate_splits <- function(x) {
  x <- as.data.table(udpipe_annotate(ud_model, x = x$text,
                                     doc_id = x$reviewID, tagger = "default",
                                     parser = "none"))
  # Remove not required columns
  x <- x[, c("sentence", "feats", "head_token_id", "dep_rel", "deps") := NULL]
  return(x)
}

corpus_splitted <- split(df_annotate, seq(1, nrow(df_annotate), by = 2))
annotation <- mclapply(corpus_splitted, FUN = function(x) annotate_splits(x))
annotation <- rbindlist(annotation)leng


write.csv(annotation, "Annotated_HC_POS.csv")
