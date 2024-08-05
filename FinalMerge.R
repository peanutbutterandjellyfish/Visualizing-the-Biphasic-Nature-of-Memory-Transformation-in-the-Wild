# Purpose: Create LIWC_Input.csv
# Code Book For Weather Data: https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/DESCRIPTION_obsgermany_climate_daily_kl_historical_en.pdf

# Install and Load Packages
packages = c("tidyverse", "data.table")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Clear Workspace
rm(list = ls())

# Set Workspace
setwd("C:/Users/TinnerF/Dropbox/RCode/MemoryEngrams")

# Merge Bewertungen
bew_kurz <- data.table::fread('bew_kurz.csv', encoding = "UTF-8")
bew_lang <- data.table::fread('bew_lang.csv', encoding = "UTF-8")

bew_lang <- bew_lang %>%
  mutate(across(all_of(c("textHotel", "textLage", "textService", 
                         "textGastronomie", "textSport", "textZimmer", "textTipp")), 
                ~ifelse(is.na(.), "", .)))

# combine text columns in one
bew_lang$text <- str_c(bew_lang$textHotel, '', bew_lang$textLage, '',
                       bew_lang$textService, '', bew_lang$textGastronomie, '',
                       bew_lang$textSport, '', bew_lang$textZimmer, '', bew_lang$textTipp)

# Combine bew_lang and bew_kurz
bew_lang <- bew_lang %>% select(id, bid, hname, oname, rid, lid, sterne, text)
bew_kurz <- bew_kurz %>% select(id, bid, hname, oname, rid, lid, sterne, text)
bew <- rbind(bew_kurz, bew_lang)
bew <- bew %>% filter(!is.na(text))
bew <- bew %>% filter(text != "")
bew <- bew %>% filter(sterne >= 1 & sterne <=6)

# Check if there are duplicates based on text
duplicated <- bew[duplicated(bew$text)]
identify <- bew %>% filter(text %in% duplicated$text)

# Remove duplicates
bew <- bew[!duplicated(bew$text)]

# Remove Testbewertungen (# 84)
bew <- bew %>% filter(!grepl('Testbewertung', text))
readr::write_csv(bew, 'ReviewsOnly.csv')

dff <- data.table::fread("Bookings_with_reviews.csv", encoding = "UTF-8")

# Merge 1st time to reduce number of reviews to check for language
dff$reviewID.x <- as.double(dff$reviewID.x)
bew$bid <- as.numeric(bew$bid)
df <- merge(dff, bew, by.x = "reviewID.x", by.y = "bid", all.x = TRUE)
# Remove Duplicated Columns (columns ending in .y)
df <- select(df,-ends_with(".y"))

# Filter Out Reviews Not Written in German
library(textcat)
revs <- df %>% filter(review.provided==1)
revs$language <- textcat(revs$text)
revs <- revs %>% dplyr::filter(language == 'german')
# Check for Duplicates Again
text <- revs[(duplicated(revs$text)),] 
IDs <- text$reviewID.x
# Remove Duplicates
revs <- revs %>% filter(!reviewID.x %in% IDs)

# Remerge with bookings
df <- merge(dff, revs, by = "reviewID.x", all.x = TRUE)

# Remove Duplicated Columns (columns ending in .y)
df <- select(df, -ends_with(".y"))
df$hname.x <- NULL
# Remove Suffixes (.x)
colnames(df)<-gsub(".x","",colnames(df))
names(df) <- make.names(names(df), unique = TRUE)
df <- df %>% rename(text = tt)
df <- df %>% filter(review.provided==1 & text != "")
#################################################################################
# Append BigWords Variable (Generated Using LIWC-22 Since LIWC-2015 Only Counts Six Letter Words)
BigWords <- read.csv("LIWC_BigWords_07_24.csv") %>% select(BigWords, reviewID, buID)
df$reviewID <- as.integer(df$reviewID)
df <- df %>%
  inner_join(BigWords, by = "reviewID") %>%
  distinct()

df <- df %>%
  select(-contains(".y")) %>%       # Remove columns with '.y' suffix
  rename_with(~ gsub("\\.x$", "", .), contains(".x"))  # Remove '.x' suffix from column names

readr::write_csv(df, 'LIWC_Input.csv')




