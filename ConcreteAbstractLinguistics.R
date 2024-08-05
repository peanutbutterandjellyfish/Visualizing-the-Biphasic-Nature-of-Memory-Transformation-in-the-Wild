packages <- c("doc2concrete", "tidyverse") # https://cran.r-project.org/web/packages/doc2concrete/doc2concrete.pdf

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

setwd("C:/Users/TinnerF/Dropbox/RCode/MemoryEngrams")

# Import Dictionaries
# https://www.ims.uni-stuttgart.de/forschung/ressourcen/experiment-daten/affective-norms/
AAIV <- data.table::fread("ratings_lrec16_koeper_ssiw.txt", encoding = "UTF-8")
AAIV$Word <- tolower(AAIV$Word)

# Import Reviews
df <- data.table::fread("LIWC_Output.csv", encoding = "UTF-8") 

# Add Document IDs to Original df For Later Merge
# List of All Lemmatized Words in Each Review That Was Then Part of Speech Tagged
# See R Script: POS_HC_DATA.R 
POS <- data.table::fread("Annotated_HC_POS.csv", encoding = "UTF-8")
doc_id <- POS %>% group_by(doc_id)%>%
  summarise(
    N = n()
  )
df <- bind_cols(df, doc_id)

# Remove Punctuation, Numbers
POS <- POS %>% filter(!is.na(upos) & 
                         upos != "NUM" & 
                         upos != "SCONJ" &
                         upos != "SYM" &
                         upos != "X")
POS$lemma <- tolower(POS$lemma)

# CLEAN LEMMAS

# Define a function to remove punctuation
remove_punctuation_and_numbers <- function(text) {
  # Use regular expression to replace all punctuation and numbers with an empty string
  cleaned_text <- gsub("[[:punct:]0-9]", "", text)
  return(cleaned_text)
}
# Apply the function to the text column
POS$lemma_cleaned <- sapply(POS$lemma, remove_punctuation_and_numbers)
POS <- POS %>% filter(lemma_cleaned != "")
# Remove Whitespace
POS$lemma_cleaned <- stringr::str_trim(POS$lemma_cleaned)

# Compute Word Length
POS$NCHAR <- stringr::str_length(POS$lemma_cleaned)
POS$lemma <- POS$lemma_cleaned
POS <- POS %>% group_by(doc_id, lemma_cleaned) %>%
  mutate(N_Word_Review = n())


# REMOVE DUPLICATES - NOT REQUIRED FOR POS ANALYSIS ONLY 
AAIV_UNIQUE <- AAIV[!duplicated(AAIV$Word), ]
POS_UNIQUE <- POS[!duplicated(POS$lemma_cleaned), ]
# MERGE DATA SETS 
AAIV_UNIQUE <- AAIV_UNIQUE %>% rename(lemma_cleaned = Word)

WordType <- inner_join(POS_UNIQUE, AAIV_UNIQUE, by="lemma_cleaned")

# Create Dummy Variable That Mimicks the BigWords Variable (BigWords <= 7 Characters)
WordType$NCHAR_dummy <- ifelse(WordType$NCHAR <= 7, '<=7', '>7')
############################################################################
# Plots
# BOXPLOT
# Calculate the overall mean of AbstConc
overall_mean_AbstConc <- mean(WordType$AbstConc, na.rm = TRUE)

# Subtract this mean from each AbstConc value to center the data
WordType <- WordType %>%
  mutate(AbstConc_Centered = AbstConc - overall_mean_AbstConc)

# Prepare the group-wise means for other variables, keeping the mean-centered AbstConc for plotting
Group_LEMMA <- WordType %>%
  group_by(lemma) %>%
  mutate(
    AbstConc = mean(AbstConc_Centered, na.rm = TRUE),  # Use mean-centered AbstConc here
    Arou = mean(Arou, na.rm = TRUE),
    IMG = mean(IMG, na.rm = TRUE),
    Val = mean(Val),
    N = sum(N_Word_Review), 
    NCHAR = mean(NCHAR)
  )

# Plotting using the mean-centered AbstConc values
Boxplot <- WordType %>%
  filter(upos != "PUNCT") %>%
  arrange(desc(AbstConc_Centered)) %>%
  ggplot(aes(x = upos, y = AbstConc_Centered, fill = NCHAR_dummy)) +
  geom_boxplot(alpha = 0.5, position = position_dodge(0.8), varwidth = FALSE) +
  scale_size(range = c(.1, 24), name = "Number of Words per Linguistic Class") +
  labs(
    x = "",
    y = "Abst < 0 < Conc",
    fill = "Word Length"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(angle = 90, size = 18),
    axis.text.y = element_text(size = 18),
    legend.position = "top",
    legend.key = element_rect(fill = "white"),
    text = element_text(size = 18)
  ) +
  guides(fill = guide_legend(title = "Word Length")) +
  coord_flip()+
  ylim(-2, 2)
#################################################################################
# MEAN CENTER DATA
# First, calculate the mean of AbstConc across all data
overall_mean_AbstConc <- mean(WordType$AbstConc, na.rm = TRUE)

# Use mutate to subtract this mean from the AbstConc to center the data
# Calculate overall mean of AbstConc for mean centering
overall_mean_AbstConc <- mean(WordType$AbstConc, na.rm = TRUE)

# Mean centering AbstConc and calculate necessary statistics per group
WordType <- WordType %>%
  filter(upos != "PUNCT") %>%
  group_by(upos, NCHAR) %>%
  summarize(
    AbstConc = mean(AbstConc, na.rm = TRUE),
    AbstConc_Centered = mean(AbstConc, na.rm = TRUE) - overall_mean_AbstConc,
    Arou = mean(Arou, na.rm = TRUE),
    IMG = mean(IMG, na.rm = TRUE),
    Val = mean(Val, na.rm = TRUE),
    N = sum(N_Word_Review),
    .groups = 'drop'
  )


# Plotting the centered AbstConc with a general trend line across all subgroups
AbstConc <- ggplot(WordType, aes(x = NCHAR, y = AbstConc_Centered, size = N, color = upos)) +
  geom_point(alpha = 0.6) +  # Plot points with transparency
  geom_smooth(method = "lm", se = TRUE, color = "black", size = 1) +  # Add a single smooth line for all data with confidence band
  labs(
    x = "Word Length   ",
    y = "Abst < 0 < Conc",
    color = "Lexical Class",
    size = "Number of Words"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    text = element_text(size = 18),
    axis.text.x = element_text(angle = 90, size = 18),
    axis.text.y = element_text(size = 18),
    legend.direction="horizontal",
    legend.position = c(0.5, 0.84)
  ) +
  guides(
    color = guide_legend(title.position = "top", override.aes = list(size = 6)),
    size = guide_legend(title.position = "top")
  )+ylim(-2, 2)
    
ggpubr::ggarrange(AbstConc, Boxplot, ncol=2, nrow=1, labels = c("A", "B"))

