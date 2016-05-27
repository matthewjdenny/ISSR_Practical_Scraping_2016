# Text Processing and Analysis
# Developed by: Matthew J. Denny
# email: mdenny@psu.edu with questions or comments

# clear out environment
rm(list = ls())

# preliminaries, make sure we have the right packages downloaded
# install.packages("quanteda", dependencies = TRUE)
# install.packages("stringr", dependencies = TRUE)
# install.packages("devtools", dependencies = TRUE)
# devtools::install_github("matthewjdenny/SpeedReader")

# load packages
require(stringr)
require(quanteda)
# require(SpeedReader)

# set working directory (you will need to change this for your computer)
setwd("~/Dropbox/RA_and_Consulting_Work/ISSR_Practical_Scraping_2016/Data/Bill_Text")

# read in documents
documents <- rep("", length = 100)
# loop over documents
for (i in 1:100) {
    cat("currently working on bill:",i,"\n")
    # set the current bill number
    ind <- 97000 + i
    # get the text of the bill
    text <- readLines(paste("Bill_",ind,".txt", sep = ""))
    # collapse it together into a string and store it in a vector
    documents[i] <- paste0(text,collapse = " ")
}

# use quanteda to create a document term matrix
doc_term_matrix <- quanteda::dfm(documents,
                                 toLower = TRUE,
                                 removeNumbers = TRUE,
                                 removePunct = TRUE,
                                 removeSeparators = TRUE,
                                 removeTwitter = FALSE,
                                 stem = TRUE,
                                 language = "english")

# look at some of the vocabulary
head(doc_term_matrix@Dimnames$features)

# get column sums
word_counts <- colSums(doc_term_matrix)

# order word counts
word_counts <- word_counts[order(word_counts, decreasing = TRUE)]

# top words
head(word_counts,n = 100)

# bottom words
tail(word_counts,n = 20)
