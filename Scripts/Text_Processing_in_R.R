# R code for basic text cleaning tutorial

#0. Preliminaries
rm(list = ls())
setwd("~/Dropbox/RA_and_Consulting_Work/ISSR_Practical_Scraping_2016/Data")

require(stringr)

#1.  Basic Text Cleaning
my_string <- "Example STRING, with numbers (12, 15 and also 10.2)?!"

lower_string <- tolower(my_string)

second_string <- "Wow, two sentences."

my_string <- paste(my_string,second_string,sep = " ")

my_string_vector <- str_split(my_string, " ")[[1]]


grep("\\?",my_string_vector)

grepl("\\?",my_string_vector[1])

str_replace_all(my_string, "e","___")

str_extract_all(my_string,"[0-9]+")[[1]]

nchar(my_string)

# Function to clean and tokenize a string
Clean_String <- function(string){
    # Lowercase
    temp <- tolower(string)
    #' Remove everything that is not a number or letter (may want to keep more
    #' stuff in your actual analyses).
    temp <- stringr::str_replace_all(temp,"[^a-z\\s]", " ")
    # Shrink down to just one white space
    temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
    # Split it
    temp <- stringr::str_split(temp, " ")[[1]]
    # Get rid of trailing "" if necessary
    indexes <- which(temp == "")
    if(length(indexes) > 0){
        temp <- temp[-indexes]
    }
    return(temp)
}

sentence <- "The term 'data science' (originally used interchangeably with 'datalogy') has existed for over thirty years and was used initially as a substitute for computer science by Peter Naur in 1960."
clean_sentence <- Clean_String(sentence)
print(clean_sentence)

# Function to clean a block of text (several strings)
Clean_Text_Block <- function(text){
    if(length(text) <= 1){
        # Check to see if there is any text at all with another conditional
        if(length(text) == 0){
            cat("There was no text in this document! \n")
            to_return <- list(num_tokens = 0, unique_tokens = 0, text = "")
        }else{
            # If there is , and only only one line of text then tokenize it
            clean_text <- Clean_String(text)
            num_tok <- length(clean_text)
            num_uniq <- length(unique(clean_text))
            to_return <- list(num_tokens = num_tok, unique_tokens = num_uniq, text = clean_text)
        }
    }else{
        # Get rid of blank lines
        indexes <- which(text == "")
        if(length(indexes) > 0){
            text <- text[-indexes]
        }
        # Loop through the lines in the text and use the append() function to
        clean_text <- Clean_String(text[1])
        for(i in 2:length(text)){
            # add them to a vector
            clean_text <- append(clean_text,Clean_String(text[i]))
        }
        # Calculate the number of tokens and unique tokens and return them in a
        # named list object.
        num_tok <- length(clean_text)
        num_uniq <- length(unique(clean_text))
        to_return <- list(num_tokens = num_tok, unique_tokens = num_uniq, text = clean_text)
    }
    return(to_return)
}

# make sure that you save this file in your current working directory (in my
# case the Desktop)
con <- file("Obama_Speech_2-24-09.txt", "r", blocking = FALSE)
text <- readLines(con)
close(con)

clean_speech <- Clean_Text_Block(text)
str(clean_speech)

# Read in the file
con <- file("Obama_Speech_1-27-10.txt", "r", blocking = FALSE)
text2 <- readLines(con)
close(con)

# Clean and tokenize the text
clean_speech2 <- Clean_Text_Block(text2)

#2. Create a Document Term Matrix
# install.packages("Rcpp",dependencies = T)
# install.packages("RcppArmadillo",dependencies = T)
# install.packages("BH",dependencies = T)

# jsut make sure you saved the cpp file to the currentworking directory
setwd("~/Dropbox/RA_and_Consulting_Work/ISSR_Practical_Scraping_2016/Scripts")
Rcpp::sourceCpp('Generate_Document_Word_Matrix.cpp')


#' Create a list containing a vector of tokens in each document for each
#' document. These can be extracted from the cleaned text objects as follows.
doc_list <- list(clean_speech$text,clean_speech2$text)

#' Create a vector of document lengths (in tokens)
doc_lengths <- c(clean_speech$num_tokens,clean_speech2$num_tokens)

#' Generate a vector containing the unique tokens across all documents.
unique_words <- unique(c(clean_speech$text,clean_speech2$text))

#' The number of unique tokens across all documents
n_unique_words <- length(unique_words)

#' The number of documents we are dealing with.
ndoc <- 2

#' Now feed all of this information to the function as follows:
Doc_Term_Matrix <- Generate_Document_Word_Matrix(
    number_of_docs = ndoc,
    number_of_unique_words = n_unique_words,
    unique_words = unique_words,
    Document_Words = doc_list,
    Document_Lengths = doc_lengths
)

#' Make sure to add column names to you Doc-Term matrix, then take a look!
colnames(Doc_Term_Matrix) <- unique_words


#3. Text processing the easy way

#' load the package and generate the document-term matrix
require(quanteda)
docs <- c(paste0(text,collapse = " "),paste0(text2,collapse = " "))
doc_term_matrix <- dfm(docs, stem = FALSE)

# find the additional terms captured by quanteda
missing <- doc_term_matrix@Dimnames$features %in% colnames(Doc_Term_Matrix)

# just includes all terms with hypens and '
doc_term_matrix@Dimnames$features[which(missing == 0)]
