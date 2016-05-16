# Write a function to go get the number of results that pop up for a given name in google scholar. First we will need to load a few packages which will help us out along the way.
install.packages("scrapeR")
install.packages("stringr")
library(scrapeR)
library(stringr)

get_google_scholar_results <- function(string, return_source = FALSE){
    
    # print out the input name
    cat(string, "\n")
    
    # make the input name all lowercase
    string <- tolower(string)
    
    # split the string on spaces
    str <- str_split(string," ")[[1]]
    
    # combine the resulting parts of the string with + signs so "Matt Denny" will end up as "matt+denny" which is what Google Scholar wants as input
    str <- paste0(str,collapse = "+")
    
    # add the name (which is now in the correct format) to the search querry and we have our web address.
    str <- paste("https://scholar.google.com/scholar?hl=en&q=",str,sep = "")
    
    # downloads the web page source code
    page <- getURL(str, .opts = list(ssl.verifypeer = FALSE))
    
    # search for the 'Scholar</a><div id="gs_ab_md">' string which occurs uniquely right before google Scholar tells you how many results your querry returned
    num_results <- str_split(page,'Scholar</a><div id=\\"gs_ab_md\\">')[[1]][2]
    
    # split the resulting string on the fist time you see a "(" as this will signify the end of the text string telling you how many results were returned. 
    num_results <- str_split(num_results,'\\(')[[1]][1]
    
    # Print out the number of results returned by Google Scholar
    cat("Querry returned", tolower(num_results), "\n")
    
    # Look to see if the "User profiles" string is present -- grepl will return true if the specified text ("User profiles") is contained in the web page source. 
    if(grepl("User profiles",page)){
        
        # split the web page source (which is all one string) on the "Cited by " string and then take the second chunk of the resulting vector of substrings (so we can get at the number right after the first mention of "Cited by ")
        num_cites <- str_split(page,"Cited by ")[[1]][2]
        
        # now we want the number before the < symbol in the resulting string  (which will be the number of cites)
        num_cites <- str_split(num_cites,"<")[[1]][1]
        
        # now let the user know how many we found
        cat("Number of Cites:",num_cites,"\n")
    }else{
        # If we could not find the "User profiles" string, then the person probably does not have a profile on Google Scholar and we should let the user know this is the case
        cat("This user may not have a Google Scholar profile \n")
    }
    
    # If we specified the option at the top that we wanted to return the HTML source, then return it, otherwise don't.
    if(return_source){
        return(page) 
    }
}

#now lets have some fun...
get_google_scholar_results("Joya Misra")

get_google_scholar_results("Laurel Smith-Doerr")

get_google_scholar_results("Noam Chomsky")

get_google_scholar_results("Gary Becker")
