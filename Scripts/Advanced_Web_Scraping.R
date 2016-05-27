# Advanced Web Scraping Tutorial
# Developed by: Matthew J. Denny
# email: mdenny@psu.edu with questions or comments

# preliminaries, make sure we have the right packages downloaded
# install.packages("rvest", dependencies = TRUE)
# install.packages("stringr", dependencies = TRUE)

rm(list = ls())
setwd("~/Desktop")
library(rvest)
library(stringr)

# for this example, we are going to collect all available infomration on members
# of Congress available on congress.gov since 1929.

# We are going to use a tool tha will make it easier to automatically extract
# fields from a webpage:
# go to this website and install the Selectorgadget java applet:
# https://cran.r-project.org/web/packages/rvest/vignettes/selectorgadget.html


# function to extract metadata on a legislator
extract_metadata <- function(text) {
    # replace newlines with nothing
    text <- str_replace_all(text,"\\n","")
    # split on two or more spaces
    text <- str_split(text, "[\\s]{2,}")[[1]]
    # break up name to extract chamber and name
    chamber_name <- str_split(text[3]," ")[[1]]
    # first is chamber
    chamber <- chamber_name[1]
    # rest is name
    name <- paste0(chamber_name[-1], collapse = " ")
    # combine together in to a vector that can be row bound together. If the
    # member is a Senator, then district is NA.
    if(chamber == "Senator") {
        ret <- c(name, chamber, text[5], NA , text[7], text[9])
    } else {
        # deal with territories that do not have districts
        if (length(text) == 10) {
            ret <- c(name, chamber, text[5],NA, text[7], text[9])
        } else {
            ret <- c(name, chamber, text[5], text[7], text[9], text[11])
        }
    }
    return(ret)
}

# create a null object to append results to
legislator_data <- NULL

# get the total number of pages we will need to scrape
pages <- ceiling(2181/250)
for (i in 1:pages) {
    cat("Currently working on block",i,"of",pages,"\n")
    html <- read_html(paste("https://www.congress.gov/members?pageSize=250&page=",i,sep = ""))
    # use the tags we got off of Selectorgadget
    web_page_results <- html_nodes(html, ".results_list a")
    # get member's webpages
    web_pages <- html_attr(web_page_results, "href")
    # now get the member metadata
    metadata_results <- html_nodes(html, ".results_list")
    # parse this into a list with one entry per legislator
    metadata_list <- html_children(metadata_results)

    # loop through the list to extract metadata for each legislator
    cur_metadata <- NULL
    for (j in 1:length(metadata_list)) {
        text <- html_text(metadata_list[[j]])
        cur_metadata <- rbind(cur_metadata,extract_metadata(text))
    }

    # add on web pages
    cur_metadata <- cbind(cur_metadata,web_pages)

    # add everything onto the full metadata file
    legislator_data <- rbind(legislator_data, cur_metadata)

    # make sure we sleep for a full minute to prevent ourselves from being rate
    # limited
    Sys.sleep(30)
}

# set column names
legislator_data <- data.frame(Name = legislator_data [,1],
                              Position = legislator_data [,2],
                              State = legislator_data [,3],
                              District = legislator_data [,4],
                              Party = legislator_data [,5],
                              Years_of_Service = legislator_data [,6],
                              URL = legislator_data [,7],
                              stringsAsFactors = FALSE)

# save the data
save(legislator_data, file = "Legislator_Data_1929-2016.Rdata")


##################################################
########## Lets try going a bit further ##########
##################################################

# I have started a second scraping script below that builds on the data we
# collected above. You will be asked to extend it.

# reload in the data
load("Legislator_Data_1929-2016.Rdata")
additional_legislator_data <- NULL

# loop over each legislator's webpage
for (i in 1:nrow(legislator_data)) {
    cat("Currently working on legislator",i,"of",nrow(legislator_data),"\n")
    html <- read_html(legislator_data$URL[i])
    # use the tags we got off of Selectorgadget to find metadata on bills that
    # legislator introduced and statistics about them
    metadata_results <- html_nodes(html, "#content :nth-child(1)")
    # get all of the metadata items in a list object
    metadata_list <- html_children(metadata_results)

    # find information about the bills that legislator introduced.
    for (j in 1:length(metadata_list)) {
        # break the first time we encounter this string
        if  (grepl("collapseSponsorship",html_text(metadata_list[[j]]))) {
            text <- html_text(metadata_list[[j]])
            break
        }
    }

    # birthdate
    birthdate <- html_nodes(html, ".birthdate")
    birthdate <- html_text(birthdate)

    # link to bio page
    bio_link <- html_nodes(html, ".member_bio_link")
    bio_link <- html_attr( bio_link, "href")

    # try writing a function to extract useful information from the 'text'
    # variable...

    # make sure we sleep for a full minute to prevent ourselves from being rate
    # limited
    Sys.sleep(6)
}

