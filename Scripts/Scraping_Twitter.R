# Scraping Twitter
# based on a tutorial available: https://github.com/pablobarbera/streamR
# email mdenny@psu.edu with any comments or questions

# before you do anything with this tutorial, make sure you have a Twitter and/or
# Facbook account. You will need these to get the proper access to download
# tweets and posts.

install.packages(c("ROAuth","devtools","ggplot2", "grid"),
                 repos = "http://cran.r-project.org")
# R packages to get twitter and Facebook data
devtools::install_github("streamR", "pablobarbera", subdir = "streamR")
devtools::install_github("Rfacebook", "pablobarbera", subdir = "Rfacebook")

# before you can scrape Twitter, you will need to follow the directions here:
# https://github.com/SMAPPNYU/smappR#b-creating-your-own-twitter-oauth-token
# to create an authorization token which can be used access the Twitter API

library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

# you will need to fill in these two as suggested in the directions given at the
# webpage above:
consumerKey <- "XXX"
consumerSecret <- "XXX"
my_oauth <- OAuthFactory$new(consumerKey = consumerKey,
                             consumerSecret = consumerSecret,
                             requestURL = requestURL,
                             accessURL = accessURL,
                             authURL = authURL)

# now run this line of code and go to the URL it prints out (it may also just
# take you directly to the webpage).
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem",
                                        package = "RCurl"))

# now save your credentials for future use!
setwd("~/Dropbox/Credentials/")
save(my_oauth, file = "my_oauth.Rdata")

# now lets walk through a simple example collecting tweets containing the
# terms "Clinton" and "Trump" in the next sixty seconds. This example follows
# along with: https://github.com/pablobarbera/streamR

# load the streamR library
library(streamR)

# load in your access credential we created above
setwd("~/Dropbox/Credentials/")
load("my_oauth.Rdata")

# now set your working directory to the location where you would like to save
# the tweets you are about to collect
setwd("~/Desktop")

# here we are going to use the filter function which uses dome sort of criteria
# for determining which tweets should be saved.
filterStream("tweets.json",
             track = c("Clinton", "Trump"),
             timeout = 60,
             oauth = my_oauth)

# load in the tweets from the tweets.json file where they were stored and turn
# them into a data.frame
tweets.df <- parseTweets("tweets.json",
                         simplify = TRUE)


# we can also just collect a random (approximately 1%) sample of all tweets on
# twitter. I suggest only doign this for a short period of time on your
# computer:

# capture the tweets
sampleStream("tweetsSample.json",
             timeout = 30,
             oauth = my_oauth,
             verbose = FALSE)

# load them into a data.frame and take a look!
tweets.df2 <- parseTweets("tweetsSample.json",
                         verbose = FALSE)


# now we can follow Pablo's example and filter on tweets located in the US and
# overlay them on a map.

# start by filtering the stream on location (lat/long)
filterStream("tweetsUS.json", locations = c(-125, 25, -66, 50), timeout = 120,
             oauth = my_oauth)

# parse tweets
tweets.USA <- parseTweets("tweetsUS.json", verbose = FALSE)

# load libraries necessary for plotting
library(ggplot2)
library(grid)

# get the raster data to plot a map of the U.S.A.
map.data <- map_data("state")

# get the points we are going to plot
points <- data.frame(x = as.numeric(tweets.USA$lon),
                     y = as.numeric(tweets.USA$lat))
points <- points[points$y > 0, ]

points <- data.frame(x = as.numeric(tweets.df$lon),
                     y = as.numeric(tweets.df$lat))

# plot the points on the map using ggplot2
ggplot(map.data) + geom_map(aes(map_id = region),
                            map = map.data,
                            fill = "white",
                            color = "grey20",
                            size = 0.25) +
    expand_limits(x = map.data$long, y = map.data$lat) +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          plot.background = element_blank(),
          plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) +
    geom_point(data = points,
               aes(x = x, y = y),
               size = 1,
               alpha = 1/5,
               color = "darkblue")
