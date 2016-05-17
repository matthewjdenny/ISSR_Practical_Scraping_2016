# Scraping Twitter and Facebook
# based on a tutorial available: https://github.com/SMAPPNYU/smappR
# Tutorial developed by Matthew J. Denny
# email mdenny@psu.edu with any comments or questions

toInstall <- c("ROAuth","devtools")
install.packages(toInstall, repos = "http://cran.r-project.org")
# R packages to get twitter and Facebook data
devtools::install_github("streamR", "pablobarbera", subdir="streamR")
devtools::install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")






















