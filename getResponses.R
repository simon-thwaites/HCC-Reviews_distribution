# getResponses.R
# simon.thwaites.biomech@gmail.com
# Created: 22/11/2021

library(googlesheets4)
library(readxl)
library(dplyr)

# ----  Connect to Google Drive ----
# Give access to googlesheets within your Google Drive.
# you will be prompted to allow these credentials to be cached in a local folder (1 = yes / 2 = No)
# web browser will pop up and prompt you to accept permissions
# then copy the authorization code to Rstudio

# gs4_auth()

# ---- Get the sheets ----
resp <- list() # for response data

# create tibble with the review year and its corresponding url
all_urls <- tibble(
  response.year = c("yr.18.19", 
                    "yr.19.20", 
                    "yr.20.21",
                    "yr.21.22"),
  url = c(gForms_path1,
          gForms_path2,
          gForms_path3,
          gForms_path4) # removed URL paths
)

nResponses <- dim(all_urls)[[1]] # get number of responses

# loop through number of responses and get the sheet data
for (ii in 1:nResponses) {

  # temp variable for creating list variable
  yr.temp <- all_urls$response.year[[ii]]

  # create list item and read the sheet, removing the first name and last name columns
  resp[[yr.temp]] <- read_sheet(all_urls$url[[ii]])[,-c(2,3)]
}