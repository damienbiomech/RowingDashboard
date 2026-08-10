library(shiny)
library(tidyverse)
library(data.table)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(DBI)
library(smartabaseR)
library(httr2)
library(readr)

################################################################################

# Helpers #

cDir <- '/srv/shiny-server'

shiny::addResourcePath('www', '/srv/shiny-server/www')

options(shiny.host = '0.0.0.0')
options(shiny.port = 80)

headers <- c('X-api-key' = Sys.getenv('LUDIS_API_TOKEN'))
LUDIS_KEY <- Sys.getenv("LUDIS_API_TOKEN")
dataset_id = "90c3600"
print(LUDIS_KEY)
print(dataset_id)
##############################################################################

## Import Local Data Files ###

# Load Athlete & Club list ##
filename = 'Athlete_list.csv'
url_file  <- sprintf(
  "https://prod-backend.ludisanalytics.com/v2/api/ludisurl/%s?filePath=%s", dataset_id, filename)
print(url_file)