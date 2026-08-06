
### Rowing Dashboard ###

library(tidyverse)
library(plotly)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
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

###############################################################################

### Import Local Data Files ###

## Load Athlete & Club list ##
#filename = 'Athlete_list.csv'
#url_file  <- sprintf(
#  "https://prod-backend.ludisanalytics.com/v2/api/ludisurl/%s?filePath=%s", dataset_id, filename)
#print(url_file)
#resp <- request(url_file) |>
#req_headers(`x-api-key` = LUDIS_KEY) |>
#req_perform()
#Club_list <- fread(I(resp_body_string(resp)))
Club_list <- read.csv("./Athlete_list.csv") 

## Load Benchmarks Data & Physiology Data ##
#filename = 'Athlete Profile_DB.csv'
#url_file  <- sprintf(
#  "https://prod-backend.ludisanalytics.com/v2/api/ludisurl/%s?filePath=%s", dataset_id, filename)
#resp <- request(url_file) |>
#req_headers(`x-api-key` = LUDIS_KEY) |>
#req_perform()
#db <- fread(I(resp_body_string(resp)))
db <- fread("./Athlete Profile_DB.csv") 

db <- db %>% mutate(Date = as.Date(Date))
db <- left_join(db, Club_list, by = "Name", suffix = c("", ".y")) %>% 
  mutate(Gender = coalesce(Gender, Gender.y)) %>%
  select(-Gender.y)

setnames(db, names(db), gsub(" ", "_", names(db)))

## Load RA Benchmarks ##
#filename = 'benchmarks.csv',
#url_file  <- sprintf(
#  "https://prod-backend.ludisanalytics.com/v2/api/ludisurl/%s?filePath=%s", dataset_id, filename)
#resp <- request(url_file) |>
#req_headers(`x-api-key` = LUDIS_KEY) |>
#req_perform()
#benchmark_lookup <- fread(I(resp_body_string(resp)))
benchmark_lookup <- read.csv("./benchmarks.csv")

### Import Database Data ###

## AMS Download ##

# Credentials #
ams_un <- Sys.getenv("AMS_UN")
ams_pw <- Sys.getenv("AMS_PW")

# Step 1: Trunk Testing #
AMS_Trunk <- sb_get_event(form = "NSWIS - Rowing - Trunk Testing",
                          date_range = sb_date_range("5", "years"),
                          url = "ams.ausport.gov.au/nswis/",
                          username = ams_un,
                          password = ams_pw,
                          filter = sb_get_event_filter(
                            user_key = "about",
                            user_value = Club_list$Name
                          ))

df_trunk <- AMS_Trunk %>% rename(Name = about) %>% 
  mutate(Date = as.Date(start_date, format = "%d/%m/%Y")) %>% 
  select(Date, Name,`Prone Endurance`,`Supine Endurance`,
         `Left Side Hold`,`Right Side Hold`)
df_trunk <- left_join(df_trunk, Club_list, by = "Name")

gender_means <- df_trunk %>%
  group_by(Gender) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
            across(where(is.character), \(x) "Squad Mean"),
            across(where(\(x) inherits(x, "Date")), \(x) max(x, na.rm = TRUE)))

df_trunk <- bind_rows(df_trunk, gender_means)


# Step 2: Strength Testing #
AMS_Strength <- sb_get_event(form = "NSWIS - Rowing - Strength Testing",
                             date_range = sb_date_range("5", "years"),
                             url = "ams.ausport.gov.au/nswis/",
                             username = ams_un,
                             password = ams_pw,
                             filter = sb_get_event_filter(
                               user_key = "about",
                               user_value = Club_list$Name
                             ))

df_strength <- AMS_Strength %>% rename(Name = about) %>% 
  mutate(Date = as.Date(start_date, format = "%d/%m/%Y")) %>% 
  select(Date, Name, `Squat (kg)`,`Bench Pull (kg)`,`Deadlift (kg)`,`Bench Press (kg)`,`Pull/ Chin Ups (reps)`)
df_strength <- left_join(df_strength, Club_list, by = "Name") %>% 
  dplyr::filter(!is.na(Club)) %>% 
  rename_with(~ gsub(" \\(kg\\)", "", .)) %>% 
  rename_with(~ gsub(" \\(reps\\)", "", .))

strength_gender_means <- df_strength %>%
  group_by(Gender) %>%
  summarise(
    across(c(Squat, `Bench Pull`, Deadlift, `Bench Press`,`Pull/ Chin Ups`), \(x) mean(x, na.rm = TRUE)),
    Name = "Squad Mean",
    Date = as.Date(NA))

df_strength <- bind_rows(df_strength, strength_gender_means)


# Step 3: MSK Screening
AMS_MSK <- sb_get_event(form = "Rowing - MSK Screening",
                        date_range = sb_date_range("5", "years"),
                        url = "ams.ausport.gov.au/nswis/",
                        username = ams_un,
                        password = ams_pw,
                        filter = sb_get_event_filter(
                          user_key = "about",
                          user_value = Club_list$Name
                        ))

df_MSK <- AMS_MSK %>% rename(Name = about) %>% 
  mutate(Date = as.Date(start_date, format = "%d/%m/%Y")) %>% 
  select(Date, Name, starts_with(c("Thoracic Rotation","Active Slump",
                                      "Thomas Hip","Hip Flexion Left",
                                      "Hip Flexion Right","Hip Internal Rotation",
                                      "Active Knee","Ankle Dorsiflexion",
                                      "Sit & Reach","Long Sit Rockover")))

df_MSK <- left_join(df_MSK, Club_list, by = "Name") %>% 
  dplyr::filter(!is.na(Club))

MSK_gender_means <- df_MSK %>%
  group_by(Gender) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
            Name = "Squad Mean",
            Date = as.Date(NA))

df_msk <- bind_rows(df_MSK, MSK_gender_means)


# Step 4: Force Decks Trials

# Connect to NSWIS Database #
username_db <- Sys.getenv("username")
password_db <-  Sys.getenv("password")
ludis_db <-  Sys.getenv("ludis_ip")
mydb <- DBI::dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 18 for SQL Server",
                       Server = ludis_db,
                       Database = "nswis_dw",
                       UID = username_db,
                       PWD = password_db,
                       TrustServerCertificate='yes')
DBdirectory <- "dbo"

## Query Database for Athlete & Session Data ##
Col_list <- "about, start_date, Test_Type, Metric, Value"
query <- paste0("SELECT ",Col_list," FROM ",DBdirectory, ".Force_Decks_Flow")

df_FDecks <- DBI::dbGetQuery(mydb,query) %>% 
  dplyr::filter(about %in% Club_list$Name) %>%
  rename(Name = about) %>%
  mutate(Date = as.Date(start_date, format = "%d/%m/%Y")) %>%
  fill(`Test_Type`, .direction = "down")

df_FDecks <- left_join(df_FDecks, Club_list, by = "Name") %>%
  dplyr::filter(!is.na(Club))

FDecks_gender_means <- df_FDecks %>%
  group_by(Gender,Test_Type,Metric) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
            Name = "Squad Mean",
            Date = as.Date(NA), .groups = "drop_last") %>%
  dplyr::filter(!is.na(Value)) %>%
  dplyr::filter(!is.na(Test_Type))

df_FDecks <- bind_rows(df_FDecks, FDecks_gender_means)


# Step 5: Bridge Athletic Table #

## Query Database for Athlete & Session Data ##
query <- paste0("SELECT * FROM ",DBdirectory, ".BridgeAthletic_Questionnaire")

df_Bridge <- DBI::dbGetQuery(mydb,query) %>% 
  dplyr::filter(about %in% Club_list$Name) %>% 
  rename(Name = about) %>%
  mutate(Date = as.Date(`Date of Data`, format = "%Y-%m-%d"))

# Step 6: Biomechanics Data

Bins <- c(19.5,20.9,24,28,32,36,40,50)
Bin_labels <- c("20","20-24","24-28","28-32","32-36","36-40",">40")

## Load Boat Class & Age Group Means ##
SBS_ave <- fread("./DB_ave.csv")

## Query Database for Athlete SBS data ##

# Connect to Database #
ludis_ip <- Sys.getenv('ludis_ip')
mydb <- DBI::dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 18 for SQL Server",
                       Server = ludis_ip,
                       Database = "RowingDB",
                       UID = username_db,
                       PWD = password_db,
                       TrustServerCertificate='yes')
DBdirectory <- "rowing_new"

## Query Database for Athlete, Session & Piece Data ##
query <- paste0("SELECT AthleteId,Name FROM ",DBdirectory, ".Athlete")
Athlete_list <- DBI::dbGetQuery(mydb,query)

## Athlete Session Data ##
query <- paste0("SELECT SessionId,AthleteId,Position FROM ",
                DBdirectory, ".session_athlete")
Session_Athlete <- DBI::dbGetQuery(mydb,query) %>%
  rename(SeatPosition = Position)

## Session Data ##
query <- paste0("SELECT SessionId,Year,Month,Day,BoatClass,AgeGroup,Comments FROM ",
                DBdirectory, ".Session")
Session_data <- DBI::dbGetQuery(mydb,query)

## Piece Data ##
query <- paste0("SELECT SessionId,Rating,Pace,Comment,Wind,Stream FROM ",
                DBdirectory, ".Piece")
Piece_data <- DBI::dbGetQuery(mydb,query)

## Merge DB Tables into a single Meta data Table ##
Session_Athlete <- merge(Athlete_list,Session_Athlete,by='AthleteId')
MetaData <- merge(Session_Athlete,Session_data,by='SessionId')
MetaData <- merge(MetaData,Piece_data,by='SessionId')
MetaData <- MetaData %>%
  group_by(SessionId) %>%
  mutate(Crew = paste(Name, collapse = ", ")) %>%
  ungroup()
MetaData <- MetaData %>% 
  mutate(Rate.Bin = cut(Rating, breaks = Bins, labels = Bin_labels),
         Date = make_date(year = Year, month = Month, day = Day))


################################################################################

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Rowing Dashboard", titleWidth = 230),
  dashboardSidebar(width = 230,
                   sidebarMenu(
                     img(src="www/nswis_logo.jpg", width = "100%"),
                     selectizeInput("club_ui", label = "Club", choices = ""),
                     hr(),
                     selectizeInput("athlete_ui", label = "Athlete", choices = "")
                   )
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Benchmarks",
               h4(),
               box(title = "Profile", status = "primary", width = 10,
                   solidHeader = TRUE, collapsible = TRUE, height = "600px", 
                   plotlyOutput("spider", height = "500px"))
      ),
      tabPanel("MSK",
               h4(),
               box(title = "MSK Metric", status = "warning", solidHeader = TRUE, width = 10,
                   selectInput("MSK_metric_ui", "Select Metric:", 
                               choices = c("All", "Thoracic Rotation", "Active Slump", 
                                           "Thomas Hip Extension", "Thomas Hip Abduction", 
                                           "Hip Flexion", "Hip Internal Rotation", 
                                           "Active Knee Extension", "Ankle Dorsiflexion", 
                                           "Sit & Reach", "Long Sit Rockover"),
                               selected = "All")),
               box(title = "Profile", status = "primary", 
                   solidHeader = TRUE, collapsible = TRUE, 
                   height = "500px", width = 10,
                   plotlyOutput("MSK_plot", height = "400px"))
      ),
      tabPanel("Trunk",
               h4(),
               box(title = "Trunk Metrics", status = "warning", solidHeader = TRUE, width = 12,
                   selectInput("trunk_metric_ui", "Select Metric:", 
                               choices = c("All", "Prone Endurance", "Supine Endurance", 
                                           "Left Side Hold", "Right Side Hold"),
                               selected = "All")),
               box(title = "Profile", status = "primary", width = 12,
                   solidHeader = TRUE, collapsible = TRUE, height = "500px",
                   fluidRow(
                     column(width = 6, 
                            plotlyOutput("trunk_plot_left", height = "400px")),
                     column(width = 6, 
                            plotlyOutput("trunk_plot_right", height = "400px"))
                   ))
      ),
      tabPanel("Strength",
               h4(),
               box(title = "Strength Metrics", status = "warning", solidHeader = TRUE, width = 10,
                   selectInput("strength_metric_ui", "Select Metric:", 
                               choices = c("All", "Squat", "Bench Pull", "Deadlift", "Bench Press", "Pull/ Chin Ups"),
                               selected = "All")),
               box(title = "Profile", status = "primary", 
                   solidHeader = TRUE, collapsible = TRUE, 
                   height = "500px", width = 10,
                   plotlyOutput("strength_plot", height = "400px"))
      ),
      tabPanel("Force Decks",
               h4(),
               box(title = "Test", status = "warning", solidHeader = TRUE, width = 12,
                   selectInput("FDecks_test_ui", "Select Test:", 
                               choices = c("All", "CMJ", "SJ","IMTP","ISOT","LCMJ"),
                               selected = "CMJ")),
               box(title = "Profile", status = "primary", 
                   solidHeader = TRUE, collapsible = TRUE, 
                   height = "600px", width = 12,
                   fluidRow(column(width = 12,
                                   plotlyOutput("FDecks_plot_left", width = "100%", height = "160px"))),
                   br(),
                   fluidRow(column(width = 12,
                                   plotlyOutput("FDecks_plot_mid", width = "100%", height = "160px"))),
                   br(),
                   fluidRow(column(width = 12,
                                   plotlyOutput("FDecks_plot_right", width = "100%", height = "160px")))
               )
      )
    )
  )
)


### Server ###
server <- function(input, output, session) {
  
  clubs <- c("all", sort(unique(db$Club)))
  updateSelectizeInput(session, "club_ui", choices = clubs, server = TRUE)
  
  format_split <- function(x) {
    ifelse(is.na(x) | x == "No data","No data",sub("^0","", sub("^.* ","",x)))
  }
  
  filtered_db <- reactive({
    req(input$club_ui)
    if (input$club_ui == "all") db else db %>% filter(Club == input$club_ui)
  })
  
  observe({
    req(filtered_db())
    athlete_choices <- sort(unique(filtered_db()$Name))
    updateSelectizeInput(session, "athlete_ui", choices = athlete_choices, server = TRUE)
  })
  
  
  ### Trunk ###
  
  filtered_df_trunk <- reactive({
    req(input$athlete_ui, input$trunk_metric_ui)
    
    ath_gender <- db %>% 
      filter(Name == input$athlete_ui) %>% 
      pull(Gender) %>% 
      unique()
    
    res <- df_trunk %>% 
      filter(Name == input$athlete_ui | (Name == "Squad Mean" & Gender == ath_gender)) %>%
      pivot_longer(cols = c(`Prone Endurance`, `Supine Endurance`, 
                            `Left Side Hold`, `Right Side Hold`),
                   names_to = "Metric", values_to = "value") %>%
      mutate(Legend_Label = paste(Name, ifelse(is.na(Date), "", as.character(Date)), sep = "<br>"))
    
    res <- res %>% 
      left_join(benchmark_lookup %>% 
                  filter(Gender == ath_gender), by = c("Metric" = "Test"))
    
    if (input$trunk_metric_ui != "All") {
      res <- res %>% filter(Metric == input$trunk_metric_ui)
    }
    
    res <- res %>% 
      mutate(Legend_Label = ifelse(Name == "Squad Mean", "Squad Mean",
                                   paste(Name, ifelse(is.na(Date), "", 
                                                         as.character(Date)), 
                                         sep = "<br>")),
             Metric_Wrapped = gsub(" ", "<br>", Metric))
    
    return(res)
  })
  
  
  ### MSK ###
  
  filtered_df_MSK <- reactive({
    req(input$athlete_ui, input$MSK_metric_ui)
    
    ath_gender <- db %>% 
      filter(Name == input$athlete_ui) %>% 
      pull(Gender) %>% 
      unique()
    
    res <- df_MSK %>% 
      filter(Name == input$athlete_ui | (Name == "Squad Mean" & Gender == ath_gender)) %>%
      pivot_longer(cols = -c("Date","Name","Club","Gender"),
                   names_to = "Metric", values_to = "value") %>%
      mutate(Legend_Label = paste(Name, ifelse(is.na(Date), "", as.character(Date)), sep = "<br>"),
              Metric = Metric %>%
                       sub(" (left|right)$", "", ., ignore.case = TRUE)) %>%
              group_by(Date,Name,Club,Gender,Metric) %>%
              summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
      )
    
    res <- res %>%
      left_join(benchmark_lookup %>% filter(Gender == ath_gender), 
                by = c("Metric" = "Test")) 
    
    if (input$MSK_metric_ui != "All") {
      res <- res %>% filter(grepl(input$MSK_metric_ui, Metric, fixed = TRUE))
    }
    
    res <- res %>% 
      mutate(Legend_Label = ifelse(Name == "Squad Mean", "Squad Mean",
                                   paste(Name, ifelse(is.na(Date), "", 
                                                         as.character(Date)), 
                                         sep = "<br>")),
             Metric_Wrapped = gsub(" ", "<br>", Metric))
    return(res)
  })
  
  
  ### Strength ###
  
  filtered_df_strength <- reactive({
    req(input$athlete_ui, input$strength_metric_ui)
    
    ath_gender <- db %>% 
      filter(Name == input$athlete_ui) %>% 
      pull(Gender) %>% 
      unique()
    
    res <- df_strength %>% 
      filter(Name == input$athlete_ui | (Name == "Squad Mean" & Gender == ath_gender)) %>%
      pivot_longer(cols = c(`Squat`, `Bench Pull`, `Deadlift`, `Bench Press`),
                   names_to = "Metric", values_to = "value") %>%
      mutate(Legend_Label = paste(Name, ifelse(is.na(Date), "", as.character(Date)), sep = "<br>"))
    
    res <- res %>%
      left_join(benchmark_lookup %>% filter(Gender == ath_gender), 
                by = c("Metric" = "Test")) 
    
    if (input$strength_metric_ui != "All") {
      res <- res %>% filter(Metric == input$strength_metric_ui)
    }
    
    res <- res %>% 
      mutate(Legend_Label = ifelse(Name == "Squad Mean", "Squad Mean",
                                   paste(Name, ifelse(is.na(Date), "", 
                                                         as.character(Date)), 
                                         sep = "<br>")))
    return(res)
  })
  
  
   ### Force Decks ###
   
  filtered_df_FDecks <- reactive({
    req(input$athlete_ui, input$FDecks_test_ui)
    
    ath_gender <- db %>% 
      filter(Name == input$athlete_ui) %>% 
      pull(Gender) %>% 
      unique()
    
    res <- df_FDecks %>% 
      filter(Name == input$athlete_ui | (Name == "Squad Mean" & Gender == ath_gender)) %>%
      mutate(Legend_Label = paste(Name, ifelse(is.na(Date), "", 
                                                  as.character(Date)), 
                                  Test_Type, sep = "<br>"))
    
    res <- res %>%
      left_join(benchmark_lookup %>% filter(Gender == ath_gender), 
                by = c("Metric" = "Test")) 
    
    if (input$FDecks_test_ui != "All") {
      res <- res %>% filter(Test_Type == input$FDecks_test_ui)
    }
    
    res <- res %>% 
      mutate(Legend_Label = ifelse(Name == "Squad Mean", "Squad Mean",
                                   paste(Name, ifelse(is.na(Date), "", 
                                                         as.character(Date)), 
                                         sep = "<br>")),
             Metric_Wrapped = gsub(" ", "<br>", Metric))
    return(res)
  })
  
  
  ## Output Plots ##
  
  output$spider <- renderPlotly({
    req(input$athlete_ui)
    
    test_order <- c("2000", "5000", "30minR20", "4mmol", "2mmol")
    selected_athlete <- input$athlete_ui
    
    most_recent_profile_raw <- db %>%
      filter(Name == selected_athlete) %>%
      group_by(Test) %>%
      slice_max(order_by = Date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(Name, Gender, Date, Test, Name_Label, Power, Score, Time_or_Split) %>%
      mutate(Test = as.character(Test))
    
    best_profile_raw <- db %>%
      filter(Name == selected_athlete) %>%
      group_by(Test) %>%
      slice_max(order_by = Score, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(Name, Gender, Date, Test, Name_Label, Power, Score, Time_or_Split) %>%
      mutate(Test = as.character(Test))
    
    test_template <- tibble(Test = test_order)
    
    selected_gender <- db %>%
      filter(Name == selected_athlete) %>%
      slice(1) %>%
      pull(Gender)
    
    most_recent_profile <- test_template %>%
      left_join(most_recent_profile_raw, by = "Test") %>%
      mutate(
        Name = coalesce(Name, selected_athlete),
        Gender = coalesce(Gender, selected_gender),
        Score = coalesce(Score, 0),
        Power = coalesce(Power, 0),
        Time_or_Split = ifelse(is.na(Time_or_Split), "No data", as.character(Time_or_Split)),
        Test = factor(Test, levels = test_order)
      ) %>%
      arrange(Test)
    
    best_profile <- test_template %>%
      left_join(best_profile_raw, by = "Test") %>%
      mutate(
        Name = coalesce(Name, selected_athlete),
        Gender = coalesce(Gender, selected_gender),
        Score = coalesce(Score, 0),
        Power = coalesce(Power, 0),
        Time_or_Split = ifelse(is.na(Time_or_Split), "No data", as.character(Time_or_Split)),
        Test = factor(Test, levels = test_order)
      ) %>%
      arrange(Test)
    
    benchmark_gender <- benchmark_lookup %>%
      filter(Gender == selected_gender) %>%
      mutate(Test = factor(Test, levels = test_order)) %>%
      arrange(Test)
    
    junior_benchmark <- benchmark_gender %>% filter(Tier == "Junior") %>% arrange(Test)
    u21_benchmark <- benchmark_gender %>% filter(Tier == "U21") %>% arrange(Test)
    u23_benchmark <- benchmark_gender %>% filter(Tier == "U23") %>% arrange(Test)
    senior_benchmark <- benchmark_gender %>% filter(Tier == "Senior") %>% arrange(Test)
    
    most_recent_vals <- most_recent_profile %>% arrange(Test) %>% pull(Score)
    most_recent_power <- most_recent_profile %>% arrange(Test) %>% pull(Power)
    most_recent_time <- most_recent_profile %>% arrange(Test) %>% pull(Time_or_Split)
    most_recent_date <- most_recent_profile %>% arrange(Test) %>% pull(Date)
    
    best_vals <- best_profile %>% arrange(Test) %>% pull(Score)
    best_power <- best_profile %>% arrange(Test) %>% pull(Power)
    best_time <- best_profile %>% arrange(Test) %>% pull(Time_or_Split)
    best_date <- best_profile %>% arrange(Test) %>% pull(Date)
    
    theta_vals <- c(test_order, test_order[1])
    
    most_recent_vals  <- c(most_recent_vals, most_recent_vals[1])
    most_recent_power <- c(most_recent_power, most_recent_power[1])
    most_recent_time  <- c(most_recent_time, most_recent_time[1])
    most_recent_date  <- c(most_recent_date, most_recent_date[1])
    
    best_vals  <- c(best_vals, best_vals[1])
    best_power <- c(best_power, best_power[1])
    best_time  <- c(best_time, best_time[1])
    best_date  <- c(best_date, best_date[1])
    
    junior_vals <- rep(25, length(theta_vals))
    u21_vals    <- rep(50, length(theta_vals))
    u23_vals    <- rep(75, length(theta_vals))
    senior_vals <- rep(100, length(theta_vals))
    
    junior_power <- c(junior_benchmark$Power, junior_benchmark$Power[1])
    u21_power    <- c(u21_benchmark$Power, u21_benchmark$Power[1])
    u23_power    <- c(u23_benchmark$Power, u23_benchmark$Power[1])
    senior_power <- c(senior_benchmark$Power, senior_benchmark$Power[1])
    
    junior_time <- c(junior_benchmark$Time_or_Split, junior_benchmark$Time_or_Split[1])
    u21_time    <- c(u21_benchmark$Time_or_Split, u21_benchmark$Time_or_Split[1])
    u23_time    <- c(u23_benchmark$Time_or_Split, u23_benchmark$Time_or_Split[1])
    senior_time <- c(senior_benchmark$Time_or_Split, senior_benchmark$Time_or_Split[1])
    
    most_recent_date_label <- ifelse(is.na(most_recent_date), "No data", format(most_recent_date, "%d/%m/%Y"))
    best_date_label <- ifelse(is.na(best_date), "No data", format(best_date, "%d/%m/%Y"))
    
    most_recent_hover <- paste0(
      "<b>", theta_vals, "</b><br>",
      "Score: ", most_recent_vals, "<br>",
      "Power: ", round(most_recent_power, 0), " W<br>",
      "Time/Split: ", format_split(most_recent_time), "<br>",
      "Date: ", most_recent_date_label
    )
    
    best_hover <- paste0(
      "<b>", theta_vals, "</b><br>",
      "Score: ", best_vals, "<br>",
      "Power: ", round(best_power, 0), " W<br>",
      "Time/Split: ", format_split(best_time), "<br>",
      "Date: ", best_date_label
    )
    
    junior_hover <- paste0("<b>", theta_vals, "</b><br>Score: 25<br>Power: ", round(junior_power,0), " W<br>", "Time/Split: ", junior_time)
    u21_hover    <- paste0("<b>", theta_vals, "</b><br>Score: 50<br>Power: ", round(u21_power,0), " W<br>", "Time/Split: ", u21_time)
    u23_hover    <- paste0("<b>", theta_vals, "</b><br>Score: 75<br>Power: ", round(u23_power,0), " W<br>", "Time/Split: ", u23_time)
    senior_hover <- paste0("<b>", theta_vals, "</b><br>Score: 100<br>Power: ", round(senior_power,0), " W<br>", "Time/Split: ", senior_time)
    
    junior_col <- "#BDBDBD"; u21_col <- "#CD7F32"; u23_col <- "#2E8B57"; senior_col <- "#D4AF37"
    recent_col <- "#1F4E79"; best_col <- "#C00000"
    
    
    plot_ly() %>%
      add_trace(type = "scatterpolar", mode = "lines",
                r = junior_vals, theta = theta_vals, text = junior_hover,
                hovertemplate = "%{text}<extra>Junior</extra>", name = "Junior",
                line = list(color = junior_col, width = 2, dash = "solid")) %>%
      add_trace(type = "scatterpolar", mode = "lines",
                r = u21_vals, theta = theta_vals, text = u21_hover,
                hovertemplate = "%{text}<extra>U21</extra>", name = "U21",
                line = list(color = u21_col, width = 2, dash = "solid")) %>%
      add_trace(type = "scatterpolar", mode = "lines",
                r = u23_vals, theta = theta_vals, text = u23_hover,
                hovertemplate = "%{text}<extra>U23</extra>", name = "U23",
                line = list(color = u23_col, width = 2, dash = "solid")) %>%
      add_trace(type = "scatterpolar", mode = "lines",
                r = senior_vals, theta = theta_vals, text = senior_hover,
                hovertemplate = "%{text}<extra>Senior</extra>", name = "Senior",
                line = list(color = senior_col, width = 2, dash = "solid")) %>%
      add_trace(type = "scatterpolar", mode = "lines+markers",
                r = most_recent_vals, theta = theta_vals, text = most_recent_hover,
                hovertemplate = paste0("%{text}<extra>", " Most Recent</extra>"),
                name = paste("Most Recent"), fill = "toself",
                fillcolor = "rgba(31,78,121,0.20)", 
                line = list(color = recent_col, width = 3),
                marker = list(size = 8, color = recent_col)) %>%
      add_trace(type = "scatterpolar", mode = "lines+markers",
                r = best_vals, theta = theta_vals, text = best_hover,
                hovertemplate = paste0("%{text}<extra>", " Best</extra>"),
                name = paste("Best"), fill = "none",
                line = list(color = best_col, width = 3),
                marker = list(size = 8, color = best_col)) %>%
      layout(title = list(text = selected_athlete, font = list(color = "black")),
             font = list(color = "black"),
             legend = list(font = list(color = "black")),
             showlegend = TRUE,
             polar = list(
               bgcolor = "white",
               radialaxis = list(visible = TRUE, range = c(0,100),
                                 tickvals = c(25,50,75,100),
                                 ticktext = c("Junior","U21","U23","Senior"),
                                 tickfont = list(color = "black"),
                                 showline = TRUE, 
                                 showgrid = FALSE, 
                                 linecolor = "black"),
               angularaxis = list(type = "category",
                                  tickfont = list(color = "black"),
                                  showline = TRUE, 
                                  showgrid = TRUE, 
                                  linecolor = "black")))
    
  })
  
  output$trunk_plot_left <- renderPlotly({
    req(filtered_df_trunk())
    
    data <- filtered_df_trunk() %>% 
      dplyr::filter(Metric %in% c("Prone Endurance","Supine Endurance"))
    
    # Get benchmark values for the background band
    o_low <- na.omit(data$OrangeLow)[1]
    o_high <- na.omit(data$OrangeHigh)[1]
    
    all_labels <- unique(data$Legend_Label)
    athlete_labels <- all_labels[all_labels != "Squad Mean"]
    
    ath_colors <- colorRampPalette(c("#0073b7", "#66b3ff"))(length(athlete_labels))
    
    pal <- c(ath_colors, "#999999")
    names(pal) <- c(athlete_labels, "Squad Mean")
    
    plot_ly(data) %>%
      add_bars(x = ~Metric_Wrapped, 
               y = ~value, 
               color = ~Legend_Label,
               colors = pal) %>% 
      layout(
        margin = list(l = 20, r = 20, b = 20, t = 10, pad = 3),
        hovermode = "closest", 
        hoverlabel = list(bgcolor = "white", font = list(size = 12)),
        yaxis = list(tickangle = 0, automargin = TRUE, title = "Seconds"),
        xaxis = list(title = "", type = "category"),
        barmode = 'group',
        legend = list(orientation = "h",
                      xanchor = "center", 
                      x = 0.5,  
                      y = 1.1,  
                      yanchor = "bottom"
        ),
        shapes = list(
          list(type = "rect", layer = "below",
               fillcolor = "rgba(0, 255, 0, 0.2)", # Green
               line = list(width = 0),
               x0 = 0, x1 = 1, xref = "paper",
               y0 = o_high, y1 = 200), 
          list(type = "rect", layer = "below",
               fillcolor = "rgba(255, 165, 0, 0.2)", # Orange
               line = list(width = 0),
               x0 = 0, x1 = 1, xref = "paper",
               y0 = o_low, y1 = o_high), 
          list(type = "rect", layer = "below",
               fillcolor = "rgba(255, 0, 0, 0.2)", # Red
               line = list(width = 0),
               x0 = 0, x1 = 1, xref = "paper",
               y0 = 0, y1 = o_low) 
        )) %>% config(displayModeBar = FALSE)
    
  })
  
  output$trunk_plot_right <- renderPlotly({
    req(filtered_df_trunk())
    
    data <- filtered_df_trunk() %>% 
      dplyr::filter(Metric %in% c("Left Side Hold","Right Side Hold"))
    
    o_low <- na.omit(data$OrangeLow)[1]
    o_high <- na.omit(data$OrangeHigh)[1]
    
    all_labels <- unique(data$Legend_Label)
    athlete_labels <- all_labels[all_labels != "Squad Mean"]
    
    ath_colors <- colorRampPalette(c("#0073b7", "#66b3ff"))(length(athlete_labels))
    
    pal <- c(ath_colors, "#999999")
    names(pal) <- c(athlete_labels, "Squad Mean")
    
    plot_ly(data) %>%
      add_bars(x = ~Metric_Wrapped, 
               y = ~value, 
               color = ~Legend_Label,
               colors = pal) %>% 
      layout(
        margin = list(l = 20, r = 20, b = 20, t = 10, pad = 3),
        yaxis = list(title = "Seconds"),
        xaxis = list(tickangle = 0, automargin = TRUE, title = "", type = "category"),
        barmode = 'group',
        legend = list(orientation = "h",   # Horizontal legend
                      xanchor = "center",  # Anchor point for horizontal alignment
                      x = 0.5,             # Center it (0 is left, 1 is right)
                      y = 1.1,             # Position above plot (1 is top edge)
                      yanchor = "bottom"   # Anchor point for vertical alignment
        ),
        shapes = list(
          list(type = "rect", layer = "below",
               fillcolor = "rgba(0, 255, 0, 0.2)", # Green
               line = list(width = 0),
               x0 = 0, x1 = 1, xref = "paper",
               y0 = o_high, y1 = 150), 
          list(type = "rect", layer = "below",
               fillcolor = "rgba(255, 165, 0, 0.2)", # Orange
               line = list(width = 0),
               x0 = 0, x1 = 1, xref = "paper",
               y0 = o_low, y1 = o_high), 
          list(type = "rect", layer = "below",
               fillcolor = "rgba(255, 0, 0, 0.2)", # Red
               line = list(width = 0),
               x0 = 0, x1 = 1, xref = "paper",
               y0 = 0, y1 = o_low) 
        )
      ) %>% config(displayModeBar = FALSE) 
    
  })
  
  output$MSK_plot <- renderPlotly({
    data <- filtered_df_MSK()
    req(nrow(data) > 0)
    
    # Get benchmark values safely now that we know data exists
    tmp_low <- na.omit(data$OrangeLow)
    tmp_high <- na.omit(data$OrangeHigh)
    
    # Handle fallback if benchmarks themselves are missing in lookup table
    o_low  <- if (length(tmp_low)  > 0) as.numeric(tmp_low[1])  else 0
    o_high <- if (length(tmp_high) > 0) as.numeric(tmp_high[1]) else 100
    
    all_labels <- unique(as.character(data$Legend_Label))
    athlete_labels <- setdiff(all_labels, "Squad Mean")
    ath_colors <- if (length(athlete_labels) > 0) colorRampPalette(c("#0073b7","#66b3ff"))(length(athlete_labels)) else character(0)
    
    pal <- c(ath_colors, "#999999")
    names(pal) <- c(athlete_labels, "Squad Mean")
    
    pal_vec <- pal[all_labels]
    pal_vec[is.na(pal_vec)] <- "#999999"
    buffer <- 10
    min_y <- min(c((data$value - buffer), (o_low - buffer)), na.rm = TRUE)
    max_y <- max(c((data$value + buffer), (o_high + buffer)), na.rm = TRUE)
    
    plot_ly(data) %>%
      add_bars(x = ~Metric_Wrapped, 
               y = ~value, 
               color = ~Legend_Label,
               colors = pal) %>% 
      layout(
        yaxis = list(title = "Score", range=c(min_y,max_y)),
        xaxis = list(tickangle = 0, automargin = TRUE, title = "", type = "category"),
        barmode = 'group',
        legend = list(orientation = "v"),
        shapes = if (input$MSK_metric_ui == "All") list() else list(
          list(type = "rect", layer = "below", fillcolor = "rgba(0,255,0,0.2)", line = list(width = 0),
               x0 = 0, x1 = 1, xref = "paper", yref = "y", y0 = o_high, y1 = max_y),
          list(type = "rect", layer = "below", fillcolor = "rgba(255,165,0,0.2)", line = list(width = 0),
               x0 = 0, x1 = 1, xref = "paper", yref = "y", y0 = o_low, y1 = o_high),
          list(type = "rect", layer = "below", fillcolor = "rgba(255,0,0,0.2)", line = list(width = 0),
               x0 = 0, x1 = 1, xref = "paper", yref = "y", y0 = min_y, y1 = o_low)
        )
      )
  })
  
  output$strength_plot <- renderPlotly({
    data <- filtered_df_strength()
    req(nrow(data) > 0)
    
    # Get benchmark values safely now that we know data exists
    tmp_low <- na.omit(data$OrangeLow)
    tmp_high <- na.omit(data$OrangeHigh)
    
    # Handle fallback if benchmarks themselves are missing in lookup table
    o_low  <- if (length(tmp_low)  > 0) as.numeric(tmp_low[1])  else 0
    o_high <- if (length(tmp_high) > 0) as.numeric(tmp_high[1]) else 100
    
    all_labels <- unique(data$Legend_Label)
    athlete_labels <- all_labels[all_labels != "Squad Mean"]
    ath_colors <- if (length(athlete_labels) > 0) colorRampPalette(c("#0073b7","#66b3ff"))(length(athlete_labels)) else character(0)
    
    pal <- c(ath_colors, "#999999")
    names(pal) <- c(athlete_labels, "Squad Mean")
    
    pal_vec <- pal[all_labels]
    pal_vec[is.na(pal_vec)] <- "#999999"
    buffer <- 10
    min_y <- min(c((data$value - buffer), (o_low - buffer)), na.rm = TRUE)
    max_y <- max(c((data$value + buffer), (o_high + buffer)), na.rm = TRUE)
    
    plot_ly(data) %>%
      add_bars(x = ~Metric, 
               y = ~value, 
               color = ~Legend_Label,
               colors = pal) %>% 
      layout(
        yaxis = list(title = "Load (kg)", range=c(min_y,max_y)),
        xaxis = list(title = "", type = "category"),
        barmode = 'group',
        legend = list(orientation = "v"),
        shapes = if (input$strength_metric_ui == "All") list() else list(
          list(type = "rect", layer = "below", fillcolor = "rgba(0,255,0,0.2)", line = list(width = 0),
               x0 = 0, x1 = 1, xref = "paper", yref = "y", y0 = o_high, y1 = max_y),
          list(type = "rect", layer = "below", fillcolor = "rgba(255,165,0,0.2)", line = list(width = 0),
               x0 = 0, x1 = 1, xref = "paper", yref = "y", y0 = o_low, y1 = o_high),
          list(type = "rect", layer = "below", fillcolor = "rgba(255,0,0,0.2)", line = list(width = 0),
               x0 = 0, x1 = 1, xref = "paper", yref = "y", y0 = min_y, y1 = o_low)
        )
      )
  })
  
  output$FDecks_plot_left <- renderPlotly({
    data <- filtered_df_FDecks() %>% 
      filter(Metric %in% c("Concentric Mean Force", "Concentric Peak Force")) %>%
      group_by(Legend_Label,Metric_Wrapped) %>%
      summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
      
    req(nrow(data) > 0)
    
    # Get benchmark values safely now that we know data exists
    tmp_low <- na.omit(data$OrangeLow)
    tmp_high <- na.omit(data$OrangeHigh)
    
    # Handle fallback if benchmarks themselves are missing in lookup table
    o_low  <- if (length(tmp_low)  > 0) as.numeric(tmp_low[1])  else 0
    o_high <- if (length(tmp_high) > 0) as.numeric(tmp_high[1]) else 100
    
    all_labels <- unique(data$Legend_Label)
    athlete_labels <- all_labels[all_labels != "Squad Mean"]
    ath_colors <- if (length(athlete_labels) > 0) colorRampPalette(c("#0073b7","#66b3ff"))(length(athlete_labels)) else character(0)
    
    pal <- c(ath_colors, "#999999")
    names(pal) <- c(athlete_labels, "Squad Mean")
    
    buffer <- 10
    min_y <- min(c((data$Value - buffer), (o_low - buffer)), na.rm = TRUE)
    max_y <- max(c((data$Value + buffer), (o_high + buffer)), na.rm = TRUE)
    
    plot_ly(data) %>%
      add_bars(x = ~Metric_Wrapped, 
               y = ~Value, 
               color = ~Legend_Label,
               colors = pal) %>% 
      layout(
        yaxis = list(title = "units [N]",range = c(min_y,max_y)),
        xaxis = list(title = "", type = "category"),
        barmode = 'group',
        legend = list(orientation = "v",
                      font = list(size=10))
      )
  })
  
  output$FDecks_plot_mid <- renderPlotly({
    data <- filtered_df_FDecks() %>% 
      filter(Metric %in% c("Eccentric Deceleration Impulse","Concentric Impulse")) %>%
      group_by(Legend_Label,Metric_Wrapped) %>%
      summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
    
    req(nrow(data) > 0)
    
    # Get benchmark values safely now that we know data exists
    tmp_low <- na.omit(data$OrangeLow)
    tmp_high <- na.omit(data$OrangeHigh)
    
    # Handle fallback if benchmarks themselves are missing in lookup table
    o_low  <- if (length(tmp_low)  > 0) as.numeric(tmp_low[1])  else 0
    o_high <- if (length(tmp_high) > 0) as.numeric(tmp_high[1]) else 100
    
    all_labels <- unique(data$Legend_Label)
    athlete_labels <- all_labels[all_labels != "Squad Mean"]
    ath_colors <- if (length(athlete_labels) > 0) colorRampPalette(c("#0073b7","#66b3ff"))(length(athlete_labels)) else character(0)
    
    pal <- c(ath_colors, "#999999")
    names(pal) <- c(athlete_labels, "Squad Mean")
    
    buffer <- 10
    min_y <- min(c((data$Value - buffer), (o_low - buffer)), na.rm = TRUE)
    max_y <- max(c((data$Value + buffer), (o_high + buffer)), na.rm = TRUE)
    
    plot_ly(data) %>%
      add_bars(x = ~Metric_Wrapped, 
               y = ~Value, 
               color = ~Legend_Label,
               colors = pal) %>% 
      layout(
        yaxis = list(title = "Impulse [Ns]",range = c(min_y,max_y)),
        xaxis = list(title = "", type = "category"),
        barmode = 'group',
        legend = list(orientation = "v",
                      font = list(size=10))
      )
  })
  
  output$FDecks_plot_right <- renderPlotly({
    data <- filtered_df_FDecks() %>% 
      filter(Metric == "Max Jump Height (Flight Time)") %>%
      group_by(Legend_Label,Metric_Wrapped) %>%
      summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
      
    req(nrow(data) > 0)
    
    # Get benchmark values safely now that we know data exists
    tmp_low <- na.omit(data$OrangeLow)
    tmp_high <- na.omit(data$OrangeHigh)
    
    # Handle fallback if benchmarks themselves are missing in lookup table
    o_low  <- if (length(tmp_low)  > 0) as.numeric(tmp_low[1])  else 0
    o_high <- if (length(tmp_high) > 0) as.numeric(tmp_high[1]) else 100
    
    all_labels <- unique(data$Legend_Label)
    athlete_labels <- all_labels[all_labels != "Squad Mean"]
    ath_colors <- if (length(athlete_labels) > 0) colorRampPalette(c("#0073b7","#66b3ff"))(length(athlete_labels)) else character(0)
    
    pal <- c(ath_colors, "#999999")
    names(pal) <- c(athlete_labels, "Squad Mean")
    
    buffer <- 10
    min_y <- min(c((data$Value - buffer), (o_low - buffer)), na.rm = TRUE)
    max_y <- max(c((data$Value + buffer), (o_high + buffer)), na.rm = TRUE)
    
    plot_ly(data) %>%
      add_bars(x = ~Metric_Wrapped, 
               y = ~Value, 
               color = ~Legend_Label,
               colors = pal) %>% 
      layout(
        yaxis = list(title = "Height [cm]", range = c(min_y,max_y)),
        xaxis = list(title = "", type = "category"),
        barmode = 'group',
        legend = list(orientation = "v",
                      font = list(size=10))
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

