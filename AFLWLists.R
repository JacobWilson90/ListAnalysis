library(shiny)
library(dplyr)
library(DBI)
library(DT)
library(tidyverse)
library(openxlsx)
library(RColorBrewer)
library(tidyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(gt)
library(bslib)
library(htmltools)
library(shinyWidgets)
library(RPostgres)
library(DBI)
library(odbc)

if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

# Create a connection to the database
conn <- dbConnect(odbc(),
                  Driver = "FreeTDS",
                  Server = "sd7erbhj7d.database.windows.net",
                  Database = "aflw_db",
                  UID = Sys.getenv("DB_USER"),
                  PWD = Sys.getenv("DB_PASS"),
                  Port = 1433,
                  TDS_Version = "8.0",
                  Encrypt = "yes",
                  TrustServerCertificate = "no",
                  Authentication = "SqlPassword"
)




# Get team names
teamLookup <- dbGetQuery(conn, "
    SELECT TeamID, TeamName
    FROM Team
  ")


seasons <- dbGetQuery(conn, "
  SELECT season as Season, Height, PlayerID, TeamID, Games, ChampionPosition, ChampionID,  SeasonNo
  FROM [dbo].[Season] 
  WHERE Season >= 2017
")


players <- dbGetQuery(conn, "
    SELECT PlayerID, FullName, DOB, TotalGames, CurrentHeight, CurrentTeamID
    FROM Player
  ")


ladders <- dbGetQuery(conn, "
  SELECT *
  FROM Ladder
") %>%
  select(Season, Position, TeamID) %>%
  left_join(teamLookup, by = 'TeamID')



draftData <- dbGetQuery(conn, "
    SELECT PlayerID, PickType as DraftType, Pick, Club, LeagueID, Season as Year
    FROM AFLWDraftHistory
    WHERE PlayerID != '3350'
  ")


premiers <- data.frame(
  Year = c(2017,2018,2019,2020,2021,2022.1,2022.2,2023,2024,2025),
  TeamName = c(
    "Adelaide",             # 2017
    "Western Bulldogs",     # 2018
    "Adelaide",             # 2019
     NA,                    # 2020
    "Brisbane",             # 2022
    "Adelaide",             # 2101
    "Melbourne",            # 2023
    "Brisbane",             # 2024
    "North Melbourne",      # 2025
    NA
  ),
  stringsAsFactors = FALSE
)



get_premiership_team <- function(year, premiers) {
  premiershipTeam <- premiers %>%
    filter(Year == year) %>%
    pull(TeamName)
  return(premiershipTeam)
}



add_premiership_icon <- function(gt_table, premiershipTeam, imagePath = "premiership.png") {
  gt_table %>%
    text_transform(
      locations = cells_stub(rows = TeamName == premiershipTeam),
      fn = function(x) {
        paste0(x, " <img src='", imagePath, "' style='height:20px; width:auto; margin-left:5px;'>")
      }
    )
}



get_top4_positions <- function(seasonYear, ladders_df) {
  position_labels <- c("1st", "2nd", "3rd", "4th")
  
  top4 <- ladders %>%
    filter(Season == seasonYear, Position %in% 1:4) %>%
    arrange(Position)
  
  if (nrow(top4) < 4) {
    warning(paste("Only", nrow(top4), "top teams found for season:", seasonYear))
  }
  
  top4 <- top4 %>%
    mutate(Label = position_labels[match(Position, 1:4)]) %>%
    select(TeamName, Label)
  
  deframe(top4)
}



getAgeSummary <- function(conn, seasons, players, teamLookup, seasonYear) {
  
  
  # Calculate age as of January 1st of the selected season
  refDate <- as.Date(
    paste0(
      ifelse(seasonYear %in% c(2022.1, 2022.2), 2022, as.integer(seasonYear)),
      "-12-31"
    )
  )
  
  seasons %>%
    group_by(PlayerID) %>%
    mutate(gamesPostYear = sum(Games[Season > seasonYear], na.rm = TRUE)) %>%
    ungroup() %>%
    filter(Season == seasonYear) %>%
    left_join(teamLookup, by = 'TeamID') %>%
    left_join(players, by = 'PlayerID') %>%
    mutate(totalGamesAtTime = TotalGames - gamesPostYear) %>%
    mutate(
      DOB = as.Date(DOB),
      age = as.integer(floor(interval(DOB, refDate) / years(1)))
    ) %>%
    mutate(ageGroup = case_when(
      age < 20 ~ "<20",
      age <= 22 ~ "20-22",
      age <= 25 ~ "23-25",
      age <= 28 ~ "26-28",
      age <= 31 ~ "29-31",
      TRUE ~ "32+"
    ))
}




getYearly <- function(conn, seasons, players, teamLookup) {
  
  # Work out all years to calculate
  years <- c(2017,2018,2019,2020,2021,2022.1,2022.2,2023,2024,2025)
  
  map_dfr(years, function(seasonYear) {
    
    refDate <- as.Date(
      paste0(
        ifelse(seasonYear %in% c(2022.1, 2022.2), 2022, as.integer(seasonYear)),
        "-12-31"
      )
    )
    
    seasons %>%
      group_by(PlayerID) %>%
      mutate(gamesPostYear = sum(Games[Season > seasonYear], na.rm = TRUE)) %>%
      ungroup() %>%
      filter(Season == seasonYear) %>%
      left_join(teamLookup, by = 'TeamID') %>%
      left_join(players, by = 'PlayerID') %>%
      mutate(
        totalGamesAtTime = TotalGames - gamesPostYear,
        DOB = as.Date(DOB),
        age = as.integer(floor(interval(DOB, refDate) / years(1))),
        seasonYear = seasonYear
      ) %>% 
      mutate(
        gamesStart = TotalGames - gamesPostYear - Games,
        experienceContribution = Games * gamesStart + (Games * (Games - 1)) / 2,
        ageContribution = age * Games
      )
    
  })
}



getExperienceSummary <- function(conn, seasons, players, teamLookup, seasonYear) {
  
  gamesPost <- seasons %>%
    group_by(PlayerID) %>%
    mutate(gamesPostYear = sum(Games[Season > seasonYear], na.rm = TRUE)) %>%
    ungroup() %>%
    filter(Season == seasonYear) %>%
    left_join(teamLookup, by = 'TeamID') %>%
    left_join(players, by = 'PlayerID') %>%
    mutate(totalGamesAtTime = TotalGames - gamesPostYear) %>%
    mutate(experienceGroup = case_when(
      is.na(totalGamesAtTime) | totalGamesAtTime < 25 ~ "0-24",
      totalGamesAtTime < 50 ~ "25-49",
      totalGamesAtTime < 75 ~ "50-74",
      totalGamesAtTime < 100 ~ "75-99",
      totalGamesAtTime >= 100 ~ "100+"
    ))
}



getHeightSummary <- function(conn, seasons, players, teamLookup, seasonYear) {
  
  heights <- seasons %>%
    filter(Season == seasonYear) %>%
    mutate(heightGroup = case_when(
      Height < 160 ~ "<160",
      Height < 165 ~ "160-164",
      Height < 170 ~ "165-169",
      Height < 175 ~ "170-174",
      Height < 180 ~ "175-179",
      TRUE ~ "180+"
    )) %>%
    left_join(teamLookup, by = "TeamID") %>%
    left_join(players, by = 'PlayerID')
}



getInitialDraftSummary <- function(conn, seasons, players, teamLookup, seasonYear, draftData) {
  
  seasons <- seasons %>%
    filter(Season == seasonYear) %>%
    left_join(players, by = 'PlayerID') %>%
    left_join(teamLookup, by = 'TeamID') 
  
  # Step: Get earliest draft year per player
  initialDraft <- draftData %>%
    filter(!is.na(Year)) %>%
    arrange(PlayerID, Year) %>%
    group_by(PlayerID) %>%
    slice(1) %>%
    ungroup()
  
  
  summary <- seasons %>%
    left_join(initialDraft, by = 'PlayerID') %>%
    select(
      PlayerID,
      FullName,
      TeamName,
      DraftYear = Year,
      Pick,
      DraftType
    ) %>%
    mutate(
      draftGroup = case_when(
        DraftType == "National" & Pick >= 1 & Pick <= 10 ~ "1-10",
        DraftType == "National" & Pick >= 11 & Pick <= 20 ~ "11-20",
        DraftType == "National" & Pick >= 21 & Pick <= 30 ~ "21-30",
        DraftType == "National" & Pick >= 31 & Pick <= 40 ~ "31-40",
        DraftType == "National" & Pick >= 41 & Pick <= 50 ~ "41-50",
        DraftType == "National" & Pick > 50 ~ "51+",
        DraftType == "Rookie" ~ "Rookie",
        DraftType == "Rookie Elev" ~ "Rookie",
        DraftType == "Mid-Season" | DraftType == "Pre-Season" ~ "Pre/Mid",
        TRUE ~ "Other"
      )
    ) %>%
    group_by(TeamName, draftGroup) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = draftGroup, values_from = count, values_fill = 0) %>%
    arrange(TeamName)
  
}



getDraftGamesSummary <- function(conn, seasons, players, teamLookup, seasonYear, draftData) {
  
  seasons <- seasons %>%
    group_by(PlayerID) %>%
    mutate(gamesPostYear = sum(Games[Season > seasonYear], na.rm = TRUE)) %>%
    ungroup() %>%
    filter(Season == seasonYear) %>%
    left_join(players, by = 'PlayerID') %>%
    left_join(teamLookup, by = 'TeamID') %>%
    mutate(totalGamesAtTime = TotalGames - gamesPostYear)
  
  
  # Step: Get earliest draft year per player
  initialDraft <- draftData %>%
    filter(!is.na(Year)) %>%
    arrange(PlayerID, Year) %>%
    group_by(PlayerID) %>%
    slice(1) %>%
    ungroup()
  
  summary <- seasons %>%
    left_join(initialDraft, by = 'PlayerID') %>%
    select(
      PlayerID,
      FullName,
      TeamName,
      DraftYear = Year,
      Pick,
      DraftType,
      LeagueID,
      totalGamesAtTime
    )
  
  # Create draft group and summarise total games by team
  summary %>%
    mutate(
      draftGroup = case_when(
        DraftType == "National" & Pick >= 1 & Pick <= 10 ~ "1-10",
        DraftType == "National" & Pick >= 11 & Pick <= 20 ~ "11-20",
        DraftType == "National" & Pick >= 21 & Pick <= 30 ~ "21-30",
        DraftType == "National" & Pick >= 31 & Pick <= 40 ~ "31-40",
        DraftType == "National" & Pick >= 41 & Pick <= 50 ~ "41-50",
        DraftType == "National" & Pick > 50 ~ "51+",
        DraftType == "Expansion Signing" ~ "Expansion",
        DraftType == "National" ~ "National",
        LeagueID == 1103 ~ "Irish",
        DraftType %in% c("Pre-Seas", "Mid-Season", "DelistedFA") ~ "Pre/Mid Draft, Delisted FA",
        DraftType %in% c("Priority Selection", "Uncontr Pl Sel") ~ "Priority Selection",
        DraftType %in% c("RestFA", "UnRestFA") ~ "Free Agents",
        DraftType == "Rookie" ~ "Rookie",
        DraftType == "Rookie Elev" ~ "Rookie Elev",
        TRUE ~ "Other"
      )
    ) %>%
    group_by(TeamName, draftGroup) %>%
    summarise(totalGamesAtTime = sum(totalGamesAtTime, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = draftGroup,
      values_from = totalGamesAtTime,
      values_fill = 0
    ) %>%
    arrange(TeamName)
}



getAcquiredSummary <- function(conn, seasons, players, teamLookup, seasonYear, draftData) {
  
  seasons <- seasons %>%
    filter(Season == seasonYear) %>%
    left_join(players, by = 'PlayerID') %>%
    left_join(teamLookup, by = 'TeamID')
  
  # Step: Get earliest draft year per player
  latestDraft <- draftData %>%
    filter(!is.na(Year)) %>%
    arrange(PlayerID, desc(Year)) %>%
    group_by(PlayerID) %>%
    slice(1) %>%
    ungroup()
  
  
  summary <- seasons %>%
    left_join(latestDraft, by = 'PlayerID') %>%
    select(
      PlayerID,
      FullName,
      TeamName,
      DraftType,
      LeagueID,
      Year
    ) %>%
    mutate(
      acquiredGroup = case_when(
        DraftType == "Expansion Signing" ~ "Expansion",
        DraftType == "National" ~ "National",
        LeagueID == 1103 ~ "Irish",
        DraftType %in% c("Pre-Seas", "Mid-Season", "DelistedFA") ~ "Pre/Mid Draft, Delisted FA",
        DraftType %in% c("Priority Selection", "Uncontr Pl Sel") ~ "Priority Selection",
        DraftType %in% c("RestFA", "UnrestFA") ~ "Free Agents",
        DraftType == "Rookie" ~ "Rookie",
        DraftType == "Rookie Elev" ~ "Rookie Elev",
        DraftType == "Trade" ~ "Trade",
        TRUE ~ "Other"
      )
    )
  
  
}



getExperienceContribution <- function(conn, seasons, players, teamLookup, seasonYear) {
  
  
  expContrib <- seasons %>%
    group_by(PlayerID) %>%
    mutate(gamesPostYear = sum(Games[Season > seasonYear], na.rm = TRUE)) %>%
    ungroup() %>%
    filter(Season == seasonYear) %>%
    left_join(players, by = 'PlayerID') %>%
    mutate(
      gamesStart = TotalGames - gamesPostYear - Games,
      experienceContribution = Games * gamesStart + (Games * (Games - 1)) / 2
    ) %>%
    left_join(teamLookup, by = 'TeamID')
}



add_heatmap <- function(gt_table) {
  gt_table %>%
    data_color(
      columns = where(is.numeric),
      colors = scales::col_numeric(
        palette = c("#ffffff", "#2ca25f"),
        domain = NULL
      )
    )
}



highlight_team <- function(gt_table, team = "Richmond", color = "#ffff00", text_color = "#000000") {
  gt_table %>%
    tab_style(
      style = list(
        cell_fill(color = color),
        cell_text(weight = "bold", color = text_color)
      ),
      locations = cells_stub(rows = TeamName == team)
    )
}



highlight_top4_teams <- function(gt_table, top4_teams, highlighted_team = NULL, enable = TRUE) {
  if (!enable || length(top4_teams) == 0) return(gt_table)
  
  for (team in top4_teams) {
    # Skip Richmond and the selected highlighted team
    if (team != "Richmond" && team != highlighted_team) {
      gt_table <- gt_table %>%
        tab_style(
          style = list(
            cell_fill(color = "#66cc66"),
            cell_text(weight = "bold", color = "black")
          ),
          locations = cells_stub(rows = TeamName == team)
        )
    }
  }
  
  gt_table
}



darkThemeGT <- function(gtTable) {
  gtTable %>%
    tab_style(
      style = cell_text(color = "white", weight = "bold"),
      locations = cells_title(groups = "title")  # Apply styling to the title
    ) %>%
    tab_style(
      style = cell_text(color = "white", weight = "bold"),
      locations = cells_column_labels(everything())
    ) %>%
    tab_options(
      table.background.color = "#1e1e1e",
      heading.background.color = "#1e1e1e",
      column_labels.background.color = "#2c2c2c",
      table.font.color = "white",
      stub.background.color = "#1e1e1e",
      row.striping.background_color = "#2a2a2a"
    )
}



darkThemePlot <- function(include_legend = FALSE) {
  base_theme <- theme(
    plot.background = element_rect(fill = "#1e1e1e", color = NA),
    panel.background = element_rect(fill = "#1e1e1e", color = NA),
    panel.grid.major.x = element_blank(),  # Hide vertical grid lines
    panel.grid.major.y = element_line(color = "grey40", size = 0.5),  # Horizontal grid lines
    panel.grid.minor = element_line(color = "#1e1e1e"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "white", size = 12),
    axis.text.y = element_text(color = "white", size = 12),
    axis.title = element_text(color = "white", size = 14),
    plot.title = element_text(face = "bold", hjust = 0.5, color = "white", size = 16)
  )
  
  if (include_legend) {
    base_theme <- base_theme + theme(
      legend.background = element_rect(fill = "#1e1e1e"),
      legend.key = element_rect(fill = "#1e1e1e"),
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white")
    )
  }
  
  return(base_theme)
}



boxPlot <- list(
  geom_boxplot(fill = "grey30", color = "grey80", outlier.shape = NA, width = 0.4),
  geom_jitter(width = 0.1, color = "lightblue", alpha = 0.7, size = 2)
)



# Disconnect when app stops
onStop(function() {
  dbDisconnect(conn)
})

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly"  # Options: "darkly", "cyborg", "superhero", etc.
  ),
  titlePanel("AFL Player Summary Dashboard"),
  
  fluidRow(
    column(2,
           selectInput("category", "Category", choices = c("Age", "Experience", "Height", "Draft/Trade"))
    ),
    column(2,
           uiOutput("breakdownUI")
    ),
    column(2,
           selectInput("seasonYear", "Select Season", choices = sort(c(2017,2018,2019,2020,2021,2022.1,2022.2,2023,2024,2025), decreasing = TRUE))
    ),
    column(2,
           selectInput("otherTeamHightlight", "Highlight Team", choices = c(
             "None",
             sort(
               
               unique(teamLookup$TeamName[teamLookup$TeamID %in% c(2416,2417,2418,2419,2616,2420,2421,2428,2429,2614,2423,2422,2613,2424,2425,2615,2426,2427)])
               ,
               decreasing = FALSE
             )
           ))
    ),
    column(2,
           shinyWidgets::switchInput(
             inputId = "top4Toggle",
             label = "Top 4",
             onLabel = "On",
             offLabel = "Off",
             value = FALSE,
             size = "mini"  # can be "mini", "small", "normal", "large"
           )
    )
  ),
  
  hr(),
  # All possible outputs shown reactively
  conditionalPanel("input.breakdown == 'Age Summary'",
                   tableOutput("ageSummaryTable")),
  
  conditionalPanel("input.breakdown == 'Age Games Played Summary'",
                   tableOutput("ageGamesPlayedTable")),
  
  conditionalPanel("input.breakdown == 'Age Demographics'",
                   tableOutput("ageDemographicsTable")),
  
  conditionalPanel("input.breakdown == 'Age Boxplot'",
                   plotOutput("ageBoxplot", width = "100%", height = "700px")),
  
  conditionalPanel("input.breakdown == 'Selected Team Age Progression'",
                   plotOutput("selectedAgeProfile", width = "100%", height = "700px")),
  
  conditionalPanel("input.breakdown == 'List Age Progression'",
                   plotOutput("ageProfile", width = "100%", height = "700px")),
  
  conditionalPanel("input.breakdown == 'Experience Summary'",
                   tableOutput("experienceSummaryTable")),
  
  conditionalPanel("input.breakdown == 'Games Played by Selected Team'",
                   plotOutput("experienceContributionTable", width = "100%", height = "700px")),
  
  conditionalPanel("input.breakdown == 'Experience Boxplot'",
                   plotOutput("experienceBoxplot", width = "100%", height = "700px")),
  
  conditionalPanel("input.breakdown == 'Selected Team Games Progression'",
                   plotOutput("selectedGamesProfile", width = "100%", height = "700px")),
  
  conditionalPanel("input.breakdown == 'List Games Progression'",
                   plotOutput("gamesProfile", width = "100%", height = "700px")),
  
  conditionalPanel("input.breakdown == 'Height Summary'",
                   tableOutput("heightSummaryTable")),
  
  conditionalPanel("input.breakdown == 'Height Boxplot'",
                   plotOutput("heightBoxplot", width = "100%", height = "700px")),
  
  conditionalPanel("input.breakdown == 'Initial Draft Summary'",
                   tableOutput("initialDraftTable")),
  
  conditionalPanel("input.breakdown == 'Draft Games Summary'",
                   tableOutput("draftGamesTable")),
  conditionalPanel("input.breakdown == 'Acquired Summary'",
                   plotOutput("acquiredSummaryBarChart", width = "100%", height = "700px"))
)


server <- function(input, output, session) {
  
  ageSummary <- reactive({
    req(input$seasonYear, input$otherTeamHightlight)
    getAgeSummary(conn, seasons, players, teamLookup, input$seasonYear)
  })
  
  yearlyProfile <- reactive({
    req(input$otherTeamHightlight)
    getYearly(conn, seasons, players, teamLookup)
  })
  
  experienceSummary <- reactive({
    req(input$seasonYear, input$otherTeamHightlight)
    getExperienceSummary(conn, seasons, players, teamLookup, input$seasonYear)
  })
  
  experienceContribution <- reactive({
    req(input$seasonYear, input$otherTeamHightlight)
    getExperienceContribution(conn, seasons, players, teamLookup, input$seasonYear)
  })
  
  heightSummary <- reactive({
    req(input$seasonYear, input$otherTeamHightlight)
    getHeightSummary(conn, seasons, players, teamLookup, input$seasonYear)
  })
  
  initialDraftSummary <- reactive({
    req(input$seasonYear, input$otherTeamHightlight)
    getInitialDraftSummary(conn, seasons, players, teamLookup, input$seasonYear, draftData)
  })
  
  draftGamesSummary <- reactive({
    req(input$seasonYear, input$otherTeamHightlight)
    getDraftGamesSummary(conn, seasons, players, teamLookup, input$seasonYear, draftData)
  })
  
  acquiredSummary <- reactive({
    req(input$seasonYear, input$otherTeamHightlight)
    getAcquiredSummary(conn, seasons, players, teamLookup, input$seasonYear, draftData)
  })
  
  
  
  # Dynamic breakdown options
  output$breakdownUI <- renderUI({
    breakdownChoices <- switch(input$category,
                               "Age" = c("Age Summary", "Age Games Played Summary", "Age Demographics", "Age Boxplot", "Selected Team Age Progression", "List Age Progression"),
                               "Experience" = c("Experience Summary", "Games Played by Selected Team", "Experience Boxplot", "Selected Team Games Progression", "List Games Progression"),
                               "Height" = c("Height Summary", "Height Boxplot"),
                               "Draft/Trade" = c("Initial Draft Summary", "Draft Games Summary", "Acquired Summary")
    )
    selectInput("breakdown", "Breakdown", choices = breakdownChoices)
  })
  
  ## ---- AGE ----
  output$ageSummaryTable <- render_gt({
    ageSummary <- ageSummary()
    selectedYear <- input$seasonYear
    premiershipTeam <- get_premiership_team(selectedYear, premiers)
    top4_positions <- get_top4_positions(selectedYear, ladders)
    
    
    df <- ageSummary %>%
      mutate(ageGroup = factor(ageGroup, levels = c("<20", "20-22", "23-25", "26-28", "29-31", "32+"))) %>%
      group_by(TeamName, ageGroup) %>%
      summarise(count = n(), .groups = "drop") %>%
      pivot_wider(names_from = ageGroup, values_from = count, values_fill = 0) %>%
      arrange(TeamName)
    
    df %>%
      gt(rowname_col = "TeamName") %>%
      add_premiership_icon(premiershipTeam) %>%
      add_heatmap() %>%
      highlight_team("Richmond") %>%
      highlight_team(input$otherTeamHightlight, color = 'red') %>%
      highlight_top4_teams(
        top4_teams = names(top4_positions),
        highlighted_team = input$otherTeamHightlight,
        enable = input$top4Toggle
      ) %>%
      tab_header(
        title = "Age Summary"
      ) %>%
      darkThemeGT()
    
    
  })
  
  
  
  output$ageGamesPlayedTable <- render_gt({
    ageSummary <- ageSummary()
    selectedYear <- input$seasonYear
    premiershipTeam <- get_premiership_team(selectedYear, premiers)
    top4_positions <- get_top4_positions(selectedYear, ladders)
    
    
    df <- ageSummary %>%
      mutate(ageGroup = factor(ageGroup, levels = c("<20", "20-22", "23-25", "26-28", "29-31", "32+"))) %>%
      group_by(TeamName, ageGroup) %>%
      summarise(totalGamesAtTime = sum(totalGamesAtTime, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = ageGroup, values_from = totalGamesAtTime, values_fill = 0) %>%
      arrange(TeamName)
    
    df %>%
      gt(rowname_col = "TeamName") %>%
      add_premiership_icon(premiershipTeam) %>%
      highlight_team("Richmond") %>%
      highlight_team(input$otherTeamHightlight, color = 'red') %>%
      highlight_top4_teams(
        top4_teams = names(top4_positions),
        highlighted_team = input$otherTeamHightlight,
        enable = input$top4Toggle
      ) %>%
      add_heatmap() %>%
      tab_header(title = "Total Games Played by Age Group") %>%
      darkThemeGT()
  })
  
  
  
  output$ageDemographicsTable <- render_gt({
    ageDemographics <- ageSummary()
    selectedYear <- input$seasonYear
    premiershipTeam <- get_premiership_team(selectedYear, premiers)
    top4_positions <- get_top4_positions(selectedYear, ladders)
    
    
    desiredOrder <- c("18", "19", "20", "21", "22", "23", "24", "25", 
                      "26", "27", "28", "29", "30", "31", "32", "33",
                      "34", "35", "36", "37", NA)
    
    df <- ageDemographics %>%
      group_by(TeamName, age) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(age = as.character(age)) %>%
      pivot_wider(
        names_from = age,
        values_from = count,
        values_fill = 0
      ) %>%
      arrange(TeamName)
    
    currentCols <- colnames(df)
    ageCols <- setdiff(currentCols, "TeamName")
    orderedCols <- desiredOrder[desiredOrder %in% ageCols]
    df <- df[, c("TeamName", orderedCols)]
    
    df %>%
      gt(rowname_col = "TeamName") %>%
      add_premiership_icon(premiershipTeam) %>%
      add_heatmap() %>%
      highlight_team("Richmond") %>%
      highlight_team(input$otherTeamHightlight, color = 'red') %>%
      highlight_top4_teams(
        top4_teams = names(top4_positions),
        highlighted_team = input$otherTeamHightlight,
        enable = input$top4Toggle
      ) %>%
      tab_header(
        title = "Age Demographics"
      ) %>%
      darkThemeGT()
  })
  
  
  
  output$ageBoxplot <- renderPlot({
    df <- ageSummary()  
    
    ggplot(df, aes(x = TeamName, y = age)) +
      boxPlot +
      labs(
        title = "Player Age by Team",
        x = "Team",
        y = "Age"
      ) +
      scale_y_continuous(breaks = seq(18, 38, by = 2)) +
      darkThemePlot()
  })
  
  
  
  output$selectedAgeProfile <- renderPlot({
    df <- yearlyProfile()  
    selectedTeam <- input$otherTeamHightlight
    
    # fallback if no team selected
    if (is.null(selectedTeam) || selectedTeam == "" || selectedTeam == "None") {
      selectedTeam <- "Richmond"
    }
    
    
    
    fixtures <- dbGetQuery(conn, "
      SELECT *
      FROM GamesBySeason
    ") 
    
    
    df <- df %>%
      group_by(seasonYear, TeamName) %>%
      filter(Games > 0) %>%
      summarise(
        teamContrib = round(sum(ageContribution, na.rm = TRUE) / 23, 1),
      ) %>%
      left_join(fixtures, by = c("seasonYear" = "Season", "TeamName" = "TeamName")) %>%
      mutate(avgAge = round(teamContrib / Mt, 1))
    
    
    
    ggplot(df %>% filter(TeamName == selectedTeam),
           aes(x = seasonYear, y = avgAge)) +
      geom_line(color = "steelblue", size = 1.2) +
      geom_point(color = "steelblue", size = 2) +
      geom_text(
        aes(label = round(avgAge, 1)),   # show age (1 decimal place)
        vjust = -1,                      # push text slightly above point
        color = "white",                 # text color (fits dark theme)
        size = 5
      ) +
      labs(
        title = paste("Average Age of Selected Team per Season -", selectedTeam),
        x = "Season",
        y = "Average Age"
      ) +
      scale_x_continuous(
        breaks = unique(df$seasonYear)  # label every season
      ) +
      darkThemePlot()
    
    
  })
  
  
  
  output$ageProfile <- renderPlot({
    df <- yearlyProfile()  
    selectedTeam <- input$otherTeamHightlight
    
    # fallback if no team selected
    if (is.null(selectedTeam) || selectedTeam == "" || selectedTeam == "None") {
      selectedTeam <- "Richmond"
    }
    
    
    df <- df %>%
      group_by(seasonYear, TeamName) %>%
      summarise(
        avgAge = mean(age, na.rm = TRUE),
      ) %>%
      ungroup()
    
    
    ggplot(df %>% filter(TeamName == selectedTeam),
           aes(x = seasonYear, y = avgAge)) +
      geom_line(color = "steelblue", size = 1.2) +
      geom_point(color = "steelblue", size = 2) +
      geom_text(
        aes(label = round(avgAge, 1)),   # show age (1 decimal place)
        vjust = -1,                      # push text slightly above point
        color = "white",                 # text color (fits dark theme)
        size = 5
      ) +
      labs(
        title = paste("Average Age of Selected Team per Season -", selectedTeam),
        x = "Season",
        y = "Average Age"
      ) +
      scale_x_continuous(
        breaks = unique(df$seasonYear)  # label every season
      ) +
      darkThemePlot()
    
    
  })
  
  ## ---- EXPERIENCE ----
  output$experienceSummaryTable <- render_gt({
    experienceSummary <- experienceSummary()
    selectedYear <- input$seasonYear
    premiershipTeam <- get_premiership_team(selectedYear, premiers)
    top4_positions <- get_top4_positions(selectedYear, ladders)
    
    
    desired_order <- c("0-24", "25-49", "50-74", "75-99", "100+")
    
    df <- experienceSummary %>%
      mutate(experienceGroup = factor(experienceGroup, levels = desired_order)) %>%
      group_by(TeamName, experienceGroup) %>%
      summarise(count = n(), .groups = "drop") %>%
      pivot_wider(names_from = experienceGroup, values_from = count, values_fill = 0) %>%
      arrange(TeamName)
    
    # Ensure columns are in the right order
    df <- df[, c("TeamName", desired_order[desired_order %in% colnames(df)])]
    
    # Create the gt table
    df %>%
      gt(rowname_col = "TeamName") %>%
      add_premiership_icon(premiershipTeam) %>%
      add_heatmap() %>%
      highlight_team("Richmond") %>%
      highlight_team(input$otherTeamHightlight, color = 'red') %>%
      highlight_top4_teams(
        top4_teams = names(top4_positions),
        highlighted_team = input$otherTeamHightlight,
        enable = input$top4Toggle
      ) %>%
      tab_header(
        title = "Experience Summary"
      ) %>%
      darkThemeGT()
  })
  
  
  
  output$experienceContributionTable <- renderPlot({
    experienceContribution <- experienceContribution()
    selectedYear <- input$seasonYear
    premiershipTeam <- get_premiership_team(selectedYear, premiers)
    top4_positions <- get_top4_positions(selectedYear, ladders)
    
    
    #fixtures <- dbGetQuery(conn, "
    #  SELECT *
    #  FROM game
    #  WHERE LeagueID = 1
    #") %>% 
    #filter(SeasonID == selectedYear) %>%
    #select(Team1ID, Team2ID) %>%
    #pivot_longer(cols = everything(), names_to = "whichTeam", values_to = "TeamID") %>%
    #count(TeamID, name = "seasonGames")
    
    fixtures <- dbGetQuery(conn, "
      SELECT *
      FROM GamesBySeason
    ") %>% 
      filter(Season == selectedYear)
    
    
    
    df <- experienceContribution %>%
      group_by(TeamName, TeamID) %>%
      summarise(
        teamContrib = round(sum(experienceContribution, na.rm = TRUE) / 23),
        .groups = "drop"
      ) %>%
      left_join(fixtures %>% select(TeamID, Mt), by = 'TeamID') %>%
      mutate(teamContrib = round(teamContrib / Mt)) %>%
      arrange(TeamName)
    
    
    
    # Create the gt table
    #df %>%
    #  gt(rowname_col = "TeamName") %>%
    #  add_premiership_icon(premiershipTeam) %>%
    #  add_heatmap() %>%
    #  highlight_team("Richmond") %>%
    #  highlight_team(input$otherTeamHightlight, color = 'red') %>%
    #  highlight_top4_teams(
    #    top4_teams = names(top4_positions),
    #    highlighted_team = input$otherTeamHightlight,
    #    enable = input$top4Toggle
    #  ) %>%
    #  tab_header(
    #    title = "Experience Contribution"
    #  ) %>%
    #  darkThemeGT()
    # Bar chart
    
    ggplot(df, aes(x = TeamName, y = teamContrib, fill = TeamName)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = teamContrib), vjust = -0.5, size = 3) +  # show value above bar
      labs(
        title = "Average Games Played by Selected Team",
        x = "Team",
        y = "Average Games Played"
      ) +
      geom_text(
        aes(label = teamContrib),   # display contribution value
        vjust = -0.5,               # place text slightly above the bar
        size = 5,                     # adjust text size
        color = 'white'
      ) +
      theme_minimal(base_size = 14) +
      darkThemePlot(include_legend = TRUE) +
      theme(legend.position = "none")
    
  })
  
  
  
  
  output$experienceBoxplot <- renderPlot({
    df <- experienceSummary()
    
    ggplot(df, aes(x = TeamName, y = totalGamesAtTime)) +
      boxPlot +
      labs(
        title = "Player Experience by Team",
        x = "Team",
        y = "Experience (Games)"
      ) +
      darkThemePlot()
  })
  
  
  
  output$selectedGamesProfile <- renderPlot({
    df <- yearlyProfile()  
    selectedTeam <- input$otherTeamHightlight
    
    # fallback if no team selected
    if (is.null(selectedTeam) || selectedTeam == "" || selectedTeam == "None") {
      selectedTeam <- "Richmond"
    }
    
    
    fixtures <- dbGetQuery(conn, "
      SELECT *
      FROM GamesBySeason
    ") 
    
    
    df <- df %>%
      group_by(seasonYear, TeamName) %>%
      filter(Games > 0) %>%
      summarise(
        teamContrib = round(sum(experienceContribution, na.rm = TRUE) / 23, 1),
      ) %>%
      left_join(fixtures, by = c("seasonYear" = "Season", "TeamName" = "TeamName")) %>%
      mutate(avgGames = round(teamContrib / Mt))
    
    
    ggplot(df %>% filter(TeamName == selectedTeam),
           aes(x = seasonYear, y = avgGames)) +
      geom_line(color = "steelblue", size = 1.2) +
      geom_point(color = "steelblue", size = 2) +
      geom_text(
        aes(label = avgGames),   # show age (1 decimal place)
        vjust = -1,                      # push text slightly above point
        color = "white",                 # text color (fits dark theme)
        size = 5
      ) +
      labs(
        title = paste("Average Games of Selected Team per Season -", selectedTeam),
        x = "Season",
        y = "Average Games"
      ) +
      scale_x_continuous(
        breaks = unique(df$seasonYear)  # label every season
      ) +
      darkThemePlot()
    
    
  })
  
  
  
  output$gamesProfile <- renderPlot({
    df <- yearlyProfile()  
    selectedTeam <- input$otherTeamHightlight
    
    # fallback if no team selected
    if (is.null(selectedTeam) || selectedTeam == "" || selectedTeam == "None") {
      selectedTeam <- "Richmond"
    }
    
    
    df <- df %>%
      group_by(seasonYear, TeamName) %>%
      summarise(
        avgGames = mean(totalGamesAtTime, na.rm = TRUE),
      ) %>%
      ungroup()
    
    
    ggplot(df %>% filter(TeamName == selectedTeam),
           aes(x = seasonYear, y = avgGames)) +
      geom_line(color = "steelblue", size = 1.2) +
      geom_point(color = "steelblue", size = 2) +
      geom_text(
        aes(label = round(avgGames, 1)),   # show age (1 decimal place)
        vjust = -1,                      # push text slightly above point
        color = "white",                 # text color (fits dark theme)
        size = 5
      ) +
      labs(
        title = paste("Average Games of Selected Team per Season -", selectedTeam),
        x = "Season",
        y = "Average Games"
      ) +
      scale_x_continuous(
        breaks = unique(df$seasonYear)  # label every season
      ) +
      darkThemePlot()
    
    
  })
  
  
  ## ---- HEIGHT ----
  output$heightSummaryTable <- render_gt({
    heightSummary <- heightSummary()
    selectedYear <- input$seasonYear
    premiershipTeam <- get_premiership_team(selectedYear, premiers)
    top4_positions <- get_top4_positions(selectedYear, ladders)
    
    
    # Prepare the height summary data
    df <- heightSummary %>%
      mutate(heightGroup = factor(heightGroup, levels = c("<160", "160-164", "165-169", "170-174", "175-179", "180+"))) %>%
      group_by(TeamName, heightGroup) %>%
      summarise(count = n(), .groups = "drop") %>%
      pivot_wider(names_from = heightGroup, values_from = count, values_fill = 0) %>%
      arrange(TeamName)
    
    # Create the gt table
    df %>%
      gt(rowname_col = "TeamName") %>%
      add_premiership_icon(premiershipTeam) %>%
      add_heatmap() %>%
      highlight_team("Richmond") %>%
      highlight_team(input$otherTeamHightlight, color = 'red') %>%
      highlight_top4_teams(
        top4_teams = names(top4_positions),
        highlighted_team = input$otherTeamHightlight,
        enable = input$top4Toggle
      ) %>%
      tab_header(
        title = "Height Summary"
      ) %>%
      darkThemeGT() 
  })
  
  
  output$heightBoxplot <- renderPlot({
    df <- heightSummary()  # should return player-level data with `Height`
    
    ggplot(df, aes(x = TeamName, y = Height)) +
      boxPlot +
      labs(
        title = "Player Height by Team",
        x = "Team",
        y = "Height (cm)"
      ) +
      darkThemePlot()
  })
  
  
  
  ## ---- DRAFT / TRADE ----
  output$initialDraftTable <- render_gt({
    draftSummary <- initialDraftSummary()
    selectedYear <- input$seasonYear
    premiershipTeam <- get_premiership_team(selectedYear, premiers)
    top4_positions <- get_top4_positions(selectedYear, ladders)
    
    
    orderedCols <- c("TeamName", "1-10", "11-20", "21-30", "31-40", "41-50", "51+", "Rookie", "Pre/Mid", "Other")
    
    draftSummary %>%
      select(any_of(orderedCols)) %>%  # Ensures the specified order; ignores missing columns if any
      gt(rowname_col = "TeamName") %>%
      add_premiership_icon(premiershipTeam) %>%
      add_heatmap() %>%
      highlight_team("Richmond") %>%
      highlight_team(input$otherTeamHightlight, color = 'red') %>%
      highlight_top4_teams(
        top4_teams = names(top4_positions),
        highlighted_team = input$otherTeamHightlight,
        enable = input$top4Toggle
      ) %>%
      tab_header(
        title = "Initial Draft Pick Summary"
      ) %>%
      darkThemeGT()
  })
  
  
  
  output$draftGamesTable <- render_gt({
    draftGamesSummary <- draftGamesSummary()
    selectedYear <- input$seasonYear
    premiershipTeam <- get_premiership_team(selectedYear, premiers)
    top4_positions <- get_top4_positions(selectedYear, ladders)
    
    
    orderedCols <- c("TeamName", "1-10", "11-20", "21-30", "31-40", "41-50", "51+", "Rookie", "Pre/Mid", "Other")
    
    draftGamesSummary %>%
      select(any_of(orderedCols)) %>%
      gt(rowname_col = "TeamName") %>%
      add_premiership_icon(premiershipTeam) %>%
      add_heatmap() %>%
      highlight_team("Richmond") %>%
      highlight_team(input$otherTeamHightlight, color = 'red') %>%
      highlight_top4_teams(
        top4_teams = names(top4_positions),
        highlighted_team = input$otherTeamHightlight,
        enable = input$top4Toggle
      ) %>%
      tab_header(
        title = "Total Games by Initial Draft Pick"
      ) %>%
      darkThemeGT()
  })
  
  output$acquiredSummaryBarChart <- renderPlot({
    acquiredSummary <- acquiredSummary()
    selectedYear <- input$seasonYear
    premiershipTeam <- get_premiership_team(selectedYear, premiers)
    
    # Aggregate data
    plotData <- acquiredSummary %>%
      count(TeamName, acquiredGroup)
    
    # Create stacked bar chart
    ggplot(plotData, aes(x = TeamName, y = n, fill = acquiredGroup)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = n), 
                position = position_stack(vjust = 0.5), 
                color = "white", size = 5) +
      labs(x = "Team", y = "Number of Players", fill = "Acquired Group", title = 'Acquired Summary') +
      theme_minimal(base_size = 14) +
      darkThemePlot(include_legend = TRUE)
    
  })
  
}

shinyApp(ui, server)
