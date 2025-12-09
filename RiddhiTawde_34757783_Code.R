# load libraries 
library(shiny)
library(plotly)
library(leaflet)
library(dplyr)
library(tidyr)
library(sf) # handle spatial data
library(rnaturalearth)  # for world map and country boundary data 
library(htmlwidgets) # embed JavaScript visualizations in R
library(stringr)
library(lubridate)
library(shinyjs)

# data loading and data preparation

# load csv file
df_all <- read.csv("Final1.csv")

# convert to date format, extract year-month, capitalize names of the state , fetch first day of the month, remove spaces from scam type
df_all$Date <- as.Date(df_all$Date, format = "%d-%m-%Y")
df_all$Month <- format(df_all$Date, "%Y-%m")
df_all$State <- str_to_title(df_all$State)
df_all$Year <- year(df_all$Date)
df_all$Month_Date <- as.Date(paste0(df_all$Month, "-01"))
df_all$Scam_Type <- str_trim(df_all$Scam_Type)


# sort out unique age groups and gender categories
age_groups <- sort(unique(df_all$Age))
genders <- sort(unique(df_all$Gender))
# map data for Australia as country and match the state names 
australia <- ne_states(country = "Australia", returnclass = "sf")
australia$State <- str_to_title(australia$name)

# calculating KPI 
total_loss_all_time <- sum(df_all$Amount_Lost, na.rm = TRUE)
total_reports_all_time <- sum(df_all$Number_of_Reports, na.rm = TRUE)
total_data_points <- nrow(df_all)


# colour schemes for the project 
bg_color <- "#FDFDFD"
primary_text <- "#2D2D2D"
secondary_text <- "#6A6A6A"
border_color <- "#EEEEEE"
seq_palette <- c("#B9DD79", "#62C485", "#2CA19A", "#3E618C")
qual_palette <- c("#3E618C", "#62C485", "#9F6D9F", "#2CA19A", "#B9DD79", "#FEEB9A")
accent_teal <- "#2CA19A"
accent_blue <- "#3E618C"
accent_green <- "#62C485"

# formating big  numbers
format_kpi_num <- function(num, prefix = "") {
  if (is.na(num) || !is.numeric(num)) return("NA") # invalid input
  if (num >= 1e9) {
    val <- sprintf("%.1f", num / 1e9) # billions
    unit <- "B"
  } else if (num >= 1e6) {
    val <- sprintf("%.1f", num / 1e6) # millions
    unit <- "M"
  } else if (num >= 1e3) {
    val <- sprintf("%.0f", num / 1e3) # thousands 
    unit <- "K"
  } else {
    val <- format(round(num, 0), big.mark = ",") # small numbers 
    unit <- ""
  }
  return(paste0(prefix, val, unit))
}


# ui functions 
# kpi display box 
kpi_box <- function(value, subtitle, icon_name) {
  tags$div(
    class = "kpi-box",
    tags$div(class = "kpi-icon", tags$i(class = paste0("fas fa-", icon_name))),
    tags$div(
      class = "kpi-content",
      tags$div(class = "kpi-value", value),
      tags$div(class = "kpi-subtitle", subtitle)
    )
  )
}
# container styles for the charts 
chart_box <- function(title, content, height = NULL) {
  tags$div(
    class = "chart-box",
    style = if(!is.null(height)) paste0("height: ", height, ";"),
    tags$div(class = "chart-box-header", title),
    tags$div(class = "chart-box-body", content)
  )
}

# landing page
description_page_ui <- tags$div(
  id = "description_page",
  tags$div(
    class = "description-container",
    tags$div(class = "project-title", "Exploring Digital Scams in Australia"),
    tags$div(
      class = "kpi-circle-container",
      tags$div(
        class = "kpi-circle",
        style = "text-align: center;",
        tags$div(class = "kpi-circle-value", textOutput("overall_total_loss_kpi_landing")),
        tags$div(class = "kpi-circle-label", "Total Loss (2021-2023)")
      ),
      tags$div(
        class = "kpi-circle",
        style = "text-align: center;",
        tags$div(class = "kpi-circle-value", textOutput("overall_total_reports_kpi_landing")),
        tags$div(class = "kpi-circle-label", "Total Reports (2021-2023)")
      ),
      tags$div(
        class = "kpi-circle",
        style = "text-align: center;",
        tags$div(class = "kpi-circle-value", format(total_data_points, big.mark = ",")),
        tags$div(class = "kpi-circle-label", "Data Points (Aggregated Suburb Data)")
      )
    ),
    tags$div(class = "section-header", "Project Summary"),
    tags$p(
      class = "summary-text",
      tags$ul(class = "summary-text",
              tags$li("Following the ", tags$b("COVID-19 pandemic"), ", Australia witnessed a significant surge in digital fraud, with losses exceeding ", tags$b(format_kpi_num(total_loss_all_time, prefix = "$")), "."),
              tags$li("This project analyzes digital scam data from 2021 to 2023 to explore the critical relationship between scam activity, regional socioeconomic status, and digital accessibility."),
              tags$li("The goal is to understand how socioeconomic factors might contribute to a region's vulnerability to digital fraud, providing insights into a pressing societal and cybersecurity issue.")
      )
    ),
    
    tags$div(class = "section-header", "Core Findings & Discoveries"),
    tags$ul(class = "summary-text",
            tags$li(tags$b("Vulnerable Demographics:"), " Citizens aged ", tags$b("65 years and older"), " consistently recorded the highest total dollar losses, identifying them as a highly vulnerable population."),
            tags$li(tags$b("Socioeconomic & Reporting Link:"), " The analysis found ", tags$b("no strong linear correlation"), " between socioeconomic characteristics and financial losses. However, there was a ", tags$b("strong, statistically significant positive link"), " between higher digital inclusion (IEO scores) and higher per capita scam reporting rates.")
    ),
    tags$div(class = "section-header", "Quick Guide to the Dashboard"),
    tags$ul(class = "summary-text",
            tags$li(tags$b("Features:")," The dashboard has two tabs", tags$b("Geo Analyis & Demographic Analysis")," that answer all the questions and showcase the scam effect on Australai in 2021 to 2021"),
            tags$li(tags$b("Interact:"), " The dashboard is fully interactive. Click on data points (like states on the map or sections on the treemap) to instantly filter all other charts."),
            tags$li(tags$b("Filters:"), " Use the sliders and dropdowns in the dashboard's top control bar to change the year, gender, and view losses vs. reports.")
    ),
    actionButton("go_to_dashboard_from_landing", "Go to Dashboard", class = "dashboard-button") # button to navigate to the main dashbaord 
  )
)

# UI and styling of elements for the dashboard 
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML(paste0("
      body {
        background-color: #FFFFFF !important;
        color: ", primary_text, " !important;
        font-family: 'Open Sans', sans-serif;
        overflow: hidden;
      }
      .navbar-default {
        background-color: ", accent_blue, ";
        border-color: ", accent_blue, ";
      }
      .navbar-default .navbar-brand {
        color: white;
        font-weight: bold;
      }
      .navbar-default .navbar-nav > li > a {
        color: white;
      }
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > li > a:hover {
        background-color: rgba(255, 255, 255, 0.2);
        color: white;
      }
      .kpi-box {
        background-color: white;
        border: 1px solid ", accent_blue, ";
        border-left: 5px solid ", accent_teal, ";
        padding: 0px 15px;
        margin-bottom: 0px;
        border-radius: 4px;
        box-shadow: 0 1px 1px rgba(0,0,0,0.05);
        min-height: 110px;
        display: flex;
        align-items: center;
      }
      .kpi-icon {
        font-size: 24px;
        color: ", accent_blue, ";
        padding-right: 15px;
      }
      .kpi-content {
        flex-grow: 1;
      }
      .kpi-value {
        font-size: 28px;
        font-weight: 700;
        color: ", primary_text, ";
        line-height: 1.2;
      }
      .kpi-subtitle {
        font-size: 14px;
        color: ", secondary_text, ";
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        line-height: 1.2;
      }
      .chart-box {
        background-color: white;
        border: 1px solid ", accent_blue, ";
        border-top: 3px solid ", accent_blue, ";
        padding: 0;
        margin-bottom: 15px;
        border-radius: 4px;
        box-shadow: 0 1px 1px rgba(0,0,0,0.05);
      }
      .chart-box-header {
        color: ", primary_text, ";
        font-size: 17px;
        font-weight: 600;
        padding: 10px 15px;
        border-bottom: 1px solid ", border_color, ";
      }
      .chart-box-body {
        padding: 10px;
      }
      .controls-box {
        background-color: white;
        border: 1px solid ", accent_blue, ";
        border-top: 3px solid ", accent_blue, ";
        padding: 4px 0px 0px 0px;
        margin-bottom: 10px;
        border-radius: 4px;
        box-shadow: 0 1px 1px rgba(0,0,0,0.05);
        min-height: 95px;
        display: flex;
        align-items: flex-start;
        width: 100%;
      }
      .control-col {
        flex-grow: 1;
        flex-shrink: 0;
        flex-basis: 25%;
        max-width: 25%;
        padding: 0 15px;
        box-sizing: border-box;
      }
      .control-col-text {
        flex-grow: 1;
        flex-shrink: 0;
        flex-basis: 25%;
        max-width: 25%;
        padding: 0 15px;
        box-sizing: border-box;
      }
      .controls-box .form-group {
        margin-bottom: 0;
      }
      .controls-box .shiny-text-output {
        font-size: 13px;
        color: ", secondary_text, ";
        margin: 0;
        line-height: 1.4;
        min-height: 80px;
        white-space: pre-line;
        overflow-wrap: break-word;
      }
      .controls-box .radio-inline {
        margin-top: 0;
        margin-bottom: 10px;
      }
      .controls-box .action-button {
        margin-top: 5px;
        width: 100%;
      }
      .selectize-dropdown-content {
        max-height: 38px;
        overflow-y: auto;
      }
      #description_page {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: ", accent_blue, ";
        z-index: 1000;
        overflow: hidden;
        display: flex;
        align-items: center;
        justify-content: justify;
        padding: 15px;
      }
      .description-container {
        max-width: 1500px;
        width: 100%;
        height: calc(100vh - 30px);
        overflow-y: hidden;
        margin: 0 auto;
        padding: 30px 50px;
        background-color: white;
        border: 1px solid #EEEEEE;
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.2);
        text-align: center;
      }
      .project-title {
        font-size: 34px;
        font-weight: 700;
        color: ", accent_blue, ";
        margin-bottom: 20px;
        text-align: center;
      }
      .summary-text {
        text-align: justify;
        font-size: 16px;
        color: ", primary_text, ";
        line-height: 1.7;
        margin-bottom: 15px;
      }
      .section-header {
        font-size: 20px;
        font-weight: 600;
        color: ", accent_blue, ";
        border-bottom: 1px dashed ", border_color, ";
        padding-bottom: 5px;
        margin-top: 20px;
        margin-bottom: 12px;
        text-align: center;
      }
      .kpi-circle-container {
        display: flex;
        justify-content: space-around;
        margin: 25px 0;
        padding: 0 10px;
      }
      .kpi-circle {
        width: 200px;
        height: 200px;
        border: 5px solid ", accent_blue, ";
        border-radius: 50%;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        text-align: center;
        margin: 0 15px;
        transition: transform 0.3s ease-in-out;
        cursor: default;
      }
      .kpi-circle:hover {
        transform: scale(1.05);
      }
      .kpi-circle-value {
        font-size: 32px;
        font-weight: 700;
        color: ", accent_blue, ";
        line-height: 1.1;
        padding: 0 10px;
      }
      .kpi-circle-label {
        font-size: 14px;
        color: ", primary_text, ";
        margin-top: 10px;
        padding: 0 10px;
      }
      .dashboard-button {
        background-color: ", accent_blue, ";
        color: white;
        padding: 12px 40px;
        font-size: 16px;
        border: none;
        border-radius: 4px;
        cursor: pointer;
        margin-top: 20px;
        transition: background-color 0.3s, transform 0.2s;
      }
      .dashboard-button:hover {
        background-color: #315178;
        transform: scale(1.03);
      }
    "))),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")
  ),
  
  description_page_ui,
  
  hidden(
    tags$div(
      id = "main_dashboard",
      navbarPage(
        title = "Exploring Digital Scams in Australia",
        id = "menu_nav",
        collapsible = TRUE,
        
        # Ui and styling of containers for graphs and other elements on Tab 1 
        tabPanel(
          "🗺 Geo Analysis",
          value = "geo",
          fluidRow(
            column(
              8,
              tags$div(
                class = "controls-box",
                div(
                  class = "control-col",
                  tags$label("Year Range", `for` = "year_geo", style = "font-weight: bold;"),
                  sliderInput("year_geo", label = NULL, min = 2021, max = 2023, value = c(2021, 2023), step = 1, sep = "")
                ),
                div(
                  class = "control-col",
                  tags$label("Gender", `for`="gender_geo", style="font-weight: bold;"),
                  selectInput("gender_geo", label = NULL, choices = c("All" = "", genders), selected = "")
                ),
                div(
                  class = "control-col",
                  tags$label("Metric", style = "font-weight: bold;"),
                  radioButtons("metric_geo", label = NULL, choices = c("Loss" = "loss", "Report" = "report"), inline = TRUE),
                  actionButton("clear_geo", "Clear Filters", style = paste0("background-color: ", accent_blue, "; color: white; border: none;"))
                ),
                div(
                  class = "control-col-text",
                  tags$label("Active Filter", style = "font-weight: bold;"),
                  textOutput("geo_description")
                )
              )
            ),
            column(
              4,
              fluidRow(
                column(6, kpi_box(value = textOutput("total_loss_geo_text"), subtitle = "Total Losses (AUD)", icon_name = "dollar-sign")),
                column(6, kpi_box(value = textOutput("total_report_geo_text"), subtitle = "Total Reports", icon_name = "file-alt"))
              )
            )
          ),
          fluidRow(
            column(6, chart_box("Geographical Analysis (Click State to Filter)", leafletOutput("map_geo", height = "625px"))),
            column(6,
                   chart_box("Contextual Analysis (Click Point to Filter State)", tabsetPanel(
                     tabPanel("Socioeconomic Status", plotlyOutput("scatter_socio", height = "245px")),
                     tabPanel("Digital Inclusion", plotlyOutput("scatter_digital", height = "245px"))
                   )),
                   chart_box("Trend Comparison (Filtered vs. All Australia)", plotlyOutput("line_chart", height = "250px"))
            )
          )
        ),
        
        # Ui and styling of containers for graphs and other elements on Tab 2
        tabPanel(
          "👥 Demographic Analysis",
          value = "demo",
          fluidRow(
            column(
              8,
              tags$div(
                class = "controls-box",
                div(
                  class = "control-col",
                  tags$label("Year Range", `for` = "year_demo", style = "font-weight: bold;"),
                  sliderInput("year_demo", label = NULL, min = 2021, max = 2023, value = c(2021, 2023), step = 1, sep = "")
                ),
                div(
                  class = "control-col",
                  tags$label("Gender", `for`="gender_demo", style="font-weight: bold;"),
                  selectInput("gender_demo", label = NULL, choices = c("All" = "", genders), selected = "")
                ),
                div(
                  class = "control-col",
                  tags$label("Metric", style = "font-weight: bold;"),
                  radioButtons("metric_demo", label = NULL, choices = c("Loss" = "loss", "Report" = "report"), inline = TRUE),
                  actionButton("clear_demo", "Clear Filters", style = paste0("background-color: ", accent_blue, "; color: white; border: none;"))
                ),
                div(
                  class = "control-col-text",
                  tags$label("Active Filter", style = "font-weight: bold;"),
                  textOutput("demo_description")
                )
              )
            ),
            column(
              4,
              fluidRow(
                column(6, kpi_box(value = textOutput("total_loss_demo_text"), subtitle = "Total Losses (AUD)", icon_name = "dollar-sign")),
                column(6, kpi_box(value = textOutput("total_report_demo_text"), subtitle = "Total Reports", icon_name = "file-alt"))
              )
            )
          ),
          fluidRow(
            column(6,
                   chart_box("Scam Types Distribution (Click to Filter)", plotlyOutput("treemap_scam", height = "270px")),
                   chart_box("Age and Gender Trends (Click Bar for Age Filter)", plotlyOutput("line_gender_age", height = "270px"))
            ),
            column(6, chart_box("Contact Method → Scam Category Flow (Click Left Node to Filter)", plotlyOutput("sankey_contact", height = "625px")))
          )
        )
      )
    )
  )
)

# server  logic 
server <- function(input, output, session) {
  
  # button click to go to the dashboard from the landing page 
  observeEvent(input$go_to_dashboard_from_landing, {
    hide("description_page")
    show("main_dashboard")
  })
  
  # show KPI on landing page 
  output$overall_total_loss_kpi_landing <- renderText({
    format_kpi_num(total_loss_all_time, prefix = "$")
  })
  output$overall_total_reports_kpi_landing <- renderText({
    format_kpi_num(total_reports_all_time)
  })
  
  # reactive values of filters and settings for dahsbaord 
  shared_year <- reactiveVal(c(2021, 2023))
  shared_metric <- reactiveVal("loss")
  shared_state <- reactiveVal(NULL)
  shared_gender <- reactiveVal(NULL)
  shared_age <- reactiveVal(NULL)
  shared_scam_type <- reactiveVal(NULL)
  shared_contact_mode <- reactiveVal(NULL)
  
  # storing data from reactive plots 
  treemap_data <- reactiveVal(NULL)
  sankey_nodes_list <- reactiveVal(NULL)
  sankey_contact_list <- reactiveVal(NULL)
  
  # storing user selected metrics to change all the visualizations 
  get_metric_label <- reactive({
    if(shared_metric() == "loss") "Amount Lost (AUD)" else "Number of Reports"
  })
  
  # format numbers based on metric type 
  format_number <- function(x, metric_type) {
    if(metric_type == "loss") {
      paste0("$", format(round(x), big.mark = ","))
    } else {
      format(round(x), big.mark = ",")
    }
  }
  
  # change in year when the slider moves 
  observeEvent(input$year_geo, { shared_year(input$year_geo) }, ignoreNULL = FALSE)
  # change in gender when gender is selected form the dropdown 
  observeEvent(input$gender_geo, {
    if(is.null(input$gender_geo) || input$gender_geo == "") shared_gender(NULL) else shared_gender(input$gender_geo)
  }, ignoreNULL = FALSE)
  # update metric when the toggle is selected 
  observeEvent(input$metric_geo, { shared_metric(input$metric_geo) }, ignoreNULL = FALSE)
  # clear all by reset button and update to default value 
  observeEvent(input$clear_geo, {
    shared_state(NULL); shared_gender(NULL); shared_age(NULL); shared_scam_type(NULL); shared_contact_mode(NULL)
    updateSliderInput(session, "year_geo", value = c(2021, 2023))
    updateSelectInput(session, "gender_geo", selected = "")
    updateRadioButtons(session, "metric_geo", selected = "loss")
  })
  
  # same as the above for tab 2 
  observeEvent(input$year_demo, { shared_year(input$year_demo) }, ignoreNULL = FALSE)
  observeEvent(input$gender_demo, {
    if(is.null(input$gender_demo) || input$gender_demo == "") shared_gender(NULL) else shared_gender(input$gender_demo)
  }, ignoreNULL = FALSE)
  observeEvent(input$metric_demo, { shared_metric(input$metric_demo) }, ignoreNULL = FALSE)
  observeEvent(input$clear_demo, {
    shared_state(NULL); shared_gender(NULL); shared_age(NULL); shared_scam_type(NULL); shared_contact_mode(NULL)
    updateSliderInput(session, "year_demo", value = c(2021, 2023))
    updateSelectInput(session, "gender_demo", selected = "")
    updateRadioButtons(session, "metric_demo", selected = "loss")
  })
  
  # update shared_age when the bar in the bar chart is clicked 
  observeEvent(event_data("plotly_click", source = "gender_age_plot"), {
    d <- event_data("plotly_click", source = "gender_age_plot")
    req(d)
    if (!is.null(d$x)) {
      clicked_age <- as.character(d$x)
      if (clicked_age %in% age_groups) {
        if(!is.null(shared_age()) && shared_age() == clicked_age) {
          shared_age(NULL) 
        } else {
          shared_age(clicked_age) 
        }
      }
    }
  })
  # update scam type when treemap element is selected 
  observeEvent(event_data("plotly_click", source = "treemap_plot"), {
    d <- event_data("plotly_click", source = "treemap_plot")
    req(d, !is.null(d$pointNumber))
    
    tm_data <- treemap_data()
    req(tm_data, nrow(tm_data) > 0)
    
    point_idx <- d$pointNumber + 1
    if(point_idx <= nrow(tm_data)) {
      clicked_scam <- as.character(tm_data$Scam_Type[point_idx])
      if(nchar(clicked_scam) > 0 && clicked_scam != "NA") {
        if(!is.null(shared_scam_type()) && shared_scam_type() == clicked_scam) {
          shared_scam_type(NULL)
        } else {
          shared_scam_type(clicked_scam)
        }
      }
    }
  })
  
  # update sate when a map region is clicked 
  observeEvent(input$map_geo_shape_click, {
    req(input$map_geo_shape_click)
    clicked_state <- input$map_geo_shape_click$id
    if(!is.null(shared_state()) && shared_state() == clicked_state) {
      shared_state(NULL)
    } else {
      shared_state(clicked_state)
    }
  })
  
  # update contact method when a node in sankey plot 
  observeEvent(event_data("plotly_click", source = "sankey_plot"), {
    d <- event_data("plotly_click", source = "sankey_plot")
    req(d, !is.null(d$pointNumber))
    
    node_labels <- sankey_nodes_list()
    contact_modes <- sankey_contact_list()
    req(node_labels, contact_modes)
    
    point_idx <- d$pointNumber
    if(point_idx >= 0 && (point_idx + 1) <= length(node_labels)) {
      clicked_node <- as.character(node_labels[point_idx + 1])
      if(clicked_node %in% contact_modes) {
        current_filter <- shared_contact_mode()
        if(!is.null(current_filter) && current_filter == clicked_node) {
          shared_contact_mode(NULL)
        } else {
          shared_contact_mode(clicked_node)
        }
      }
    }
  })
  
  # sync filters across tabs
  observe({
    req(!is.null(input$menu_nav))
    current_tab <- input$menu_nav
    
    # update geo tab input if user is not on gep tab 
    if (current_tab != "geo") {
      updateSliderInput(session, "year_geo", value = shared_year())
      updateSelectInput(session, "gender_geo", selected = ifelse(is.null(shared_gender()), "", shared_gender()))
      updateRadioButtons(session, "metric_geo", selected = shared_metric())
    }
    
    # update dempgrpahic tab input if user is not on it 
    if (current_tab != "demo") {
      updateSliderInput(session, "year_demo", value = shared_year())
      updateSelectInput(session, "gender_demo", selected = ifelse(is.null(shared_gender()), "", shared_gender()))
      updateRadioButtons(session, "metric_demo", selected = shared_metric())
    }
  })
  
  # data filtering 
  data_all_filtered <- reactive({
    req(df_all)
    df <- df_all %>%
      # filter by selected year and the metric 
      filter(Year >= shared_year()[1] & Year <= shared_year()[2]) %>%
      mutate(Metric = if(shared_metric() == "loss") Amount_Lost else Number_of_Reports)
    
    # apply additional filters if selected 
    if (!is.null(shared_state())) df <- df %>% filter(State == shared_state())
    if (!is.null(shared_gender())) df <- df %>% filter(Gender == shared_gender())
    if (!is.null(shared_age())) df <- df %>% filter(Age == shared_age())
    if (!is.null(shared_scam_type())) df <- df %>% filter(Scam_Type == shared_scam_type())
    if (!is.null(shared_contact_mode())) df <- df %>% filter(Contact_Mode == shared_contact_mode())
    
    df
  })
  
  
  # output description to show active filters
  create_filter_text <- reactive({
    parts <- c()
    if(!is.null(shared_state())) parts <- c(parts, paste0("State: ", shared_state()))
    if(!is.null(shared_gender())) parts <- c(parts, paste0("Gender: ", shared_gender()))
    if(!is.null(shared_age())) parts <- c(parts, paste0("Age: ", shared_age()))
    if(!is.null(shared_scam_type())) parts <- c(parts, paste0("Scam Type: ", shared_scam_type()))
    if(!is.null(shared_contact_mode())) parts <- c(parts, paste0("Contact: ", shared_contact_mode()))
    
    if(length(parts) > 0) paste(parts, collapse = "\n") else "No active filters."
  })
  
  # show filter summary on both tabs
  output$geo_description <- renderText({ create_filter_text() })
  output$demo_description <- renderText({ create_filter_text() })
  
  # KPI output on both tabs
  output$total_loss_geo_text <- renderText({
    req(data_all_filtered())
    total <- sum(data_all_filtered()$Amount_Lost, na.rm = TRUE)
    format_kpi_num(total, prefix = "$")
  })
  output$total_report_geo_text <- renderText({
    req(data_all_filtered())
    total <- sum(data_all_filtered()$Number_of_Reports, na.rm = TRUE)
    format_kpi_num(total)
  })
  output$total_loss_demo_text <- renderText({
    req(data_all_filtered())
    total <- sum(data_all_filtered()$Amount_Lost, na.rm = TRUE)
    format_kpi_num(total, prefix = "$")
  })
  output$total_report_demo_text <- renderText({
    req(data_all_filtered())
    total <- sum(data_all_filtered()$Number_of_Reports, na.rm = TRUE)
    format_kpi_num(total)
  })
  
  # map output on geo tab 
  output$map_geo <- renderLeaflet({
    leaflet(australia) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 135, lat = -27, zoom = 4.3)
  })
  
  # reactive msp updates on filtered dat a
  observe({
    df_map <- data_all_filtered() %>%
      group_by(State) %>%
      summarise(Metric = sum(Metric, na.rm = TRUE),
                Total_Loss = sum(Amount_Lost, na.rm = TRUE),
                Total_Reports = sum(Number_of_Reports, na.rm = TRUE),
                Avg_IRSD = mean(Avg_IRSD_Score, na.rm = TRUE),
                .groups = "drop")
    
    # join aggregated data with spatial state polygons
    australia_data <- left_join(australia, df_map, by = "State")
    # colour paette 
    pal <- colorBin(palette = seq_palette, domain = australia_data$Metric, bins = 6, na.color = border_color)
    metric_label <- get_metric_label()
    
    leafletProxy("map_geo", data = australia_data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal(Metric), weight = 1, opacity = 1, color = border_color, fillOpacity = 0.7,
        highlight = highlightOptions(weight = 2, color = primary_text, fillOpacity = 0.9, bringToFront = TRUE),
        label = ~lapply(paste0(
          "<div style='font-family: Arial; font-size: 13px; min-width: 250px;'>",
          "<b style='font-size: 14px; color: ", primary_text, ";'>State:</b> <span style='color: ", primary_text, "; font-weight: bold;'>", State, "</span><br>",
          "<b style='color: #6A6A6A;'>Avg IRSD Score:</b> <span style='color: ", primary_text, ";'>", format(round(Avg_IRSD, 1), nsmall = 1), "</span><br>",
          "<b style='color: #6A6A6A;'>", metric_label, ":</b> <span style='color: ", primary_text, ";'>", format_number(Metric, shared_metric()), "</span><br>",
          "<b style='color: #6A6A6A;'>Total Reported Losses:</b> <span style='color: ", primary_text, ";'>$", format(Total_Loss, big.mark = ","), "</span><br>",
          "<b style='color: #6A6A6A;'>Total Scam Reports:</b> <span style='color: ", primary_text, ";'>", format(Total_Reports, big.mark = ","), "</span><br>",
          "<i style='font-size:11px; color: gray;'>Click to filter by state</i>",
          "</div>"
        ), htmltools::HTML),
        layerId = ~State
      ) %>%
      addLegend(pal = pal, values = ~Metric, opacity = 0.7, title = metric_label, position = "bottomright",
                labFormat = labelFormat(prefix = if(shared_metric() == "loss") "$" else "", big.mark = ",", digits = 0))
  })
  
  # scatter plots 
  output$scatter_socio <- renderPlotly({
    # aggregate filltered 
    df_scatter <- data_all_filtered() %>%
      group_by(State) %>%
      summarise(Metric = sum(Metric, na.rm = TRUE), IRSD = mean(Avg_IRSD_Score, na.rm = TRUE), .groups = "drop")
    
    req(nrow(df_scatter) > 0, !all(is.na(df_scatter$IRSD)))
    
    fit_model <- lm(Metric ~ IRSD, data = df_scatter)
    r_squared <- summary(fit_model)$r.squared
    
    # plotly scatter plot with trend line
    plot_ly(df_scatter, x = ~IRSD, y = ~Metric, type = "scatter", mode = "markers",
            color = ~State, colors = qual_palette,
            marker = list(size = 12, line = list(color = "white", width = 1.5)),
            customdata = ~State,
            hovertemplate = paste0(
              "<b style='font-size: 13px;'>%{customdata}</b><br>",
              "<b>IRSD Score:</b> %{x:.1f}<br>",
              "<b>", get_metric_label(), ":</b> %{y:,.0f}<br>",
              "<extra></extra>"
            )) %>%
      add_trace(x = df_scatter$IRSD, y = fitted(fit_model), mode = "lines",
                line = list(color = accent_blue, width = 2, dash="dash"), name = paste0("Trend (R² = ", round(r_squared, 3), ")"),
                showlegend = FALSE, inherit = FALSE,
                hovertemplate = paste0("<b>Trend Line</b><br><b>Predicted:</b> %{y:,.0f}<extra></extra>")) %>%
      layout(
        xaxis = list(title = "IRSD Score (Lower = More Disadvantaged)", showgrid = FALSE, zeroline = FALSE),
        yaxis = list(title = get_metric_label(), showgrid = TRUE, gridcolor = border_color, zeroline = FALSE),
        paper_bgcolor = "white", plot_bgcolor = "white", showlegend = TRUE,
        legend = list(x = 1.02, y = 1, xanchor = "left", yanchor = "top", font = list(size = 10)),
        font = list(color = primary_text),
        margin = list(l = 50, r = 120, t = 30, b = 40)
      )
  })
  # scatter plot: metric vs digital inclusion (IEO)
  output$scatter_digital <- renderPlotly({
    df_scatter <- data_all_filtered() %>%
      group_by(State) %>%
      summarise(Metric = sum(Metric, na.rm = TRUE), IEO = mean(Avg_IEO_Score, na.rm = TRUE), .groups = "drop")
    
    req(nrow(df_scatter) > 0, !all(is.na(df_scatter$IEO)))
    
    fit_model <- lm(Metric ~ IEO, data = df_scatter)
    r_squared <- summary(fit_model)$r.squared
    
    # plotly scatter plot with trend line
    plot_ly(df_scatter, x = ~IEO, y = ~Metric, type = "scatter", mode = "markers",
            color = ~State, colors = qual_palette,
            marker = list(size = 12, line = list(color = "white", width = 1.5)),
            customdata = ~State,
            hovertemplate = paste0(
              "<b style='font-size: 13px;'>%{customdata}</b><br>",
              "<b>Digital Inclusion Score:</b> %{x:.1f}<br>",
              "<b>", get_metric_label(), ":</b> %{y:,.0f}<br>",
              "<extra></extra>"
            )) %>%
      add_trace(x = df_scatter$IEO, y = fitted(fit_model), mode = "lines",
                line = list(color = accent_blue, width = 2, dash="dash"), name = paste0("Trend (R² = ", round(r_squared, 3), ")"),
                showlegend = FALSE, inherit = FALSE,
                hovertemplate = paste0("<b>Trend Line</b><br><b>Predicted:</b> %{y:,.0f}<extra></extra>")) %>%
      layout(
        xaxis = list(title = "IEO Score (Digital Inclusion Index)", showgrid = FALSE, zeroline = FALSE),
        yaxis = list(title = get_metric_label(), showgrid = TRUE, gridcolor = border_color, zeroline = FALSE),
        paper_bgcolor = "white", plot_bgcolor = "white", showlegend = TRUE,
        legend = list(x = 1.02, y = 1, xanchor = "left", yanchor = "top", font = list(size = 10)),
        font = list(color = primary_text),
        margin = list(l = 50, r = 120, t = 30, b = 40)
      )
  })
  
  # line chart 
  output$line_chart <- renderPlotly({
    df_line_all <- df_all %>%
      group_by(Month_Date) %>%
      summarise(Metric_All = if(shared_metric() == "loss") sum(Amount_Lost, na.rm = TRUE) else sum(Number_of_Reports, na.rm = TRUE), .groups = "drop")
    
    # aggregate filtered dataset by month
    df_line_filtered <- data_all_filtered() %>%
      mutate(Metric = if(shared_metric() == "loss") Amount_Lost else Number_of_Reports) %>%
      group_by(Month_Date) %>%
      summarise(Metric_Filtered = sum(Metric, na.rm = TRUE), .groups = "drop")
    
    # merge full and filtered data
    df_merged <- full_join(df_line_all, df_line_filtered, by = "Month_Date") %>% arrange(Month_Date) %>% filter(!is.na(Month_Date))
    req(nrow(df_merged) > 0)
    
    # plotly line chart
    plot_ly() %>%
      add_trace(data = df_merged, x = ~Month_Date, y = ~Metric_Filtered, type = "scatter", mode = "lines", name = "Filtered Selection",
                line = list(color = accent_blue, width = 2),
                hovertemplate = paste0("<b>Filtered Data</b><br><b>Month:</b> %{x|%b %Y}<br><b>", get_metric_label(), ":</b> %{y:,.0f}<extra></extra>")) %>%
      add_trace(data = df_merged, x = ~Month_Date, y = ~Metric_All, type = "scatter", mode = "lines", name = "All Australia",
                line = list(color = accent_green, width = 1, dash = "dot"),
                hovertemplate = paste0("<b>All Australia</b><br><b>Month:</b> %{x|%b %Y}<br><b>", get_metric_label(), ":</b> %{y:,.0f}<extra></extra>")) %>%
      layout(
        xaxis = list(title = "Month", showgrid = FALSE, zeroline = FALSE, type = "date", tickformat = "%b %Y", dtick = "M3", tickangle = -45, tickfont = list(color = secondary_text, size = 10)),
        yaxis = list(title = get_metric_label(), showgrid = TRUE, gridcolor = border_color, zeroline = FALSE, tickfont = list(color = secondary_text)),
        paper_bgcolor = "white", plot_bgcolor = "white", showlegend = TRUE,
        legend = list(x = 1.02, y = 1, xanchor = "left", yanchor = "top"), font = list(color = primary_text)
      )
  })
  
  # bar chart
  output$line_gender_age <- renderPlotly({
    df_chart <- data_all_filtered() %>%
      group_by(Age, Gender) %>%
      summarise(Metric = sum(Metric, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(Age), !is.na(Gender))
    
    req(nrow(df_chart) > 0)
    
    p <- plot_ly(df_chart, x = ~Age, y = ~Metric, color = ~Gender, type = "bar", colors = qual_palette,
                 customdata = ~Gender,
                 hovertemplate = paste0("<b>%{fullData.name}</b><br><b>Age Group:</b> %{x}<br><b>", get_metric_label(), ":</b> %{y:,.0f}<br><i style='font-size:11px; color: gray;'>Click X-axis label to filter by Age</i><extra></extra>"),
                 source = "gender_age_plot") %>%
      layout(
        xaxis = list(title = "Age Group", showgrid = FALSE, zeroline = FALSE, tickfont = list(color = primary_text)),
        yaxis = list(title = get_metric_label(), showgrid = TRUE, gridcolor = border_color, zeroline = FALSE, tickfont = list(color = secondary_text)),
        paper_bgcolor = "white", plot_bgcolor = "white", barmode = "group", showlegend = TRUE,
        legend = list(x = 1.02, y = 1, xanchor = "left", yanchor = "top", font = list(size = 11)),
        font = list(color = primary_text), margin = list(l = 50, r = 100, t = 30, b = 50)
      )
    event_register(p, 'plotly_click')
  })
  
  # tree map
  output$treemap_scam <- renderPlotly({
    df_treemap <- data_all_filtered() %>%
      group_by(Scam_Type) %>%
      summarise(Metric = sum(Metric, na.rm = TRUE), .groups = "drop") %>%
      filter(Metric > 0) %>% arrange(Scam_Type)
    
    req(nrow(df_treemap) > 0)
    treemap_data(df_treemap)
    
    # plotly treemap
    plot_ly(data = df_treemap, type = "treemap", labels = ~Scam_Type, parents = "", values = ~Metric,
            textinfo = "label+value",
            hovertemplate = paste0("<b>%{label}</b><br><b>", get_metric_label(), ":</b> %{value:,.0f}<br><i style='color: #999; font-size: 11px;'>Click to filter by scam type</i><extra></extra>"),
            marker = list(colorscale = list(c(0, "#FEEB9A"), c(0.25, "#B9DD79"), c(0.5, "#62C485"), c(0.75, "#2CA19A"), c(1, "#3E618C"))),
            source = "treemap_plot") %>%
      layout(paper_bgcolor = "white", font = list(color = primary_text), hoverlabel = list(bgcolor = "white"),
             margin = list(t = 0, b = 0, l = 0, r = 0, pad = 0))
  })
  
  # sankey diagram 
  output$sankey_contact <- renderPlotly({
    df_sankey <- data_all_filtered() %>%
      group_by(Contact_Mode, Scam_Category) %>%
      summarise(Metric = sum(Metric, na.rm = TRUE), .groups = "drop") %>% filter(Metric > 0)
    
    req(nrow(df_sankey) > 0)
    
    # define nodes
    contact_modes <- sort(unique(df_sankey$Contact_Mode))
    scam_categories <- sort(unique(df_sankey$Scam_Category))
    nodes <- c(contact_modes, scam_categories)
    sankey_nodes_list(nodes)
    sankey_contact_list(contact_modes)
    
    # define links
    links <- data.frame(
      source = match(df_sankey$Contact_Mode, nodes) - 1,
      target = match(df_sankey$Scam_Category, nodes) - 1,
      value = df_sankey$Metric
    )
    
    node_colors <- c(rep(accent_blue, length(contact_modes)), rep(accent_green, length(scam_categories)))
    
    # plotly sankey diagram
    p <- plot_ly(type = "sankey", arrangement = "fixed",
                 node = list(label = nodes, pad = 20, thickness = 25, line = list(color = "white", width = 1), color = node_colors,
                             hovertemplate = "<b>%{label}</b><br><i style='color: #999; font-size: 11px;'>Click blue nodes to filter</i><extra></extra>"),
                 link = list(source = links$source, target = links$target, value = links$value, color = "rgba(200,200,200,0.4)",
                             hovertemplate = "<b>%{source.label}</b> → <b>%{target.label}</b><br><b>Count:</b> %{value:,.0f}<extra></extra>"),
                 source = "sankey_plot") %>%
      layout(margin = list(t = 10, b = 10, l = 20, r = 0))
    
    event_register(p, 'plotly_click')
  })
}

shinyApp(ui = ui, server = server)
