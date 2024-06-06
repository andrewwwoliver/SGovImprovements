library(shiny)
library(highcharter)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)
library(rsconnect)
library(png)
library(htmltools)

# Highchart options
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
hcoptslang$numericSymbols <- " "
options(highcharter.lang = hcoptslang)

# Load the theme
thm <- source("hc_theme.R")

# Set the options for the sidebar menu
crops <- c("Total cereals", "Spring barley", "Winter barley", "Wheat", "Oats", "Oilseed rape")
PAY_type <- c("Production", "Area", "Yield")

# Set up the UI
ui <- fluidPage(
  tags$head(tags$title("SG - Cereal and Oilseed Rape Harvest")),
  tags$head(includeHTML("google-analytics.html"),
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
            titlePanel(
              div(column(width = 2, tags$img(src = "RESAS Logo.png", align = "left", width = "60%")),
                  column(width = 7, h1("Cereal and Oilseed Rape Harvest in Scotland", tags$img(src = "NS logo.jpg", width = "5%"))),
                  column(width = 2, tags$img(src = "sg.png", align = "right", width = "100%")))
            )
  ),
  br(), br(), br(), br(),
  div(class = "sidebar",
      sidebarPanel(width = 12,
                   checkboxGroupInput("crop", "Choose crop to add to chart", choiceNames = crops, choiceValues = crops, selected = "Total cereals"),
                   actionButton("selectAll", label = "Select All"),
                   actionButton("deselectAll", label = "Deselect All"),
                   radioButtons("pay", "Select variable", choiceNames = PAY_type, choiceValues = PAY_type),
                   div(chooseSliderSkin("Flat"),
                       sliderInput("year", "Select year range", value = c(2003, 2023), min = 2003, max = 2023, step = 1, sep = "", ticks = TRUE))
      )
  ),
  div(class = "main",
      mainPanel(width = 12,
                tabsetPanel(type = "tabs",
                            tabPanel("Information",
                                     h2("Notes on the data"),
                                     p("These interactive charts are based on the final estimates of area, yield and production for spring and winter barley, wheat, oats and oilseed rape in Scotland. Data cover the period from 2003 to the latest 2023 harvest."),
                                     p("Data are available in the", tags$a(href = "https://www.gov.scot/collections/scottish-cereal-harvest-estimates", target = "_blank", "Scottish cereal harvest: estimates (link opens in a new window).")),
                                     p("Information about data sources is provided in the", tags$a(href = "https://www.gov.scot/publications/cereal-and-oilseed-rape-harvest-methodology/", target = "_blank", "methodology (link opens in a new window).")),
                                     br(),
                                     h2("How to plot the data"),
                                     p("The sidebar on the left displays options for you to choose from. An interactive chart will display the data for the options you have selected in the 'Plot' tab."),
                                     p("The 'Summary' tab displays a general overview of the latest harvest year, along with tables of the latest figures and percentage changes from the last year to the current."),
                                     p("The 'Plot' tab displays the interactive chart."),
                                     p("The 'Data Table' tab displays the table of data used in the interactive chart. There is an option to download a 'csv' file of the data, by clicking on the 'Download the data' link below the table.")
                            ),
                            tabPanel("Summary",
                                     h1("Key findings for the 2023 Scottish harvest", size = "20px"),
                                     p("Total cereal production from the 2023 harvest is just under 3.1 million tonnes. This is a 2% decrease compared to 2022 and just above the ten-year average of 3.0 million tonnes."),
                                     p("Weather conditions in 2023 led to one of the more challenging harvests in recent years. Overall cereal production decreased despite a small increase in planted area."),
                                     p("Production decreased for barley, wheat and oats compared to 2022. This was mostly as a result of decreased yield."),
                                     p("Oilseed rape production reached its highest value in at least 20 years at around 166 thousand tonnes. The 2023 area of oilseed rape is the largest recorded in at least 20 years, at around 41 thousand hectares. Combined with an above average yield this made for a very good year for oilseed rape production."),
                                     br(),
                                     tags$img(src = "vs.png", align = "center", width = "100%")
                            ),
                            tabPanel("Plot",
                                     h1(textOutput("title")),
                                     h2(align = "center", highchartOutput("line_chart")),
                                     h2("Note:"),
                                     p("To remove a crop from the chart, either deselect from the sidebar menu or click on its legend."),
                                     p("You can see data values for a specific year by hovering your mouse over the line.")
                            ),
                            tabPanel("Data Table",
                                     tableOutput("pay_table"),
                                     downloadLink("downloadData", "Download the data")
                            )
                )
      )
  )
)

# Set up server

server <- function(input, output, session) { 
  load("ch_data.RData") # Load the data
  
  # Load the theme
  thm <- source("hc_theme.R")$value
  
  # Unit for production, area or yield 
  pay_unit <- reactive({
    switch(input$pay,
           "Production" = " (Tonnes)",
           "Area" = " (Hectares)",
           "Yield" = "(Tonnes per hectare)"
    )
  })
  
  # User selections of crop(s), year(s) and one of production, area or yield
  crop_pay <- reactive({
    data <- filter(ch_data, PAY == input$pay & crop %in% input$crop)
    data <- data %>% filter(Year >= min(input$year) & Year <= max(input$year))
    if (input$pay == "Production") {
      data$Value <- data$Value * 0.001
    }
    data
  })
  
  # For the table displayed in the "Data Table" tab - pivot wider and add units to the headings
  pretty_data_table <- reactive({
    data <- crop_pay()
    data$PAY <- as.character(data$PAY)
    data$PAY <- paste(data$PAY, pay_unit(), sep = " ")
    pivot_wider(data, names_from = c(crop, PAY), values_from = Value, names_sep = " ")
  })
  
  # Event when user presses "Select All": ticking all boxes to select all crops. 
  observeEvent(input$selectAll, {
    updateCheckboxGroupInput(session, "crop", selected = crops)
  })
  
  # Event when user presses "Deselect All": unticking all boxes to deselect all crops. 
  observeEvent(input$deselectAll, {
    updateCheckboxGroupInput(session, "crop", selected = character(0))
  })  
  
  # Plot 
  output$line_chart <- renderHighchart({ 
    data <- crop_pay()
    hchart(data, "line", hcaes(x = Year, y = Value, group = crop), style = list(fontFamily = "Roboto")) %>%   
      hc_yAxis(
        labels = list(style = list(color =  "#000000", fontSize = "20px", fontFamily = "Roboto"), 
                      format = if (input$pay %in% c("Production", "Area")) "{value:,.0f}" else "{value:1.f}"),
        title = list(text = if (input$pay == "Production") "Thousand tonnes" else if (input$pay == "Area") "Hectares" else "Tonnes per hectare",
                     style = list(color = "#000000", fontSize = "20px", fontFamily = "Roboto"))
      ) %>% 
      hc_xAxis(
        labels = list(style = list(color =  "#000000", fontSize = "20px", fontFamily = "Roboto")),
        title = list(text = "", style = list(color = "#000000", fontSize = "20px", fontFamily = "Roboto")),
        type = "category"
      ) %>% 
      hc_legend(align = "left", alignColumns = FALSE, layout = "horizontal") %>% 
      hc_tooltip(pointFormat = "{point.y:,.1f}")  %>% 
      hc_add_theme(thm)
  })
  
  # Title of plot
  output$title <- renderText({
    title_text <- if (length(input$crop) == 6) "Total Cereals and Oilseed Rape" else ""
    paste(title_text, ifelse(input$pay == "Area", "Sown Area", input$pay), "in Scotland,", min(input$year), "to", max(input$year))
  })
  
  # Rendering the Data Table
  output$pay_table <- renderTable({
    data <- pretty_data_table()
    if (input$pay != "Yield") {
      data <- format.data.frame(data, digits = 5, nsmall = 0, big.mark = ",", big.interval = 3L)
    }
    as_tibble(data)
  }, bordered = TRUE, striped = TRUE)
  
  # Download the data link beneath data table in "Data Table" tab
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Cereal and oilseed rape harvest estimates for", input$pay, min(input$year), "to", max(input$year), ".csv")
    },
    content = function(file) { 
      write.csv(pretty_data_table(), file)
    }
  )
}
# Load the app
shinyApp(ui, server)
