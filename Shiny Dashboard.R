library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(tidyr)
library(dplyr)
library(maps)
library(bslib)
library(reshape2)
library(DataExplorer)
library(stringr)

# Read the data (Import to Global Environment)
life_expectancy_data <- read.csv("dataset.csv")

# Rename Dataset Columns
life_expectancy_data <- life_expectancy_data %>%
  rename(
    Country = Country.Name,
    Year = Year,
    LifeExpectancy = Life.Expectancy.World.Bank,
    Undernourishment = Prevelance.of.Undernourishment,
    HealthExpenditure = Health.Expenditure..,
    EducationExpenditure = Education.Expenditure..,
    CountryCode = Country.Code
  )

# Clean Dataset
life_expectancy_data <- life_expectancy_data %>%
  drop_na(LifeExpectancy, CO2) %>%
  mutate(across(
    c(Undernourishment, HealthExpenditure, EducationExpenditure, Unemployment, Sanitation),
    ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
  ))

# Exclude Corruption
life_expectancy_data <- subset(life_expectancy_data, select = -c(Corruption))

# Count data per region
count_region <- life_expectancy_data %>% 
  group_by(Region) %>% 
  summarize(count = n(), percentage = n() / nrow(life_expectancy_data))

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "AoL Data Mining"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dataset", tabName = "dataset", icon = icon("database")),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
        menuItem("Linear Regression Model", tabName = "continent", icon = icon("chart-line")),
        menuItem("Conclusion", tabName = "conclusion", icon = icon("check-circle"))
      )
    ),
    dashboardBody(
      tags$head(
        tags$style(HTML("
          .verbatim-box {
            white-space: pre-wrap;
            overflow-x: auto;
          }
        "))
      ),
      tabItems(
        tabItem(tabName = "dataset",
                tabBox(id = "t1", width = 12,
                       tabPanel("About", icon = icon("address-card"),
                                fluidRow(
                                  column(width = 7, h1("About Dataset"),
                                         tags$img(src = "life_expectancy.jpg", width = 500, height = 300),
                                         tags$br(),
                                         tags$a("Busy People on Streets"), align = "center"),
                                  column(width = 4, tags$br(),
                                         tags$p("Life expectancy at birth indicates the number of years a newborn infant would live if prevailing
patterns of mortality at the time of its birth were to stay the same throughout its life. It is a key
metric for assessing population health. In the table, we have shown how life expectancy varies between high-income and low-income countries.
However, further analysis is necessary to determine how some socioeconomic
factors have an overall effect in determining average life expectancy.")
                                  )
                                )
                       ),
                       tabPanel("Data", icon = icon("table"),
                                fluidPage(
                                  titlePanel("Life Expectancy Data by Region"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      selectInput("inRegion", "Select a region", 
                                                  choices = c("All", unique(life_expectancy_data$Region)))
                                    ),
                                    mainPanel(
                                      style = "height: 400px; overflow-x: scroll;",
                                      tableOutput("lifeExpectancyData")
                                    )
                                  )
                                )
                       ),
                       tabPanel("Summary Stats", icon = icon("chart-pie"),
                                div(class = "verbatim-box", verbatimTextOutput("summary"))
                       )
                )
        ),
        tabItem(tabName = "dashboard",
                h1("Dashboard"),
                fluidRow(
                  infoBox("Number of Columns", 16, icon = icon("line-chart")),
                  infoBox("Number of Rows", 3306, icon = icon("bar-chart"), color = "teal"),
                  infoBox("Number of Countries", 174, icon = icon("person"), color = "purple")
                ),
                fluidRow(
                  column(width = 11.9,
                         box(title = "World Average Life Expectancy Over the Years", 
                             status = "primary",
                             solidHeader = TRUE,
                             plotlyOutput("linechart", width = "100%"),
                             htmlOutput("linechartText")
                         )
                  ),
                  column(width = 11.9,
                         box(title = "Average Life Expectancy Based on Income Group", 
                             status = "primary",
                             solidHeader = TRUE,
                             plotlyOutput("barchart", width = "100%"),
                             htmlOutput("barchartText")
                         )
                  )
                ),
                fluidRow(
                  column(width = 12,
                         box(title = "Average Life Expectancy Distribution on World Map",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 12,
                             plotOutput("mapchart", width = "100%"),
                             htmlOutput("mapchartText")
                         )
                  )
                ),
                fluidRow(
                  box(title = "Correlation between Numerical Variables",
                      width = 12,
                      status = "primary",
                      solidHeader = TRUE,
                      plotlyOutput("corrtest", width = "100%"),
                      htmlOutput("corrtestText")
                  )
                )
        ),
        tabItem(tabName = "continent",
                h1("Linear Regression Model Life Expectancy"),
                fluidRow(
                  column(width = 4,
                         selectInput(inputId = "based_cont",
                                     label = "Choose your Desired Continent",
                                     choices = c("All", unique(life_expectancy_data$Region)))
                  ),
                  column(width = 4,
                         selectInput(inputId = "selected_factor",
                                     label = "Select Factor for Regression",
                                     choices = names(life_expectancy_data)[sapply(life_expectancy_data, is.numeric) & 
                                                                             names(life_expectancy_data) != "LifeExpectancy" & 
                                                                             names(life_expectancy_data) != "Year"])
                  ),
                  column(width = 4,
                         selectizeInput(inputId = "income_groups",
                                        label = "Select Income Groups",
                                        choices = unique(life_expectancy_data$IncomeGroup),
                                        selected = unique(life_expectancy_data$IncomeGroup),
                                        multiple = TRUE)
                  )
                ),
                fluidRow(
                  infoBoxOutput("regionInfoBox"),
                  infoBoxOutput("countCountry"),
                  infoBoxOutput("countRow")
                ),
                plotlyOutput("regressionPlot"),
                htmlOutput("regressionPlotText")
        ),
        tabItem(tabName = "conclusion",
                h1("Conclusion"),
                fluidRow(
                  column(width = 12,
                         box(title = "Summary of Findings",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 12,
                             htmlOutput("conclusionText")
                         )
                  )
                )
        )
      )
    )
  ),
  server = function(input, output) {
    
    # Calculate average life expectancy by year
    avg_life_expectancy <- aggregate(LifeExpectancy ~ Year, life_expectancy_data, mean)
    
    # Render line chart with Plotly
    output$linechart <- renderPlotly({
      plot_ly(
        data = avg_life_expectancy, 
        x = ~Year, 
        y = ~LifeExpectancy, 
        type = 'scatter', 
        mode = 'lines+markers', 
        line = list(color = 'darkblue'), 
        marker = list(color = 'darkblue')
      ) %>%
        layout(
          xaxis = list(title = "Year"), 
          yaxis = list(title = "Life Expectancy")
        )
    })
    
    # Calculate average life expectancy by region
    avg_life_expectancy2 <- aggregate(LifeExpectancy ~ IncomeGroup, life_expectancy_data, mean)
    
    # Render bar chart
    output$barchart <- renderPlotly({
      avg_life_expectancy2$IncomeGroup <- str_wrap(avg_life_expectancy2$IncomeGroup, width = 14)  
      
      plot_ly(data = avg_life_expectancy2, y = ~IncomeGroup, x = ~LifeExpectancy, type = 'bar', orientation = 'h', marker = list(color = 'darkblue')) %>%
        layout(
          yaxis = list(title = ""),  
          xaxis = list(title = "Life Expectancy")
        )
    })
    
    
    
    # Render map chart
    world_tbl <- map_data("world") %>% 
      as_tibble()
    
    mapdata <- left_join(world_tbl, life_expectancy_data, by = c("region" = "Country"))
    
    output$mapchart <- renderPlot({
      ggplot(mapdata, aes(x = long, y = lat, group = group, fill = LifeExpectancy)) +
        geom_polygon(color = "black") +
        scale_fill_gradientn(
          name = "Life Expectancy",
          colors = c("lightblue", "darkblue"),
          na.value = "lightgray",
          breaks = c(min(mapdata$LifeExpectancy, na.rm = TRUE), max(mapdata$LifeExpectancy, na.rm = TRUE)),  
          labels = c("Low", "High")
        ) +
        theme(
          axis.line = element_blank(), 
          axis.text = element_blank(),
          axis.title = element_blank(), 
          axis.ticks = element_blank(), 
          plot.title = element_blank(),  
          panel.background = element_rect(fill = "white"),
          legend.position = "right"  
        )
    })
    
    
    # Render correlation heatmap
    corr_matrix <- round(cor(life_expectancy_data %>% 
                               select(LifeExpectancy, HealthExpenditure, EducationExpenditure, CO2, Sanitation, Injuries,Communicable,NonCommunicable, Undernourishment)
                             , use = "complete.obs"), 2)
    corr_data <- reshape2::melt(corr_matrix)
    
    variable_names <- c(
      LifeExpectancy = "Life Expectancy",
      HealthExpenditure = "Health Expenditure",
      NonCommunicable = "Non Communicable",
      Injuries = "Injuries",
      Communicable = "Communicable",
      CO2 = "CO2", 
      Sanitation = "Sanitation", 
      EducationExpenditure = "Education Expenditure",
      Undernourishment = "Undernourishment"
    )
    
    original_names <- names(variable_names)
    display_names <- as.character(variable_names)
    wrapped_display_names <- str_wrap(display_names, width = 10)
    
    
    output$corrtest <- renderPlotly({
      plot_ly(
        data = corr_data,
        x = ~Var1,
        y = ~Var2,
        z = ~value,
        type = 'heatmap',
        colorscale = list(c(0, "lightblue"), c(0.5, "white"), c(1, "darkblue")),
        text = ~round(value, 2),
        texttemplate = '%{text}',  
        hovertemplate = "Variable 1: %{x}<br>Variable 2: %{y}<br>Correlation: %{z}<extra></extra>",
        showscale = TRUE
      ) %>%
        layout(
          xaxis = list(
            title = "",
            tickvals = original_names,  
            ticktext = wrapped_display_names, 
            tickangle = 0  
          ),
          yaxis = list(
            title = "",
            tickvals = original_names,
            ticktext = wrapped_display_names  
          )
        )
    })
    
    # Render life expectancy data
    output$lifeExpectancyData <- renderTable({
      if (input$inRegion == "All") {
        life_expectancy_data
      } else {
        subset(life_expectancy_data, Region == input$inRegion)
      }
    })
    
    # Render life expectancy summary
    output$summary <- renderPrint({
      life_expectancy_data %>% 
        summary()
    })
    
    # InfoBox average life expectancy for the selected region
    output$regionInfoBox <- renderInfoBox({
      selected_region <- input$based_cont
      if (selected_region == "All") {
        avg_life_exp <- mean(life_expectancy_data$LifeExpectancy, na.rm = TRUE)
      } else {
        avg_life_exp <- mean(subset(life_expectancy_data, Region == selected_region)$LifeExpectancy, na.rm = TRUE)
      }
      
      
      infoBox(
        "Average Life Expectancy",
        round(avg_life_exp, 2),
        icon = icon("heartbeat"),
        color = "light-blue"
      )
    })
    
    # InfoBox for the total number of countries per region
    output$countCountry <- renderInfoBox({
      selected_region <- input$based_cont
      if (selected_region == "All") {
        count_countries <- n_distinct(life_expectancy_data$Country, na.rm = TRUE)
      } else {
        count_countries <- n_distinct(subset(life_expectancy_data, Region == selected_region)$Country, na.rm = TRUE)
      }
      
      infoBox(
        "Total Countries",
        count_countries,
        icon = icon("globe"),
        color = "teal"
      )
    })
    
    # InfoBox for the total number of data entries per region
    output$countRow <- renderInfoBox({
      selected_region <- input$based_cont
      if (selected_region == "All") {
        count_data <- nrow(life_expectancy_data)
      } else {
        count_data <- nrow(subset(life_expectancy_data, Region == selected_region))
      }
      
      infoBox(
        "Total Data Entries",
        count_data,
        icon = icon("database"),
        color = "purple"
      )
    })
    
    # Render linear regression plot with Plotly
    output$regressionPlot <- renderPlotly({
      selected_region <- input$based_cont
      selected_factor <- input$selected_factor
      selected_income_groups <- input$income_groups
      
      # Define custom colors for income groups
      income_colors <- c("Low income" = "skyblue", "Lower middle income" = "royalblue", "Upper middle income" = "#719CA3", "High income" = "#574CAA")
      
      if (selected_region == "All") {
        filtered_data <- life_expectancy_data
      } else {
        filtered_data <- subset(life_expectancy_data, Region == selected_region)
      }
      
      filtered_data <- filtered_data %>%
        filter(IncomeGroup %in% selected_income_groups)
      
      if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
        formula <- as.formula(paste("LifeExpectancy ~", selected_factor))
        lm_model <- lm(formula, data = filtered_data)
        
        plot_ly(filtered_data, 
                x = ~get(selected_factor), 
                y = ~LifeExpectancy, 
                type = 'scatter', 
                mode = 'markers', 
                color = ~IncomeGroup, 
                colors = income_colors,
                text = ~IncomeGroup, 
                showlegend = TRUE) %>%
          add_lines(x = ~get(selected_factor), y = ~fitted(lm_model), line = list(color = 'darkblue'), showlegend = FALSE) %>%
          layout(
            title = list(
              text = paste("Linear Regression of Life Expectancy vs", selected_factor),
              y = 0.95  
            ),
            margin = list(t = 50),  
            xaxis = list(title = selected_factor),
            yaxis = list(title = "Life Expectancy"),
            showlegend = TRUE  
          ) %>%
          style(
            hovertemplate = paste(
              "<b>", selected_factor, ":</b> %{x}<br>",
              "<b>Life Expectancy:</b> %{y}<br>",
              "<extra></extra>"
            )
          )
      } else {
        plot_ly() %>%
          add_annotations(text = "No data available for this region and income group", x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 20)) %>%
          layout(
            title = "Linear Regression Plot",
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            margin = list(t = 40),  
            showlegend = FALSE  
          )
      }
    })
    
    #Render text for chart description
    output$linechartText <- renderUI({
      HTML("<p style='margin-top: 10px;'>The average global <strong>life expectancy consistently increased</strong> from 2001 to 2019.</p>")
    })
    
    output$barchartText <- renderUI({
      HTML("<p style='margin-top: 10px;'>
It is evident that as <strong>income levels increase</strong>, <strong>life expectancy also rises</strong>.</p>")
    })
    
    output$mapchartText <- renderUI({
      HTML("<p style='margin-top: 10px;'>Based on world map, America, Europe, and Australia have <strong>higher life expectancy</strong> compared to Asia and Africa.</p>")
    })
    
    output$corrtestText <- renderUI({
      HTML("<p style='margin-top: 10px;'>Based on plot above, life expectancy is most likely have <strong>correlation with health factors</strong>: health expenditure, sanitation, and undernourishment.</p>")
    })
    
    output$regressionPlotText <- renderUI({
      HTML("<p style='margin-top: 10px;'>This plot depicts the <strong>linear regression model</strong> for life expectancy based on selected factors and income groups.</p>")
    })
    
    #Render text for conclusion page
    output$conclusionText <- renderUI({
      HTML("<p>Based on data given, our analysis has revealed several critical insights into the factors affecting life expectancy globally:</p>
          <ul>
            <li>Global life expectancy has steadily increased from 2001 to 2019, highlighting <strong>improvements in healthcare, nutrition, and living conditions.</strong></li>
            <li>Regions with higher income levels generally exhibit higher average life expectancy, underscoring the <strong>importance of economic stability</strong> in health outcomes.</li>
            <li><strong>Significant correlations</strong> exist between life expectancy and various socioeconomic factors, such as education, income, and healthcare access.</li>
          </ul>
          <p>These findings emphasize the need for <strong>continued investment in healthcare infrastructure, education, and economic development</strong> to further <strong>enhance life expectancy</strong> globally.</p>")
    })
  }
)

