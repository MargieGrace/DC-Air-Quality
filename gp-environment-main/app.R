library(shiny)
library(tidyverse)
library(magrittr)
library(bslib)
library(DT)
library(scales)
library(thematic)
library(broom)

# BUSINESS LOGIC
## cleaning/tidying the data
AirQuality_data <- readRDS("data-raw/Air_Quality_History.rds")
AirQuality_data <- AirQuality_data %>%
  mutate(across(starts_with("PARAMETER_NAME"), 
                ~ str_replace_all(., " ", "_") %>%
                  str_to_lower() %>%
                  str_remove_all("[^a-z_]"))) %>%
  mutate(SITE_NAME = case_match(SITE_NUM, 
                                41 ~ "River_Terrace_NE", 
                                43 ~ "McMillan_NW", 
                                50 ~ "Takoma_Recreation_NW",
                                51 ~ "Anacostia_Freeway_NE", 
                                53 ~ "Greenleaf_Recreation_SW", 
                                42 ~ "Hains_Point_SW")) %>%
  select(-c(LONGITUDE, LATITUDE, UNCERTAINTY, STATE_CODE, STATE_NAME, COUNTY_NAME, POC, DATUM, QUALIFIER, OBJECTID, UNITS_OF_MEASURE)) %>%
  mutate(DATETIME_LOCAL = as.POSIXct(DATETIME_LOCAL, tz = "UTC"),
         Year = year(DATETIME_LOCAL),
         Month = month(DATETIME_LOCAL, label = TRUE, abbr = TRUE) %>% as.character()) %>%
  mutate(Season = case_when(
    Month %in% c("Dec", "Jan", "Feb") ~ 'Winter',
    Month %in% c("Mar", "Apr", "May") ~ 'Spring',
    Month %in% c("Jun", "Jul", "Aug") ~ 'Summer',
    TRUE ~ 'Fall'
  ))

# Grouping and summarizing
AirQuality_data <- AirQuality_data %>%
  group_by(Season, SITE_NAME, Month, Year, PARAMETER_NAME) %>%
  summarize(SAMPLE_MEASUREMENT = mean(SAMPLE_MEASUREMENT, na.rm = TRUE), .groups = 'drop')

# Pivot the data
AirQuality_data <- AirQuality_data %>%
  pivot_wider(names_from = PARAMETER_NAME, values_from = SAMPLE_MEASUREMENT)
AirQuality_original_data <- AirQuality_data

# One-hot encoding
AirQuality_data <- fastDummies::dummy_cols(AirQuality_data, select_columns = c("SITE_NAME", "Season"), remove_selected_columns = TRUE)

Predictor_Vars <- c("barometric_pressure", "carbon_monoxide", "nitrogen_dioxide_no", "outdoor_temperature",
                    "pm__local_conditions", "relative_humidity", "wind_direction__resultant", "wind_speed__resultant",
                    "black_carbon_pm_at__nm", "ozone", "pm_total_um_stp", "sulfur_dioxide",
                    "uv_carbon_pm_at__nm", "SITE_NAME", 'Season')
num_pred_vars <- c("barometric_pressure", "carbon_monoxide", "nitrogen_dioxide_no", "outdoor_temperature",
                   "pm__local_conditions", "relative_humidity", "wind_direction__resultant", "wind_speed__resultant",
                   "black_carbon_pm_at__nm", "ozone", "pm_total_um_stp", "sulfur_dioxide",
                   "uv_carbon_pm_at__nm")

Response_Vars <- c("barometric_pressure", "carbon_monoxide", "nitrogen_dioxide_no", "outdoor_temperature",
                   "pm__local_conditions", "relative_humidity", "wind_direction__resultant", "wind_speed__resultant",
                   "black_carbon_pm_at__nm", "ozone", "pm_total_um_stp", "sulfur_dioxide",
                   "uv_carbon_pm_at__nm")
# functions
t_test <- function(dv, null_val) {
  t_test_result <- t.test(dv, mu = null_val)
  return(tidy(t_test_result))
}
linear <- function(var1, var2, dat) {
  lm_out <- lm(var1 ~ var2, data = dat)
  return(tidy(summary(lm_out)))
}

thematic_shiny(font = "auto")

# UI
ui <- fluidPage(
  
  # set theme
  theme = bslib::bs_theme(version = 5, bootswatch = "solar"),
  
  # app title
  titlePanel("Air Quality in DC"),
  tags$p("Context of the problem: Similarly to most major American 
            metropolitan cities, the problem of environmental pollution in the 
            District of Columbia is a serious environmental issue. As a busy 
            city, DC has many different sources of pollution, including cars, 
            factories, and power plants, all of which particularly affect air 
            quality which can be seriously harmful to citizens of the district. 
            For DC in particular, the large number of workers commuting into 
            the city in cars, causes increases in traffic and thus air 
            pollution. All these factors together mean that the pollution from 
            emissions affect the air in and around the city. Understanding the 
            source and quantity of pollution from different factors can help to 
            identify ways in which the air in DC and environment can be 
            improved. "),
  tabsetPanel(
    # tab for linear regression
    tabPanel("Linear Regression Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput('x_var', 'Select X Variable:', choices = Response_Vars),
                 selectInput('y_var', 'Select Y Variable:', choices = Response_Vars),
                 checkboxInput('sum', 'Show Summary Stats?', value = FALSE),
                 actionButton('reg_button', 'Run Regression')
               ), 
               mainPanel(
                 plotOutput('lm'),
                 conditionalPanel(
                   condition = 'input.sum == TRUE',
                   verbatimTextOutput('sum_out')
                 )
               )
             ) 
    ), # end of lin reg
    # Tab for Multiple Regression Analysis
    tabPanel("Multiple Regression Analysis", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("selectedYear", "Select Year:", 
                             choices = c('All', unique(AirQuality_data$Year)), selected = 'All', multiple = TRUE),
                 checkboxGroupInput("selectedMonth", "Select Month:", 
                                    choices = c('All', unique(AirQuality_data$Month)), selected = "All"),
                 selectInput("responseVar", "Choose the response variable:", 
                             choices = Response_Vars, selected = 'sulfur_dioxide'),  
                 selectInput("predictorVars", "Choose predictor variables:", 
                             choices = Predictor_vars,
                             selected = "barometric_pressure", 
                             multiple = TRUE),
                 checkboxInput("logTransform", "Apply log10 transformation to the response variable", value = FALSE)
                 
               ),
               mainPanel(
                 verbatimTextOutput("regOutput"),
                 verbatimTextOutput("multregOutput"),
                 HTML('<div style="padding: 10px; border: 1px solid #ddd; margin-bottom: 15px;">
                        <p><strong>Note*:</strong> The predictor variables SITE_NAME and Season are categorical and encoded.
                        When interpreting the model be sure to refer to the prediction variables in context of the
                        omitted reference variable.</p>
                    </div>')
               )
             ),
             fluidRow(
               column(12,
                      h3("Make Predictions"),
                      # Dynamic UI for predictor inputs
                      uiOutput("predictionInputs"),
                      numericInput("confInterval", "Confidence Interval (%)", value = 95, min = 50, max = 99.99),
                      actionButton("predictButton", "Predict"),
                      verbatimTextOutput("predictionOutput")
               )
             )
             
    ), #end multiple regression
    # start of hypothesis testing
    tabPanel("Hypothesis Testing",
             sidebarLayout(
               sidebarPanel(
                 selectInput("oneSampleVar", "Select Variable for One-Sample T-Test:", 
                             choices = num_pred_vars, selected = NULL),
                 numericInput("oneSampleNull", "Null Hypothesis Mean:", value = 0),
                 actionButton("oneSampleTestButton", "Run One-Sample T-Test"),
                 # Inputs for two-sample t-test
                 hr(), # Horizontal line for visual separation
                 h4("Two-sample t-test"),
                 selectInput("testVariable1", "Select the First Variable:",
                             num_pred_vars, selected = NULL),
                 selectInput("testVariable2", "Select the Second Variable:",
                             num_pred_vars, selected = NULL),
                 actionButton("testButtonTwoSample", "Run Two-Sample Test")
               ),
               mainPanel(
                 verbatimTextOutput("oneSampleTestOutput"),
                 # Output for two-sample t-test
                 verbatimTextOutput("testOutputTwoSample")
               )
             )
             
    ), # end hypothesis testing
    # start plotting
    tabPanel("Plotting",
             sidebarLayout(
               sidebarPanel(
                 # Dropdown to select the type of plot
                 selectInput("plotType", "Select Plot Type:",
                             choices = c("Histogram", "Density Plot", "Box Plot", "Scatter Plot")),
                 # Dropdown to select variables for x and y axes
                 selectInput("xVar", "Select X Variable:",
                             choices = names(AirQuality_data)),
                 # Conditional panel to select Y variable only for Scatter Plot
                 conditionalPanel(
                   condition = "input.plotType == 'Scatter Plot'",
                   selectInput("yVar", "Select Y Variable for Boxplot and Scatter Plot:",
                               choices = names(AirQuality_data))
                 ),
                 # Button to generate plot
                 actionButton("plotButton", "Generate Plot")
               ),
               mainPanel(
                 # Output for plot
                 plotOutput("plotOutput")
               )
             )
    ) # end plotting
  )
)
# ui ends
# SERVER LOGIC
server <- function(input, output) {
  
  filteredData <- reactive({
    data <- AirQuality_data
    
    # Filter by selected years if 'All' is not chosen
    if (!'All' %in% input$selectedYear) {
      data <- data |>
        filter(Year %in% input$selectedYear)
    }
    
    # Filter by selected months if 'All' is not chosen
    if (!'All' %in% input$selectedMonth) {
      data <- data |>
        filter(Month %in% input$selectedMonth)
    }
    
    data
  })
  
  ## linear regression tab 
  observeEvent(input$reg_button, {
    output$lm <- renderPlot({
      reg_formula <- as.formula(paste(input$x_var, "~", input$y_var))
      lm_out <- lm(reg_formula, data = AirQuality_original_data)
      ggplot(AirQuality_original_data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point() +
        geom_smooth(method = 'lm', se = FALSE, color = 'red') +
        labs(x = input$x_var, y = input$y_var, title = 'Simple Linear Regression')
    })
    # summary stats
    output$sum_out <- renderPrint({
      if (input$sum) {
        tryCatch({
          cat("\nFive-number summary for the Y variable (", input$x_var, "):\n")
          print(summary(AirQuality_original_data[[input$x_var]]))
          
          cat("\nFive-number summary for the Y variable (", input$y_var, "):\n")
          print(summary(AirQuality_original_data[[input$y_var]]))
          
          cat("\nLinear Regression Model Summary:\n")
          lmr_out <- lm(as.formula(paste(input$x_var, '~', input$y_var)), data = AirQuality_original_data)
          print(summary(lmr_out))
        }, error = function(e) {
          paste('Error in modeling fit or summary statistics: ', e$message)
        })
      }
    })
  })
  
  ## Mult Regression tab
  output$regOutput <- renderPrint({
    validate(
      need(input$selectedYear, 'Choose a year'),
      need(input$selectedMonth, 'Choose a month'),
      need(input$responseVar, "Choose a response variable"),
      need(input$predictorVars, 'Choose at least one predictor variable')
    )
    
    data <- filteredData()

    response_var <- input$responseVar
    if (input$logTransform) {
      data[[response_var]] <- log10(data[[response_var]] + 1)  # log10 transformation
    }
    
    # Function to include one-hot encoded variables for SITE_NAME or Season
    include_one_hot_vars <- function(predictors, data) {
      one_hot_vars <- predictors
      for (pred in predictors) {
        if (pred %in% c("SITE_NAME", "Season")) {
          one_hot_cols <- names(data)[str_detect(names(data), paste0("^", pred, "_")) & !str_detect(names(data), "_reference$")]
          one_hot_vars <- c(one_hot_vars, one_hot_cols)
        }
      }
      one_hot_vars <- unique(one_hot_vars)
      one_hot_vars[!one_hot_vars %in% c("SITE_NAME", "Season")]
    }
    
    predictors <- include_one_hot_vars(input$predictorVars, data)
    reg_formula <- as.formula(paste(input$responseVar, "~", paste(predictors, collapse = " + ")))
    
    # Fit and summarize the model
    model <- lm(reg_formula, data = data)
    summary(model)
  })

  # Render UI elements for predictor inputs based on selected predictor variables
  output$predictionInputs <- renderUI({
    # Create input fields for each selected predictor variable
    input_fields <- lapply(input$predictorVars, function(var) {
      if (var %in% c("SITE_NAME", "Season")) {
        # If the predictor is categorical, remove from prediction
        selectInput(paste0("input_", var), var, choices = unique(AirQuality_original_data[[var]]))
      } else {
        # For numerical predictors, use a numeric input
        numericInput(paste0("input_", var), var, value = 0)
      }
    })
    do.call(fluidRow, input_fields)
  })
  
  # Prediction logic
  observeEvent(input$predictButton, {
  
    data <- filteredData()
    
    # Prepare new data for prediction
    
    prediction_data <- data.frame(matrix(ncol = length(input$predictorVars), nrow = 1))
    colnames(prediction_data) <- input$predictorVars
    for (var in input$predictorVars) {
      prediction_data[[var]] <- input[[paste0("input_", var)]] 
    }
    
    # Fit the model
    model <- lm(as.formula(paste(input$responseVar, "~", paste(input$predictorVars, collapse = " + "))), data = data)
    
    # Convert confidence interval from percentage to proportion
    conf_level <- input$confInterval / 100
    
    # Make prediction with confidence interval
    prediction <- predict(model, newdata = prediction_data, interval = "confidence", level = conf_level)
    
    # Output the prediction and confidence interval
    output$predictionOutput <- renderPrint({
      prediction
    })
  }) # end mult regression tab
  
  
  # begin hypothesis testing tab
  # one sample t test
  observeEvent(input$oneSampleTestButton, {
    output$oneSampleTestOutput <- renderPrint({
      data <- AirQuality_data[[input$oneSampleVar]]
      null_value <- input$oneSampleNull
      # Perform the one-sample t-test
      if (!is.null(data) && !is.na(null_value)) {
        t_test_result <- t.test(data, mu = null_value)
        print(t_test_result)
      } else {
        "Please select a valid variable and null hypothesis value."
      }
    })
  })
  
  # two sample t test
  observeEvent(input$testButtonTwoSample, {
    data <- filteredData()
    # Ensure that the user has selected two different variables
    if (input$testVariable1 != input$testVariable2) {
      test_result <- t.test(data[[input$testVariable1]], data[[input$testVariable2]])
      output$testOutputTwoSample <- renderPrint({
        test_result
      })
    } else {
      output$testOutputTwoSample <- renderPrint({
        "Please select two different variables for the two-sample t-test."
      })
    }
  })
  
  ## end hypothesis testing ##
  
  
  ## begin plotting ##
  observeEvent(input$plotButton, {
    output$plotOutput <- renderPlot({
      data <- filteredData()
      plot <- NULL
      
      # Define a color palette
      my_palette <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
      
      if (input$plotType == "Histogram") {
        # Check if the variable is discrete or continuous
        if (is.numeric(data[[input$xVar]]) || is.integer(data[[input$xVar]])) {
          # For continuous data, use geom_histogram()
          plot <- ggplot(data, aes_string(x = input$xVar, fill = input$xVar)) +
            geom_histogram(bins = 30, color = "black", fill = my_palette[1]) +
            theme_minimal() +
            labs(x = input$xVar, y = "Frequency", title = "Histogram of " %>% paste(input$xVar)) +
            theme(
              text = element_text(size = 14, color = "white"),
              axis.title = element_text(size = 12, color = "white"),
              axis.text = element_text(size = 10, color = "white"),
              plot.title = element_text(size = 16, color = "white", hjust = 0.5)
            )
        } else {
          # For discrete data, use geom_bar()
          plot <- ggplot(data, aes_string(x = input$xVar, fill = input$xVar)) +
            geom_bar(color = "black", fill = my_palette[2]) +
            theme_minimal() +
            labs(x = input$xVar, y = "Count", title = "Bar Plot of " %>% paste(input$xVar)) +
            theme(
              text = element_text(size = 14, color = "white"),
              axis.title = element_text(size = 12, color = "white"),
              axis.text = element_text(size = 10, color = "white"),
              plot.title = element_text(size = 16, color = "white", hjust = 0.5)
            ) 
        }
      } else if (input$plotType == "Density Plot") {
        plot <- ggplot(data, aes_string(x = input$xVar, fill = input$xVar)) +
          geom_density(alpha = 0.75, fill = my_palette[3]) +
          theme_minimal() +
          theme(
            text = element_text(size = 14, color = "white"),
            axis.title = element_text(size = 12, color = "white"),
            axis.text = element_text(size = 10, color = "white"),
            plot.title = element_text(size = 16, color = "white", hjust = 0.5)
          )+
          labs(x = input$xVar, y = "Density", title = "Density Plot of " %>% paste(input$xVar))
      } else if (input$plotType == "Box Plot") {
        plot <- ggplot(data, aes_string(x = input$xVar, y = input$yVar, fill = input$xVar)) +
          geom_boxplot() +
          theme_minimal() +
          theme(
            text = element_text(size = 14, color = "white"),
            axis.title = element_text(size = 12, color = "white"),
            axis.text = element_text(size = 10, color = "white"),
            plot.title = element_text(size = 16, color = "white", hjust = 0.5)
          )+
          labs(x = input$xVar, y = input$yVar, title = "Box Plot of " %>% paste(input$yVar, "by", input$xVar)) +
          scale_fill_manual(values = my_palette)
      } else if (input$plotType == "Scatter Plot") {
        plot <- ggplot(data, aes_string(x = input$xVar, y = input$yVar)) +
          geom_point(color = my_palette[4], alpha = 0.6) +
          theme_minimal() +
          theme(
            text = element_text(size = 14, color = "white"),
            axis.title = element_text(size = 12, color = "white"),
            axis.text = element_text(size = 10, color = "white"),
            plot.title = element_text(size = 16, color = "white", hjust = 0.5)
          )+
          labs(x = input$xVar, y = input$yVar, title = "Scatter Plot of " %>% paste(input$yVar, "vs", input$xVar))
      }
      
      plot
    }) 
    ## end ploting ##
  })
  
  
}# server logic ends

# Run the application 
shinyApp(ui = ui, server = server)
