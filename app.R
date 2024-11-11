
library(shiny)
library(ggplot2)
library(reticulate)

use_python("/home/shiny/new_env/bin/python", required = TRUE)


# Load your Python-based XGBoost model
joblib <- import("joblib")
model <- joblib$load("real_estate_model.pkl")

ui <- fluidPage(
  titlePanel("🏠 Housing Price Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      # Conditional Side Panel for Model Results
      conditionalPanel(
        condition = "input.tabs == 'Model Results'",
        tags$div(
          style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
          tags$h3("Select Variable to Remove", style = "color: #2c3e50;"),
          radioButtons(
            "remove_variable",
            "Variable:",
            choices = c(
              "None" = "None",
              "Number of Bedrooms" = "resoFacts/bedrooms",
              "Number of Bathrooms" = "resoFacts/bathrooms",
              "Parking Capacity" = "resoFacts/parkingCapacity",
              "Year Built" = "resoFacts/yearBuilt",
              "Living Area (sq ft)" = "livingArea",
              "Distance to Parks (km)" = "distance_to_parksKM",
              "Distance to City (km)" = "Distance_to_city",
              "Distance to Mall (km)" = "Distance_to_mall",
              "Walk Score" = "Walk_score",
              "Number of Photos" = "photoCount",
              "Has View" = "resoFacts/hasView",
              "Has Spa" = "resoFacts/hasSpa"
            ),
            selected = "None"
          )
        )
      ),
      
      # Conditional Side Panel for Variable Effects
      conditionalPanel(
        condition = "input.tabs == 'Variable Effects'",
        tags$div(
          style = "background-color: #f1f1f1; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
          tags$h3("Explore Variable Impact", style = "color: #2c3e50;"),
          selectInput(
            "variable",
            "Variable:",
            choices = c(
              "Number of Bedrooms" = "resoFacts/bedrooms",
              "Number of Bathrooms" = "resoFacts/bathrooms",
              "Parking Capacity" = "resoFacts/parkingCapacity",
              "Year Built" = "resoFacts/yearBuilt",
              "Living Area (sq ft)" = "livingArea",
              "Distance to Parks (km)" = "distance_to_parksKM",
              "Distance to City (km)" = "Distance_to_city",
              "Distance to Mall (km)" = "Distance_to_mall",
              "Walk Score" = "Walk_score",
              "Number of Photos" = "photoCount"
            ),
            selected = "livingArea"
          ),
          numericInput("variable_value", "Enter Value for Selected Variable:", value = 0)
        )
      ),
      
      # Conditional Side Panel for Price Prediction
      conditionalPanel(
        condition = "input.tabs == 'Price Prediction'",
        tags$div(
          style = "background-color: #e9ecef; padding: 15px; border-radius: 5px;",
          tags$h3("Enter Property Details", style = "color: #2c3e50;"),
          textInput("bedrooms", "Number of Bedrooms:", ""),
          textInput("bathrooms", "Number of Bathrooms:", ""),
          textInput("parkingCapacity", "Parking Capacity:", ""),
          textInput("yearBuilt", "Year Built:", ""),
          textInput("livingArea", "Living Area (sq ft):", ""),
          textInput("distance_to_parksKM", "Distance to Parks (km):", ""),
          textInput("Distance_to_city", "Distance to City (km):", ""),
          textInput("Distance_to_mall", "Distance to Mall (km):", ""),
          textInput("Walk_score", "Walk Score:", ""),
          textInput("photoCount", "Number of Photos:", ""),
          textInput("hasView", "Has View (1 = Yes, 0 = No):", ""),
          textInput("hasSpa", "Has Spa (1 = Yes, 0 = No):", ""),
          actionButton("predict_price", "Predict Price", class = "btn btn-success btn-lg", icon = icon("calculator"))
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Model Results", 
                 tags$div(style = "padding: 10px;",
                          h3("Model Metrics"),
                          textOutput("model_metrics"),
                          br(),
                          h3("Actual vs Predicted Plot"),
                          imageOutput("model_plot"),
                          br(),
                          h3("Variables"),
                          tableOutput("model_results")
                 )),
        
        tabPanel("RMSE Impact by Feature",
                 tags$div(style = "padding: 10px;",
                          h3("Change in Average Predicted Home Pricing Error (RMSE) When Removing Each Feature"),
                          plotOutput("rmse_difference_plot")
                 )),
        
        tabPanel("Variable Effects", 
                 tags$div(style = "padding: 10px;",
                          plotOutput("variable_plot"),
                          textOutput("predicted_from_variable")
                 )),
        tabPanel("Price Prediction", 
                 tags$div(style = "padding: 10px;",
                          h3("Predicted Home Price:", style = "color: #2c3e50;"),
                          textOutput("predicted_price")
                 ))
      )
    )
  )
)

server <- function(input, output, session) {
  # Precomputed metrics and plots for each variable
  variable_metrics <- list(
    "None" = list(rmse = 176071.02, r2 = 0.715, plot = "overall_plot.png"),
    "resoFacts/bathrooms" = list(rmse = 175118.08, r2 = 0.718, plot = "bathrooms_plot.png"),
    "resoFacts/bedrooms" = list(rmse = 177102.94, r2 = 0.712, plot = "bedrooms_plot.png"),
    "resoFacts/parkingCapacity" = list(rmse = 179830.27, r2 = 0.703, plot = "parkingCapacity_plot.png"),
    "resoFacts/yearBuilt" = list(rmse = 189194.55, r2 = 0.671, plot = "yearBuilt_plot.png"),
    "livingArea" = list(rmse = 188285.57, r2 = 0.675, plot = "livingArea_plot.png"),
    "distance_to_parksKM" = list(rmse = 173105.39, r2 = 0.725, plot = "distance_to_parksKM_plot.png"),
    "Distance_to_city" = list(rmse = 175225.56, r2 = 0.718, plot = "Distance_to_city_plot.png"),
    "Distance_to_mall" = list(rmse = 174829.03, r2 = 0.719, plot = "Distance_to_mall_plot.png"),
    "Walk_score" = list(rmse = 241500.88, r2 = 0.465, plot = "Walk_score_plot.png"),
    "photoCount" = list(rmse = 186795.784, r2 = 0.680, plot = "photoCount_plot.png"),
    "resoFacts/hasView" = list(rmse = 178859.92, r2 = 0.706, plot = "hasView_plot.png"),
    "resoFacts/hasSpa" = list(rmse = 178547.76, r2 = 0.707, plot = "hasSpa_plot.png")
  )
  
  # Create RMSE difference data frame without dplyr
  rmse_diff_data <- reactive({
    baseline_rmse <- variable_metrics[["None"]]$rmse
    rmse_diffs <- sapply(variable_metrics[-1], function(x) x$rmse - baseline_rmse)
    rmse_diff_df <- data.frame(
      Variable = names(rmse_diffs),
      RMSE_Difference = rmse_diffs
    )
    rmse_diff_df <- rmse_diff_df[order(-rmse_diff_df$RMSE_Difference), ] # Sort in descending order
    rmse_diff_df
  })
  
  # Render the RMSE Difference plot
  output$rmse_difference_plot <- renderPlot({
    ggplot(rmse_diff_data(), aes(x = reorder(Variable, -RMSE_Difference), y = RMSE_Difference)) +
      geom_bar(stat = "identity") +
      labs(title = "RMSE Change by Feature Removal",
           x = "Variable Removed",
           y = "$ RMSE Difference") +
      coord_flip() # Flip to make the largest changes appear at the top
  })
  # Display metrics dynamically based on selected variable
  output$model_metrics <- renderText({
    selected_variable <- input$remove_variable
    metrics <- variable_metrics[[selected_variable]]
    
    if (!is.null(metrics)) {
      paste(
        "RMSE: ", round(metrics$rmse, 2), "\n",
        "R²: ", round(metrics$r2, 2)
      )
    } else {
      "No metrics available for the selected variable."
    }
  })
  
  # Display plot dynamically based on selected variable
  output$model_plot <- renderImage({
    selected_variable <- input$remove_variable
    plot_path <- variable_metrics[[selected_variable]]$plot
    
    if (file.exists(plot_path)) {
      list(
        src = plot_path,
        contentType = "image/png",
        width = 400,
        height = 400
      )
    } else {
      stop("Error: Plot image not found at the specified path.")
    }
  }, deleteFile = FALSE)
  
  reactive_vars <- reactiveValues(variables = setdiff(names(variable_metrics), "None"))
  
  observeEvent(input$update_model, {
    if (!is.null(input$remove_variable) && input$remove_variable != "None") {
      reactive_vars$variables <- setdiff(reactive_vars$variables, input$remove_variable)
    }
  })
  
  output$model_results <- renderTable({
    data.frame(
      Metric = c("Variables Remaining", "Number of Variables"),
      Value = c(
        paste(reactive_vars$variables, collapse = ", "),
        length(reactive_vars$variables)
      )
    )
  })
  
  output$variable_plot <- renderPlot({
    selected_var <- input$variable
    variable_values <- switch(selected_var,
                              "resoFacts/bedrooms" = seq(1, 10, length.out = 50),
                              "resoFacts/bathrooms" = seq(1, 6, length.out = 50),
                              "resoFacts/parkingCapacity" = seq(0, 5, length.out = 50),
                              "resoFacts/yearBuilt" = seq(1900, 2020, length.out = 50),
                              "livingArea" = seq(500, 5000, length.out = 50),
                              "distance_to_parksKM" = seq(0, 10, length.out = 50),
                              "Distance_to_city" = seq(0, 10, length.out = 50),
                              "Distance_to_mall" = seq(0, 20, length.out = 50),
                              "Walk_score" = seq(0, 100, length.out = 50),
                              "photoCount" = seq(0, 10, length.out = 50),
                              "resoFacts/hasView" = c(0, 1),
                              "resoFacts/hasSpa" = c(0, 1)
    )
    
    data <- setNames(as.list(rep(0, 12)), c(
      "resoFacts/bedrooms", "resoFacts/bathrooms", "resoFacts/parkingCapacity",
      "resoFacts/yearBuilt", "livingArea", "distance_to_parksKM",
      "Distance_to_city", "Distance_to_mall", "Walk_score", "photoCount",
      "resoFacts/hasView", "resoFacts/hasSpa"
    ))
    
    prices <- sapply(variable_values, function(x) {
      data[[selected_var]] <- x
      model$predict(matrix(unlist(data), nrow = 1))
    })
    
    ggplot(data.frame(variable_values, prices), aes(x = variable_values, y = prices)) +
      geom_line() +
      labs(title = paste("Effect of", selected_var, "on Price"),
           x = selected_var, y = "Predicted Price")
  })
  
  output$predicted_from_variable <- renderText({
    selected_var <- input$variable
    var_value <- input$variable_value
    data <- setNames(as.list(rep(0, 12)), reactive_vars$variables)
    data[[selected_var]] <- var_value
    prediction <- model$predict(matrix(unlist(data), nrow = 1))
    paste("Predicted Price for", selected_var, "=", var_value, ": $", round(prediction, 2))
  })
  
  observeEvent(input$predict_price, {
    user_input <- c(
      input$bedrooms, input$bathrooms, input$parkingCapacity, input$yearBuilt,
      input$livingArea, input$distance_to_parksKM, input$Distance_to_city,
      input$Distance_to_mall, input$Walk_score, input$photoCount,
      input$hasView, input$hasSpa
    )
    numeric_input <- as.numeric(user_input)
    prediction <- model$predict(matrix(numeric_input, nrow = 1))
    output$predicted_price <- renderText({
      paste("Predicted Home Price: $", round(prediction, 2))
    })
  })
}

shinyApp(ui = ui, server = server)

