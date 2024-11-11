#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(ggplot2)
library(reticulate)

# Load your Python-based XGBoost model

joblib <- import("joblib")
model <- joblib$load("real_estate_model.pkl")


server <- function(input, output, session) {
  # Precomputed metrics and plots for each variable
  variable_metrics <- list(
    "None" = list(rmse = 176071.02, r2 = 0.715, plot = "overall_plot.png"), # Overall model results
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
  
  # Display metrics dynamically based on selected variable
  output$model_metrics <- renderText({
    selected_variable <- input$remove_variable
    
    # Fetch metrics for the selected variable
    metrics <- variable_metrics[[selected_variable]]
    
    # Ensure metrics exist for the selection
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
    
    # Get the path to the selected variable's plot
    plot_path <- variable_metrics[[selected_variable]]$plot
    
    # Ensure the file exists
    if (file.exists(plot_path)) {
      list(
        src = plot_path,
        contentType = "image/png",
        width = 400,  # Set a fixed width (in pixels)
        height = 400
      )
    } else {
      stop("Error: Plot image not found at the specified path.")
    }
  }, deleteFile = FALSE)
  
  # Example: Remaining variables logic
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
    
    # Define reasonable ranges for each variable
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
                              "resoFacts/hasView" = c(0, 1),  # Binary variable
                              "resoFacts/hasSpa" = c(0, 1)    # Binary variable
    )
    
    # Initialize data with all values set to 0 and the correct 12 feature names
    data <- setNames(as.list(rep(0, 12)), c(
      "resoFacts/bedrooms", "resoFacts/bathrooms", "resoFacts/parkingCapacity",
      "resoFacts/yearBuilt", "livingArea", "distance_to_parksKM",
      "Distance_to_city", "Distance_to_mall", "Walk_score", "photoCount",
      "resoFacts/hasView", "resoFacts/hasSpa"
    ))
    
    # Calculate predicted prices based on varying selected variable
    prices <- sapply(variable_values, function(x) {
      data[[selected_var]] <- x
      model$predict(matrix(unlist(data), nrow = 1))
    })
    
    # Plot the effect of the selected variable on price
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
  
  # 3. Price Predictor Function
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
