#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

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
