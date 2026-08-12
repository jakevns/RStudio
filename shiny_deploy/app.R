# app.R
# Deploys the tuned Lasso model (from M08) as an interactive Shiny app.
# Predicts Sales Revenue (USD) from discount %, marketing spend, category,
# day of week, holiday status, and month.
#
# Deploy with: rsconnect::deployApp()
# Make sure lasso_model.rds sits in the SAME folder as this app.R.

library(shiny)
library(tidymodels)
library(glmnet)
library(bslib)

model <- readRDS("lasso_model.rds")

ui <- page_sidebar(
  title = "Retail Discount & Marketing \u2192 Sales Revenue Predictor",
  theme = bs_theme(bootswatch = "cosmo"),

  sidebar = sidebar(
    width = 320,
    h5("Set promotion conditions"),
    sliderInput("discount_percentage", "Discount (%)",
                min = 0, max = 20, value = 10, step = 1),
    sliderInput("marketing_spend_usd", "Marketing spend ($)",
                min = 0, max = 199, value = 75, step = 5),
    selectInput("product_category", "Product category",
                choices = c("Clothing", "Electronics", "Furniture", "Groceries")),
    selectInput("day_of_the_week", "Day of the week",
                choices = c("Monday", "Tuesday", "Wednesday", "Thursday",
                            "Friday", "Saturday", "Sunday")),
    selectInput("holiday_effect", "Is this a holiday?",
                choices = c("No" = "False", "Yes" = "True")),
    selectInput("month", "Month",
                choices = c("Jan","Feb","Mar","Apr","May","Jun",
                            "Jul","Aug","Sep","Oct","Nov","Dec"))
  ),

  layout_columns(
    col_widths = c(12),
    value_box(
      title = "Predicted Sales Revenue",
      value = textOutput("prediction"),
      showcase = bsicons::bs_icon("graph-up-arrow"),
      theme = "success"
    )
  ),

  card(
    card_header("What this shows"),
    p("This tool predicts a single transaction's sales revenue based on the ",
      "promotion conditions you set on the left. The model is a ",
      strong("Lasso-regularized linear regression"), " trained on 30,000 ",
      "retail transactions (2022\u20132024), tuned via 10-fold cross-validation ",
      "to select the penalty that minimizes prediction error."),
    p("Use it to compare scenarios \u2014 for example, raise the discount slider ",
      "and watch how the predicted revenue changes, holding everything else ",
      "constant. That's the core question behind our group project: ",
      em("how much does discounting actually move revenue, once marketing ",
         "spend, seasonality, and category are accounted for?"))
  )
)

server <- function(input, output, session) {

  new_case <- reactive({
    tibble(
      discount_percentage = as.numeric(input$discount_percentage),
      marketing_spend_usd = as.numeric(input$marketing_spend_usd),
      product_category    = factor(input$product_category,
                                    levels = c("Clothing","Electronics","Furniture","Groceries")),
      day_of_the_week     = factor(input$day_of_the_week,
                                    levels = c("Monday","Tuesday","Wednesday","Thursday",
                                               "Friday","Saturday","Sunday")),
      holiday_effect       = factor(input$holiday_effect, levels = c("False","True")),
      month                = factor(input$month,
                                     levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                                "Jul","Aug","Sep","Oct","Nov","Dec"))
    )
  })

  output$prediction <- renderText({
    pred <- predict(model, new_data = new_case())
    paste0("$", format(round(pred$.pred, 2), big.mark = ",", nsmall = 2))
  })
}

shinyApp(ui, server)
