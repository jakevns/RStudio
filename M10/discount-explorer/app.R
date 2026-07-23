library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(scales)

# ---- Data prep (loaded once at app start, not reactive) ----

sales <- read_csv("Retail_sales.csv", show_col_types = FALSE) |>
  rename(
    store_id         = `Store ID`,
    product_id       = `Product ID`,
    date             = Date,
    units_sold       = `Units Sold`,
    revenue          = `Sales Revenue (USD)`,
    discount_pct     = `Discount Percentage`,
    marketing_spend  = `Marketing Spend (USD)`,
    store_location   = `Store Location`,
    category         = `Product Category`,
    day_of_week      = `Day of the Week`,
    holiday          = `Holiday Effect`
  ) |>
  mutate(
    date = as_date(date),
    discount_tier = case_when(
      discount_pct <= 5  ~ "Low (0-5%)",
      discount_pct <= 12 ~ "Moderate (6-12%)",
      TRUE               ~ "High (13-20%)"
    ),
    discount_tier = factor(
      discount_tier,
      levels = c("Low (0-5%)", "Moderate (6-12%)", "High (13-20%)")
    )
  )

categories <- sort(unique(sales$category))
min_date   <- min(sales$date)
max_date   <- max(sales$date)

# ---- UI ----

ui <- fluidPage(
  titlePanel("Discount Effectiveness Explorer"),
  p("Does discounting actually move units and revenue? Filter by category, ",
    "discount range, and date window below to explore the relationship ",
    "between discount depth and sales performance."),

  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "category", "Product Category",
        choices  = categories,
        selected = categories
      ),
      sliderInput(
        "discount_range", "Discount Percentage Range",
        min = 0, max = 20, value = c(0, 20), step = 1, post = "%"
      ),
      dateRangeInput(
        "date_range", "Date Range",
        start = min_date, end = max_date,
        min   = min_date, max = max_date
      ),
      checkboxInput("holiday_only", "Holiday dates only", value = FALSE),
      hr(),
      helpText("Discount tiers used in the summary table below: ",
               "Low = 0-5%, Moderate = 6-12%, High = 13-20%.")
    ),

    mainPanel(
      fluidRow(
        column(3, uiOutput("box_transactions")),
        column(3, uiOutput("box_avg_units")),
        column(3, uiOutput("box_avg_revenue")),
        column(3, uiOutput("box_avg_discount"))
      ),
      br(),
      plotlyOutput("scatter_plot", height = "350px"),
      br(),
      plotlyOutput("trend_plot", height = "300px"),
      br(),
      h4("Summary by Discount Tier"),
      DTOutput("tier_table")
    )
  )
)

# ---- Server ----

server <- function(input, output, session) {

  filtered <- reactive({
    req(input$category, input$discount_range, input$date_range)

    df <- sales |>
      filter(
        category     %in% input$category,
        discount_pct >= input$discount_range[1],
        discount_pct <= input$discount_range[2],
        date         >= input$date_range[1],
        date         <= input$date_range[2]
      )

    if (input$holiday_only) {
      df <- df |> filter(holiday == TRUE)
    }

    df
  })

  value_box <- function(value, label) {
    div(
      style = "text-align:center; background:#EAF4EA; border-radius:8px; padding:12px; margin-bottom:10px;",
      h3(style = "margin:0; color:#1B4332;", value),
      p(style = "margin:0; color:#52796F;", label)
    )
  }

  output$box_transactions <- renderUI({
    value_box(comma(nrow(filtered())), "Transactions")
  })

  output$box_avg_units <- renderUI({
    df <- filtered()
    val <- if (nrow(df) == 0) "—" else round(mean(df$units_sold), 1)
    value_box(val, "Avg Units Sold")
  })

  output$box_avg_revenue <- renderUI({
    df <- filtered()
    val <- if (nrow(df) == 0) "—" else dollar(mean(df$revenue))
    value_box(val, "Avg Revenue")
  })

  output$box_avg_discount <- renderUI({
    df <- filtered()
    val <- if (nrow(df) == 0) "—" else paste0(round(mean(df$discount_pct), 1), "%")
    value_box(val, "Avg Discount")
  })

  output$scatter_plot <- renderPlotly({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data for the selected filters."))

    plot_df <- df |>
      group_by(discount_pct, category) |>
      summarise(avg_units = mean(units_sold), .groups = "drop")

    p <- ggplot(plot_df, aes(x = discount_pct, y = avg_units, color = category)) +
      geom_point(size = 2, alpha = 0.7) +
      geom_smooth(aes(group = category), method = "lm", se = FALSE, linewidth = 0.7) +
      scale_color_brewer(palette = "Set2") +
      labs(
        x = "Discount Percentage", y = "Avg Units Sold", color = "Category",
        title = "Discount Level vs. Average Units Sold"
      ) +
      theme_minimal(base_size = 12)

    ggplotly(p)
  })

  output$trend_plot <- renderPlotly({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data for the selected filters."))

    plot_df <- df |>
      mutate(year_month = floor_date(date, "month")) |>
      group_by(year_month) |>
      summarise(total_revenue = sum(revenue), .groups = "drop")

    p <- ggplot(plot_df, aes(x = year_month, y = total_revenue)) +
      geom_line(color = "#2C5F2D", linewidth = 1) +
      geom_point(color = "#2C5F2D", size = 1.5) +
      scale_y_continuous(labels = label_dollar(scale = 1/1000, suffix = "K")) +
      labs(x = NULL, y = "Total Revenue", title = "Revenue Over Time (Filtered)") +
      theme_minimal(base_size = 12)

    ggplotly(p)
  })

  output$tier_table <- renderDT({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data for the selected filters."))

    df |>
      group_by(discount_tier) |>
      summarise(
        transactions   = n(),
        avg_units_sold = round(mean(units_sold), 1),
        avg_revenue    = round(mean(revenue), 2),
        total_revenue  = round(sum(revenue), 2),
        .groups = "drop"
      ) |>
      rename(
        `Discount Tier`   = discount_tier,
        `Transactions`    = transactions,
        `Avg Units Sold`  = avg_units_sold,
        `Avg Revenue`     = avg_revenue,
        `Total Revenue`   = total_revenue
      ) |>
      datatable(rownames = FALSE, options = list(dom = "t"))
  })
}

shinyApp(ui, server)
