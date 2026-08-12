# save_model.R
# Refits the tuned Lasso model from M08 (Evans,Jake-IBM6540-M08.qmd)
# and saves the fitted workflow as an .RDS for the Shiny deployment app.
#
# Run this INSIDE your M08 project (same folder as data/Retail_sales.csv)
# so the relative path below resolves correctly.

library(tidymodels)
library(tidyverse)
library(glmnet)
library(janitor)
library(lubridate)

tidymodels_prefer()
set.seed(2025)

# ---- Load + clean (identical to M08) ----
sales_raw <- read_csv("data/Retail_sales.csv", show_col_types = FALSE) |>
  clean_names()

sales_data <- sales_raw |>
  mutate(
    date = ymd(date),
    month = factor(month(date, label = TRUE), ordered = FALSE),
    product_category = factor(product_category),
    day_of_the_week = factor(
      day_of_the_week,
      levels = c(
        "Monday", "Tuesday", "Wednesday", "Thursday",
        "Friday", "Saturday", "Sunday"
      )
    ),
    holiday_effect = factor(holiday_effect, levels = c("False", "True"))
  )

sales_model_data <- sales_data |>
  select(
    sales_revenue_usd,
    discount_percentage,
    marketing_spend_usd,
    product_category,
    day_of_the_week,
    holiday_effect,
    month
  )

# ---- Split (identical seed/strata to M08) ----
set.seed(617)
sales_split <- initial_split(sales_model_data, prop = 0.80, strata = sales_revenue_usd)
sales_train <- training(sales_split)
sales_test  <- testing(sales_split)

# ---- Shared recipe (identical to M08) ----
sales_rec <- recipe(sales_revenue_usd ~ ., data = sales_train) |>
  step_impute_median(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())

# ---- Tune penalty (identical grid/folds to M08) ----
lasso_tune_spec <- linear_reg(penalty = tune(), mixture = 1) |>
  set_engine("glmnet")

lasso_tune_wf <- workflow() |>
  add_recipe(sales_rec) |>
  add_model(lasso_tune_spec)

set.seed(2025)
sales_folds <- vfold_cv(sales_train, v = 10, strata = sales_revenue_usd)

penalty_grid <- grid_regular(
  penalty(range = c(-4, 1)),
  levels = 30
)

set.seed(2025)
lasso_tune_results <- tune_grid(
  lasso_tune_wf,
  resamples = sales_folds,
  grid = penalty_grid,
  metrics = metric_set(rmse, rsq, mae)
)

best_penalty <- select_best(lasso_tune_results, metric = "rmse")

# ---- Finalize + fit on training data (same as final_lasso_fit in M08) ----
final_lasso_wf  <- finalize_workflow(lasso_tune_wf, best_penalty)
final_lasso_fit <- fit(final_lasso_wf, data = sales_train)

# ---- Sanity check on test set ----
test_metrics <- augment(final_lasso_fit, new_data = sales_test) |>
  metric_set(rmse, rsq, mae)(truth = sales_revenue_usd, estimate = .pred)
print(test_metrics)

# ---- Save the fitted workflow for the Shiny app ----
dir.create("shiny_deploy", showWarnings = FALSE)
saveRDS(final_lasso_fit, "shiny_deploy/lasso_model.rds")
cat("\nSaved model to shiny_deploy/lasso_model.rds\n")
