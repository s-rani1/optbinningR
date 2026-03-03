#!/usr/bin/env Rscript

root <- normalizePath(".", mustWork = TRUE)
out_dir <- file.path(root, "inst", "extdata")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(4201)
n_train <- 2400
n_update <- 1200
fico_train <- data.frame(
  external_risk_estimate = round(runif(n_train, 20, 95)),
  revolving_utilization = runif(n_train, 0, 1),
  age_oldest_trade = rexp(n_train, rate = 1 / 120),
  delinquency_30 = rpois(n_train, lambda = 0.25),
  inquiry_6m = rpois(n_train, lambda = 1.4)
)
logit <- -3.2 +
  0.035 * (100 - fico_train$external_risk_estimate) +
  1.9 * fico_train$revolving_utilization +
  0.2 * fico_train$delinquency_30 +
  0.08 * fico_train$inquiry_6m -
  0.003 * fico_train$age_oldest_trade
fico_train$y <- rbinom(n_train, 1, stats::plogis(logit))

fico_update <- data.frame(
  external_risk_estimate = round(runif(n_update, 18, 92)),
  revolving_utilization = pmin(1, pmax(0, runif(n_update, 0, 1) + rnorm(n_update, 0.04, 0.08))),
  age_oldest_trade = rexp(n_update, rate = 1 / 110),
  delinquency_30 = rpois(n_update, lambda = 0.35),
  inquiry_6m = rpois(n_update, lambda = 1.6)
)
logit_u <- -3.1 +
  0.038 * (100 - fico_update$external_risk_estimate) +
  1.95 * fico_update$revolving_utilization +
  0.23 * fico_update$delinquency_30 +
  0.09 * fico_update$inquiry_6m -
  0.0025 * fico_update$age_oldest_trade
fico_update$y <- rbinom(n_update, 1, stats::plogis(logit_u))

set.seed(4202)
n <- 3000
contract <- sample(c("Month-to-month", "One year", "Two year"), n, replace = TRUE, prob = c(0.55, 0.25, 0.20))
internet <- sample(c("DSL", "Fiber optic", "No"), n, replace = TRUE, prob = c(0.35, 0.45, 0.20))
tenure <- pmax(1, round(rgamma(n, shape = 2.4, scale = 12)))
monthly_charges <- pmax(18, rnorm(n, mean = 70, sd = 25))
total_charges <- pmax(20, tenure * monthly_charges + rnorm(n, sd = 60))
payment_method <- sample(c("Electronic check", "Mailed check", "Bank transfer", "Credit card"), n, replace = TRUE)
telco <- data.frame(
  tenure = tenure,
  monthly_charges = monthly_charges,
  total_charges = total_charges,
  contract = contract,
  internet_service = internet,
  payment_method = payment_method,
  stringsAsFactors = FALSE
)
logit_t <- -2.0 +
  0.95 * (contract == "Month-to-month") +
  0.45 * (internet == "Fiber optic") +
  0.40 * (payment_method == "Electronic check") +
  0.012 * (monthly_charges - 70) -
  0.03 * pmin(tenure, 48)
telco$y <- rbinom(n, 1, stats::plogis(logit_t))
idx <- sample.int(n, size = floor(0.75 * n))
telco_train <- telco[idx, , drop = FALSE]
telco_test <- telco[-idx, , drop = FALSE]

utils::write.csv(fico_train, file.path(out_dir, "fico_train.csv"), row.names = FALSE)
utils::write.csv(fico_update, file.path(out_dir, "fico_update.csv"), row.names = FALSE)
utils::write.csv(telco_train, file.path(out_dir, "telco_train.csv"), row.names = FALSE)
utils::write.csv(telco_test, file.path(out_dir, "telco_test.csv"), row.names = FALSE)

set.seed(4203)
n_ls <- 25000
x_ls <- rnorm(n_ls)
p_ls <- stats::plogis(0.9 * x_ls - 0.15 + 0.2 * (x_ls > 1))
y_ls <- rbinom(n_ls, 1, p_ls)
utils::write.csv(data.frame(x = x_ls, y = y_ls), file.path(out_dir, "large_scale_synth.csv"), row.names = FALSE)

cat("Generated tutorial datasets in inst/extdata\n")
