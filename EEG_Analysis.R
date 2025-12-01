rm(list = ls())
required_packages <- c("ggplot2", "dplyr", "tidyr", "car", "moments")
for (package in required_packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}
folders <- c("figures", "report", "scripts", "data")
for (folder in folders) {
  if (!dir.exists(folder)) {
    dir.create(folder)
    cat("Created folder:", folder, "\n")
  }
}
cat("\n=== CHECKING DATA FILES ===\n")
data_files <- c("data/X.csv", "data/y.csv", "data/time.csv")
for (file in data_files) {
  if (file.exists(file)) {
    cat("Found:", file, "\n")
  } else {
    cat("MISSING:", file, "\n")
  }
}
cat("\n=== LOADING DATASETS ===\n")
X_data <- read.csv("data/X.csv")
y_data <- read.csv("data/y.csv")
time_data <- read.csv("data/time.csv")
full_data <- data.frame(
  time = time_data[, 1],
  x1 = X_data[, 1],
  x2 = X_data[, 2],
  x3 = X_data[, 3],
  x4 = X_data[, 4],
  y = y_data[, 1]
)

cat("Data loaded successfully\n")
cat("Dataset dimensions:", dim(full_data), "\n")
cat("Variables:", names(full_data), "\n")
cat("\n=== DATA SUMMARY ===\n")
print(summary(full_data))

cat("\n=== SETUP COMPLETE ===\n")
cat("Ready to begin Task 1 analysis!\n")
cat("\n")
cat(rep("=", 60), "\n")
cat("STARTING TASK 1: PRELIMINARY DATA ANALYSIS\n")
cat(rep("=", 60), "\n")
cat("\n--- Creating Time Series Plots ---\n")
ts_data <- full_data %>%
  pivot_longer(cols = c(x1, x2, x3, x4, y),
               names_to = "signal", 
               values_to = "amplitude")
time_plot <- ggplot(ts_data, aes(x = time, y = amplitude, color = signal)) +
  geom_line(alpha = 0.7, linewidth = 0.3) +
  facet_wrap(~ signal, ncol = 1, scales = "free_y") +
  labs(title = "Task 1.1: Time Series of EEG Signals",
       subtitle = "Input signals (x1-x4) and Output signal (y)",
       x = "Time (seconds)", 
       y = "Amplitude") +
  theme_minimal() +
  theme(legend.position = "none")

print(time_plot)
ggsave("figures/task1_1_time_series.png", time_plot, width = 10, height = 12, dpi = 300)
cat("Saved: figures/task1_1_time_series.png\n")
cat("\n--- Analyzing Distributions ---\n")

for(signal in c("x1", "x2", "x3", "x4", "y")) {
  
  signal_mean <- mean(full_data[[signal]])
  signal_sd <- sd(full_data[[signal]])
  signal_skew <- moments::skewness(full_data[[signal]])
  
  dist_plot <- ggplot(full_data, aes(x = .data[[signal]])) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", alpha = 0.7) +
    geom_density(color = "darkblue", linewidth = 1) +
    stat_function(fun = dnorm, 
                  args = list(mean = signal_mean, sd = signal_sd),
                  color = "red", linewidth = 1, linetype = "dashed") +
    labs(title = paste("Task 1.2: Distribution of", signal),
         subtitle = paste("Mean =", round(signal_mean, 3), 
                          ", SD =", round(signal_sd, 3),
                          ", Skew =", round(signal_skew, 3)),
         x = "Amplitude", 
         y = "Density") +
    theme_minimal()
  
  print(dist_plot)
  ggsave(paste0("figures/task1_2_", signal, "_distribution.png"), 
         dist_plot, width = 8, height = 5, dpi = 300)
}
cat("All distribution plots saved to figures/ folder\n")
cat("\n--- Analyzing Correlations ---\n")
cor_matrix <- cor(full_data[, c("x1", "x2", "x3", "x4", "y")])
cat("Correlation Matrix:\n")
print(round(cor_matrix, 3))

y_correlations <- cor_matrix["y", c("x1", "x2", "x3", "x4")]
strongest_cor <- names(which.max(abs(y_correlations)))
strongest_value <- y_correlations[strongest_cor]

cat("\nStrongest correlation with y:", strongest_cor, "=", round(strongest_value, 3), "\n")

for(signal in c("x1", "x2", "x3", "x4")) {
  
  correlation_val <- cor(full_data[[signal]], full_data$y)
  
  scatter_plot <- ggplot(full_data, aes(x = .data[[signal]], y = y)) +
    geom_point(alpha = 0.5, color = "darkgreen", size = 1) +
    geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.3) +
    labs(title = paste("Task 1.3: y vs", signal),
         subtitle = paste("Correlation =", round(correlation_val, 3)),
         x = signal, 
         y = "y") +
    theme_minimal()
  
  print(scatter_plot)
  ggsave(paste0("figures/task1_3_scatter_", signal, "_vs_y.png"), 
         scatter_plot, width = 8, height = 5, dpi = 300)
}
cat("\n")
cat(rep("=", 60), "\n")
cat("TASK 1 COMPLETED SUCCESSFULLY!\n")
cat(rep("=", 60), "\n")

cat("\n All time series plots created and saved\n")
cat("All distribution analyses completed\n") 
cat("Correlation matrix calculated\n")
cat("All scatter plots created and saved\n")
cat("Check 'figures/' folder for all generated plots\n")

cat("\nKEY INSIGHTS FOR YOUR REPORT:\n")
cat("• Dataset: 1000 time points, 5 EEG signals\n")
cat("• Time range: 0.002 to 0.4 seconds\n")
cat("• Signal with strongest relationship to y:", strongest_cor, 
    "(r =", round(strongest_value, 3), ")\n")
cat("• All signals centered around zero (typical for EEG)\n")
cat("• Ready for Task 2: Regression Modeling\n")

save(full_data, file = "data/full_data.RData")
cat("Data saved for Task 2: data/full_data.RData\n")
cat("\n")
cat(rep("=", 60), "\n")
cat("STARTING TASK 2: REGRESSION MODELING\n")
cat(rep("=", 60), "\n")

# Load the data we saved from Task 1
load("data/full_data.RData")
n <- nrow(full_data)

cat("Data loaded:", n, "samples\n")
build_design_matrix <- function(model_num, data) {
  switch(as.character(model_num),
         "1" = {
           # Model 1: y = θ1*x4 + θ2*x1² + θ3*x1³ + θ4*x3⁴ + θ_bias
           X_matrix <- cbind(
             x4 = data$x4,
             x1_sq = data$x1^2,
             x1_cub = data$x1^3,
             x3_quad = data$x3^4,
             intercept = 1
           )
           cat("Model 1: y = θ1*x4 + θ2*x1² + θ3*x1³ + θ4*x3⁴ + θ_bias\n")
           return(X_matrix)
         },
         "2" = {
           # Model 2: y = θ1*x3³ + θ2*x3⁴ + θ_bias
           X_matrix <- cbind(
             x3_cub = data$x3^3,
             x3_quad = data$x3^4,
             intercept = 1
           )
           cat("Model 2: y = θ1*x3³ + θ2*x3⁴ + θ_bias\n")
           return(X_matrix)
         },
         "3" = {
           # Model 3: y = θ1*x2 + θ2*x1³ + θ3*x3⁴ + θ_bias
           X_matrix <- cbind(
             x2 = data$x2,
             x1_cub = data$x1^3,
             x3_quad = data$x3^4,
             intercept = 1
           )
           cat("Model 3: y = θ1*x2 + θ2*x1³ + θ3*x3⁴ + θ_bias\n")
           return(X_matrix)
         },
         "4" = {
           # Model 4: y = θ1*x4 + θ2*x1³ + θ4*x3⁴ + θ_bias
           X_matrix <- cbind(
             x4 = data$x4,
             x1_cub = data$x1^3,
             x3_quad = data$x3^4,
             intercept = 1
           )
           cat("Model 4: y = θ1*x4 + θ2*x1³ + θ4*x3⁴ + θ_bias\n")
           return(X_matrix)
         },
         "5" = {
           # Model 5: y = θ1*x4 + θ2*x1² + θ3*x1³ + θ4*x3⁴ + θ5*x1⁴ + θ_bias
           X_matrix <- cbind(
             x4 = data$x4,
             x1_sq = data$x1^2,
             x1_cub = data$x1^3,
             x3_quad = data$x3^4,
             x1_quad = data$x1^4,
             intercept = 1
           )
           cat("Model 5: y = θ1*x4 + θ2*x1² + θ3*x1³ + θ4*x3⁴ + θ5*x1⁴ + θ_bias\n")
           return(X_matrix)
         }
  )
}
cat("\n--- Task 2.1: Estimating Model Parameters ---\n")
results <- list()

for(i in 1:5) {
  cat("\nFitting Model", i, "...\n")
  
  X_matrix <- build_design_matrix(i, full_data)
  y_vector <- full_data$y
  
  theta_hat <- solve(t(X_matrix) %*% X_matrix) %*% t(X_matrix) %*% y_vector
  
  results[[i]] <- list(
    model_num = i,
    design_matrix = X_matrix,
    theta_hat = theta_hat,
    k = ncol(X_matrix)  
  )
  
  cat("Parameters estimated. k =", ncol(X_matrix), "parameters\n")
}
cat("\n--- Task 2.2: Computing Residual Sum of Squares (RSS) ---\n")

for(i in 1:5) {
  X_mat <- results[[i]]$design_matrix
  theta <- results[[i]]$theta_hat
  
  y_pred <- X_mat %*% theta
  
  RSS <- sum((full_data$y - y_pred)^2)
  
  results[[i]]$RSS <- RSS
  results[[i]]$y_pred <- y_pred
  results[[i]]$residuals <- full_data$y - y_pred
  
  cat("Model", i, "RSS:", round(RSS, 3), "\n")
}
cat("\n--- Task 2.3: Computing Log-Likelihood ---\n")

for(i in 1:5) {
  RSS <- results[[i]]$RSS
  k <- results[[i]]$k
  
  sigma2_hat <- RSS / (n - 1)
  
  log_likelihood <- - (n/2) * log(2 * pi) - (n/2) * log(sigma2_hat) - (1/(2 * sigma2_hat)) * RSS
  
  results[[i]]$sigma2_hat <- sigma2_hat
  results[[i]]$log_likelihood <- log_likelihood
  
  cat("Model", i, "Log-Likelihood:", round(log_likelihood, 3), "\n")
}
cat("\n--- Task 2.4: Computing AIC and BIC ---\n")

model_comparison <- data.frame(
  Model = 1:5,
  k = sapply(results, function(x) x$k),
  RSS = sapply(results, function(x) x$RSS),
  LogLik = sapply(results, function(x) x$log_likelihood)
)

model_comparison$AIC <- 2 * model_comparison$k - 2 * model_comparison$LogLik
model_comparison$BIC <- model_comparison$k * log(n) - 2 * model_comparison$LogLik

cat("\n=== MODEL COMPARISON TABLE ===\n")
print(model_comparison)

best_aic <- which.min(model_comparison$AIC)
best_bic <- which.min(model_comparison$BIC)

cat("\nBest model by AIC: Model", best_aic, "(AIC =", round(model_comparison$AIC[best_aic], 2), ")\n")
cat("Best model by BIC: Model", best_bic, "(BIC =", round(model_comparison$BIC[best_bic], 2), ")\n")
cat("\n--- Task 2.5: Checking Residual Distributions ---\n")


for(i in 1:5) {
  qq_plot <- ggplot(data.frame(residuals = results[[i]]$residuals), 
                    aes(sample = residuals)) +
    geom_qq(color = "blue", alpha = 0.6) +
    geom_qq_line(color = "red", linewidth = 1) +
    labs(title = paste("Model", i, "Q-Q Plot of Residuals"),
         subtitle = paste("RSS =", round(results[[i]]$RSS, 2)),
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()
  
  print(qq_plot)
  ggsave(paste0("figures/task2_5_qq_model_", i, ".png"), qq_plot, width = 8, height = 6, dpi = 300)
}

for(i in 1:5) {
  resid_plot <- ggplot(data.frame(residuals = results[[i]]$residuals), 
                       aes(x = residuals)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightcoral", alpha = 0.7) +
    geom_density(color = "darkred", linewidth = 1) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(results[[i]]$residuals), 
                              sd = sd(results[[i]]$residuals)),
                  color = "blue", linewidth = 1, linetype = "dashed") +
    labs(title = paste("Model", i, "Residual Distribution"),
         subtitle = paste("Mean =", round(mean(results[[i]]$residuals), 4),
                          "SD =", round(sd(results[[i]]$residuals), 4)),
         x = "Residuals",
         y = "Density") +
    theme_minimal()
  
  print(resid_plot)
  ggsave(paste0("figures/task2_5_residuals_model_", i, ".png"), resid_plot, width = 8, height = 5, dpi = 300)
}
cat("\n--- Task 2.6: Selecting Best Model ---\n")

if (best_aic == best_bic) {
  selected_model <- best_aic
  selection_reason <- "AIC and BIC both select this model"
} else {
  
  selected_model <- best_bic
  selection_reason <- paste("BIC selected (more conservative), AIC preferred Model", best_aic)
}

cat("SELECTED BEST MODEL: Model", selected_model, "\n")
cat("Reason:", selection_reason, "\n")
cat("AIC:", round(model_comparison$AIC[selected_model], 2), "\n")
cat("BIC:", round(model_comparison$BIC[selected_model], 2), "\n")
cat("RSS:", round(model_comparison$RSS[selected_model], 2), "\n")
cat("\n--- Task 2.7: Train-Test Split and Confidence Intervals ---\n")

set.seed(123)

train_indices <- sample(1:n, size = round(0.7 * n))
train_data <- full_data[train_indices, ]
test_data <- full_data[-train_indices, ]

cat("Training samples:", nrow(train_data), "\n")
cat("Testing samples:", nrow(test_data), "\n")

cat("\nRetraining Model", selected_model, "on training data...\n")
X_train <- build_design_matrix(selected_model, train_data)
y_train <- train_data$y
theta_train <- solve(t(X_train) %*% X_train) %*% t(X_train) %*% y_train

X_test <- build_design_matrix(selected_model, test_data)
y_pred_test <- X_test %*% theta_train

residuals_train <- y_train - (X_train %*% theta_train)
sigma2_hat_train <- sum(residuals_train^2) / (nrow(train_data) - results[[selected_model]]$k)

se_fit <- sqrt(diag(X_test %*% solve(t(X_train) %*% X_train) %*% t(X_test)) * sigma2_hat_train)

t_val <- qt(0.975, df = nrow(train_data) - results[[selected_model]]$k)

ci_lower <- y_pred_test - t_val * se_fit
ci_upper <- y_pred_test + t_val * se_fit

prediction_df <- data.frame(
  index = 1:nrow(test_data),
  actual = test_data$y,
  predicted = as.numeric(y_pred_test),
  ci_lower = as.numeric(ci_lower),
  ci_upper = as.numeric(ci_upper)
)

confidence_plot <- ggplot(prediction_df, aes(x = index)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = predicted), color = "red", linewidth = 1) +
  geom_point(aes(y = actual), alpha = 0.6, size = 1, color = "darkgreen") +
  labs(title = paste("Model", selected_model, "Predictions on Test Data with 95% CI"),
       subtitle = "Red line = predictions, Green points = actual values, Blue band = 95% CI",
       x = "Test Sample Index",
       y = "EEG Amplitude") +
  theme_minimal()

print(confidence_plot)
ggsave("figures/task2_7_confidence_intervals.png", confidence_plot, width = 10, height = 6, dpi = 300)

test_rss <- sum((test_data$y - y_pred_test)^2)
test_mse <- test_rss / nrow(test_data)

cat("\nTest Performance for Model", selected_model, ":\n")
cat("Test RSS:", round(test_rss, 3), "\n")
cat("Test MSE:", round(test_mse, 5), "\n")
cat("\n")
cat(rep("=", 60), "\n")
cat("TASK 2 COMPLETED SUCCESSFULLY!\n")
cat(rep("=", 60), "\n")

cat("\nSUMMARY:\n")
cat("All 5 models fitted and evaluated\n")
cat("Best model selected: Model", selected_model, "\n")
cat("AIC:", round(model_comparison$AIC[selected_model], 2), "\n")
cat("BIC:", round(model_comparison$BIC[selected_model], 2), "\n")
cat("RSS:", round(model_comparison$RSS[selected_model], 2), "\n")
cat("Test MSE:", round(test_mse, 5), "\n")
cat("All plots saved to figures/ folder\n")
save(results, model_comparison, selected_model, file = "data/task2_results.RData")
cat("Results saved for Task 3: data/task2_results.RData\n")
cat("\n")
cat(rep("=", 60), "\n")
cat("STARTING TASK 3: DESCRIPTIVE STATISTICS\n")
cat(rep("=", 60), "\n")

load("data/full_data.RData")
n <- nrow(full_data)
cat("\n--- Task 3.1: Confidence Intervals for Signal Means ---\n")

compute_ci <- function(data_vector, confidence_level) {
  n <- length(data_vector)
  mean_val <- mean(data_vector)
  se <- sd(data_vector) / sqrt(n)
  alpha <- 1 - confidence_level
  t_critical <- qt(1 - alpha/2, df = n-1)
  margin_error <- t_critical * se
  
  return(c(
    lower = mean_val - margin_error,
    upper = mean_val + margin_error,
    mean = mean_val,
    variance = var(data_vector),
    margin_error = margin_error
  ))
}

signals <- c("x1", "x2", "x3", "x4", "y")
confidence_levels <- c(0.90, 0.95, 0.99)

ci_results <- list()

cat("\n=== CONFIDENCE INTERVALS FOR SIGNAL MEANS ===\n")
for(signal in signals) {
  signal_data <- full_data[[signal]]
  ci_results[[signal]] <- list()
  
  cat("\n---", signal, "---\n")
  cat("Mean:", round(mean(signal_data), 4), "\n")
  cat("Variance:", round(var(signal_data), 4), "\n")
  cat("Standard Deviation:", round(sd(signal_data), 4), "\n")
  
  for(conf_level in confidence_levels) {
    ci <- compute_ci(signal_data, conf_level)
    ci_results[[signal]][[as.character(conf_level)]] <- ci
    
    cat(conf_level*100, "% CI: [", 
        round(ci["lower"], 4), ", ", 
        round(ci["upper"], 4), "]",
        " (Margin of error: ±", round(ci["margin_error"], 4), ")\n", sep = "")
  }
}
cat("\n--- Task 3.2: Scale, Skewness, and Distribution Analysis ---\n")

descriptive_stats <- data.frame(
  Signal = signals,
  Mean = sapply(signals, function(s) mean(full_data[[s]])),
  Variance = sapply(signals, function(s) var(full_data[[s]])),
  Std_Dev = sapply(signals, function(s) sd(full_data[[s]])),
  Skewness = sapply(signals, function(s) moments::skewness(full_data[[s]])),
  Kurtosis = sapply(signals, function(s) moments::kurtosis(full_data[[s]]))
)

cat("\n=== DESCRIPTIVE STATISTICS TABLE ===\n")
descriptive_stats_numeric <- descriptive_stats
descriptive_stats_numeric[, -1] <- round(descriptive_stats_numeric[, -1], 4)
print(descriptive_stats_numeric)

for(signal in signals) {
  signal_data <- full_data[[signal]]
  
  signal_mean <- mean(signal_data)
  signal_sd <- sd(signal_data)
  signal_skew <- moments::skewness(signal_data)
  
  hist_info <- hist(signal_data, plot = FALSE, breaks = 30)
  mode_val <- hist_info$mids[which.max(hist_info$counts)]
  
  dist_plot <- ggplot(full_data, aes(x = .data[[signal]])) +
    geom_histogram(aes(y = after_stat(density)), 
                   bins = 30, 
                   fill = "lightgreen", 
                   alpha = 0.7,
                   color = "black") +
    stat_function(fun = dnorm, 
                  args = list(mean = signal_mean, sd = signal_sd),
                  color = "darkgreen", 
                  linewidth = 1.2) +
    geom_vline(xintercept = signal_mean, 
               color = "blue", 
               linewidth = 1, 
               linetype = "solid") +
    geom_vline(xintercept = mode_val, 
               color = "red", 
               linewidth = 1, 
               linetype = "dashed") +
    annotate("text", 
             x = signal_mean, 
             y = max(hist_info$density) * 0.9,
             label = paste("Mean:", round(signal_mean, 3)),
             hjust = -0.1, 
             color = "blue",
             size = 3) +
    annotate("text", 
             x = mode_val, 
             y = max(hist_info$density) * 0.8,
             label = paste("Mode:", round(mode_val, 3)),
             hjust = -0.1, 
             color = "red",
             size = 3) +
    labs(title = paste("Task 3.2: Distribution of", signal),
         subtitle = paste("Skewness =", round(signal_skew, 3), 
                          "| Blue line = Mean", 
                          "| Red dashed line = Mode"),
         x = "Amplitude", 
         y = "Density") +
    theme_minimal()
  
  print(dist_plot)
  ggsave(paste0("figures/task3_2_", signal, "_distribution_with_mode.png"), 
         dist_plot, width = 9, height = 6, dpi = 300)
}
cat("\n--- Task 3.3: Interpretation of Results ---\n")

cat("\n=== KEY INTERPRETATIONS FOR YOUR REPORT ===\n")
cat("\n1. SKEWNESS INTERPRETATION:\n")
for(signal in signals) {
  skew_val <- descriptive_stats$Skewness[descriptive_stats$Signal == signal]
  if(abs(skew_val) < 0.5) {
    skew_desc <- "approximately symmetric"
  } else if(skew_val > 0.5) {
    skew_desc <- "right-skewed"
  } else if(skew_val < -0.5) {
    skew_desc <- "left-skewed"
  }
  cat("  -", signal, ": skewness =", round(skew_val, 3), "->", skew_desc, "\n")
}
cat("\n2. SCALE/VARIABILITY ANALYSIS:\n")
var_rank <- order(descriptive_stats$Variance, decreasing = TRUE)
most_variable <- descriptive_stats$Signal[var_rank[1]]
least_variable <- descriptive_stats$Signal[var_rank[5]]
cat("  - Most variable signal:", most_variable, 
    "(variance =", round(descriptive_stats$Variance[descriptive_stats$Signal == most_variable], 3), ")\n")
cat("  - Least variable signal:", least_variable, 
    "(variance =", round(descriptive_stats$Variance[descriptive_stats$Signal == least_variable], 3), ")\n")
cat("\n3. CONFIDENCE INTERVAL PRECISION:\n")
for(signal in signals) {
  ci_95 <- ci_results[[signal]][["0.95"]]
  width <- ci_95["upper"] - ci_95["lower"]
  cat("  -", signal, ": 95% CI width =", round(width, 4), "\n")
}
cat("\n4. NORMALITY ASSESSMENT:\n")
for(signal in signals) {
  skew_val <- descriptive_stats$Skewness[descriptive_stats$Signal == signal]
  if(abs(skew_val) < 0.5) {
    norm_desc <- "Close to normal"
  } else {
    norm_desc <- "Deviates from normal"
  }
  cat("  -", signal, ":", norm_desc, "(skewness =", round(skew_val, 3), ")\n")
}
cat("\n5. OUTLIER DETECTION:\n")
cat("  - Check histograms for deviations from the normal curve\n")
cat("  - Large gaps between mean and mode suggest skewness/outliers\n")
cat("  - Heavy tails in distributions indicate potential outliers\n")
cat("\n")
cat(rep("=", 60), "\n")
cat("TASK 3 COMPLETED SUCCESSFULLY!\n")
cat(rep("=", 60), "\n")

cat("\n All descriptive statistics computed\n")
cat("Confidence intervals calculated for 90%, 95%, 99% levels\n")
cat("Skewness and scale measures computed\n")
cat("Distribution plots with normal overlays created\n")
cat("All plots saved to figures/ folder\n")
cat("Interpretation guidelines provided\n")

cat("\n=== ALL CODING TASKS COMPLETED ===\n")
cat("You are now ready to write your report!\n")
cat("Check the 'figures/' folder for all generated plots\n")
cat("Use the interpretations above for your report discussion\n")
save(descriptive_stats, ci_results, file = "data/task3_results.RData")
cat("✓ Results saved: data/task3_results.RData\n")



