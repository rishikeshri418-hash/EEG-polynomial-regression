# EEG-polynomial-regression
EEG signal analysis using polynomial regression - Implementing statistical modeling, model selection (AIC/BIC), and validation for neural signal processing.
# EEG Signal Modelling using Polynomial Regression

## 📋 Project Overview
This research implements statistical modeling of simulated EEG signals using polynomial regression. The study examines relationships between multi-channel EEG signals and identifies optimal polynomial specifications through rigorous information-theoretic model selection criteria (AIC, BIC).

## 🎯 Research Objectives
- Model complex relationships between simulated EEG input and output signals
- Identify optimal polynomial regression specifications using statistical criteria
- Validate model performance through train-test validation and residual analysis
- Provide a reproducible statistical workflow for EEG signal analysis

## 📊 Methodology
- **Dataset:** 200 temporal samples with 5 EEG channels (4 inputs, 1 output)
- **Models:** 5 candidate polynomial regression specifications
- **Model Selection:** Akaike Information Criterion (AIC), Bayesian Information Criterion (BIC), Residual Sum of Squares (RSS)
- **Validation:** 70-30 train-test split, confidence intervals, residual diagnostics
- **Statistical Framework:** Ordinary Least Squares regression, descriptive statistics, correlation analysis

## 🏆 Key Findings
- **Optimal Model:** y = θ₀ + θ₁x₄ + θ₂x₁³ + θ₃x₃⁴ (Model 4)
- **Performance:** AIC = 321.58, BIC = 334.78, Test MSE = 0.216
- **Key Insight:** Signal x1 showed strongest correlation with output y (r = 0.728)
- **Validation:** Excellent generalization performance on unseen data

## 📁 Project Structure
eeg-polynomial-regression/
│
├── EEG_Polynomial_Regression.R # Complete R analysis script
├── EEG_Signal_Modelling.pdf # Research paper (PDF)
├── README.md # Project documentation
└── figures/ # Generated analysis visualizations
├── time_series_plots.png
├── correlation_analysis.png
├── model_residuals.png
└── confidence_intervals.png

## 🛠️ Installation & Requirements

### Required Software
- **R** (version 4.1.0 or higher)
- **RStudio** (recommended for better development experience)

### Required R Packages
```r
install.packages(c("ggplot2", "dplyr", "tidyr", "car", "moments"))
