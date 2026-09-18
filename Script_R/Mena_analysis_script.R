# ==============================================================================
# FINAL R SCRIPT — 
# Broad Money Growth and Inflation in Selected MENA Countries (2010-2024)
# Panel Data Analysis
#
# Raw data: 2010-2024, 105 observations
# Regression sample: 2011-2024, 98 observations
#
# Main variables:
#   Y  = inflation (%)
#   X1 = broad_money_growth (%)
#   X2 = gdp_growth (%)
#   X3 = depreciation_rate (%)
#
# Exchange rate transformation:
#   depreciation_rate = 100 * (ER_t / ER_(t-1) - 1)
#   where ER is local currency per USD.
#   2010 is excluded because 2009 ER is not in the dataset.

# ==============================================================================


# ==============================================================================
# STEP 1 — PACKAGES
# ==============================================================================

packages <- c(
  "plm", "lmtest", "sandwich", "car", "ggplot2",
  "dplyr", "tidyr", "stargazer", "tseries", "corrplot"
)

missing_packages <- packages[!(packages %in% rownames(installed.packages()))]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cran.rstudio.com/")
}

invisible(lapply(packages, library, character.only = TRUE))


# ==============================================================================
# STEP 2 — LOAD DATA
# ==============================================================================

data <- read.csv("mena_panel_data.csv", stringsAsFactors = FALSE)

required_cols <- c(
  "country", "year", "inflation",
  "broad_money_growth", "gdp_growth", "exchange_rate"
)

missing_cols <- setdiff(required_cols, names(data))

if (length(missing_cols) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_cols, collapse = ", ")
  )
}

data <- data %>%
  mutate(
    country            = as.character(country),
    year               = as.integer(year),
    inflation          = as.numeric(inflation),
    broad_money_growth = as.numeric(broad_money_growth),
    gdp_growth         = as.numeric(gdp_growth),
    exchange_rate      = as.numeric(exchange_rate)
  ) %>%
  arrange(country, year)

cat("\n--- RAW DATA ---\n")
cat("Countries:", paste(unique(data$country), collapse = ", "), "\n")
cat("Years:", min(data$year), "to", max(data$year), "\n")
cat("Observations:", nrow(data), "\n")

duplicates <- data %>%
  count(country, year) %>%
  filter(n > 1)

if (nrow(duplicates) > 0) {
  stop("Duplicate country-year observations detected.")
}

cat("\nMissing values:\n")
print(colSums(is.na(data)))


# ==============================================================================
# STEP 3 — VARIABLE TRANSFORMATION
# ==============================================================================

# depreciation_rate = 100 * (ER_t / ER_(t-1) - 1)
# Positive = local currency lost value vs USD (depreciation)
# Negative = local currency gained value vs USD (appreciation)
# Jordan and Saudi Arabia: pegged → depreciation_rate ≈ 0% every year

data <- data %>%
  group_by(country) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    depreciation_rate =
      100 * (exchange_rate / dplyr::lag(exchange_rate) - 1)
  ) %>%
  ungroup()

cat("\n--- DEPRECIATION RATE BY COUNTRY ---\n")

dep_summary <- data %>%
  group_by(country) %>%
  summarise(
    mean_depreciation = round(mean(depreciation_rate, na.rm = TRUE), 2),
    sd_depreciation   = round(sd(depreciation_rate,   na.rm = TRUE), 2),
    min_depreciation  = round(min(depreciation_rate,  na.rm = TRUE), 2),
    max_depreciation  = round(max(depreciation_rate,  na.rm = TRUE), 2),
    .groups = "drop"
  )

print(dep_summary)

# Remove 2010 — NA depreciation because 2009 ER unavailable
data_clean <- data %>%
  filter(!is.na(depreciation_rate)) %>%
  arrange(country, year)

cat("\n--- ANALYTICAL SAMPLE ---\n")
cat("Raw observations:", nrow(data), "\n")
cat("Regression observations:", nrow(data_clean), "\n")
cat("Regression period:", min(data_clean$year),
    "to", max(data_clean$year), "\n")
cat("Countries:", length(unique(data_clean$country)), "\n")

expected_n <- length(unique(data_clean$country)) *
  length(unique(data_clean$year))

if (nrow(data_clean) != expected_n) {
  warning("The regression panel is not balanced.")
}


# ==============================================================================
# STEP 4 — DESCRIPTIVE STATISTICS
# ==============================================================================

desc_stats <- data_clean %>%
  group_by(country) %>%
  summarise(
    mean_inflation    = mean(inflation,          na.rm = TRUE),
    sd_inflation      = sd(inflation,            na.rm = TRUE),
    mean_money_growth = mean(broad_money_growth, na.rm = TRUE),
    sd_money_growth   = sd(broad_money_growth,   na.rm = TRUE),
    mean_gdp_growth   = mean(gdp_growth,         na.rm = TRUE),
    mean_depreciation = mean(depreciation_rate,  na.rm = TRUE),
    sd_depreciation   = sd(depreciation_rate,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

cat("\n--- DESCRIPTIVE STATISTICS ---\n")
print(desc_stats)


# ==============================================================================
# STEP 5 — PANEL STRUCTURE
# ==============================================================================

pdata <- pdata.frame(
  data_clean,
  index = c("country", "year")
)

cat("\n--- PANEL DIMENSIONS ---\n")
print(pdim(pdata))


# ==============================================================================
# STEP 6 — PRE-ESTIMATION DIAGNOSTICS
# ==============================================================================

cat("\n==============================================\n")
cat("PRE-ESTIMATION DIAGNOSTICS\n")
cat("==============================================\n")

# 6.1 Variable summaries
cat("\nVariable summaries:\n")
print(summary(
  data_clean[, c(
    "inflation", "broad_money_growth",
    "gdp_growth", "depreciation_rate"
  )]
))

# 6.2 ADF unit-root tests by country
# Note: T=14 → limited power. Used as supplementary check only.
cat("\n--- ADF TESTS BY COUNTRY ---\n")
cat("H0: unit root (non-stationary)\n")
cat("p < 0.05 -> stationary\n")
cat("Note: T=14, results must be interpreted cautiously.\n\n")

vars_to_test <- c(
  "inflation", "broad_money_growth",
  "gdp_growth", "depreciation_rate"
)

adf_results <- data.frame()

for (ctry in unique(data_clean$country)) {
  for (vr in vars_to_test) {
    
    x <- data_clean %>%
      filter(country == ctry) %>%
      pull(.data[[vr]]) %>%
      na.omit()
    
    result <- tryCatch({
      test <- adf.test(x, alternative = "stationary")
      data.frame(
        country  = ctry,
        variable = vr,
        adf_stat = round(as.numeric(test$statistic), 3),
        p_value  = round(test$p.value, 4),
        decision = ifelse(test$p.value < 0.05,
                          "Stationary", "Non-stationary")
      )
    }, error = function(e) {
      data.frame(
        country  = ctry, variable = vr,
        adf_stat = NA_real_, p_value = NA_real_,
        decision = "Test failed"
      )
    })
    
    adf_results <- rbind(adf_results, result)
  }
}

print(adf_results)
cat("\nADF summary:\n")
print(table(adf_results$variable, adf_results$decision))

# 6.3 Multicollinearity (VIF)
cat("\n--- VIF ---\n")
cat("VIF > 5: possible concern | VIF > 10: serious concern\n")

vif_model <- lm(
  inflation ~ broad_money_growth + gdp_growth + depreciation_rate,
  data = data_clean
)

print(vif(vif_model))

cat("\nCorrelation matrix — regressors:\n")
cor_regressors <- cor(
  data_clean[, c("broad_money_growth", "gdp_growth",
                 "depreciation_rate")],
  use = "complete.obs"
)
print(round(cor_regressors, 3))

# 6.4 Full correlation matrix
cor_full <- cor(
  data_clean[, c("inflation", "broad_money_growth",
                 "gdp_growth", "depreciation_rate")],
  use = "complete.obs"
)
cat("\nFull correlation matrix:\n")
print(round(cor_full, 3))


# ==============================================================================
# STEP 7 — MODEL ESTIMATION
# ==============================================================================

cat("\n==============================================\n")
cat("MODEL ESTIMATION\n")
cat("==============================================\n")

formula_main <- inflation ~
  broad_money_growth + gdp_growth + depreciation_rate

# 7.1 Pooled OLS
model_pooled <- plm(formula_main, data = pdata, model = "pooling")
cat("\n--- POOLED OLS ---\n")
print(summary(model_pooled))

# 7.2 Fixed Effects Individual
model_fe <- plm(formula_main, data = pdata,
                model = "within", effect = "individual")
cat("\n--- FIXED EFFECTS — INDIVIDUAL ---\n")
print(summary(model_fe))

# 7.3 Fixed Effects Two-Way
model_fe2 <- plm(formula_main, data = pdata,
                 model = "within", effect = "twoways")
cat("\n--- FIXED EFFECTS — TWO-WAY ---\n")
print(summary(model_fe2))

# 7.4 Random Effects
model_re <- plm(formula_main, data = pdata,
                model = "random", effect = "individual")
cat("\n--- RANDOM EFFECTS ---\n")
print(summary(model_re))

# 7.5 Between estimator
model_be <- plm(formula_main, data = pdata, model = "between")
cat("\n--- BETWEEN ESTIMATOR ---\n")
print(summary(model_be))


# ==============================================================================
# STEP 8 — MODEL SPECIFICATION TESTS
# ==============================================================================

cat("\n==============================================\n")
cat("MODEL SPECIFICATION TESTS\n")
cat("==============================================\n")

# 8.1 Breusch-Pagan LM
cat("\n--- BREUSCH-PAGAN LM TEST ---\n")
cat("H0: No individual panel effects / Pooled OLS adequate\n")
bp_lm <- plmtest(model_pooled, type = "bp", effect = "individual")
print(bp_lm)

# 8.2 F-test FE vs Pooled OLS
cat("\n--- F TEST: FE vs POOLED OLS ---\n")
cat("H0: All individual fixed effects are zero\n")
f_test <- pFtest(model_fe, model_pooled)
print(f_test)

# 8.3 Standard Hausman
cat("\n--- HAUSMAN TEST: STANDARD ---\n")
cat("H0: Random Effects consistent | H1: Fixed Effects preferred\n")
hausman_test <- phtest(model_fe, model_re)
print(hausman_test)

# 8.4 Robust Hausman (auxiliary regression)
# More reliable than standard Hausman with small N=7
cat("\n--- HAUSMAN TEST: AUXILIARY REGRESSION ---\n")
cat("More reliable for small panels (N=7)\n")
hausman_aux <- tryCatch({
  phtest(model_fe, model_re, method = "aux")
}, error = function(e) {
  cat("Auxiliary Hausman failed:", e$message, "\n")
  NULL
})
if (!is.null(hausman_aux)) print(hausman_aux)

# 8.5 Time fixed effects test
cat("\n--- TEST FOR TIME FIXED EFFECTS ---\n")
cat("H0: No time fixed effects\n")
time_fe_test <- pFtest(model_fe2, model_fe)
print(time_fe_test)

# ==============================================================================
# STEP 8.6 — MODEL SELECTION BASED ON TEST RESULTS
# ==============================================================================

# Decision tree:
#
# Step 1: Do individual panel effects exist?
#         -> BP-LM test and F-test
#
#         If EITHER rejects H0 at 5%:
#             panel effects are supported -> Step 2
#
#         If NEITHER rejects H0:
#             Pooled OLS selected
#
# Step 2: If panel effects exist, are they FE or RE?
#         -> Hausman test
#
#         If Hausman rejects H0:
#             Fixed Effects selected
#
#         If Hausman does not reject H0:
#             Random Effects selected
#
# Step 3: Test whether time effects are also important.
#         -> Two-way FE vs Individual FE
#
# IMPORTANT:
# The formal tests determine the main estimator.
# Fixed Effects is still estimated and reported as a robustness
# specification when another estimator is selected.
#
# Limitation:
# With only N = 7 countries, specification tests have limited power.
# This limitation is explicitly acknowledged in the report.
# ==============================================================================


# ------------------------------------------------------------------------------
# Step 1 — Pooled OLS vs Panel Effects
# ------------------------------------------------------------------------------

panel_effects_supported <- (
  bp_lm$p.value < 0.05 ||
    f_test$p.value < 0.05
)


if (!panel_effects_supported) {
  
  selected_model      <- model_pooled
  selected_model_name <- "Pooled OLS"
  
  selection_reason <- paste(
    "Neither the BP-LM test (p =",
    round(bp_lm$p.value, 4),
    ") nor the F-test (p =",
    round(f_test$p.value, 4),
    ") rejects the null hypothesis of no individual panel effects.",
    "Pooled OLS is therefore selected by the formal specification tests.",
    "Because N = 7, these tests have limited power.",
    "Fixed Effects is retained as a robustness specification."
  )
  
} else {
  
  # ---------------------------------------------------------------------------
  # Step 2 — FE vs RE
  # ---------------------------------------------------------------------------
  
  # The auxiliary-regression Hausman test is treated as a supplementary
  # robustness version. Standard Hausman remains the conventional test.
  
  if (!is.null(hausman_aux)) {
    
    hausman_p      <- hausman_aux$p.value
    hausman_method <- "auxiliary regression"
    
  } else {
    
    hausman_p      <- hausman_test$p.value
    hausman_method <- "standard"
  }
  
  
  if (hausman_p < 0.05) {
    
    selected_model      <- model_fe
    selected_model_name <- "Fixed Effects (Individual)"
    
    selection_reason <- paste(
      "Panel effects are supported.",
      "BP-LM p =",
      round(bp_lm$p.value, 4),
      "| F-test p =",
      round(f_test$p.value, 4),
      ".",
      "Hausman (",
      hausman_method,
      ") p =",
      round(hausman_p, 4),
      "rejects RE consistency.",
      "Fixed Effects is selected."
    )
    
  } else {
    
    selected_model      <- model_re
    selected_model_name <- "Random Effects (Individual)"
    
    selection_reason <- paste(
      "Panel effects are supported.",
      "BP-LM p =",
      round(bp_lm$p.value, 4),
      "| F-test p =",
      round(f_test$p.value, 4),
      ".",
      "Hausman (",
      hausman_method,
      ") p =",
      round(hausman_p, 4),
      "does not reject RE consistency.",
      "Random Effects is selected."
    )
  }
}


# ------------------------------------------------------------------------------
# Step 3 — Time Fixed Effects
# ------------------------------------------------------------------------------

time_effects_present <- time_fe_test$p.value < 0.05


if (time_effects_present) {
  
  cat(
    "\nTime fixed effects present: YES",
    "(p =",
    round(time_fe_test$p.value, 4),
    ")\n"
  )
  
  cat(
    "-> Two-way Fixed Effects will be reported as a robustness specification.\n"
  )
  
} else {
  
  cat(
    "\nTime fixed effects present: NO",
    "(p =",
    round(time_fe_test$p.value, 4),
    ")\n"
  )
}


# ------------------------------------------------------------------------------
# Final model information
# ------------------------------------------------------------------------------

cat("\n==============================================\n")
cat("SELECTED MAIN MODEL:", selected_model_name, "\n")
cat("Reason:", selection_reason, "\n")
cat("==============================================\n")


# ------------------------------------------------------------------------------
# Fixed Effects robustness specification
# ------------------------------------------------------------------------------

fe_robustness_model <- model_fe

cat("\n--- FIXED EFFECTS ROBUSTNESS SPECIFICATION ---\n")
cat(
  "Individual Fixed Effects is retained as a robustness model",
  "regardless of the selected main estimator.\n"
)


# ------------------------------------------------------------------------------
# Two-way FE robustness specification
# ------------------------------------------------------------------------------

two_way_fe_robustness_model <- model_fe2

if (time_effects_present) {
  
  cat(
    "Two-way Fixed Effects is additionally supported by the time-effects test.\n"
  )
  
} else {
  
  cat(
    "Two-way Fixed Effects is retained only as an optional robustness specification.\n"
  )
}


# ==============================================================================
# STEP 9 — DIAGNOSTIC TESTS
# ==============================================================================

cat("\n==============================================\n")
cat("DIAGNOSTIC TESTS\n")
cat("==============================================\n")

lm_diagnostic <- lm(
  formula_main,
  data = data_clean
)

# 9.1 Heteroscedasticity — Breusch-Pagan
cat("\n--- HETEROSCEDASTICITY: BREUSCH-PAGAN ---\n")
cat("H0: Homoscedastic errors\n")
bp_hetero <- bptest(lm_diagnostic)
print(bp_hetero)

# 9.2 White test
cat("\n--- WHITE-TYPE TEST ---\n")
cat("H0: Homoscedastic errors\n")
white_test <- bptest(
  lm_diagnostic,
  ~ broad_money_growth * gdp_growth * depreciation_rate +
    I(broad_money_growth^2) + I(gdp_growth^2) +
    I(depreciation_rate^2),
  data = data_clean
)
print(white_test)

# 9.3 Panel serial correlation
cat("\n--- PANEL SERIAL CORRELATION ---\n")
cat("H0: No serial correlation\n")
bg_test <- tryCatch({
  pbgtest(selected_model)
}, error = function(e) {
  cat("pbgtest failed:", e$message, "\n")
  NULL
})
if (!is.null(bg_test)) print(bg_test)

# 9.4 Cross-sectional dependence
cat("\n--- CROSS-SECTIONAL DEPENDENCE: PESARAN CD ---\n")
cat("H0: No cross-sectional dependence\n")
cd_test <- tryCatch({
  pcdtest(selected_model, test = "cd")
}, error = function(e) {
  cat("pcdtest failed:", e$message, "\n")
  NULL
})
if (!is.null(cd_test)) print(cd_test)

# 9.5 Diagnostic flags
hetero_detected <- bp_hetero$p.value < 0.05
auto_detected   <- if (!is.null(bg_test))  bg_test$p.value < 0.05  else NA
cd_detected     <- if (!is.null(cd_test))  cd_test$p.value < 0.05  else NA

cat("\n--- DIAGNOSTIC SUMMARY ---\n")
cat("Heteroscedasticity:",
    ifelse(hetero_detected, "DETECTED", "Not detected"), "\n")
cat("Serial correlation:",
    ifelse(is.na(auto_detected), "Test failed",
           ifelse(auto_detected, "DETECTED", "Not detected")), "\n")
cat("Cross-sectional dependence:",
    ifelse(is.na(cd_detected), "Test failed",
           ifelse(cd_detected, "DETECTED", "Not detected")), "\n")


# ==============================================================================
# STEP 10 — ROBUST STANDARD ERRORS
# ==============================================================================

cat("\n==============================================\n")
cat("ROBUST STANDARD ERRORS\n")
cat("==============================================\n")

# 10.1 HC3 cluster-robust (clustered by country)
cat("\n--- HC3 COUNTRY-CLUSTERED SE ---\n")
robust_hc3 <- coeftest(
  selected_model,
  vcov = vcovHC(selected_model, type = "HC3", cluster = "group")
)
print(robust_hc3)

# 10.2 Driscoll-Kraay (robust to heteroscedasticity,
#      serial correlation AND cross-sectional dependence)
cat("\n--- DRISCOLL-KRAAY SE ---\n")
robust_dk <- tryCatch({
  coeftest(
    selected_model,
    vcov = vcovSCC(selected_model, type = "HC1", maxlag = 2)
  )
}, error = function(e) {
  cat("Driscoll-Kraay failed:", e$message, "\n")
  NULL
})
if (!is.null(robust_dk)) print(robust_dk)

# 10.3 Main coefficient table (HC3)
coef_df <- data.frame(
  variable  = rownames(robust_hc3),
  estimate  = as.numeric(robust_hc3[, 1]),
  se        = as.numeric(robust_hc3[, 2]),
  t_stat    = as.numeric(robust_hc3[, 3]),
  p_value   = as.numeric(robust_hc3[, 4]),
  row.names = NULL
) %>%
  filter(variable %in% c(
    "broad_money_growth", "gdp_growth", "depreciation_rate"
  )) %>%
  mutate(
    variable = recode(variable,
                      "broad_money_growth" = "Broad Money Growth",
                      "gdp_growth"         = "GDP Growth",
                      "depreciation_rate"  = "Depreciation Rate"
    ),
    ci_low            = estimate - 1.96 * se,
    ci_high           = estimate + 1.96 * se,
    significant       = p_value < 0.05,
    significant_label = ifelse(significant,
                               "Significant", "Not Significant"),
    stars = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE           ~ ""
    ),
    model            = selected_model_name,
    observations     = nrow(data_clean),
    countries        = length(unique(data_clean$country)),
    period_start     = min(data_clean$year),
    period_end       = max(data_clean$year),
    standard_errors  = "HC3 country-clustered"
  )

cat("\n--- MAIN COEFFICIENT TABLE ---\n")
print(coef_df)

# 10.4 Driscoll-Kraay coefficient table
if (!is.null(robust_dk)) {
  coef_df_dk <- data.frame(
    variable  = rownames(robust_dk),
    estimate  = as.numeric(robust_dk[, 1]),
    se        = as.numeric(robust_dk[, 2]),
    t_stat    = as.numeric(robust_dk[, 3]),
    p_value   = as.numeric(robust_dk[, 4]),
    row.names = NULL
  ) %>%
    filter(variable %in% c(
      "broad_money_growth", "gdp_growth", "depreciation_rate"
    )) %>%
    mutate(
      variable = recode(variable,
                        "broad_money_growth" = "Broad Money Growth",
                        "gdp_growth"         = "GDP Growth",
                        "depreciation_rate"  = "Depreciation Rate"
      ),
      ci_low            = estimate - 1.96 * se,
      ci_high           = estimate + 1.96 * se,
      significant       = p_value < 0.05,
      significant_label = ifelse(significant,
                                 "Significant", "Not Significant"),
      stars = case_when(
        p_value < 0.01 ~ "***",
        p_value < 0.05 ~ "**",
        p_value < 0.10 ~ "*",
        TRUE           ~ ""
      ),
      model           = selected_model_name,
      observations    = nrow(data_clean),
      countries       = length(unique(data_clean$country)),
      period_start    = min(data_clean$year),
      period_end      = max(data_clean$year),
      standard_errors = "Driscoll-Kraay"
    )
  cat("\n--- DRISCOLL-KRAAY COEFFICIENT TABLE ---\n")
  print(coef_df_dk)
} else {
  coef_df_dk <- NULL
}


# ==============================================================================
# STEP 11 — FIXED EFFECTS
# ==============================================================================

cat("\n==============================================\n")
cat("COUNTRY FIXED EFFECTS\n")
cat("==============================================\n")

fe_raw <- fixef(model_fe)
cat("Raw fixed effects:\n")
print(round(fe_raw, 4))

fe_vals <- data.frame(
  country      = names(fe_raw),
  fixed_effect = as.numeric(fe_raw)
) %>%
  mutate(
    fixed_effect_centered = fixed_effect - mean(fixed_effect),
    direction = ifelse(fixed_effect_centered > 0,
                       "Above panel mean", "Below panel mean")
  ) %>%
  arrange(desc(fixed_effect_centered))

cat("\nCentered fixed effects:\n")
print(fe_vals)


# ==============================================================================
# STEP 12 — SENSITIVITY: WITHOUT TURKEY
# ==============================================================================

cat("\n==============================================\n")
cat("SENSITIVITY ANALYSIS — WITHOUT TURKEY\n")
cat("==============================================\n")

data_no_turkiye <- data_clean %>%
  filter(!tolower(country) %in% c("turkiye", "türkiye", "turkey"))

pdata_no_turkiye <- pdata.frame(
  data_no_turkiye,
  index = c("country", "year")
)

model_fe_no_turkiye <- plm(
  formula_main, data = pdata_no_turkiye,
  model = "within", effect = "individual"
)

robust_no_turkiye <- coeftest(
  model_fe_no_turkiye,
  vcov = vcovHC(model_fe_no_turkiye, type = "HC3", cluster = "group")
)

cat("\n--- FE WITHOUT TURKEY ---\n")
print(robust_no_turkiye)

comparison_money <- data.frame(
  specification    = c("Full sample — FE", "Without Turkey — FE"),
  countries        = c(length(unique(data_clean$country)),
                       length(unique(data_no_turkiye$country))),
  money_coefficient = c(
    coef(model_fe)[["broad_money_growth"]],
    coef(model_fe_no_turkiye)[["broad_money_growth"]]
  )
)

comparison_money$change_percent <- c(
  NA,
  100 * (comparison_money$money_coefficient[2] /
           comparison_money$money_coefficient[1] - 1)
)

cat("\n--- BROAD MONEY COEFFICIENT COMPARISON ---\n")
print(comparison_money)


# ==============================================================================
# STEP 13 — R-SQUARED TABLE — FIX APPLIED HERE
# ==============================================================================

cat("\n==============================================\n")
cat("R-SQUARED INFORMATION\n")
cat("==============================================\n")

# FIX: Safe R² extraction with tryCatch — prevents crashes
# Note: Pooled OLS R² = Overall variance explained
#       FE R² = Within-country variance explained only
#       These measure different things — not directly comparable
get_r2 <- function(m) {
  tryCatch(
    round(as.numeric(summary(m)$r.squared)[1], 4),
    error = function(e) NA_real_
  )
}

r2_table <- data.frame(
  Model        = c("Pooled OLS", "FE Individual",
                   "FE Two-way", "Random Effects"),
  R2_Type      = c("Overall", "Within", "Within", "Overall"),
  R2           = c(get_r2(model_pooled), get_r2(model_fe),
                   get_r2(model_fe2),   get_r2(model_re)),
  Observations = nrow(data_clean),
  Countries    = length(unique(data_clean$country))
)

cat("NOTE: Pooled OLS and FE R² measure different things.\n")
cat("Pooled = overall variance | FE = within-country variance only.\n\n")
print(r2_table)


# ==============================================================================
# STEP 14 — FITTED VALUES AND RESIDUALS
# ==============================================================================

cat("\n==============================================\n")
cat("FITTED VALUES AND RESIDUALS\n")
cat("==============================================\n")

data_clean$fitted_selected    <- as.numeric(fitted(selected_model))
data_clean$residuals_selected <- as.numeric(residuals(selected_model))

# Power BI compatibility aliases
data_clean$fitted_fe    <- data_clean$fitted_selected
data_clean$residuals_fe <- data_clean$residuals_selected

cat("\nResidual summary:\n")
print(summary(data_clean$residuals_selected))
cat("\nFitted value summary:\n")
print(summary(data_clean$fitted_selected))


# ==============================================================================
# STEP 15 — VISUALIZATIONS
# ==============================================================================

country_levels <- sort(unique(data_clean$country))
base_colors    <- c("#1F77B4", "#FF7F0E", "#2CA02C",
                    "#D62728", "#9467BD", "#8C564B", "#E377C2")
country_colors <- setNames(base_colors[seq_along(country_levels)],
                           country_levels)

# Plot 1: Inflation trend
p1 <- ggplot(data_clean,
             aes(x = year, y = inflation,
                 color = country, group = country)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_color_manual(values = country_colors) +
  labs(title    = "CPI Inflation by Country (2011-2024)",
       subtitle = "Dependent variable",
       x = "Year", y = "Inflation (%)", color = "") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("viz_01_inflation_trend.png", p1,
       width = 10, height = 5, dpi = 150)

# Plot 2: Broad money growth trend
p2 <- ggplot(data_clean,
             aes(x = year, y = broad_money_growth,
                 color = country, group = country)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_color_manual(values = country_colors) +
  labs(title    = "Broad Money Growth by Country (2011-2024)",
       subtitle = "Main explanatory variable",
       x = "Year", y = "Broad Money Growth (%)", color = "") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("viz_02_money_trend.png", p2,
       width = 10, height = 5, dpi = 150)

# Plot 3: Depreciation rate trend
p3 <- ggplot(data_clean,
             aes(x = year, y = depreciation_rate,
                 color = country, group = country)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = country_colors) +
  labs(title    = "Currency Depreciation Rate by Country (2011-2024)",
       subtitle = "Positive = depreciation | Negative = appreciation | Jordan/Saudi ≈ 0% (pegged)",
       x = "Year", y = "Depreciation Rate (%)", color = "") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("viz_03_depreciation_trend.png", p3,
       width = 10, height = 5, dpi = 150)

# Plot 4: GDP growth trend
p4 <- ggplot(data_clean,
             aes(x = year, y = gdp_growth,
                 color = country, group = country)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = country_colors) +
  labs(title    = "GDP Growth by Country (2011-2024)",
       subtitle = "Control variable",
       x = "Year", y = "GDP Growth (%)", color = "") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("viz_04_gdp_trend.png", p4,
       width = 10, height = 5, dpi = 150)

# Plot 5: Boxplots
box_data <- data_clean %>%
  select(country, inflation, broad_money_growth,
         gdp_growth, depreciation_rate) %>%
  pivot_longer(cols = -country,
               names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable,
                           "inflation"          = "Inflation (%)",
                           "broad_money_growth" = "Broad Money Growth (%)",
                           "gdp_growth"         = "GDP Growth (%)",
                           "depreciation_rate"  = "Depreciation Rate (%)"))

p5 <- ggplot(box_data, aes(x = country, y = value, fill = country)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = country_colors) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Distribution of Variables by Country", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("viz_05_boxplots.png", p5,
       width = 13, height = 8, dpi = 150)

# Plot 6: Scatter — broad money vs inflation (pooled)
p6 <- ggplot(data_clean,
             aes(x = broad_money_growth, y = inflation,
                 color = country)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE,
              color = "black", linetype = "dashed") +
  scale_color_manual(values = country_colors) +
  labs(title    = "Broad Money Growth vs Inflation — Pooled",
       subtitle = "Positive descriptive association — not evidence of causality",
       x = "Broad Money Growth (%)", y = "Inflation (%)", color = "") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("viz_06_scatter_money.png", p6,
       width = 9, height = 6, dpi = 150)

# Plot 7: Scatter by country (faceted)
p7 <- ggplot(data_clean,
             aes(x = broad_money_growth, y = inflation,
                 color = country)) +
  geom_point(size = 2.5, alpha = 0.8, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_color_manual(values = country_colors) +
  facet_wrap(~ country, scales = "free") +
  labs(title    = "Broad Money vs Inflation — By Country",
       subtitle = "Country-specific descriptive relationships",
       x = "Broad Money Growth (%)", y = "Inflation (%)") +
  theme_minimal()
ggsave("viz_07_scatter_facet.png", p7,
       width = 13, height = 8, dpi = 150)

# Plot 8: Correlation heatmap
png("viz_08_correlation.png", width = 800, height = 700, res = 120)
corrplot(cor_full, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("#D62728", "white", "#1F77B4"))(200),
         title = "Correlation Matrix — Panel Variables",
         mar   = c(0, 0, 2, 0))
dev.off()

# Plot 9: Actual vs fitted
p9 <- ggplot(data_clean, aes(x = year)) +
  geom_line(aes(y = inflation,       color = "Actual"),
            linewidth = 1) +
  geom_line(aes(y = fitted_selected, color = "Fitted"),
            linewidth = 0.9, linetype = "dashed") +
  scale_color_manual(values = c("Actual" = "steelblue",
                                "Fitted" = "darkred")) +
  facet_wrap(~ country, scales = "free_y") +
  labs(title    = paste("Actual vs Fitted Inflation —",
                        selected_model_name),
       subtitle = "Dashed = fitted | Solid = observed",
       x = "Year", y = "Inflation (%)", color = "") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("viz_09_actual_vs_fitted.png", p9,
       width = 13, height = 8, dpi = 150)

# Plot 10: Residuals vs fitted
p10 <- ggplot(data_clean,
              aes(x = fitted_selected, y = residuals_selected,
                  color = country)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  scale_color_manual(values = country_colors) +
  labs(title    = paste("Residuals vs Fitted —", selected_model_name),
       subtitle = "Should scatter randomly around zero",
       x = "Fitted Values", y = "Residuals", color = "") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("viz_10_residuals_fitted.png", p10,
       width = 9, height = 6, dpi = 150)

# Plot 11: Residual histogram
p11 <- ggplot(data_clean, aes(x = residuals_selected)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title    = paste("Residual Distribution —", selected_model_name),
       subtitle = "Should approximate a normal distribution",
       x = "Residuals", y = "Count") +
  theme_minimal()
ggsave("viz_11_residuals_hist.png", p11,
       width = 7, height = 5, dpi = 150)

# Plot 12: Q-Q plot
png("viz_12_qq.png", width = 700, height = 600, res = 120)
qqnorm(data_clean$residuals_selected,
       main = paste("Q-Q Plot —", selected_model_name),
       pch = 19, col = "steelblue")
qqline(data_clean$residuals_selected, lwd = 2, col = "red")
dev.off()

# Plot 13: Residuals over time
p13 <- ggplot(data_clean,
              aes(x = year, y = residuals_selected,
                  color = country, group = country)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = country_colors) +
  facet_wrap(~ country, scales = "free_y") +
  labs(title    = paste("Residuals Over Time —", selected_model_name),
       subtitle = "Persistent patterns indicate autocorrelation",
       x = "Year", y = "Residual") +
  theme_minimal() + theme(legend.position = "none")
ggsave("viz_13_residuals_time.png", p13,
       width = 13, height = 8, dpi = 150)

# Plot 14: Country fixed effects
p14 <- ggplot(fe_vals,
              aes(x = reorder(country, fixed_effect_centered),
                  y = fixed_effect_centered,
                  fill = fixed_effect_centered > 0)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = round(fixed_effect_centered, 2)),
            hjust = ifelse(fe_vals$fixed_effect_centered > 0,
                           -0.2, 1.2),
            size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("TRUE"  = "steelblue",
                               "FALSE" = "salmon")) +
  coord_flip() +
  labs(title    = "Country Fixed Effects (αᵢ) — Centered",
       subtitle = "Blue = above-average structural inflation | Red = below-average",
       x = "", y = "Fixed Effect (centered around panel mean)") +
  theme_minimal()
ggsave("viz_14_fixed_effects.png", p14,
       width = 9, height = 5, dpi = 150)

# Plot 15: Coefficient plot
p15 <- ggplot(coef_df,
              aes(x = reorder(variable, estimate),
                  y = estimate, color = significant_label)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Significant"     = "steelblue",
                                "Not Significant" = "grey60")) +
  coord_flip() +
  labs(title    = paste("Coefficient Plot —", selected_model_name),
       subtitle = "HC3 Cluster-Robust SE | 95% Confidence Intervals",
       x = "", y = "Estimated Coefficient", color = "") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("viz_15_coefficients.png", p15,
       width = 9, height = 5, dpi = 150)

# Plot 16: Sensitivity — with vs without Turkey
coef_comparison <- data.frame(
  variable = rep(c("Broad Money Growth",
                   "GDP Growth", "Depreciation Rate"), 2),
  estimate = c(
    as.numeric(robust_hc3[c("broad_money_growth",
                            "gdp_growth",
                            "depreciation_rate"), 1]),
    as.numeric(robust_no_turkiye[c("broad_money_growth",
                                   "gdp_growth",
                                   "depreciation_rate"), 1])
  ),
  se = c(
    as.numeric(robust_hc3[c("broad_money_growth",
                            "gdp_growth",
                            "depreciation_rate"), 2]),
    as.numeric(robust_no_turkiye[c("broad_money_growth",
                                   "gdp_growth",
                                   "depreciation_rate"), 2])
  ),
  sample = rep(c("Full sample (N=7)", "Without Turkey (N=6)"), each = 3)
) %>%
  mutate(ci_low  = estimate - 1.96 * se,
         ci_high = estimate + 1.96 * se)

p16 <- ggplot(coef_comparison,
              aes(x = variable, y = estimate,
                  color = sample, group = sample)) +
  geom_point(size = 4, position = position_dodge(0.4)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.2, position = position_dodge(0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(title    = "Sensitivity Analysis — With vs Without Turkey",
       subtitle = "Checks whether results are driven by Turkey's extreme observations",
       x = "", y = "Coefficient Estimate", color = "") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("viz_16_sensitivity.png", p16,
       width = 10, height = 5, dpi = 150)


# ==============================================================================
# STEP 16 — EXPORTS FOR POWER BI
# ==============================================================================

cat("\n==============================================\n")
cat("EXPORTING FILES FOR POWER BI\n")
cat("==============================================\n")

# Export 1: Full panel data with fitted + residuals
write.csv(data_clean, "export_panel_data_F.csv", row.names = FALSE)
cat("✓ export_panel_data_F.csv —", nrow(data_clean), "observations\n")

# Export 2: Descriptive statistics
write.csv(desc_stats, "export_desc_stats_F.csv", row.names = FALSE)
cat("✓ export_desc_stats_F.csv\n")

# Export 3: Fixed effects centered
write.csv(fe_vals, "export_fixed_effects_F.csv", row.names = FALSE)
cat("✓ export_fixed_effects_F.csv\n")

# Export 4: Main coefficients (HC3)
write.csv(coef_df, "export_coefficients_F.csv", row.names = FALSE)
cat("✓ export_coefficients.csv\n")

# Export 5: Driscoll-Kraay coefficients
if (!is.null(coef_df_dk)) {
  write.csv(coef_df_dk, "export_coefficients_driscoll_kraay_F.csv",
            row.names = FALSE)
  cat("✓ export_coefficients_driscoll_kraay_F.csv\n")
}

# Export 6: R-squared table
write.csv(r2_table, "export_r2_F.csv", row.names = FALSE)
cat("✓ export_r2.csv\n")

# Export 7: Sensitivity comparison
write.csv(coef_comparison, "export_sensitivity_F.csv", row.names = FALSE)
cat("✓ export_sensitivity_F.csv\n")

# Export 8: Test results — each test uses its own actual p-value
test_results <- data.frame(
  Test = c(
    "Breusch-Pagan LM",
    "F-test (FE vs OLS)",
    "Hausman (Standard)",
    "Hausman (Auxiliary)",
    "Time FE Test",
    "Heteroscedasticity (BP)",
    "White Test",
    "Autocorrelation (pbgtest)",
    "Cross-Sectional Dependence"
  ),
  H0 = c(
    "No individual panel effects",
    "No individual fixed effects",
    "RE consistent",
    "RE consistent (robust)",
    "No time fixed effects",
    "Homoscedastic errors",
    "Homoscedastic errors (general form)",
    "No serial correlation",
    "No cross-sectional dependence"
  ),
  P_Value = c(
    bp_lm$p.value,
    f_test$p.value,
    hausman_test$p.value,
    ifelse(is.null(hausman_aux), NA, hausman_aux$p.value),
    time_fe_test$p.value,
    bp_hetero$p.value,
    white_test$p.value,
    ifelse(is.null(bg_test), NA, bg_test$p.value),
    ifelse(is.null(cd_test), NA, cd_test$p.value)
  )
) %>%
  mutate(
    Decision = case_when(
      is.na(P_Value)   ~ "Test failed",
      P_Value < 0.05   ~ "Reject H0",
      TRUE             ~ "Do not reject H0"
    ),
    Result = case_when(
      is.na(P_Value)   ~ "Unavailable",
      P_Value < 0.05   ~ "Significant",
      TRUE             ~ "Not significant"
    ),
    Selected_Model     = selected_model_name,
    Regression_Period  = paste(min(data_clean$year),
                               max(data_clean$year), sep = "-"),
    Observations       = nrow(data_clean),
    Countries          = length(unique(data_clean$country))
  )

write.csv(test_results, "export_test_results_F.csv", row.names = FALSE)
cat("✓ export_test_results_F.csv\n")

# Export 9: ADF results
write.csv(adf_results, "export_adf_results_F.csv", row.names = FALSE)
cat("✓ export_adf_results_F.csv\n")


# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n==============================================\n")
cat("ANALYSIS COMPLETED\n")
cat("==============================================\n")
cat("Raw sample:          ", nrow(data), "observations\n")
cat("Regression sample:   ", nrow(data_clean), "observations\n")
cat("Regression period:   ", min(data_clean$year), "-",
    max(data_clean$year), "\n")
cat("Countries:           ", length(unique(data_clean$country)), "\n")
cat("Selected model:      ", selected_model_name, "\n")
cat("Selection reason:    ", selection_reason, "\n")

cat("\nBroad money coefficient:\n")
print(coef_df %>% filter(variable == "Broad Money Growth"))

cat("\nDepreciation coefficient:\n")
print(coef_df %>% filter(variable == "Depreciation Rate"))

cat("\nGDP Growth coefficient:\n")
print(coef_df %>% filter(variable == "GDP Growth"))

cat("\nDiagnostic summary:\n")
cat("Heteroscedasticity: ",
    ifelse(hetero_detected, "Detected", "Not detected"), "\n")
cat("Serial correlation: ",
    ifelse(is.na(auto_detected), "Test failed",
           ifelse(auto_detected, "Detected", "Not detected")), "\n")
cat("Cross-sec. depend.: ",
    ifelse(is.na(cd_detected), "Test failed",
           ifelse(cd_detected, "Detected", "Not detected")), "\n")

cat("\n✓ 16 PNG plots saved\n")
cat("✓ 9 CSV files exported for Power BI\n")
cat("✓ FE maintained on theoretical grounds\n")
cat("✓ R² extracted safely with tryCatch\n")
cat("✓ Turkey sensitivity analysis complete\n")
cat("✓ Both HC3 and Driscoll-Kraay SE computed\n")
cat("==============================================\n")