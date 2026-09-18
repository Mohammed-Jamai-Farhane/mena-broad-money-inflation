# 📊 Broad Money Growth and Inflation in MENA Countries (2010–2024)

## 📌 Project Overview

This project investigates the relationship between **broad money growth and inflation** in seven selected MENA countries over the period **2010–2024**.

The analysis combines:

- 📈 Macroeconomic analysis
- 📊 Panel-data econometrics
- 🧮 Statistical diagnostics
- 💻 R programming
- 📉 Power BI data visualization
- 📄 LaTeX research reporting

### Research Question

> To what extent is broad money growth associated with inflation in selected MENA countries after controlling for GDP growth and exchange-rate depreciation?

---

## 🌍 Countries Covered

The study includes seven countries:

| Country |
|---|
| 🇩🇿 Algeria |
| 🇪🇬 Egypt |
| 🇯🇴 Jordan |
| 🇲🇦 Morocco |
| 🇸🇦 Saudi Arabia |
| 🇹🇳 Tunisia |
| 🇹🇷 Türkiye |

---

## 📅 Dataset

The raw dataset covers:

- **Period:** 2010–2024
- **Countries:** 7
- **Raw observations:** 105

Because the depreciation rate requires the previous year's exchange rate, the econometric sample covers:

- **Regression period:** 2011–2024
- **Observations:** 98
- **Panel structure:** Balanced panel
- **7 × 14 = 98 observations**

---

## 📐 Variables

| Variable | Description |
|---|---|
| `inflation` | CPI inflation rate (%) |
| `broad_money_growth` | Annual broad money growth (%) |
| `gdp_growth` | Real GDP growth (%) |
| `exchange_rate` | Local currency per USD |
| `depreciation_rate` | Annual exchange-rate depreciation (%) |

The depreciation rate is calculated as:

\[
Depreciation_{it}
=
100
\left(
\frac{ER_{it}}{ER_{i,t-1}}-1
\right)
\]

A positive value represents a depreciation of the domestic currency.

---

## 🧮 Econometric Model

The main empirical specification is:

\[
Inflation_{it}
=
\alpha
+
\beta_1 MoneyGrowth_{it}
+
\beta_2 GDPGrowth_{it}
+
\beta_3 Depreciation_{it}
+
\varepsilon_{it}
\]

The following models are estimated:

- Pooled OLS
- Individual Fixed Effects
- Two-Way Fixed Effects
- Random Effects
- Between Estimator

---

## 📊 Main Results

The main pooled OLS model with country-clustered HC3 standard errors gives:

| Variable | Coefficient | p-value | Result |
|---|---:|---:|---|
| **Broad Money Growth** | **0.436** | **<0.001** | ✅ Significant |
| GDP Growth | -0.148 | 0.541 | ❌ Not significant |
| **Depreciation Rate** | **0.377** | **<0.001** | ✅ Significant |

### Interpretation

The coefficient of **0.436** means that a one-percentage-point increase in broad money growth is associated with approximately a **0.436 percentage-point increase in inflation**, holding GDP growth and depreciation constant.

For example:

\[
0.436 \times 5 = 2.18
\]

A 5-percentage-point increase in broad money growth is therefore associated with approximately **2.18 percentage points higher inflation**, ceteris paribus.

⚠️ This is an **association**, not a causal elasticity.

---

## 🔎 Key Robustness Finding: Türkiye

Türkiye is a major high-inflation, high-money-growth and high-depreciation observation in the sample.

The broad-money coefficient changes substantially when Türkiye is excluded:

| Specification | Broad Money Coefficient |
|---|---:|
| Full sample | **0.436** |
| Without Türkiye | **0.055** |

This represents an attenuation of approximately **87.5%**.

The confidence interval of the Türkiye-excluded coefficient includes zero.

### Interpretation

This means that the positive regional money-growth relationship is strongly influenced by Türkiye.

Therefore, the analysis does **not** support the interpretation that broad money has a homogeneous causal effect on inflation across all MENA countries.

---

## 🧪 Econometric Diagnostics

The analysis identifies several important issues in the error structure:

| Test | p-value | Conclusion |
|---|---:|---|
| Breusch-Pagan LM | 0.1021 | No significant panel effects |
| F-test FE vs OLS | 0.9838 | Pooled OLS not rejected |
| Hausman Test | 0.9968 | RE not rejected |
| Time FE Test | 0.0819 | No time effects at 5% |
| BP Heteroskedasticity | <0.001 | ⚠️ Detected |
| White-type Test | <0.001 | ⚠️ Detected |
| Panel BG Test | 0.0075 | ⚠️ Serial correlation |
| Pesaran CD | 0.0333 | ⚠️ Cross-sectional dependence |

Because of these issues, the analysis uses robust inference:

- **HC3 country-clustered standard errors**
- **Driscoll–Kraay standard errors**

---

## 📈 Power BI Dashboard

The project includes a Power BI dashboard presenting:

### 1. Cross-country comparison
Comparison of average inflation, broad money growth, GDP growth and depreciation.

### 2. Time-series analysis
Evolution of inflation, money growth, GDP growth and exchange-rate depreciation over time.

### 3. Bivariate relationships
Broad money growth versus inflation at both pooled and country-specific levels.

### 4. Econometric results
Estimated coefficients, model fit and country fixed effects.

### 5. Diagnostic analysis
Residuals, model diagnostics and robustness results.

---


The analysis also considers:

- Exchange-rate pass-through
- Aggregate demand
- Inflation expectations
- Monetary credibility
- Fiscal-monetary interactions
- Country-specific institutional differences

---

## 🗂️ Repository Structure

```text
mena-broad-money-inflation/
│
├── README.md
├── .gitignore
│
├── data/
│   ├── mena_panel_data.csv
│   └── data_dictionary.md
│
├── R/
│   └── mena_analysis_script.R
│
├── dashboard/
│   └── MENA_analysis_Dashboard.pdf
│
├── report/
│   └── MENA_analysis_Report.pdf
│
├── outputs/
│   ├── export_panel_data_F.csv
│   ├── export_desc_stats_F.csv
│   ├── export_fixed_effects_F.csv
│   ├── export_coefficients_F.csv
│   ├── export_coefficients_driscoll_kraay_F.csv
│   ├── export_r2_F.csv
│   ├── export_sensitivity_F.csv
│   ├── export_test_results_F.csv
│   └── export_adf_results_F.csv
│
└── figures/
    ├── viz_01_inflation_trend.png
    ├── viz_02_money_trend.png
    ├── viz_03_depreciation_trend.png
    ├── viz_04_gdp_trend.png
    ├── viz_05_boxplots.png
    ├── viz_06_scatter_money.png
    ├── viz_07_scatter_facet.png
    ├── viz_08_correlation.png
    ├── viz_09_actual_vs_fitted.png
    ├── viz_10_residuals_fitted.png
    ├── viz_11_residuals_hist.png
    ├── viz_12_qq.png
    ├── viz_13_residuals_time.png
    ├── viz_14_fixed_effects.png
    ├── viz_15_coefficients.png
    └── viz_16_sensitivity.png
