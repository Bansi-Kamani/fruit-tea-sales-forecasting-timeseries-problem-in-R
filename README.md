# fruit-tea-sales-forecasting-timeseries-problem-in-R

**Course:** Applied Statistics and Business Forecasting
**University:** University of Manchester
**Author:** Bansi Kamani

---

## Overview
This project analyses fruit tea sales data across 5 regions (USA, UK, Spain, Mexico, and Singapore) using R. It covers exploratory data analysis, linear regression modelling, marketing cost-effectiveness evaluation, and time-series forecasting.

---

## Project Structure

| File | Description |
|------|-------------|
| `Fruit_Tea_Sales_Analysis_Report.pdf` | Full written report with methodology and findings |
| `Fruit_Tea_Sales_Presentation.pdf` | Presentation slides summarising the project |
| `BMAN71791_FruitTea_Analysis.R` | Complete R code for all analysis |

---

## Key Questions Answered

**Q1 — How do marketing activities affect sales?**
Built a linear regression model with log-transformed sales as the response variable. TV ads (Ad1), online banner ads (Ad2), and store promotions (Ad3) all showed a positive relationship with sales.

**Q2 — Which advertising method is most cost-effective?**
Compared TV ads (£2,000,000) vs. banner ads (£500,000) using a cost-effectiveness ratio. Banner ads were found to be significantly more cost-effective.

**Q3 — What other factors drive sales variation?**
Identified three key non-marketing factors:
- **Seasonality** — Sales peak in summer months and dip from November to April
- **Wage percentage** — Regions with higher wages tend to have higher sales
- **Population size** — Larger populations (e.g. USA) correlate with higher sales volume

**Q4 — Can we forecast future sales?**
Tested four time-series forecasting methods across all 5 regions:
- Simple Naïve Forecasting
- Naïve Seasonal Forecasting ✅ *(best performer)*
- Seasonal + Trend Decomposition
- Seasonal + Trend + Remainder Decomposition

Naïve Seasonal Forecasting achieved the lowest MAPE across most regions and was used for final predictions.

---

## Tools & Libraries Used

- **Language:** R
- **Libraries:** `ggplot2`, `olsrr`, `lmtest`, `nortest`, `aTSA`, `forecast`, `astsa`

---

## Key Results

| Region | Best Model MAPE (Validation) |
|--------|------------------------------|
| USA | 64.29% |
| Spain | 48.66% |
| UK | 78.06% |
| Mexico | 146.81% |
| Singapore | 93.83% |

---

## Note
The dataset is not included in this repository as it was provided by the University of Manchester for academic purposes only.
