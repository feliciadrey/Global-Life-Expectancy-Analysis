# Global-Life-Expectancy-Analysis

Comprehensive exploratory data analysis (EDA) of global life expectancy (2001–2019) using R, featuring an interactive Shiny dashboard and an infographic summarizing key findings.

---

## Overview
This project was developed for the **Data Mining & Visualization** course (2nd semester) and focuses on analyzing global life expectancy trends and their socioeconomic determinants. The work combines in-depth statistical exploration, interactive visualizations, and clear communication of insights through a public-facing infographic.

**Deliverables include:**
- **EDA in R Markdown** – Trend analysis, factor correlations, and statistical summaries.
- **Shiny dashboard** – Interactive exploration by region, income group, and influencing factor.
- **Infographic** – Concise visual storytelling for non-technical audiences.
- **Documentation** – Feature-by-feature guide to the dashboard


## 📊 Data

The dataset combines information from **World Bank Open Data** and **Our World in Data**, covering **174 countries** from **2000 to 2019**.  
It contains socioeconomic, health, and environmental indicators relevant to the analysis of global life expectancy.

**Key Information**
- **Countries:** 174 (with 3-letter ISO codes, grouped by region and income level)  
- **Time Period:** 2000–2019 (inclusive)  
- **Entries:** ~3,000 country–year observations  

### Data Dictionary

| Variable | Description |
|----------|-------------|
| **Country** | Country name |
| **Country Code** | 3-letter ISO country code |
| **Region** | Geographical region of the country |
| **IncomeGroup** | World Bank income classification |
| **Year** | Observation year (2000–2019) |
| **Life Expectancy** | Average number of years a newborn is expected to live under current mortality rates |
| **Prevalence of Undernourishment (%)** | % of the population with insufficient dietary energy intake |
| **CO₂ Emissions (kilotons)** | Emissions from fossil fuel use and cement production |
| **Health Expenditure (% of GDP)** | Annual spending on healthcare goods and services, excluding capital investments |
| **Education Expenditure (% of GDP)** | Government spending on education (current, capital, and transfers) |
| **Unemployment (%)** | % of the labor force unemployed but actively seeking work |
| **Corruption (CPIA rating)** | Transparency, accountability, and corruption rating in the public sector |
| **Sanitation (% of population)** | Population using safely managed sanitation services |
| **DALYs – Injuries** | Years of healthy life lost due to injuries |
| **DALYs – Communicable** | Years of healthy life lost due to communicable diseases |
| **DALYs – Non-Communicable** | Years of healthy life lost due to non-communicable diseases |


## 🚀 Features
- Interactive filtering by **region**, **income group**, and factor
- Linear regression with dynamic charts
- Key indicators including average life expectancy and total data entries
- Dashboard design optimized for exploration and presentation

## 🛠️ Tools & Technologies
- **R** – data analysis and visualization
- **R Markdown** – reporting and documentation
- **Shiny** – interactive dashboard
- **Plotly** – interactive charts
- **Canva** – infographic design


