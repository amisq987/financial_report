# FestMan Stores Financial Report

# Table of Contents
1. [Scope](#scope)
2. [Project Objectives](#project_objectives)
3. [Technology Used](#technology_used)
4. [Dataset](#dataset)
5. [Data Loading and Cleaning](#data_loading_and_cleaning)
6. [Unveiling the Financial Narrative of the Company](#unveiling_the_financial_narrative_of_the_company)
7. [Conclusion](#conclusion)
8. [Dashboard](#dashboard)

# Scope
As a Junior Data Analyst at Festman Stores, your task is to assess the company’s financial health using historical sales, expense, and store performance data. The leadership team wants to know where the business is thriving, where it's struggling, and what strategic actions can drive growth and profitability. Your analysis will uncover key insights into revenue trends, cost patterns, store and product performance, and highlight any unexpected trends or anomalies—supported by clear, data-driven visualizations to guide executive decisions on expansion, resource allocation, and operational improvements.

# Project Objectives
To evaluate the financial health of the company by analyzing sales and profit performance across customer segments, products, geographies, and time periods. The goal is to uncover where the business is thriving, identify areas of concern—such as unprofitable Enterprise deals and discount impacts—and provide strategic recommendations to improve profitability, optimize pricing, and capitalize on high-margin opportunities and seasonal trends.

# Technology Used
- R
- Power BI

# Dataset
- [FestMan Stores Financial Dataset](Financial dataset.xlsx)
The dataset contains financial transaction data for a company selling six products (Carretera, Montana, Paseo, Velo, VTT, Amarilla) across five countries (Canada, France, Germany, Mexico, USA) to five customer segments (Government, Midmarket, Channel Partners, Enterprise, Small Business) from 2013 to 2014.

# Data Loading and Cleaning
```R
# Loading required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# Loading the dataset
data <- read_excel("Financial dataset.xlsx")
```
```R
# Cleaning data: Ensure numeric columns are properly typed and handle missing values
data <- data %>%
  mutate(
    `Units Sol` = as.numeric(`Units Sold`),
    `Manufacturing Price` = as.numeric(`Manufacturing Price`),
    `Sale Price` = as.numeric(`Sale Price`),
    `Gross Sales` = as.numeric(`Gross Sales`),
    Discounts = as.numeric(Discounts),
    Sales = as.numeric(Sales),
    COGS = as.numeric(COGS),
    Profit = as.numeric(Profit),
    Date = as.Date(Date, origin = "1899-12-30"),
    `Month Number` = as.integer(`Month Number`),
    Year = as.integer(Year)
  ) %>%
  filter(!is.na(Sales) & !is.na(Profit)) # Remove rows with missing sales or profit
```
# Unveiling the Financial Narrative of the Company
Our first step is to assess the overall financial performance.

## The Big Picture - Sales and Profit Overview
```R
# Calculating overall summary statistics
overall_summary <- data %>%
  summarise(
    `Total Sales` = sum(Sales, na.rm = TRUE),
    `Total Profit` = sum(Profit, na.rm = TRUE),
    `Total Discounts` = sum(Discounts, na.rm = TRUE),
    `Average Profit Margin` = mean(Profit / Sales * 100, na.rm = TRUE)
  )

# Printing overall summary
cat("Overall Financial Summary:\n")
cat(sprintf("Total Sales: $%s\n", format(overall_summary$`Total Sales`, big.mark = ",")))
cat(sprintf("Total Profit: $%s\n", format(overall_summary$`Total Profit`, big.mark = ",")))
cat(sprintf("Total Discounts: $%s\n", format(overall_summary$`Total Discounts`, big.mark = ",")))
cat(sprintf("Average Profit Margin: %.2f%%\n", overall_summary$`Average Profit Margin`))
```
<center>
      <img src="png/table.info1.png" width="900" />
  </center>
  
- The dataset reveals **total sales** of approximately **$26.6 million**.
- **Profits** total **$5.5 million**, **after accounting** for **$1.4 million in discounts**.
- This indicates a **healthy revenue stream** but a **profit margin of around 21%**, pointing to **variations in profitability** across segments.
- Discount levels vary across transactions: "None," "Low," "Medium," and "High".
> These variations prompt further analysis into the impact of discounts on overall profitability.

## Segment Spotlight - Who Drives the Profits?
```R
# Analyzing sales and profit by segment
segment_summary <- data %>%
  group_by(Segment) %>%
  summarise(
    `Total Sales` = sum(Sales, na.rm = TRUE),
    `Total Profit` = sum(Profit, na.rm = TRUE),
    `Profit Margin` = mean(Profit / Sales * 100, na.rm = TRUE)
  ) %>%
  arrange(desc(`Total Sales`))

# Visualizing sales by segment
sales_by_segment_plot <- ggplot(segment_summary, aes(x = reorder(Segment, `Total Sales`), y = `Total Sales`)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = 'Total Sales by Segment', x = 'Segment', y = 'Sales ($)') +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal()
```
```R
# Visualizing profit margin by segment
profit_margin_segment_plot <- ggplot(segment_summary, aes(x = reorder(Segment, `Profit Margin`), y = `Profit Margin`)) +
 geom_bar(stat = "identity", aes(fill = ifelse(`Profit Margin` > 0, "red", "forestgreen")))+ 
coord_flip() + 
labs(title = "Profit Margin by Segment", x = "Segment", y = "Profit Margin (%)",fill = "Profit Margin Status") + 
theme_minimal()
```
> Breaking down the data by customer segment—Government, Midmarket, Channel Partners, Enterprise, and Small Business—we find that:
- The **Small Business** and **Government** segments are the **top contributors**, accounting for **38%** and **34%** of total sales, respectively. However, **profitability varies significantly** across segments.
- **Small Business** delivers a strong **26% profit margin**. In contrast, the **Enterprise** segment operates at a **loss**, with a **-3%** profit margin. The loss in the Enterprise segment is primarily due to **high COGS**, especially for **high-priced products like VTT and Amarilla**.
> This highlights the need to reassess pricing strategies or cost structures for Enterprise clients to improve profitability.

## Geographic Gems and Challenges
```R
# Analyzing sales and profit by country
country_summary <- data %>%
  group_by(Country) %>%
  summarise(
    `Total Sales` = sum(Sales, na.rm = TRUE),
    `Total Profit` = sum(Profit, na.rm = TRUE),
    `Profit Margin` = mean(Profit / Sales * 100, na.rm = TRUE)
  ) %>%
  arrange(desc(`Total Sales`))

# Visualizing sales by country
sales_by_country_plot <- ggplot(country_summary, aes(x = reorder(Country, `Total Sales`), y = `Total Sales`)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  labs(title = "Total Sales by Country", x = "Country", y = "Sales ($)") +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal()
```
```R
# Visualizing profit margin by country
profit_margin_country_plot <- ggplot(country_summary, aes(x = reorder(Country, `Profit Margin`), y = `Profit Margin`)) +
 geom_bar(stat = "identity", aes(fill = ifelse(`Profit Margin` > 0, "red", "forestgreen")))+ 
coord_flip() + 
labs(title = "Profit Margin by Country", x = "Country", y = "Profit Margin (%)",fill = "Profit Margin Status") + 
theme_minimal()
```
- **Germany** and the **United States** lead in sales volume, indicating **strong market presence**.
- **Canada** stands out in profitability, achieving a **30% profit margin**, largely due to high-value Government contracts.
- **Mexico** shows **strong sales but a lower profit margin (18%)**, impacted by **heavy discounts** in **Small Business and Government** segments.
- **France** maintains a **steady but modest performance**, suggesting **untapped potential and room for market growth**.

## Product Performance - Stars and Stragglers
```R
# Analyzing sales and profit by product
product_summary <- data %>%
  group_by(Product) %>%
  summarise(
    `Total Sales` = sum(Sales, na.rm = TRUE),
    `Total Profit` = sum(Profit, na.rm = TRUE),
    `Profit Margin` = mean(Profit / Sales * 100, na.rm = TRUE)
  ) %>%
  arrange(desc(`Total Sales`))

# Visualizing sales by country
sales_by_product_plot <- ggplot(product_summary, aes(x = reorder(Product, `Total Sales`), y = `Total Sales`)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Total Sales by Product", x = "Product", y = "Sales ($)") +
  scale_y_continuous(labels = dollar_format()) +
  theme_minimal()
```

```R
# Visualizing profit margin by product
profit_margin_product_plot <- ggplot(product_summary, aes(x = reorder(Product, `Profit Margin`), y = `Profit Margin`)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Profit Margin by Product", x = "Product", y = "Profit Margin (%)") +
  theme_minimal()
```
- VTT and Amarilla lead in sales volume, driven by their high sale prices ($250–$300).
- Carretera and Montana offer higher profit margins (30% and 28%) due to lower manufacturing costs.
- Paseo, a mid-tier product, is a consistent performer across segments, making it a reliable cash cow.
- Velo shows inconsistent profitability, especially in Enterprise deals, where discounts significantly reduce margins.

## The Discount Dilemma
```R
# Analyzing impact of discounts
discount_summary <- data %>%
  group_by(`Discount Band`) %>%
  summarise(
    `Total Sales` = sum(Sales, na.rm = TRUE),
    `Total Profit` = sum(Profit, na.rm = TRUE),
    `Average Discount` = mean(Discounts / `Gross Sales` * 100, na.rm = TRUE),
    `Profit Margin` = mean(Profit / Sales * 100, na.rm = TRUE)
  ) %>%
  arrange(desc(`Average Discount`))

# Total Sales and Profit by Discount Band
sales_profit_plot <- ggplot(discount_summary, aes(x = `Discount Band`)) +
  geom_bar(aes(y = `Total Sales`, fill = "Sales"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = `Total Profit`, fill = "Profit"), stat = "identity", position = "dodge") +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Sales vs Profit by Discount Band", y = "Amount ($)", x = "Discount Band") +
  scale_fill_manual(name = "Metric", values = c("Sales" = "steelblue", "Profit" = "firebrick")) +
  theme_minimal()
```
```R
# Discounts vs. Profit Margin Scatter Plot (by deal)
scatter_plot <- data %>%
  mutate(
    `Discount %` = Discounts / `Gross Sales` * 100,
    `Profit Margin %` = Profit / Sales * 100
  ) %>%
  ggplot(aes(x = `Discount %`, y = `Profit Margin %`, color = Segment)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Discount % vs. Profit Margin %", x = "Discount (%)", y = "Profit Margin (%)") +
  theme_minimal()
```
- Discounts, ranging from 0% to 15%, have a significant impact on profitability.
- High-discount deals, particularly in the Enterprise segment, often lead to negative profits. For example, a $305,125 sale of Carretera to Enterprise clients in France (October 2014) resulted in a $21,358 loss due to a 15% discount.
> This highlights the need for a more strategic discount policy to protect profit margins.

## Temporal Trends - Seasonal Success
```R
# Analyzing sales trends over time
monthly_sales <- data %>%
  group_by(Year, `Month Name`) %>%
  summarise(`Total Sales` = sum(Sales, na.rm = TRUE)) %>%
  mutate(Date = as.Date(paste(Year, `Month Name`, "01", sep = "-"), format = "%Y-%B-%d"))

# Visualizing sales trends over time
sales_trend_plot <- ggplot(monthly_sales, aes(x = Date, y = `Total Sales`)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Sales Trend Over Time", x = "Date", y = "Sales ($)") +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month") +
  theme_minimal()
```
- Sales peak in December each year, primarily driven by Government contracts.
- June experiences a surge in activity from Midmarket and Channel Partners.
- Profitability dips in Q3 (July–September) due to aggressive discounting during this period.
> This seasonal pattern suggests the need to align marketing efforts with high-demand periods and tighten discount controls in Q3.

## The Unexpected Twist 
```R
# Printing segment summary for reference
cat("\nSegment Summary:\n")
print(segment_summary)

# Printing country summary for reference
cat("\nCountry Summary:\n")
print(country_summary)

# Printing product summary for reference
cat("\nProduct Summary:\n")
print(product_summary)

# Printing discount summary for reference
cat("\nDiscount Summary:\n")
print(discount_summary)
```
An intriguing finding: Channel Partners, despite low sales prices ($12), achieve a 66% profit margin due to low COGS. This segment, while small in revenue, is a hidden gem for profitability.

# Conclusion
The company is financially robust but faces challenges in Enterprise profitability and discount management. To optimize performance:
1. **Reevaluate Enterprise Pricing**: Adjust pricing or reduce discounts to ensure positive margins.
2. **Expand Channel Partners**: Leverage their high margins by increasing their share of sales.
3. **Target Canada and Small Business**: Focus on these high-margin markets and segments.
4. **Refine Discount Strategy**: Limit high discounts, especially in low-margin segments like Enterprise.
5. **Capitalize on Seasonal Trends**: Boost marketing in Q4 to maximize Government contract revenue.
This financial story sets the stage for data-driven decisions to steer the company toward greater profitability.

# Dashboard
- [Link to dashboard](https://app.powerbi.com/groups/me/reports/fcedc79e-ea46-43c2-b973-be45ab0993d8?ctid=588b13a3-653b-4cf7-8ac0-59847eb2dc88&pbi_source=linkShare)
