---
title: "Mapping Child Welfare"
subtitle: "Analysing Childhood Deprivation alongside Economic and Health Factors"
author: "Arnaud Saccomano"
date: "2024-04-03"
format: 
 html:
   theme: quartz
   backgroundcolor: lightgrey
   toc: true

execute:
  echo: false
  warning: false
  message: false
---

```{r}
#| label: setup
#| include: false

#libraries
library(tidyverse)
library(plotly)
library(ggplot2)
library(maps)
library(dplyr)
#data
unicef_indicator_1_2_ <- read_csv("unicef_indicator_1 (2).csv")
unicef_metadata_1_ <- read_csv("unicef_metadata (1).csv")

#transformations
data_join <- full_join(unicef_indicator_1_2_, unicef_metadata_1_)


```

# Introduction
Welcome to **Mapping Child Welfare: Analysing Childhood Deprivation alongside Economic and Health Factors**.This interactive dashboard offers a comprehensive exploration of UNICEF data, providing valuable insights into the well-being of children worldwide.

At the **heart** of our analysis lies the ***percentage of children under 5 years of age facing at least one deprivation***, a critical measure of childhood vulnerability and socio-economic disparity. By examining this indicator, we aim to uncover the intricate interplay between deprivation, economic prosperity, and health outcomes for children across different regions and countries.

To better understand this **critical issue**, let's begin our journey with a global perspective. The map below displays the percentage of children facing deprivation in each country. Hover over each country to reveal not only the overall percentage (*avg % 2011-2018*) but also the gender breakdown, providing valuable insights into the unique challenges faced by boys and girls worldwide.

# Map
```{r}
map_world <- map_data("world")

map_children <- full_join(unicef_indicator_1_2_, map_world, by = c("country" = "region"))
 ggplot(map_children) + 
  aes(x = long, y = lat, group = group, fill = obs_value) + 
  geom_polygon(color = "black", size = 0.1) +  # Adjust outline color and size
  scale_fill_gradient(name = "Percentage",   # Customize legend title
                      low = "lightcoral",     # Adjust low color
                      high = "darkred",     # Adjust high color
                      na.value = "grey") + # Color for missing values
  labs(title = "Percentage of Children Facing Deprivation",   # Map title
       x = NULL, y = NULL,                                  # Remove axis labels
       fill = "Percentage of Children Facing Deprivation") + # Legend label
  theme_void() +  # Apply a minimal theme
  theme(legend.position = "right",  # Change legend position
        legend.title = element_text(size = 12, face = "bold"),  # Customize legend title appearance
        legend.text = element_text(size = 10))  # Customize legend text appearance
      

```

As we delve deeper into our analysis, a **stark reality emerges**: Africa bears a disproportionate burden of childhood deprivation. Out of the **ten** countries with the highest rates of deprivation, an overwhelming **eight** are located on the **African continent**.

```{r}
aggregated_data <- unicef_indicator_1_2_ %>%
  group_by(country) %>%
  summarise(avg_obs_value = mean(obs_value, na.rm = TRUE))
top_ten_worst <- aggregated_data %>%
  arrange(desc(avg_obs_value)) %>%
  head(10)
ggplot(top_ten_worst, aes(x = reorder(country, -avg_obs_value), y = avg_obs_value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Country", y = "Percentage of Children Facing Deprivation", title = "Ten Countries with Highest Deprivation") +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for better readability
  theme(panel.background = element_blank(), axis.line = element_line(color = "lightgrey"), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

```

This alarming trend underscores the urgent need for **targeted interventions** and **sustained efforts** to address the multifaceted challenges faced by children in Africa. From access to education and healthcare to protection from violence and exploitation, the barriers to a fulfilling childhood are numerous and complex.

Understanding these intertwined dynamics is essential for policymakers and stakeholders alike, guiding efforts towards sustainable development and ensuring every child has the opportunity to thrive.

# Global Socioeconomic Dynamics

Our journey into global socioeconomic dynamics begins with a deep dive into the correlation between a nation's GDP and the percentage of children facing deprivation. This exploration sheds light on the intricate relationship between economic prosperity and child well-being, offering valuable insights into potential pathways for achieving greater equity for children worldwide.

## Relationship between GDP and Child Deprivation Per Country
```{r}
merge_data2 <- merge(unicef_indicator_1_2_, unicef_metadata_1_, by = c("country"))

gdpscatter <- ggplot(merge_data2, aes(x = `GDP per capita (constant 2015 US$)`, y = `obs_value`)) +
  geom_point(aes(color = country)) + 
  geom_smooth(method = "lm", color = "black", se = FALSE) + 
  scale_y_continuous(limits = c(0, NA)) +  # Set y-axis limits to start from 0
  theme_grey() + 
  labs(x = "GDP", y = "% of Children Facing One Deprivation", title = "Relationship between GDP and Child Deprivation Per Country") + 
  theme(legend.position = "none", panel.background = element_blank(), axis.line = element_line(color = "lightgrey"))
ggplotly(gdpscatter)

```


Examining the relationship between GDP and the percentage of children facing deprivation reveals an intriguing trend. As GDP increases, there is a notable decrease in the percentage of children experiencing deprivation. This correlation underscores the significance of economic growth in mitigating child deprivation and fostering overall well-being.

Additionally, Exploring the diverse economic landscapes across nations, we draw comparisons between the GDP of countries grappling with high levels of child deprivation and those of flourishing first-world economies like France and Ireland. Through this comparison, we illuminate the profound disparities in economic development and their profound implications for child well-being

## GDP Comparison
```{r}
selected_countries <- c("France", "Benin", "Chad", "Ethiopia", "Burma", "Ghana", "India", "Ireland", "Lesotho", "Liberia", "Myanmar", "Niger", "Nigeria", "Papua New Guinea", "Togo")
GDP_time_series <- unicef_metadata_1_%>%
  filter(country %in% selected_countries)

ab <- ggplot(GDP_time_series, aes(x = year, y = `GDP per capita (constant 2015 US$)`, color = country)) +
  geom_line() +
  labs(x = "Year", y = "GDP per capita (constant 2015 US$)", title = "GDP Comparison for Selected Countries") +
  theme_bw() +
  theme(panel.background = element_blank(), axis.line = element_line(color = "lightgrey"), panel.grid.major = element_blank(),panel.grid.minor = element_blank())
    
ggplotly(ab)
```

Our analysis sheds light on the economic challenges faced by selected African countries. Despite concerted efforts to address social disparities, these countries have struggled with lower GDP levels. This economic context exacerbates issues related to child deprivation, emphasizing the need for targeted interventions to uplift vulnerable communities.

In the subsequent scatter plot, we delve into the correlation between GDP (Gross Domestic Product) and life expectancy, building upon previous findings that elucidated the inverse relationship between GDP and deprivation levels across various countries. As we explore the data, we aim to demonstrate how higher GDP levels are associated with increased life expectancy, offering insights into the socioeconomic factors influencing health outcomes worldwide.

## GDP vs Life Expectancy
```{r}
unicef_metadata_1__clean <- na.omit(unicef_metadata_1_)

scatter_end <- ggplot(unicef_metadata_1__clean, aes(x = `GDP per capita (constant 2015 US$)`, y = `Life expectancy at birth, total (years)`, color = country)) +
  geom_point() +
  labs(x = "GDP per capita (constant 2015 US$)", y = "Life Expectancy at Birth (years)", title = "Scatter Plot of GDP vs. Life Expectancy by Country") +
  theme(legend.position = "none") + # Remove the legend 
  theme(panel.background = element_blank(), axis.line = element_line(color = "lightgrey"))
ggplotly(scatter_end)
```

Ultimately, the observed positive correlation between GDP and life expectancy underscores the critical role of economic prosperity in improving healthcare access, infrastructure, and overall living standards. However, it also underscores the persistent challenge of deprivation, as countries with lower GDPs tend to face greater obstacles in providing adequate healthcare and addressing social inequalities, highlighting the need for targeted interventions to alleviate poverty and enhance well-being on a global scale.

# Conclusion

As we conclude our exploration of the interconnected challenges facing children globally, we are confronted with both sobering realities and hopeful prospects. From the intricate dynamics between GDP and child deprivation to the disparities in life expectancy linked to socioeconomic inequities, our journey has underscored the urgent need for targeted interventions and policy reforms. While the data may reveal daunting obstacles, it also illuminates pathways towards change.

As we navigate towards a future where every child can flourish, let us remain steadfast in our commitment to equity, justice, and compassion. Through collective action and unwavering dedication, we can create a world where every child's potential is nurtured, and their rights are upheld. Together, let us strive to build a brighter and more inclusive future for generations to come.
