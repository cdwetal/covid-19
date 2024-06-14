---
title: "COVID-19 Analysis"
output: pdf_document
date: "2024-06-08"
author: "Christopher Washington"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Introduction

## Required R Libraries
The following R libraries are required to run the R code contained in this document: 
```{r load_libs, echo=TRUE, message=FALSE}
library(tidyverse)
library(knitr)
library(lubridate)

set.seed(10)
``` 

## Dataset

## Data Retrieval
We will begin by loading the dataset:
```{r load_data, message=FALSE}
base_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

df_confirmed = read_csv(paste(base_url, "time_series_covid19_confirmed_US.csv", sep=" "))

df_confirmed_global = read_csv(paste(base_url, "time_series_covid19_confirmed_global.csv", sep=" "))

df_deaths = read_csv(paste(base_url, "time_series_covid19_deaths_US.csv", sep=" "))

df_deaths_global = read_csv(paste(base_url, "time_series_covid19_deaths_global.csv", sep=" "))
```
We are only interested in the date/time, borough, precint, perpatrator age group, perpatrator sex, perpatrator race, victim age group, victim sex, and victim race for this analysis, so we will remove the unnecessary columns. We will also rename the columns to simpler names for later use.
```{r rename_cols, message=FALSE}
shootings = shootings %>% 
  rename(date = 'OCCUR_DATE') %>%
  rename(boro = 'BORO') %>%
  rename(lat = 'Latitude') %>%
  rename(long = 'Longitude') %>%
  select(date, boro, lat, long)
```
We will continue to preprocess our data by converting columns to the appropriate data types. Categorical data columns will become factors, and date/time columns will be converted to the appropriate date/time data type.
```{r convert_cols, message=FALSE}
shootings = shootings %>% mutate_at(
  vars(boro), 
  factor
)
shootings = shootings %>% mutate(date = mdy(date))
```
Finally, we will filter out any null and UNKNOWN records from the dataset:
```{r filter_rows, message=FALSE}
shootings = shootings %>% 
  filter(!is.na(boro)) %>% 
  filter(boro != "(null)") %>% filter(boro != "UNKNOWN")
```
The final dataset looks like the following snapshot:
```{r show_data, echo=FALSE}
kable(head(shootings))
```
\newpage
## Visualization 1: Shootings by Borough
The first visualization that we will consider displays the number of shootings in each New Your City borough.
```{r show_shootings_by_borough, echo=FALSE}
shootings %>% ggplot() +
  geom_bar(aes(
    x = boro)) +
  labs(x = "Borough", y="Count") +
  ggtitle("Shootings by Borough") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))
```
We can see from this plot that there are a noticeable increases in shootings in the Bronx and Brooklyn boroughs. This could be simply due to those boroughs containing more residents than other boroughs, but it could also could be related to other factors such as the average socio-economic status of residents in those boroughs.

\newpage
## Visualization 2: Shootings by Year 
Another visualization that we will investigate is the total shootings per year as displayed in the following graph.
```{r show_shootings_by_year, echo=FALSE}
shootings_by_year = shootings %>% 
  group_by(year = year(date)) 

shootings_by_year %>% ggplot() +
  geom_bar(aes(
    x = year)) +
  labs(x = "Year", y="Count") +
  ggtitle("Shootings by Year") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))
```
As can be seen by this graph, the total shootings in New York City was declining from 2006-2020, but saw a sharp increase in the year 2020. Since the increase is considerably large, we should investigate the possible reason why this has happened. Some questions that we can investigate further in relation to the visualization are: is the increase in shootings in 2020 related to the COVID-19 pandemic, is it perhaps due to a change in violent crime related policy or other political perturbations in New Your City at this time, or possibly something else?

## Model 1: Proportions of Shootings by Borough
```{r build_test_borough_model, echo=FALSE, message=FALSE}
shootings_by_borough = shootings %>%
  group_by(boro) %>% 
  summarise(n = n()) %>% 
  mutate(shootings_per_1000 = 
    ifelse(boro == 'BRONX', round(1000*(n/1472654), 2), 
      ifelse(boro == 'BROOKLYN', round(1000*(n/2736074), 2), 
         ifelse(boro == 'MANHATTAN', round(1000*(n/1694251), 2), 
            ifelse(boro == 'QUEENS', round(1000*(n/2405464), 2), 
               ifelse(boro == 'STATEN ISLAND', round(1000*(n/495747), 2), n)
             )
          )
       )
    )
  )

shootings_by_borough %>% ggplot() +
  geom_col(aes(
    x = boro,
    y = shootings_per_1000)) +
  labs(x = "Borough", y="Count") +
  ggtitle("Shootings by Borough (Per 1000 Residents)") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))
  
shootings_by_borough_per_1000 = shootings_by_borough %>% select(boro, shootings_per_1000)

kable(shootings_by_borough_per_1000)
```
## Visualization 3: Spatial Representation of Shootings in NYC
The final visualization that we will examine here is a scatterplot representing the physical location of the shootings in New Tork City. Here we plot the longitude coordinate of each shooting on the x axis, and we plot the latitude coordinate of each shooting on the y axis. 
```{r spatial_visualization, echo=FALSE, message=FALSE}
ggplot(shootings, aes(x=long, y=lat)) + geom_point()
```
## Conclusion
There are many other visualization and analyses that can be brought from this dataset. Two of those have been provided here. These visualizations and analyses provide a starting point for further research and anlysis regarding shootings in New York City.