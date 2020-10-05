---
title: "Recreating the New York Times mask utilization survey data with the R opensource Leaflet package"
author: ''
date: '2020-10-04'
slug: covid19_mask_usage
categories: [rstats, leaflet]
tags: [rstats, leaflet]
subtitle: ''
summary: "Recreating the New York Times mask utilization survey data with the R opensource Leaflet package"
authors: []
# lastmod: '2020-09-28T20:32:33-07:00'
reading_time: false  # Show estimated reading time?
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---



![](/covid19_mask_usage/index_files/nytimesmap.jpg)

We're going to recreate the NY Times mask-use survey data using R and the leaflet open source interactive mapping package. We can start off by loading the data from the New York Times github repository found [here](https://github.com/nytimes/covid-19-data/tree/master/mask-use)


```r
url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv'

df <- read_csv(url)
```

Now that we have the data loaded, lets have a look at the data to see what we're working with


```r
glimpse(df)
```

```
## Rows: 3,142
## Columns: 6
## $ COUNTYFP   <chr> "01001", "01003", "01005", "01007", "01009", "01011", "0...
## $ NEVER      <dbl> 0.053, 0.083, 0.067, 0.020, 0.053, 0.031, 0.102, 0.152, ...
## $ RARELY     <dbl> 0.074, 0.059, 0.121, 0.034, 0.114, 0.040, 0.053, 0.108, ...
## $ SOMETIMES  <dbl> 0.134, 0.098, 0.120, 0.096, 0.180, 0.144, 0.257, 0.130, ...
## $ FREQUENTLY <dbl> 0.295, 0.323, 0.201, 0.278, 0.194, 0.286, 0.137, 0.167, ...
## $ ALWAYS     <dbl> 0.444, 0.436, 0.491, 0.572, 0.459, 0.500, 0.451, 0.442, ...
```

```r
df
```

```
## # A tibble: 3,142 x 6
##    COUNTYFP NEVER RARELY SOMETIMES FREQUENTLY ALWAYS
##    <chr>    <dbl>  <dbl>     <dbl>      <dbl>  <dbl>
##  1 01001    0.053  0.074     0.134      0.295  0.444
##  2 01003    0.083  0.059     0.098      0.323  0.436
##  3 01005    0.067  0.121     0.12       0.201  0.491
##  4 01007    0.02   0.034     0.096      0.278  0.572
##  5 01009    0.053  0.114     0.18       0.194  0.459
##  6 01011    0.031  0.04      0.144      0.286  0.5  
##  7 01013    0.102  0.053     0.257      0.137  0.451
##  8 01015    0.152  0.108     0.13       0.167  0.442
##  9 01017    0.117  0.037     0.15       0.136  0.56 
## 10 01019    0.135  0.027     0.161      0.158  0.52 
## # ... with 3,132 more rows
```

According to the repository, the definitions of the variables are as follows:

* **COUNTYFP**: The county FIPS code.
* **NEVER**: The estimated share of people in this county who would say never in response to the question “How often do you wear a mask in public when you expect to be within six feet of another person?”
* **RARELY**: The estimated share of people in this county who would say rarely
* **SOMETIMES**: The estimated share of people in this county who would say sometimes
* **FREQUENTLY**: The estimated share of people in this county who would say frequently
* **ALWAYS**: The estimated share of people in this county who would say always

They are also plotting the probability of encountering a mask usage among 5 random encounters in the county.

> The chance all five people are wearing masks in five random encounters is calculated by assuming that survey respondents who answered ‘Always’ were wearing masks all of the time, those who answered ‘Frequently’ were wearing masks 80 percent of the time, those who answered ‘Sometimes’ were wearing masks 50 percent of the time, those who answered ‘Rarely’ were wearing masks 20 percent of the time and those who answered ‘Never’ were wearing masks none of the time.

We can calculate this simply by using the supplied weights (1, .8, .5, .2, and 0) among ALWAYS, FREQUENTLY, SOMETIMES, RARELY, and NEVER mask usage, and taking the sum of the proportion of mask usage among all 5 different types of individuals that have equal probability of encountering.


```r
df <- df %>%
  mutate(
    prob = (ALWAYS * 1) + (FREQUENTLY * .8) + (SOMETIMES * .5) + (RARELY * .2) + (NEVER * 0)
  )
```

Since we have the county FIPS code data available, we'll need to merge this data with county geojson data for the United States which I was able to obtain from [here](https://eric.clst.org/tech/usgeojson/) 


```r
counties <- rgdal::readOGR('https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_050_00_20m.json')
```

```
## OGR data source with driver: GeoJSON 
## Source: "https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_050_00_20m.json", layer: "gz_2010_us_050_00_20m"
## with 3221 features
## It has 6 fields
```

After reading in the US counties data, we can merge the mask usage survey data with the geojson file, by the state and FIPS code. We can create a COUNTYFP variable by pasting together the STATE and COUNTY code


```r
counties@data <- counties@data %>%
  mutate(
    COUNTYFP = paste0(STATE, COUNTY)
  ) %>%
  left_join(
    df
  )
```

Furthermore after merging the data, we can create a label by merging together the % mask usage data into a HTML string


```r
counties@data <- counties@data %>%
  mutate(
    label = glue::glue(
      '<h3>{NAME}</h3><br>
      {paste0(format(round(NEVER*100, 1), 1), "%")} estimated NEVER wear a mask <br>
      {paste0(format(round(RARELY*100, 1), 1), "%")} estimated RARELY wear a mask <br>
      {paste0(format(round(SOMETIMES*100, 1), 1), "%")} estimated SOMETIMES wear a mask <br>
      {paste0(format(round(FREQUENTLY*100, 1), 1), "%")} estimated FREQUENTLY wear a mask <br>
      {paste0(format(round(ALWAYS*100, 1), 1), "%")} estimated ALWAYS wear a mask <br><br>
      <h5>This translates to a <b>{paste0(format(round(prob*100, 1), 1), "%")}</b> chance that everyone is masked in five random encounters</h5>'
    ),
    label = map(label, ~ htmltools::HTML(.x))
  )
```

Finally, let's put this all together and create a Chloropleth map using the leaflet package


```r
color_pal <- colorNumeric('viridis', counties$prob)

map <- leaflet(counties) %>%
  addTiles() %>%
  fitBounds(lng1 = -131.519605, lng2 = -64.312607, lat1 = 50.623510, lat2 = 23.415249) %>%
  addPolygons(
    fillColor = ~ color_pal(prob),
    fillOpacity = .75,
    weight = 1,
    color = 'black',
    label = ~ label
  ) %>%
  addLegend(
    position = 'bottomright',
    pal = color_pal,
    values = ~ counties$prob,
    title = '% Always Mask Usage',
    labFormat = labelFormat(
      suffix = '%',
      transform = function(x)
        x * 100
    )
  )
```

![](/covid19_mask_usage/index_files/leafletmap.jpg)






