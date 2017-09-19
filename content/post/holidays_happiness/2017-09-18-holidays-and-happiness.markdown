---
title: Holidays and Happiness - What makes a country happy?
author: Dale Richardson
date: '2017-09-18'
slug: holidays-and-happiness
categories:
  - viz
  - eda
tags:
  - R Markdown
  - plot
  - EDA
  - regression
output:
  blogdown::html_page:
    toc: true
---

## Description

Over the past year I have spent a good portion of time in the office studying and preparing for a career transition. Finding a suitable work-life balance has been difficult, especially with the clock ticking on my post-doctoral fellowship running out in December, 2017. Most free moments I have during the day are usually d towards studying something datascience related or doing some sports in an effort to keep mind and body balanced. Add a family and day-job into the mix and you have a recipe for a very tight schedule. 

Given how busy I've been, one would think I would welcome state sanctioned holidays, but to be honest I found myself slightly annoyed! This minor annoyance was probably due to the pressure of my imminently expiring fellowship. That being said, my annoyance got me thinking: does Portugal really have too many holidays? Which countries have the most public holidays? Furthermore, do countries with the most public holidays tend to be happier?

Well, I thought this to be a perfect occassion to do some exploratory analyses in R that required putting together an ad hoc dataset from various online sources. 

### Quick highlights
1. India is the country with the most public holidays at 48. The country with the second most holidays is Pakistan, with 29 - a difference of 19! In contrast, Libya only has 7 public holidays, while the median number of public holidays is 13. In the end, Portugal only has 14 public holidays. Not so bad!

2. Five of the 10 happiest nations on Earth are the Nordic countries (Norway, Denmark, Iceland, Finland and Sweden). The remaining five are Switzerland, the Netherlands, Canada, New Zealand and Australia. Portugal is ranked 89th in happiness and the USA (my birthplace) is ranked 14th. 

3. The number of public holidays, average generosity and trust are not significant predictors of a nation's happiness. This is in stark contrast to the importance of wealth, health, freedom to make choices and social support as important predictors of a nation's happiness. 

### Get a list of holiday counts by country and happiness index by country
The first thing I needed to acquire was a listing of holidays across the globe. After some googling, I found a table on the [timeanddate.com](http://timeanddate.com) website that provided this information. All I had to do was use the `rvest` package to scrape the table into a dataframe. 

Regarding the happiness index, after some googling, I discovered the [**World Happiness Report**](https://en.wikipedia.org/wiki/World_Happiness_Report), which is "a measure of happiness published by the United Nations Sustainable Development Solutions Network." Also from the wikipedia page:

"Data is collected from people in over 150 countries. Each variable measured reveals a populated-weighted average score on a scale running from 0 to 10 that is tracked over time and compared against other countries. These variables currently include: real GDP per capita, social support, healthy life expectancy, freedom to make life choices, generosity, and perceptions of corruption. Each country is also compared against a hypothetical nation called Dystopia. Dystopia represents the lowest national averages for each key variable and is, along with residual error, used as a regression benchmark."


#### Scrape the holidays from timeanddate.com
First load necessary libraries. 


```r
library(tidyverse)
library(rvest)
library(maps)
library(plotly)
library(knitr)
```

Now let's grab the table of holidays from timeanddate.com. 

```r
# store the url
holidays <- read_html("http://services.timeanddate.com/api/holiday-countries-listing.html")

# get the table of holidays
holiday_table <- holidays %>%
        html_node("table") %>%
        html_table() %>%
        rename(Total_days = `Total Days`) %>%
        rename(Public_Holidays = `State & Federal Holidays`) %>%
        select(Country, Id, Public_Holidays) %>%
        mutate(Id = toupper(Id))

head(holiday_table)
```

```
##          Country Id Public_Holidays
## 1    Afghanistan AF               9
## 2        Albania AL              14
## 3        Algeria DZ              12
## 4 American Samoa AS              12
## 5        Andorra AD              22
## 6         Angola AO              12
```

#### Get ISO-3 Standard Country Codes and join to dataframe of country holidays

As plotly takes in ISO-3 country codes to specify country location in plots, I will also scrape the country codes from the FAO. 


```r
## get ISO-3 country code names for plotting with plotly
country_codes <- read_html("http://www.fao.org/countryprofiles/iso3list/en/")

iso3_table <- country_codes %>% 
        html_node("table") %>% 
        html_table() 

# inner join
holiday_table_joined <- inner_join(holiday_table, iso3_table, by = c("Id" = "ISO2") )
```

#### Scrape happiness scores from the World Happiness Report Wikipedia page
I ended up getting data from the [2017 release of the World Happiness Report from Wikipedia](https://en.wikipedia.org/wiki/World_Happiness_Report). A few of the variables in this table are described below, from information taken from the [statistical appendix](http://worldhappiness.report/wp-content/uploads/sites/2/2017/03/StatisticalAppendixWHR2017.pdf) from the World Happiness Report. For a thorough explanation of the variables, please follow the aforementioned link.

1. `Happiness Score` - National average response to the question of life evaluations from the 2016 Gallup World Poll (GWP)
2. `Healthy Life Expectancy` - Time series of healthy life expectancy at birth as calculated by the authors based on data from the WHO and the World Development Indicators, and statistics from journal articles.
3. `Social Support` - National average of binary responses to the GWP question, "If you were in trouble, do you have relatives or friends you can count on to help you whenever you need them, or not?"
4. `Freedom to Make Life Choices` - National average of the binary responses to the GWP question, "Are you satistifed or dissatisfied with your freedom to choose what you do with your life?"
5. `Generosity` - the residual of regressing national average of response to the GWP question, "Have you donated money to a charity in the past month?"


```r
# get world happiness / GDP table
happiness <- read_html("https://en.wikipedia.org/wiki/World_Happiness_Report")

happiness_table <- happiness %>% 
        html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
        html_table(fill = TRUE) %>%
        rename(Overall_Rank = `Overall Rank`) %>%
        rename(GDP_per_Capita = `GDP per capita`) %>%
        rename(Healthy_Life_Expectancy = `Healthy life expectancy`) %>%
        rename(Social_Support = `Social support`) %>%
        rename(Freedom_to_Make_Life_Choices = `Freedom to make life choices`) %>%
        rename(Generosity = `Generosity`) %>%
        rename(Trust = `Trust`) %>%
        select(Country, Score, Overall_Rank, GDP_per_Capita, Healthy_Life_Expectancy, Social_Support, Freedom_to_Make_Life_Choices, Generosity, Trust) %>%
        mutate(Score = as.numeric(Score),
               Overall_Rank = as.integer(Overall_Rank),
               GDP_per_Capita = as.numeric(GDP_per_Capita),
               Healthy_Life_Expectancy = as.numeric(Healthy_Life_Expectancy),
               Social_Support = as.numeric(Social_Support),
               Freedom_to_Make_Life_Choices = as.numeric(Freedom_to_Make_Life_Choices),
               Generosity = as.numeric(Generosity),
               Trust = as.numeric(Trust))

# join happiness table with iso3 based on Country name
tmp <- inner_join(happiness_table, iso3_table, by = c("Country" = "Short name"))

glimpse(tmp)
```

```
## Observations: 135
## Variables: 16
## $ Country                      <chr> "Norway", "Denmark", "Iceland", "...
## $ Score                        <dbl> 7.537, 7.522, 7.504, 7.494, 7.469...
## $ Overall_Rank                 <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11...
## $ GDP_per_Capita               <dbl> 1.616, 1.482, 1.481, 1.565, 1.444...
## $ Healthy_Life_Expectancy      <dbl> 0.797, 0.793, 0.834, 0.858, 0.809...
## $ Social_Support               <dbl> 1.534, 1.551, 1.611, 1.517, 1.540...
## $ Freedom_to_Make_Life_Choices <dbl> 0.635, 0.626, 0.627, 0.620, 0.618...
## $ Generosity                   <dbl> 0.362, 0.355, 0.476, 0.291, 0.245...
## $ Trust                        <dbl> 0.316, 0.401, 0.154, 0.367, 0.383...
## $ `Official name`              <chr> "the Kingdom of Norway", "the Kin...
## $ ISO3                         <chr> "NOR", "DNK", "ISL", "CHE", "FIN"...
## $ ISO2                         <chr> "NO", "DK", "IS", "CH", "FI", "NL...
## $ UNI                          <int> 578, 208, 352, 756, 246, 528, 124...
## $ UNDP                         <chr> "NOR", "DNK", "ISL", "CHE", "FIN"...
## $ FAOSTAT                      <int> 162, 54, 99, 211, 67, 150, 33, 15...
## $ GAUL                         <int> 186, 69, 114, 237, 84, 177, 46, 1...
```

There's a slight problem with the `Country` names in the table I scraped from Wikipedia and the table of country names and ISO codes I obtained from the FAO. In order to see the countries that could not be joined on account of inconsistent naming conventions, let's perform an anti-join.


```r
# see which countries have problems with names
anti_join(happiness_table, iso3_table, by = c("Country" = "Short name")) %>% select(Country)
```

```
##                    Country
## 1            United States
## 2           Czech Republic
## 3                   Taiwan
## 4           Europe[Note 1]
## 5                   Russia
## 6                  Moldova
## 7              South Korea
## 8                  Bolivia
## 9             North Cyprus
## 10               Hong Kong
## 11                   World
## 12                  Kosovo
## 13               Venezuela
## 14               Macedonia
## 15                 Vietnam
## 16 Palestinian Territories
## 17                    Iran
## 18     Congo (Brazzaville)
## 19        Congo (Kinshasa)
## 20             Ivory Coast
## 21                   Syria
## 22                Tanzania
```

Alright. I see what I will have to do. Now that I know which country names are problematic in the `happiness_table`, I will manually replace them with the ISO country short names. 


```r
# get a vector of bad country names
replaceme <- pull(anti_join(happiness_table, iso3_table, by = c("Country" = "Short name")) 
                  %>% select(Country))

# drop irrelevant values or disputed territories, e.g. "World"
replaceme <- replaceme[-c(7, 11, 12, 13, 14, 19, 20)]

good_names <- c("United States of America",
                "Czechia",
                "Russian Federation",
                "Republic of Moldova",
                "Republic of Korea",
                "Bolivia (Plurinational State of)",
                "Venezuela (Bolivarian Republic of)",
                "The former Yugoslav Republic of Macedonia",
                "Viet nam",
                "Iran (Islamic Republic of)",
                "Congo",
                "Democratic Republic of the Congo",
                "Côte d'Ivoire",
                "Syrian Arab Republic",
                "United Republic of Tanzania"
)

# get row number indices for "bad" country names in the happiness_table
bad_country_index <- as.numeric(rownames(happiness_table[happiness_table$Country %in% replaceme,]))

# replace with "good" name
happiness_table$Country <- replace(happiness_table$Country, bad_country_index, good_names)

# try joining again the happiness table with iso3 based on Country name
happy_countries <- inner_join(happiness_table, iso3_table, by = c("Country" = "Short name"))

glimpse(happy_countries)
```

```
## Observations: 149
## Variables: 16
## $ Country                      <chr> "Norway", "Denmark", "Iceland", "...
## $ Score                        <dbl> 7.537, 7.522, 7.504, 7.494, 7.469...
## $ Overall_Rank                 <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11...
## $ GDP_per_Capita               <dbl> 1.616, 1.482, 1.481, 1.565, 1.444...
## $ Healthy_Life_Expectancy      <dbl> 0.797, 0.793, 0.834, 0.858, 0.809...
## $ Social_Support               <dbl> 1.534, 1.551, 1.611, 1.517, 1.540...
## $ Freedom_to_Make_Life_Choices <dbl> 0.635, 0.626, 0.627, 0.620, 0.618...
## $ Generosity                   <dbl> 0.362, 0.355, 0.476, 0.291, 0.245...
## $ Trust                        <dbl> 0.316, 0.401, 0.154, 0.367, 0.383...
## $ `Official name`              <chr> "the Kingdom of Norway", "the Kin...
## $ ISO3                         <chr> "NOR", "DNK", "ISL", "CHE", "FIN"...
## $ ISO2                         <chr> "NO", "DK", "IS", "CH", "FI", "NL...
## $ UNI                          <int> 578, 208, 352, 756, 246, 528, 124...
## $ UNDP                         <chr> "NOR", "DNK", "ISL", "CHE", "FIN"...
## $ FAOSTAT                      <int> 162, 54, 99, 211, 67, 150, 33, 15...
## $ GAUL                         <int> 186, 69, 114, 237, 84, 177, 46, 1...
```

Ahh, good. Everything worked. We now have a dataframe of 149 countries along with their happiness scores and ISO-3 country codes. 

#### Join the holidays table to the happy_countries dataframe

```r
# join tables based on country
#combined <- left_join(happy_countries, holiday_table, by = "Country")
combined <- inner_join(holiday_table_joined, happy_countries,  by = "ISO3") %>% 
        select(Country.y, Overall_Rank, Score, Public_Holidays, ISO3, GDP_per_Capita:Trust) %>%
        rename(Country = Country.y)

glimpse(combined)
```

```
## Observations: 147
## Variables: 11
## $ Country                      <chr> "Afghanistan", "Albania", "Algeri...
## $ Overall_Rank                 <int> 141, 109, 53, 140, 24, 121, 9, 13...
## $ Score                        <dbl> 3.794, 4.644, 5.872, 3.795, 6.599...
## $ Public_Holidays              <int> 9, 14, 12, 12, 17, 17, 20, 14, 24...
## $ ISO3                         <chr> "AFG", "ALB", "DZA", "AGO", "ARG"...
## $ GDP_per_Capita               <dbl> 0.401, 0.996, 1.092, 0.858, 1.185...
## $ Healthy_Life_Expectancy      <dbl> 0.181, 0.731, 0.618, 0.050, 0.695...
## $ Social_Support               <dbl> 0.582, 0.804, 1.146, 1.104, 1.440...
## $ Freedom_to_Make_Life_Choices <dbl> 0.106, 0.381, 0.233, 0.000, 0.495...
## $ Generosity                   <dbl> 0.312, 0.201, 0.069, 0.098, 0.109...
## $ Trust                        <dbl> 0.061, 0.040, 0.146, 0.070, 0.060...
```

Now I have a table that includes data on holidays and happiness. Let's make a few plots!

### India has the most public holidays at 48 whereas Libya has the fewest at 7

I think it would be cool to start with a plot of holiday counts by country. I will use the `holiday_table` dataframe as it contains data on 192 countries, whereas the combined dataframe only has data for 147 countries. Let's try using plotly for an interactive experience.


```r
# code adapted from https://plot.ly/r/choropleth-maps/

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

# create plot object
p <- plot_geo(holiday_table_joined) %>%
  add_trace(
    z = ~Public_Holidays, color = ~Public_Holidays, colors = 'Blues',
    text = ~Country, locations = ~ISO3, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of public holidays') %>%
  layout(
    title = 'World Public Holidays<br>Source:<a href="http://services.timeanddate.com/api/holiday-countries-listing.html">TimeandDate.com</a>',
    geo = g
  )

#display
f <- api_create(p, sharing = "public")
f
```

<iframe src="https://plot.ly/~drichardson/176.embed" width="800" height="600" id="igraph" scrolling="no" seamless="seamless" frameBorder="0"> </iframe>

Cool! Let's take a look at the top 10 countries with the most holidays.


```r
kable(top_n(combined, 10, Public_Holidays) %>% 
        arrange(desc(Public_Holidays)) %>%
        select(Country, Public_Holidays),
      caption = "Top 10 Countries with the Most Public Holidays")
```



|Country                                   | Public_Holidays|
|:-----------------------------------------|---------------:|
|India                                     |              48|
|Pakistan                                  |              29|
|Nepal                                     |              28|
|China                                     |              27|
|Iran (Islamic Republic of)                |              26|
|The former Yugoslav Republic of Macedonia |              26|
|Malaysia                                  |              25|
|Sri Lanka                                 |              25|
|Thailand                                  |              25|
|Azerbaijan                                |              24|

```r
# take a summary as well
summary(combined$Public_Holidays, na.rm = TRUE)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    7.00   11.00   13.00   14.53   16.00   48.00
```

Impressive! India has 48 public holidays! It looks like if you're looking for a place with some serious downtime, you may want to move to India. The median number of public holidays is 13, with a minimum of 7. Let's see who this country is. 


```r
# get country with lowest number of public holidays
combined[which.min(combined$Public_Holidays), "Country"]
```

```
## [1] "Libya"
```

Yikes! Perhaps public holidays are the least of Libya's concerns at the moment, given they are embroiled in conflict and have [multiple governments vying for power and control over the country](http://www.aljazeera.com/indepth/features/2017/04/happening-libya-today-170418083223563.html).

### Nordic countries, Switzerland, the Netherlands, Canada, Australia and New Zealand are the happiest countries on Earth

Let's now turn our attention to see which countries have the highest happiness scores. Once again we'll use plotly to build an interactive world map and then check out the top ten happiest countries.


```r
# code adapted from https://plot.ly/r/choropleth-maps/

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

# create plot object
q <- plot_geo(happy_countries) %>%
  add_trace(
    z = ~Score, color = ~Score, colors = 'Oranges',
    text = ~Country, locations = ~ISO3, marker = list(line = l)
  ) %>%
  colorbar(title = 'World Happiness Score') %>%
  layout(
    title = 'World Happiness Score<br>Source:<a href="https://en.wikipedia.org/wiki/World_Happiness_Report">World Happiness Report</a>',
    geo = g
  )

# display
z <- api_create(q, sharing = "public")
z
```

<iframe src="https://plot.ly/~drichardson/178.embed" width="800" height="600" id="igraph" scrolling="no" seamless="seamless" frameBorder="0"> </iframe>

Again, let's see the top ten.


```r
kable(top_n(combined, 10, Score) %>% 
        arrange(desc(Score)) %>%
        select(Country, Score),
      caption = "Top 10 Happiest Countries")
```



|Country     | Score|
|:-----------|-----:|
|Norway      | 7.537|
|Denmark     | 7.522|
|Iceland     | 7.504|
|Switzerland | 7.494|
|Finland     | 7.469|
|Netherlands | 7.377|
|Canada      | 7.316|
|New Zealand | 7.314|
|Australia   | 7.284|
|Sweden      | 7.284|

That's truly amazing. Five of the 10 happiest countries are from the Nordic countries. 

### Do more public holidays have any influence on happiness?

Since India is a bit of an outlier with 48 public holidays we'll exclude it from our linear model as it is likely a high leverage point. 

Let's also add a new categorial variable, `holiday_cat` which indicates if a country has Low (<=11), Medium (> 11 <= 16) or High (> 16) number of public holidays.


```r
combined$holiday_cat <- cut(combined$Public_Holidays, breaks = c(-Inf, 11, 16, Inf ), labels = c("Low <= 11", "Medium > 11 & <= 16", "High > 16"), right = FALSE )
```

Now let's do a quick boxplot.


```r
ggplot(combined, aes(x = holiday_cat, y = Score)) +
               geom_boxplot() +
        labs(title = "Countries with greater than 16 public holidays have a higher median happiness score",
             x = "Number of holidays",
             y = "Happiness Score") +
        theme_bw()
```

<img src="/post/holidays_happiness/2017-09-18-holidays-and-happiness_files/figure-html/unnamed-chunk-14-1.png" width="960" />

Ok, now let's see how our holidays factor into predicting a country's happiness score. 


```r
# generate a linear model to predict score without india
m2 <- lm(Score ~ GDP_per_Capita + Healthy_Life_Expectancy + 
                 Freedom_to_Make_Life_Choices + Generosity + 
                 Trust + Social_Support +
                 Public_Holidays + holiday_cat, data = combined[-116,])

summary(m2)
```

```
## 
## Call:
## lm(formula = Score ~ GDP_per_Capita + Healthy_Life_Expectancy + 
##     Freedom_to_Make_Life_Choices + Generosity + Trust + Social_Support + 
##     Public_Holidays + holiday_cat, data = combined[-116, ])
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.64015 -0.24323 -0.02916  0.33166  1.19995 
## 
## Coefficients:
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     1.91312    0.26415   7.242 3.03e-11 ***
## GDP_per_Capita                  0.85465    0.21345   4.004 0.000102 ***
## Healthy_Life_Expectancy         1.44607    0.34784   4.157 5.69e-05 ***
## Freedom_to_Make_Life_Choices    1.58930    0.37395   4.250 3.96e-05 ***
## Generosity                      0.41424    0.34070   1.216 0.226156    
## Trust                           0.85499    0.53305   1.604 0.111062    
## Social_Support                  0.95987    0.22080   4.347 2.69e-05 ***
## Public_Holidays                -0.00685    0.01308  -0.524 0.601263    
## holiday_catMedium > 11 & <= 16 -0.09642    0.13383  -0.720 0.472475    
## holiday_catHigh > 16           -0.10405    0.20356  -0.511 0.610101    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5019 on 135 degrees of freedom
##   (1 observation deleted due to missingness)
## Multiple R-squared:  0.8225,	Adjusted R-squared:  0.8106 
## F-statistic: 69.49 on 9 and 135 DF,  p-value: < 2.2e-16
```

Interestingly, the number of `Public_Holidays` (and it's categorical derivative, `holiday_cat`) as well as `Trust` and `Generosity` have no significant bearing on predicting the happiness of a country.  Without a doubt, the strongest predictors of a nation's happiness are `GDP_per_Capita`, `Healthy_Life_Expectancy`, `Freedom_to_Make_Life_Choices` and  `Social_Support`. 

What nation wouldn't be happy if their people are relatively wealthy, healthy, free and supported?


