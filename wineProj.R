#----------------#
# Load Libraries #
#----------------#

library(highcharter)
library(jsonlite)
library(tidyverse)
import::from(magrittr, `%$%`)
import::from(assertthat, assert_that)
import::from(glue, glue)
library(wordcloud)

#----------------#
# Source Helpers #
#----------------#

wineSummarizer <- 
  function(groupBy = "country"){
    assert_that(exists("wineReviews") == TRUE, msg = 'Please load variable wineReviews')
    ..groupBy <- quo(!!sym(groupBy))
    # browser()
    wineReviews %>%
      group_by(!!..groupBy) %>%
      summarize(
        avgPoints = mean(points),
        count = n()
      ) %>%
      filter(count > 100) %>%
      arrange(avgPoints %>% desc())
  }

#--------------#
#  Global Vars #
#--------------#

dataDir    <- "data"
first150k  <- "winemag-data_first150k.csv"
second130k <- "winemag-data-130k-v2.csv"
jsonData   <- "winemag-data-130k-v2.json"

#-----------#
# Load Data #
#-----------#

wineReviews <- 
  read_csv(glue("{dataDir}/{first150k}")) %>%
  bind_rows(read_csv(glue("{dataDir}/{second130k}"))) %>%
  select(-`X1`)

jsonData <- 
  read_json(glue("{dataDir}/{jsonData}")) %>% 
  map_df(
    function(x){
      t(x) %>% 
        data.frame()
    }
  )

#------------------#
# Conduct Analysis #
#------------------#

countryScores <- wineSummarizer("country")  %>% slice(1:100)

varietyScores <- wineSummarizer("variety") %>% slice(1:100)

#------------#
# Word Cloud #
#------------#

wineReviews %>% arrange(points %>% desc()) %>% slice(1:100) %$% wordcloud(description)

#----------------------#
# Price Point Relation #
#----------------------#

wineReviews %$% cor(price, points, use = 'complete.obs')

regression <- lm(points ~ log(price), data = wineReviews)

wineReviews %$% plot(log(price), points, type = 'p')



