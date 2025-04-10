###############################################
######
###### First attempt at text scraping UNGC data
######
###############################################

## packages
library(tidyverse)
library(stringr)
library(rvest)
library(lubridate)
library(rebus)
library(tictoc)

## url

url <- read_html('https://www.unglobalcompact.org/what-is-gc/participants/')

## page list

pagelist <- str_c('https://www.unglobalcompact.org/what-is-gc/participants/', '?page=', 103:1135)

## extraction functions

firmname <- function(html){
  html %>% 
    html_nodes('.name') %>% 
    html_text() %>% 
    unlist()
}

firmtype <- function(html){
  html %>% 
    html_nodes('.type') %>% 
    html_text() %>% 
    unlist()
}

sector <- function(html){
  html %>% 
    html_nodes('.sector') %>% 
    html_text() %>% 
    unlist()
}

country <- function(html){
  html %>% 
    html_nodes('.country') %>% 
    html_text() %>% 
    unlist()
}

joineddate <- function(html){
  html %>% 
    html_nodes('.joined-on') %>% 
    html_text() %>%  
    unlist()
}

## function to make data frame

get_data_table <- function(html){
  firm_name <- firmname(html)
  firm_type <- firmtype(html)
  firm_sector <- sector(html)
  firm_country <- country(html)
  joined_date <- joineddate(html)
  
  ungc_data <- tibble(firmname = firm_name,
                      firmtype = firm_type,
                      firmsector = firm_sector,
                      firmcountry = firm_country,
                      joineddate = joined_date)
}

## function to extract html data from urls

get_url_data <- function(url){
  read_html(url) %>% 
    get_data_table()
}

## function to make final dataset
make_df_final <- function(url){
  map(pagelist, get_url_data) %>% 
    bind_rows()
}

## generate dataset (all members, joined through 12/31/2018)

tic()
ungcmemberdata <- make_df_final(url)
toc()

# 464.9 sec elapsed

ungcmemberdata <- ungcmemberdata %>% filter(firmname != "Name")
ungcmemberdata <- ungcmemberdata %>% mutate(joineddate = ymd(joineddate),
                                            joinedyear = year(joineddate))

ungcmemberdata %>%
  group_by(joinedyear) %>% 
  summarize(n = n()) %>% mutate(total = cumsum(n)) %>%
  ggplot(aes(as.factor(joinedyear), n)) + 
  geom_point()

write.csv(ungcmemberdata, "~/MLE_paper/ungcmemberdata.csv")
