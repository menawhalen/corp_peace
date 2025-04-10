library(tidyverse)
library(readr)
library(haven)

## org data
org_dat <- read_dta("data/BHR_VDem_FDI_Merge 1.dta")
## are they UNGC members
## got from the scraping
ungcmember_data <- read_csv("data/ungcmemberdata.csv")
### bhrr data (did they response yes/no)

bhrr <- read_csv("data/BHRRC_cleaned.csv")

## country data table made for long/short names
## can add ccodes
country_data <- read_csv("data/country_data_table.csv")

## there are very few country codes
# org_dat %>% 
#   select(hqcountry, ccode) %>% 
#   distinct() %>% 
#   right_join(country_data, by = c("hqcountry" = "full_name")) %>% view()


############################################################################
## Try merging org data with ungc membership data

## work on name closeness
## all lower case and no punct
## remove limited

## have 924 companies
org_company <- org_dat %>% 
  select(companyname, hqcountry) %>% 
  distinct() %>% 
  mutate(company = str_remove(str_to_lower(companyname), " limited"))

### over 24k members companies
ungc_company <- ungcmember_data %>% 
  select(firmname, firmcountry) %>% 
  distinct() %>% 
  mutate(company = str_remove(str_to_lower(firmname), " inc.| llc| bhd.| ltd| inc| llc.| bhd| ltd." )) 
  
org_company %>% 
  left_join(ungc_company, by = c("company" = "company", "hqcountry" = "firmcountry")) %>% 
  filter(!is.na(firmname))
### only 59 matching

########################################
### try matching to org and bhrr data
### 5,500 companies
bhrr_company <- bhrr %>% 
  select(companies, full_name_HQ) %>% 
  distinct() %>% 
  mutate(company = str_remove(str_to_lower(companies), " inc.| llc| bhd.| ltd| inc| llc.| bhd| ltd." ))

org_company %>% 
  left_join(bhrr_company, by = c("company" = "company", "hqcountry" = "full_name_HQ")) %>% 
  filter(!is.na(companies))
## only 13 matching////