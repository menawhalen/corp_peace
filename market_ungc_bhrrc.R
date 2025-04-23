library(haven)
library(tidyverse)
library(janitor)
library(fastLink)
## market line
market <- read_dta("data/BHR foreign companies.dta") %>% clean_names()

sub_market <- market %>% 
  select(company_name, headquarters) %>% 
  mutate(company_name = str_to_lower(company_name),
         headquarters = str_to_lower(headquarters)) %>% 
  distinct()


ungc <- read_csv("data/ungcmemberdata.csv")  %>% 
  select(firmname, firmcountry, joineddate) %>% 
  rename(company_name = firmname, headquarters = firmcountry) %>% 
  mutate(company_name = str_to_lower(company_name),
         headquarters = str_to_lower(headquarters))


blocking <- blockData(dfA = sub_market, dfB = ungc, varnames = "headquarters")


linkage_results <- list()

# Iterate over each block and run fastLink()
for (i in 1:length(blocking)) {
  block_pair <- blocking[[i]]  # Get the current blocked dataset
  data_a <- sub_market[block_pair$dfA.inds,]
  data_b <- ungc[block_pair$dfB.inds,]
  # Run fastLink on the blocked datasets
  fl_out <- fastLink(
    dfA = data_a,
    dfB = data_b,
    varnames = "company_name",  # Specify matching variables
    stringdist.match = "company_name",  # Use string distance for fuzzy matching
    stringdist.method = "jw",
    threshold.match = 0.10
  )
  
  # Store the result
  linkage_results[[i]] <- list(link = fl_out, data_a = data_a, data_b = data_b)
}



test_match <- map(linkage_results, ~ bind_cols(.x$data_a[.x$link$matches$inds.a,],
                                               .x$data_b[.x$link$matches$inds.b,],
                                               posterior = .x$link$posterior))

matched_ungc <- test_match %>% 
  bind_rows() %>% 
  rename(company_name = company_name...1, headquarters = headquarters...2, ungc_date_joined = joineddate, ungc_posterior_match = posterior) %>% 
  select(company_name, headquarters, ungc_date_joined, ungc_posterior_match)


market_ungc <- sub_market %>% 
  left_join(matched_ungc, by = join_by(company_name, headquarters)) 


#######################################################

### now do bhrrc
sub_market_bhrrc <- market %>% 
  select(company_name, headquarters, country_location) %>% 
  mutate(company_name = str_to_lower(company_name),
         headquarters = str_to_lower(headquarters),
         country_location = str_to_lower(country_location)) %>% 
  distinct()



bhrrc <- read_csv("data/BHRRC_cleaned.csv") %>% 
  rename(company_name = companies, headquarters = full_name_HQ, country_location = full_name_operate_country) %>% 
  mutate(company_name = str_to_lower(company_name),
         headquarters = str_to_lower(headquarters),
         country_location = str_to_lower(country_location))

blocking_bhrrc <- blockData(dfA = sub_market_bhrrc, dfB = bhrrc, varnames = c("headquarters", "country_location"))

linkage_results_bhrrc <- list()

# Iterate over each block and run fastLink()
for (i in 1:length(blocking_bhrrc)) {
  block_pair <- blocking_bhrrc[[i]]  # Get the current blocked dataset
  data_a <- sub_market_bhrrc[block_pair$dfA.inds,]
  data_b <- bhrrc[block_pair$dfB.inds,]
  # Run fastLink on the blocked datasets
  fl_out <- fastLink(
    dfA = data_a,
    dfB = data_b,
    varnames = "company_name",  # Specify matching variables
    stringdist.match = "company_name",  # Use string distance for fuzzy matching
    stringdist.method = "jw",
    threshold.match = 0.10
  )
  
  # Store the result
  linkage_results_bhrrc[[i]] <- list(link = fl_out, data_a = data_a, data_b = data_b)
}



match_bhrrc <- map(linkage_results_bhrrc, ~ bind_cols(.x$data_a[.x$link$matches$inds.a,],
                                               .x$data_b[.x$link$matches$inds.b,],
                                               posterior = .x$link$posterior))

matched_bhrrc <- match_bhrrc %>% 
  bind_rows() %>% 
  rename(company_name = company_name...1, headquarters = headquarters...2, country_location = country_location...3) %>% 
  select(-c(company_name...4, headquarters...5, country_location...6)) %>% 
  rename( bhrrc_posterior_match = posterior) 

market_ungc_bhrrc <- market %>% 
  select(company_name, country_location, headquarters, revenue_per_employee_usd, no_of_employees, sales_growth, incorporation_year) %>% 
  mutate(company_name = str_to_lower(company_name),
         headquarters = str_to_lower(headquarters),
         country_location = str_to_lower(country_location)) %>% 
  left_join(matched_ungc, by = join_by(company_name, headquarters)) %>% 
  left_join(matched_bhrrc, by = join_by(company_name, headquarters, country_location)) %>% 
  mutate(in_ungc = ifelse(is.na(ungc_date_joined), 0, 1)) 
