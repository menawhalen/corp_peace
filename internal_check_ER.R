library(haven)
library(tidyverse)
library(janitor)
library(fastLink)
## market line
market <- read_dta("data/BHR foreign companies.dta") %>% clean_names()
## org data
org_dat <- read_dta("data/BHR_VDem_FDI_Merge 1.dta") %>% clean_names()

sub_org <- org_dat %>% 
  select(companyname, hqcountry, employees, revenue) %>% ## remove year to not over duplicate records only unique companies
  distinct() %>% 
  rename(company_name = companyname, headquarters = hqcountry)

sub_market <- market %>% 
  select(company_name, headquarters, country_location, incorporation_year, 
         no_of_employees, total_assets_usd, total_liabilities_usd) 

## step 1: what matches between org and market line
## want to find duplicates and take newest marketline
# blocking
blocking <- blockData(dfA = sub_org, dfB = sub_market, varnames = "headquarters")


# join_duplicates <- fastLink(dfA = sub_org, dfB = sub_org,
#                             varnames = c("company_name", "headquarters"),
#                             return.all = TRUE)

# Placeholder for results
linkage_results <- list()

# Iterate over each block and run fastLink()
for (i in c(1:22, 25:35)) {
  block_pair <- blocking[[i]]  # Get the current blocked dataset
  
  # Run fastLink on the blocked datasets
  fl_out <- fastLink(
    dfA = sub_org[block_pair$dfA,],
    dfB = sub_market[block_pair$dfB,],
    varnames = "company_name",  # Specify matching variables
    stringdist.match = "company_name"  # Use string distance for fuzzy matching
  )
  
  # Store the result
  linkage_results[[i]] <- fl_out
}

## wont work with 23 or 24
blocking[[23]]
sub_org[12321,]
sub_market[559,]
## morocco and mauritius not working....


linkage_results[[1]]

match_test <- getMatches(dfA = sub_org, dfB = sub_market, fl.out = linkage_results[[1]])


# 
# 
# linkage_test <- map(blocking, ~ fastLink(
#   dfA = sub_org[.x$dfA,],
#   dfB = sub_market[.x$dfB,],
#   varnames = "company_name",  # Specify matching variables
#   stringdist.match = "company_name"  # Use string distance for fuzzy matching
# ))
# library(reclin2)
# blocking <- pair_blocking(sub_org, sub_market, on = "headquarters")
# match_dup <- compare_pairs(blocking, on = "company_name", 
#                            default_comparator = cmp_jarowinkler(0.9), inplace = TRUE)
# 
# scores <- problink(~ company_name, data = match_dup)
# 
# company_match_scores <- predict(scores, pairs = match_dup, add = TRUE)