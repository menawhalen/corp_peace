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
  rename(company_name = companyname, headquarters = hqcountry) %>% 
  mutate(company_name = str_to_lower(company_name),
         headquarters = str_to_lower(headquarters)) %>% 
  distinct()
  

sub_market <- market %>% 
  select(company_name, headquarters) %>% 
  mutate(company_name = str_to_lower(company_name),
         headquarters = str_to_lower(headquarters)) %>% 
  distinct()
## step 1: what matches between org and market line
## want to find duplicates and take newest marketline
# blocking
blocking <- blockData(dfA = sub_org, dfB = sub_market, varnames = "headquarters")


# join_duplicates <- fastLink(dfA = sub_org, dfB = sub_org,
#                             varnames = c("company_name", "headquarters"),
#                             return.all = TRUE)
### issues with / in one marker of morocco

sub_org[568,"company_name"] <- str_replace(sub_org[568,"company_name"], "�", "")


# Placeholder for results
linkage_results <- list()

# Iterate over each block and run fastLink()
for (i in c(1:35)) {
  block_pair <- blocking[[i]]  # Get the current blocked dataset
  data_a <- sub_org[block_pair$dfA.inds,]
  data_b <- sub_market[block_pair$dfB.inds,]
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

# ## wont work with 23 or 24
# blocking[[23]]
# sub_org[12321,]
# sub_market[559,]
# ## morocco and mauritius not working....
# 
# bind_cols(linkage_results[[5]]$data_a[linkage_results[[5]]$link$matches$inds.a,],
#           linkage_results[[5]]$data_b[linkage_results[[5]]$link$matches$inds.b,],
#           linkage_results[[5]]$link$posterior) %>% view()

test_match <- map(linkage_results, ~ bind_cols(.x$data_a[.x$link$matches$inds.a,],
                                 .x$data_b[.x$link$matches$inds.b,],
                                 posterior = .x$link$posterior))
### INTERNAL COMPANIES
internal_companies <- test_match %>% 
  bind_rows() %>% 
  mutate(country_location = headquarters...2) %>% 
  select(company_name...1, headquarters...2, employees, revenue, country_location) %>% 
  rename(company_name = company_name...1, headquarters = headquarters...2)


market %>% 
  select(company_name, headquarters, country_location,annual_revenue_usm, no_of_employees ) %>% 
  mutate(company_name = str_to_lower(company_name)) %>% 
  left_join(bind_rows(test_match), by = c("company_name" = "company_name...5")) %>% 
  filter(is.na(employees)) %>% view()


market %>% 
  filter(headquarters == country_location) 
# linkage_results[[5]]
# 
# match_test <- getMatches(dfA = sub_org, dfB = sub_market, fl.out = linkage_results[[1]])
# 
# matches <- map(linkage_results, ~ getMatches(dfA = sub_org, dfB = sub_market, fl.out = .x))
# #agg <- aggregateEM(em.list = linkage_results)
# match <- matches %>% 
#   bind_rows()

# 
# 
# linkage_test <- map(blocking, ~ fastLink(
#   dfA = sub_org[.x$dfA,],
#   dfB = sub_market[.x$dfB,],
#   varnames = "company_name",  # Specify matching variables
#   stringdist.match = "company_name"  # Use string distance for fuzzy matching
# ))
# library(reclin2)
# blocking_rec <- pair_blocking(sub_org, sub_market, on = "headquarters")
# compare_pairs(blocking_rec, on = "company_name",
#                            default_comparator = cmp_jarowinkler(0.9), inplace = TRUE)
# 
# scores <- problink(~ company_name, data = match_dup)
# 
# company_match_scores <- predict(scores, pairs = match_dup, add = TRUE)