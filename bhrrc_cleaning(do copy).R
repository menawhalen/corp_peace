library(tidyverse)
library(haven)
library(janitor)

#dat <- read_dta("data/CIRIFarrisCOPCCRSPPI_TS.dta")
dat <- read_csv("data/BHRRC.csv") %>% 
  clean_names()

## create table for country codes to use repeatedly
country_data <- data.frame(
  country_code = c("AF", "AL", "AM", "AO", "AR", "AS", "AT", "AU", "AW", "AX", "AZ", "BA", "BB", "BD", "BE", 
                   "BF", "BG", "BH", "BI", "BJ", "BL", "BM", "BN", "BO", "BQ", "BR", "BS", "BT", "BV", "BW", 
                   "BY", "BZ", "CA", "CC", "CD", "CF", "CG", "CH", "CI", "CK", "CL", "CM", "CN", "CO", "CR", 
                   "CU", "CV", "CW", "CX", "CY", "CZ", "DE", "DJ", "DK", "DM", "DO", "DZ", "EC", "EE", "EG", 
                   "EH", "ER", "ES", "ET", "FI", "FJ", "FM", "FO", "FR", "GA", "GB", "GD", "GE", "GF", "GG", 
                   "GH", "GI", "GL", "GM", "GN", "GP", "GQ", "GR", "GT", "GU", "GW", "GY", "HK", "HM", "HN", 
                   "HR", "HT", "HU", "ID", "IE", "IL", "IM", "IN", "IO", "IQ", "IR", "IS", "IT", "JE", "JM", 
                   "JO", "JP", "KE", "KG", "KH", "KI", "KM", "KN", "KP", "KR", "KW", "KY", "KZ", "LA", "LB", 
                   "LC", "LI", "LK", "LR", "LS", "LT", "LU", "MO", "MK", "MG", "MW", "MY", "ML", "MT", "MH", "MQ", "MR", "MU", "YT", "MX", 
                   "FM", "MD", "MC", "MN", "ME", "MS", "MA", "MZ", "MM", "NA", "NR", "NP", "NL", "NC", "NZ", 
                   "NI", "NE", "NG", "NO", "OM", "PK", "PA", "PG", "PY", "PE", "PH", "PL", "PT", "PR", "QA", 
                   "RO", "RU", "RW", "WS", "ST", "SA", "SN", "RS", "SC", "SL", "SG", "SK", "SI", "SB", "SO", 
                   "ZA", "ES", "LK", "SD", "SR", "SZ", "SE", "CH", "SY", "TJ", "TH", "TG", "TM", "TL", "TN", 
                   "TR", "TV", "UG", "UA", "AE", "GB", "US", "UY", "UZ", "VU", "VE", "VN", "YE", "ZM", "ZW"),
  full_name = c("Afghanistan", "Albania", "Armenia", "Angola", "Argentina", "American Samoa", "Austria", "Australia", 
                   "Aruba", "Åland Islands", "Azerbaijan", "Bosnia and Herzegovina", "Barbados", "Bangladesh", "Belgium", 
                   "Burkina Faso", "Bulgaria", "Bahrain", "Burundi", "Benin", "Saint Barthélemy", "Bermuda", "Brunei", 
                   "Bolivia", "Bonaire, Sint Eustatius and Saba", "Brazil", "Bahamas", "Bhutan", "Bouvet Island", 
                   "Botswana", "Belarus", "Belize", "Canada", "Cocos (Keeling) Islands", "Democratic Republic of the Congo", 
                   "Central African Republic", "Republic of the Congo", "Switzerland", "Ivory Coast", "Cook Islands", 
                   "Chile", "Cameroon", "China", "Colombia", "Costa Rica", "Cuba", "Cape Verde", "Curaçao", 
                   "Christmas Island", "Cyprus", "Czech Republic", "Germany", "Djibouti", "Denmark", "Dominica", 
                   "Dominican Republic", "Algeria", "Ecuador", "Estonia", "Egypt", "Western Sahara", "Eritrea", "Spain", 
                   "Ethiopia", "Finland", "Fiji", "Federated States of Micronesia", "Faroe Islands", "France", "Gabon", 
                   "United Kingdom", "Grenada", "Georgia", "French Guiana", "Guernsey", "Ghana", "Gibraltar", "Greenland", 
                   "Gambia", "Guinea", "Guadeloupe", "Equatorial Guinea", "Greece", "Guatemala", "Guam", "Guinea-Bissau", 
                   "Guyana", "Hong Kong", "Heard Island and McDonald Islands", "Honduras", "Croatia", "Haiti", "Hungary", 
                   "Indonesia", "Ireland", "Israel", "Isle of Man", "India", "British Indian Ocean Territory", "Iraq", 
                   "Iran", "Iceland", "Italy", "Jersey", "Jamaica", "Jordan", "Japan", "Kenya", "Kyrgyzstan", "Cambodia", 
                   "Kiribati", "Comoros", "Saint Kitts and Nevis", "North Korea", "South Korea", "Kuwait", "Cayman Islands", 
                   "Kazakhstan", "Laos", "Lebanon", "Saint Lucia", "Liechtenstein", "Sri Lanka", "Liberia", "Lesotho", 
                   "Lithuania", "Luxembourg", "Macau", "North Macedonia", "Madagascar", "Malawi", "Malaysia", "Mali", "Malta", 
                   "Marshall Islands", "Martinique", "Mauritania", "Mauritius", "Mayotte", "Mexico", "Micronesia", 
                   "Moldova", "Monaco", "Mongolia", "Montenegro", "Montserrat", "Morocco", "Mozambique", "Myanmar", 
                   "Namibia", "Nauru", "Nepal", "Netherlands", "New Caledonia", "New Zealand", "Nicaragua", "Niger", 
                   "Nigeria", "Norway", "Oman", "Pakistan", "Panama", "Papua New Guinea", "Paraguay", "Peru", 
                   "Philippines", "Poland", "Portugal", "Puerto Rico", "Qatar", "Romania", "Russia", "Rwanda", 
                   "Samoa", "São Tomé and Príncipe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", 
                   "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", "Spain", 
                   "Sri Lanka", "Sudan", "Suriname", "Swaziland", "Sweden", "Switzerland", "Syria", "Tajikistan", 
                   "Thailand", "Togo", "Turkmenistan", "Timor-Leste", "Tunisia", "Turkey", "Tuvalu", "Uganda", "Ukraine", 
                   "United Arab Emirates", "United Kingdom", "United States", "Uruguay", "Uzbekistan", "Vanuatu", 
                   "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe")) %>% 
  distinct()
#write_csv(country_data, "data/country_data_table.csv")
### get long names for HQ 
## and separate rows for each country they work in
## then make the long name
cleaned <- dat %>% 
  left_join(country_data, by = c("company_headquarters" = "country_code")) %>% 
  rename(full_name_HQ = full_name) %>% 
  separate_longer_delim(countries, delim = "|") %>% 
  left_join(country_data, by = c("countries" = "country_code")) %>% 
  rename(full_name_operate_country = full_name) %>% 
  mutate(backdate = mdy(backdate)) %>% 
  select(companies, full_name_HQ, full_name_operate_country,backdate, responded, regions, response_sectors, title, responded_to, tags, authors, url, company_headquarters, countries)
  
write_csv(cleaned, "data/BHRRC_cleaned.csv")
