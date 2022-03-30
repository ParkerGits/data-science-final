covid_counties <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-counties.csv")
# covid_counties <- read_csv("us-counties.csv")
county_demographics <- read_csv("cc-est2019-alldata.csv")

cov_counties <- covid_counties %>%
  group_by(geoid) %>%
  arrange(date) %>%
  mutate(total_cases = cumsum(cases)) %>%
  separate(col="geoid", into=c("US", "FIPS"), sep="-") %>%
  select(date, FIPS, county, state, total_cases) %>%
  filter(date > "2020-03-20")

demo_counties <- county_demographics %>%
  filter(YEAR == 12) %>% 
  select(STATE:TOT_FEMALE, WAC_MALE:AAC_FEMALE) %>%
  unite(col="FIPS", STATE, COUNTY, sep="") %>%
  mutate(NW_MALE = TOT_MALE - WAC_MALE, NW_FEMALE = TOT_FEMALE - WAC_FEMALE) %>%
  select(-WAC_MALE, -WAC_FEMALE) %>%
  pivot_longer(BAC_MALE:NW_FEMALE, names_to = "race_sex", values_to = "count") %>%
  separate(race_sex, into=c("RACE", "SEX"), sep="_") %>%
  group_by(FIPS, STNAME, CTYNAME, RACE) %>%
  summarize(count = sum(count), TOT_POP = sum(TOT_POP)) %>%
  mutate(prop = count / TOT_POP, cut=cut(prop, breaks=c(0, 0.10, 0.25, 0.50, 1.00), labels=c("Less than 10%", "10%-24.9%", "25-50%", "More than 50%"), include.lowest=TRUE))

cov_demo_counties <- inner_join(cov_counties, demo_counties, by="FIPS") %>%
  mutate(propInfect = total_cases / TOT_POP) %>%
  select(date, RACE, cut, propInfect, count)

# write.csv(cov_demo_counties, '~/Documents/Data Science/Final Project Part 2/Final/covid_demo_counties.csv')

hesitancy_data <- read_csv("fb-survey-smoothed_wcovid_vaccinated_appointment_or_accept.csv")
hesitancy_data1 <- hesitancy_data %>%
  mutate(week = as.Date(cut(time_value, "week"), '%Y-%m-%d')) %>%
  group_by(geo_value, week) %>% 
  summarise(value = mean(value)) %>%
  mutate(hesitancy = (100-value)/100, fips_state=ifelse(substr(geo_value, 3, 5) == "000", substr(geo_value,1,2), NA))


us_county <- counties_sf("laea")
us_states <- usa_sf("laea")

hesitancy_county <- filter(hesitancy_data1, is.na(fips_state))

hesitancy_megacounty <- filter(hesitancy_data1, !is.na(fips_state))

# write.csv(hesitancy_megacounty, "~/Documents/Data Science/Final Project Part 2/Final/hesitancy_megacounty.csv")
# write.csv(hesitancy_county, "~/Documents/Data Science/Final Project Part 2/Final/hesitancy_county.csv")
