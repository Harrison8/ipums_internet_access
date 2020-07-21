library(glptools)
glp_load_packages(graphs = T)

# Example of creating a PUMA Map
PUMA_data <- data.frame(
  PUMA = 1701:1706,
  internet_pct = c(25, 30, 35, 35, 60, 50)
)

make_map(PUMA_data, "internet_pct", "Internet Access")

# Explore relevant ACS tables
if (FALSE) {
  # Device, detailed
  test1 <- build_census_var_df("acs5", "B28001")

  # Internet, detailed
  test2 <- build_census_var_df("acs5", "B28002")

  # Computer by internet
  test3 <- build_census_var_df("acs5", "B28003")

  # Computer by internet and cellular
  test4 <- build_census_var_df("acs5", "B28008")

  # Device
  test5 <- build_census_var_df("acs5", "B28010")

  # Internet
  test6 <- build_census_var_df("acs5", "B28011")

  # Age, device, and internet
  test7 <- build_census_var_df("acs5", "B28005")
}


# Overall Internet Access
internet <- build_census_var_df("acs5", "B28011")

internet %<>% get_census("tract")

internet %<>%
  mutate(
    internet = case_when(
      str_detect(label, "With an Internet subscription") ~ TRUE,
      str_detect(label, "Internet access without a subscription") ~ TRUE,
      str_detect(label, "No Internet access") ~ FALSE,
      TRUE ~ NA))

internet %<>% process_census(var_names = "value", cat_var = "internet",
                             output_name = "internet_access")

internet %>%
  process_map(internet_access, pop = "internet_access_pop", return_name = "internet", method = "mean") %>%
  list2env(.GlobalEnv)

make_map(list(internet_tract, internet_nh, internet_muw), "internet_access", "Internet Access")

# To save map objects, use htmlwidgets::saveWidget after setting the working directory to the save location
#
#m <- make_map(list(internet_tract, internet_nh, internet_muw), "internet_access", "Internet Access")
#
#setwd("images")
#htmlwidgets::saveWidget(m, "internet_map.R")
#setwd("..")

# Device and Internet Access among children ages 0-17
internet_age <- build_census_var_df("acs5", "B28005")

internet_age %<>% get_census("tract")

internet_age %<>%
  mutate(
    computer_broadband = case_when(
      str_detect(label, "Has a computer!!With a broadband Internet subscription") ~ TRUE,
      str_detect(label, "Has a computer!!") ~ FALSE,
      str_detect(label, "No computer") ~ FALSE,
      TRUE ~ NA))

internet_age %<>% process_census(var_names = "value", cat_var = "computer_broadband",
                                 output_name = "access", age_groups = "under_18")

internet_age %>%
  process_map(access, pop = "access_pop", return_name = "internet_age", method = "mean") %>%
  list2env(.GlobalEnv)

make_map(list(internet_age_nh, internet_age_muw), "access", "Internet Access")


