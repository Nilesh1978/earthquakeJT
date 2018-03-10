library(testthat)
library(earthquakeJT)
library(dplyr)

#test_check("earthquakeJT")
test_that(desc = "TEST earthquakeJT",{
  data <- get_eq_data()
  expect_that(data, is_a("data.frame"))
  expect_that(eq_clean_data(data), is_a("data.frame"))
  expect_that(eq_location_clean(data), is_a("data.frame"))

  data <- get_eq_data() %>% eq_clean_data()

  expect_that(eq_map(data), is_a("leaflet"))
  expect_that(eq_create_label(data), is_a("data.frame"))

  expect_that(geom_timeline(), is_a("LayerInstance"))
  expect_that(geom_timeline_label(), is_a("LayerInstance"))



})

