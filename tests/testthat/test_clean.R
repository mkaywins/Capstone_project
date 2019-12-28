context("Test: eq_clean functions")

test_that("eq_clean and eq_clean_location have right formatting behaviour", {

  filename <- system.file("extdata", "signif.txt", package = "Capstone")
  test_data <- readr::read_delim(filename, delim = "\t") %>%
    eq_clean_data() %>%
    eq_location_clean()
  
  # check classes:
  expect_is(test_data$DATE, class = 'Date')
  expect_is(test_data$LOCATION_NAME, class = 'character')
})