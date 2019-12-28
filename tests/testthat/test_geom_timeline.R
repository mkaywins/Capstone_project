context("Test: geom_timeline functions")

test_that("checking return-type of function", {

  filename <- system.file("extdata", "signif.txt", package = "Capstone")
  test_data <- readr::read_delim(filename, delim = "\t") %>%
    eq_clean_data() %>%
    eq_location_clean()
  # run functions and create ggplot-object
  gp <- test_data %>%
    dplyr::filter(COUNTRY == 'MEXICO') %>%
    ggplot2::ggplot(aes(date = date,
               xmin = as.Date('1990-01-01'),
               xmax = as.Date('2008-01-01'),
               y = COUNTRY,
               colour = DEATHS,
               fill = DEATHS,
               size = EQ_PRIMARY,
               location = LOCATION)) +
    geom_timeline() +
    geom_timelinelabel()


  #check return-type
  expect_is(gp, class = c('gg', 'ggplot'))
})
