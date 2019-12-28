context("Test: eq_map functions")

test_that("test eq_map and eq_create_label have the right return-type", {

  filename <- system.file("extdata", "signif.txt", package = "Capstone")
  test_data <- readr::read_delim(filename, delim = "\t") %>%
    eq_clean_data() %>%
    eq_location_clean()


  gp <- test_data %>%
    dplyr::filter(COUNTRY == 'MEXICO' & lubridate::year(DATE) >= 2000) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eq_map() # run leaflet-map function

  # check return-type
  expect_is(gp, class = c("leaflet", "htmlwidget"))
})
