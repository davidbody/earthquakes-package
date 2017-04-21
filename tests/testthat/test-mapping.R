context("Mapping")

earthquakes <- eq_clean_data(dget("data.R"))

test_that("eq_map creates a leaflet HTML widget", {
    map <- eq_map(earthquakes, annot_col = "DATE")
    expect_equal(class(map), c("leaflet", "htmlwidget"))
})

##TODO: Determine how to test eq_map further

test_that("eq_create_label returns a proper HTML label", {
    df <- data.frame(LOCATION_NAME = "Des Moines", EQ_PRIMARY = 3.0, TOTAL_DEATHS = 0)
    label <- eq_create_label(df)
    expect_equal(label, "<b>Location:</b> Des Moines<br/><b>Magnitude:</b> 3<br/><b>Total deaths:</b> 0")
})
