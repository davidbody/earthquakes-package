context("Timeline geoms")

library(ggplot2)

earthquakes <- eq_clean_data(dget("data.R"))

test_that("geom_timeline contains the correct mappings", {
    g <- ggplot(earthquakes,  aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, color = TOTAL_DEATHS))
    g <- g + geom_timeline(alpha = 0.5)
    expect_equal(g$mapping$x, as.name("DATE"))
    expect_equal(g$mapping$y, as.name("COUNTRY"))
    expect_equal(g$mapping$size, as.name("EQ_PRIMARY"))
    expect_equal(g$mapping$colour, as.name("TOTAL_DEATHS"))
})

test_that("geom_timeline layer is a GeomTimeline", {
    g <- ggplot(earthquakes,  aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, color = TOTAL_DEATHS))
    g <- g + geom_timeline(alpha = 0.5)
    expect_is(g$layers[[1]]$geom, "GeomTimeline")
})

test_that("geom_timeline_label layer is a GeomTimelineLabel", {
    g <- ggplot(earthquakes,  aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, color = TOTAL_DEATHS))
    g <- g + geom_timeline(alpha = 0.5)
    g <- g + geom_timeline_label(aes(label = LOCATION_NAME))
    expect_is(g$layers[[2]]$geom, "GeomTimelineLabel")
})
