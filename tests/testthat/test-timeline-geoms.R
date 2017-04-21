context("Timeline geoms")

library(ggplot2)

earthquakes <- eq_clean_data(dget("data.R"))

test_that("geom_timeline layer is a GeomTimeline", {
    g <- ggplot(earthquakes,  aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, color = TOTAL_DEATHS))
    g <- g + geom_timeline(alpha = 0.5)
    expect_is(g$layers[[1]]$geom, "GeomTimeline")
})

test_that("geom_timeline contains correct alpha aesthetic", {
    g <- ggplot(earthquakes,  aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, color = TOTAL_DEATHS))
    g <- g + geom_timeline(alpha = 0.456)
    expect_equal(g$layers[[1]]$aes_params$alpha, 0.456)
})

##TODO: Determine how to test geom_timeline further

test_that("geom_timeline_label layer is a GeomTimelineLabel", {
    g <- ggplot(earthquakes,  aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, color = TOTAL_DEATHS))
    g <- g + geom_timeline(alpha = 0.5)
    g <- g + geom_timeline_label(aes(label = LOCATION_NAME))
    expect_is(g$layers[[2]]$geom, "GeomTimelineLabel")
})

test_that("geom_timeline_label has correct label", {
    g <- ggplot(earthquakes,  aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, color = TOTAL_DEATHS))
    g <- g + geom_timeline(alpha = 0.5)
    g <- g + geom_timeline_label(aes(label = LOCATION_NAME))
    expect_equal(g$layers[[2]]$mapping$label, as.name("LOCATION_NAME"))
})

##TODO: Determine how to test geom_timeline_label further
