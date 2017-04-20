library(testthat)
library(MyPackage)

test_check("MyPackage")
x<-2020
fars_read_years(x)
expect_that(fars_read_years(x),gives_warning())

y<-2013
fars_read_years(y)
expect_that(fars_read_years(y),gives_warning())
