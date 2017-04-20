#' @title fars_read
#'
#' This is a function that checks the existence of a file
#' If the file does not exist in the repository, the function will stop
#' If the file exists, the function will return a Data Frame Tbl of the csv
#'
#'
#' @param filename A character string giving the name of the .csv file
#'

#'
#' @return return the .csv file in the format of Data Frame Tbl
#'
#' @examples \dontrun{
#' fars_read(filename = "accident_2013.csv")
#' fars_read("accident_2013.csv")
#' }
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(file.path(filename)))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(file.path(filename), progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' @title fars_make_filename Return String the name of the .csv.bz2 file
#'
#' This is a function that returns the .csv.bz2 file name with the
#' given year.
#'
#' @param year Integer value of the year as an input and creates a filename
#'
#' @return return the .csv.bz2 file name with the given year
#'
#' @examples
#' \dontrun{
#' make_filename(year = 2015)
#' make_filename(2015)}
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv", year)
}

#' @title fars_read_years
#'
#' The function fars_read_years is used to take the year as an input and create a filename
#' using make_filename function. The file is then read using the fars_read function.
#' If the year is invalid it returns an error message. The function returns the month and year
#' from a specified data set
#'
#'
#' @param years Integer input of years
#'
#'
#' @return If an error is generated a warning message "invalid year: " with
#' the invalid year information is displayed. If no error is generated it displays "NULL"
#'
#' @examples \dontrun{
#' fars_read_years(list(2013, 2014))
#' fars_read_years(c(2013, 2014, 2015))
#' fars_read_years(2014)
#' }
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' @title fars_summarize years
#'
#' The fars_summarize_years function takes a year as an input and summarizes the number of
#' accidents by month within the input year specified
#'
#' @param years Is an integer that takes the years as input and displays a monthwise summary
#' of the number of fatal injuries
#'
#'
#'
#' @return a data frame displaying a month number and the consolidated number of fatal
#' injuries foreach month of the specified year
#'
#' @examples \dontrun{
#' fars_summarize_years(list(2013, 2014))
#' fars_summarize_years(c(2013, 2014))
#' fars_summarize_years(2013)
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' @title fars_map_state
#'
#' This is a function that takes the state number and year as an input and displays a plot
#' of the state with the number of fatalaties mapped as dots at the appropriate location of the
#' incident
#'
#' @param state.num An integer representing the index of the state
#' @param year An interger representing the specified year
#'
#' @importFrom graphics points
#'
#' @return a map, it plots the location of the record on the map for the specified state
#' and year
#'
#' @examples \dontrun{
#' fars_map_state(state.num = 6, year = 2013)
#' fars_map_state(5, 2013)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
