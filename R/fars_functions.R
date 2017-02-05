#' reads csv file
#'
#' This function load csv file from the disk. First, it checks if the file
#'  returns a dataframe.
#'
#' @param filename a character string giving the filename.
#'
#' @return returns a dataframe if the file exist on the disk.
#'
#' @importFrom readr dplyr
#'
#' @examples fars_read("filename")
#'
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' print the filename
#'
#' The function get integer as input add return the modified filename.
#'
#' @param year character string or number of year.
#'
#' @return returns the modified filename with the given year added to it.
#'
#' @examples make_filename(1988)
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' extract the data for given years
#'
#' This function get vector of years, load the data belonging to that date from
#' the file and returns a datarame of including the given years. It prints
#' invalid year if the year is not included in the data.
#'
#' @param years A vector of years to be filtered from the files.
#'
#' @return returns the dataframe includings the years given in the input.
#'
#' @importFrom  dplyr tidyr
#'
#' @examples fars_read_years(1977)
#'
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

#' returns the count of months and years
#'
#' The function reads the list file given in parameter years, binds the rows of
#' each dataframe and returns the count of years and month.
#'
#' @param years a character string or integer list for set of files beloging to
#' years given in the input.
#'
#' @return returns the count of data points for each month and year given in the
#' input in a wide format.
#'
#' @importFrom tidyr dplyr
#'
#' @examples fars_summarize_years(1997)
#'
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' plot the accident on the map
#'
#' The function filters for state.num and year given in the input. if the number
#' of accident is zero for given year and state it returns "no accident to plot".
#' The function plot the map with accident as point on it.
#'
#' @param state.num a list of integers or character string of numbers for desired
#' states.
#' @param year a list of integer or character string of numbers for desired years
#' to be read from the disk.
#'
#' @return a map of accidents.
#'
#' @importFrom dplyr maps graphics
#'
#' @examples fars_map_state(1, 1977)
#'
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
