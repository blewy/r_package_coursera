#' Read csv data: fars_read
#'
#' This is a simple function that reads data from a .csv file into a data.frame using tbl_df.
#'
#' @param filename A character string that names the file the user wants to import
#'
#' @return This function returns a tibble if the file exists, if not it returns an warning
#' with a message ("file *filename* does not exist")
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{fars_read("accident_2015.csv.bz2")}
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Help function to create a file name string
#'
#' This is a function that creates a string with the needed structure to be
#' used in fars_read_years function for iterative importing several .csv files
#'
#' @param year a number that represents the year of the data to import.
#' the input does not need to be numeric, the function will parse the parameter to integer
#'
#' @return This function returns a string, with the following structure: "accident_(year).csv.bz2"
#' the year format in of the type 2001, 2011, this coincides qith the current design
#' of the .csv used for the analysis
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df mutate select
#' @importFrom matrittr '%>%'
#'
#' @examples
#' \dontrun{make_filename(1999)}
#' \dontrun{make_filename("2013")}
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read multiple csv data using fars_read and make_filename
#'
#' Iteractive function that enables the user to read multiple .csv files from a range of years
#' It will read a group of files on the same folder changing the year inside a range
#' that was defined by the user
#'
#' @param years a number that represents the year or years of data to import.
#' the user can import 1 year data or create a sequence like n:m (n<=m), the function will
#' read the files from year n to m
#'
#' @return This function returns a dataset with with the following structure:
#' one line per accident with Month and Year of accident as columns
#' if a cvs file of a year on the function parameters does not existe the funtion will
#' throw an warning with the message "invalid year: \code{current year}"
#' @examples
#' \dontrun{fars_read_years(2013)}
#' \dontrun{fars_read_years(c(2013:2015))}
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

#' Get statistics of accidents of multiple year per month
#'
#' Iteractive function that enable obtain the statistics from multiple .csv files from a range of years
#' It will read a group of files, row_bind all the data into one data.frame and them compute
#' statistics from the final data set.
#'
#' @param years a number that represents the year or years of data to import.
#' the user can import 1 year data or create a sequence like n:m (n<=m) and the function will
#' read the files from year n to m
#'
#' @return This function returns a dataset with the count os accidents of
#' every month (columns) grouped by year (rows)
#'
#' @importFrom dplyr bind_rows summarize summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{fars_summarize_years(2013)}
#' \dontrun{fars_summarize_years(2013:2015)}
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plot Accidents on a map choosing a year and US state
#'
#' Function that enables to obtain the statistics from .csv files from specific year and US State
#' and plot on a map the number of acidents per geographical location
#' It will read a group of files, select specific states, filter the imported data
#' and plot the counts on a map using longitude and latitude
#'
#' @param year a number that represents the year or years of the data to import.
#' the user can import 1 year data or create a sequence like n:m (n<=m) and the function will
#' read the files from year n to m
#'
##' @param state.num a number that the US states that we choose to analyse and plot
##' if the code state does not exists on the files imported the function will stop
##' and show the message : "invalid STATE number: ", \code{state.num}", also if there's
##' no accidents to plot the function will output a message saying :"no accidents to plot"
##' and it will return NULL not ploting on the current device
#'
#' @return This function creates a plot using maps
#'
#' @import mapdata
#' @importFrom tidyr spread
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(state.num=10134,year=2013)}
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
