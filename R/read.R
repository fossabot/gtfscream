#' Read calendar_dates.txt.
#'
#' @import dplyr
#' @import readr
read_calendar_dates <- function(csv_filename) {
    calendar_dates <- read_csv(
        csv_filename,
        col_types = cols(
            service_id = col_character(),
            date = col_date("%Y%m%d"),
            exception_type = col_integer(),
            # For possible extensions.
            .default = col_character()
        ),
        na = ""
    )
    invalid_calendar_dates <- calendar_dates %>%
        group_by(service_id, date) %>%
        distinct(exception_type) %>%
        summarize(n_exception_types = n()) %>%
        ungroup() %>%
        filter(n_exception_types > 1L)
    if (nrow(invalid_calendar_dates) > 0L) {
        message("Invalid lines in calendar_dates.txt:")
        message(invalid_calendar_dates)
        message("Removing them from the result.")
        calendar_dates <- calendar_dates %>%
            anti_join(invalid_calendar_dates, by = c("service_id", "date"))
    }
    calendar_dates
}

#' Read calendar.txt.
#'
#' @import dplyr
#' @import readr
read_calendar <- function(csv_filename) {
    calendar <- read_csv(
        csv_filename,
        col_types = cols(
            service_id = col_character(),
            monday = col_logical(),
            tuesday = col_logical(),
            wednesday = col_logical(),
            thursday = col_logical(),
            friday = col_logical(),
            saturday = col_logical(),
            sunday = col_logical(),
            start_date = col_date("%Y%m%d"),
            end_date = col_date("%Y%m%d"),
            # For possible extensions.
            .default = col_character()
        ),
        na = ""
    )
    invalid_calendar <- calendar %>%
        filter(end_date < start_date)
    if (nrow(invalid_calendar) > 0L) {
        message("Invalid lines in calendar.txt:")
        message(invalid_calendar)
        message("Removing them from the result.")
        calendar <- calendar %>%
            anti_join(invalid_calendar, by = names(invalid_calendar))
    }
    calendar
}

#' Read trips.txt.
#'
#' @import readr
read_trips <- function(csv_filename) {
    read_csv(
        csv_filename,
        col_types = cols(
            route_id = col_character(),
            service_id = col_character(),
            trip_id = col_character(),
            trip_headsign = col_character(),
            trip_short_name = col_character(),
            direction_id = col_integer(),
            block_id = col_character(),
            shape_id = col_character(),
            wheelchair_accessible = col_integer(),
            bikes_allowed = col_integer(),
            # For possible extensions.
            .default = col_character()
        ),
        na = ""
    )
}

#' Read stop_times.txt.
#'
#' @import readr
read_stop_times <- function(csv_filename) {
    read_csv(
        csv_filename,
        col_types = cols(
            trip_id = col_character(),
            arrival_time = col_character(),
            departure_time = col_character(),
            stop_id = col_character(),
            stop_sequence = col_integer(),
            stop_headsign = col_character(),
            pickup_type = col_integer(),
            drop_off_type = col_integer(),
            shape_dist_traveled = col_double(),
            timepoint = col_integer(),
            # For possible extensions.
            .default = col_character()
        ),
        na = ""
    )
}

#' Read routes.txt.
#'
#' @import readr
read_routes <- function(csv_filename) {
    read_csv(
        csv_filename,
        col_types = cols(
            route_id = col_character(),
            agency_id = col_character(),
            route_short_name = col_character(),
            route_long_name = col_character(),
            route_desc = col_character(),
            route_type = col_integer(),
            route_url = col_character(),
            route_color = col_character(),
            route_text_color = col_character(),
            route_sort_order = col_integer(),
            # For possible extensions.
            .default = col_character()
        ),
        na = ""
    )
}

#' Read stops.txt.
#'
#' @import readr
read_stops <- function(csv_filename) {
    read_csv(
        csv_filename,
        col_types = cols(
            stop_id = col_character(),
            stop_code = col_character(),
            stop_name = col_character(),
            stop_desc = col_character(),
            stop_lat = col_double(),
            stop_lon = col_double(),
            zone_id = col_character(),
            stop_url = col_character(),
            location_type = col_integer(),
            parent_station = col_character(),
            stop_timezone = col_character(),
            wheelchair_boarding = col_integer(),
            # For possible extensions.
            .default = col_character()
        ),
        na = ""
    )
}
