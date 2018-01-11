#' Read calendar_dates.txt.
#'
#' @import readr
read_calendar_dates <- function(csv_filename) {
    calendar_dates <- read_csv(csv_filename, col_types = cols(
        service_id = col_character(),
        date = col_date("%Y%m%d"),
        exception_type = col_integer(),
        # For possible extensions.
        .default = col_character()
    ))
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
#' @import readr
read_calendar <- function(csv_filename) {
    calendar <- read_csv(csv_filename, col_types = cols(
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
    ))
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
