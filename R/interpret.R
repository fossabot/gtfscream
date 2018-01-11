#' Get all valid service_ids for a given operating day.
#'
#' @import dplyr
get_service_ids <- function(calendar, calendar_dates, date) {
    exceptions <- calendar_dates %>%
        filter(date == date)
    excluded <- exceptions %>%
        filter(exception_type == 2L) %>%
        select(service_id)
    included <- exceptions %>%
        filter(exception_type == 1L) %>%
        select(service_id)
    calendar %>%
        filter(
            start_date <= date,
            end_date >= date
        ) %>%
        filter_(tolower(weekdays(date))) %>%
        select(service_id) %>%
        setdiff(excluded) %>%
        union(included)
}

#' Get all valid service_id and operating day combinations.
#'
#' @return A tibble with columns service_id and date.
#' @import dplyr
#' @import purrr
#' @import tidyr
combine_calendar <- function(calendar, calendar_dates) {
    min_date <- min(min(calendar_dates[["date"]]),
                    min(calendar[["start_date"]]))
    max_date <- max(max(calendar_dates[["date"]]),
                    max(calendar[["end_date"]]))
    dates <- seq(min_date, max_date, by = "1 day")
    map_dfr(dates, function(date) {
            tibble(
                service_id = get_service_ids(calendar, calendar_dates, date),
                date = date
            )
        }) %>%
        unnest()
}

#' Read calendar and get valid service_id and operating day combinations.
#'
#' @return A tibble with columns service_id and date.
prepare_calendar <- function(calendar_csv_filename,
                             calendar_dates_csv_filename) {
    calendar <- read_calendar(calendar_csv_filename)
    calendar_dates <- read_calendar_dates(calendar_dates_csv_filename)
    combine_calendar(calendar, calendar_dates)
}
