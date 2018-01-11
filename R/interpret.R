#' Get all valid service_ids for a given operating day.
#'
#' @import dplyr
get_service_ids <- function(calendar, calendar_dates = NULL, date) {
    service_ids <- calendar %>%
        filter(
            start_date <= date,
            end_date >= date
        ) %>%
        filter_(tolower(weekdays(date))) %>%
        select(service_id)
    if (!is.null(calendar_dates)) {
        exceptions <- calendar_dates %>%
            filter(date == date)
        excluded <- exceptions %>%
            filter(exception_type == 2L) %>%
            select(service_id)
        included <- exceptions %>%
            filter(exception_type == 1L) %>%
            select(service_id)
        service_ids <- service_ids %>%
            setdiff(excluded) %>%
            union(included)
    }
    service_ids
}

#' Get all valid trip_id and operating day combinations.
#'
#' @return A tibble with columns trip_id and date.
#' @import dplyr
#' @import purrr
#' @import tidyr
get_trip_calendar <- function(trips, calendar, calendar_dates = NULL) {
    trimmed_trips <- trips %>%
        distinct(trip_id, service_id)
    min_date <- min(calendar[["start_date"]])
    max_date <- max(calendar[["end_date"]])
    if (!is.null(calendar_dates)) {
        min_date <- min(min(calendar_dates[["date"]]), min_date)
        max_date <- max(max(calendar_dates[["date"]]), max_date)
    }
    dates <- seq(min_date, max_date, by = "1 day")
    map_dfr(dates, function(date) {
            tibble(
                service_id = get_service_ids(calendar, calendar_dates, date),
                date = date
            )
        }) %>%
        unnest() %>%
        inner_join(trimmed_trips, by = "service_id") %>%
        distinct(trip_id, date) %>%
        arrange(trip_id, date)
}
