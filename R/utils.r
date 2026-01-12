
toNum <- function(x) {
    suppressWarnings(as.numeric(x))
}



as_date_value <- function(value, ctype) {
    parsed <- suppressWarnings(parse_date_time(
        value,
        orders = c(
            # Date with time
            "Ymd HMS", "Ymd HM", "Ymd H",
            "mdy HMS", "mdy HM", "mdy H",
            "dmy HMS", "dmy HM", "dmy H",
            "ymd HMS", "ymd HM", "ymd H",
            # Date only
            "Ymd", "mdy", "dmy", "ymd",
            # Year-month
            "Ym", "my",
            # Text month names
            "dby", "dbY", "bdy", "bdY",
            # Month-day (assumes current year)
            "md", "dm", "bd", "db",
            "Y" # Year only
        )
    ))
    if (is.na(parsed)) {
        return(NULL)
    }
    if (ctype == "Date") {
        parsed <- as.Date(parsed)
    }
    return(parsed)
}


# parse_partial_date("2021")

# library(lubridate)
# library(parsedate)

parse_partial_date <- function(value, ctype = "Date") {
    if (is.null(value)) return(NULL)
    trimmed <- trimws(value)
    parsed_date <- maximum_date_precision(trimmed)
    if (is.null(parsed_date)) return(NULL)

    start <- as_date_value(parsed_date$start, ctype)
    end <- as_date_value(parsed_date$end, ctype)

    if (identical(start, end)) {
        return(
            list(type = "single", value = as.character(start))
        )
    } else {
        return(list(type="range", value = paste0(start, "~", end)))
    }
}

# turn everything into a range with precision up to second
maximum_date_precision <- function(date_str) {
    # Try various formats and detect precision
    
    # Year only: "2021"
    if (grepl("^\\d{4}$", date_str)) {
        year <- as.integer(date_str)
        return(list(
            start = ymd_hms(paste0(year, "-01-01 00:00:00")),
            end = ymd_hms(paste0(year, "-12-31 23:59:59"))
        ))
    }
    
    # Year-Month: "2021-01" or "2021-1"
    if (grepl("^\\d{4}[-/]\\d{1,2}$", date_str)) {
        parts <- strsplit(date_str, "[-/]")[[1]]
        year <- as.integer(parts[1])
        month <- as.integer(parts[2])
        start <- ymd_hms(sprintf("%04d-%02d-01 00:00:00", year, month))
        end <- ceiling_date(start, "month", change_on_boundary = TRUE) - days(1)
        end <- end + hours(23) + minutes(59) + seconds(59)
        return(list(start = start, end = end))
    }
    
    # Month/Year: "08/2021" or "8/21"
    if (grepl("^\\d{1,2}/\\d{2,4}$", date_str)) {
        parts <- strsplit(date_str, "/")[[1]]
        month <- as.integer(parts[1])
        year <- as.integer(parts[2])
        
        # Handle 2-digit year
        if (year < 100) {
            year <- ifelse(year > 50, 1900 + year, 2000 + year)
        }
        
        start <- ymd_hms(sprintf("%04d-%02d-01 00:00:00", year, month))
        end <- ceiling_date(start, "month", change_on_boundary = TRUE) - days(1)
        end <- end + hours(23) + minutes(59) + seconds(59)
        return(list(start = start, end = end))
    }
    
    # Full date: various formats
    parsed <- suppressWarnings(parse_date_time(
        date_str,
        orders = c("Ymd", "Ymd HMS", "mdy", "dmy", "Ym", "my")
    ))
    
    if (!is.na(parsed)) {
        return(list(
            start = parsed,
            end = parsed
        ))
    }
    
    NULL
}
