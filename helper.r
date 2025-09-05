


# get_price(token, start_date) est déjà défini chez toi:
# get_price <- function(my_token, start_date) {
#   binance_klines(my_token, interval = '1h', start_time = start_date)
# }

.to_utc_time <- function(x) {
  if (inherits(x, "POSIXt")) return(lubridate::with_tz(x, "UTC"))
  if (is.numeric(x)) {
    # Heuristique: ms si > 1e12, sinon secondes
    unit_ms <- any(x > 1e12, na.rm = TRUE)
    return(lubridate::as_datetime(if (unit_ms) x/1000 else x, tz = "UTC"))
  }
  if (is.character(x)) {
    # tente ISO, sinon entier (ms/sec)
    ts <- suppressWarnings(lubridate::ymd_hms(x, tz = "UTC"))
    if (!is.na(ts)) return(ts)
    nx <- suppressWarnings(as.numeric(x))
    if (!is.na(nx)) return(.to_utc_time(nx))
  }
  stop("Format de close_time non support�.")
}

# close_time peut être en ms, en secondes, POSIXct ou string ISO.
.to_utc_time <- function(x) {
  if (inherits(x, "POSIXt")) return(lubridate::with_tz(x, "UTC"))
  if (is.numeric(x)) {
    ms <- any(x > 1e12, na.rm = TRUE)
    return(lubridate::as_datetime(if (ms) x/1000 else x, tz = "UTC"))
  }
  if (is.character(x)) {
    ts <- suppressWarnings(lubridate::ymd_hms(x, tz = "UTC"))
    if (!is.na(ts)) return(ts)
    nx <- suppressWarnings(as.numeric(x))
    if (!is.na(nx)) return(.to_utc_time(nx))
  }
  stop("Format de close_time non supporté.")
}

# Normalise tokens: accepte list OU tibble (colonne 'symbol') OU vecteur
.norm_tokens <- function(tokens) {
  if (is.list(tokens) && !inherits(tokens, "data.frame")) tokens <- unlist(tokens, use.names = FALSE)
  if (inherits(tokens, "data.frame")) {
    tokens <- if ("symbol" %in% names(tokens)) tokens$symbol else tokens[[1]]
  }
  tokens <- as.character(tokens)
  tokens <- tokens[!is.na(tokens) & nzchar(tokens)]
  unique(tokens)
}

# Construit les instants de clôture quotidiens (19:59:59 Europe/Paris par défaut)
.build_daily_closes <- function(start_date, end_date = NULL,
                                close_hms = "19:59:59",
                                tz_local  = "Europe/Paris") {
  if (is.null(end_date)) end_date <- as.Date(with_tz(Sys.time(), tz_local))
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  if (end_date < start_date) stop("Fenêtre de dates invalide.")
  
  # Chaque jour -> POSIXct local à close_hms, puis converti en UTC
  days <- seq.Date(start_date, end_date, by = "1 day")
  tibble(day = days) %>%
    mutate(
      close_local = as.POSIXct(paste(day, close_hms), tz = tz_local),
      close_utc   = with_tz(close_local, "UTC")
    ) %>%
    select(close_time = close_utc)            # on travaille en UTC côté données de marché
}
