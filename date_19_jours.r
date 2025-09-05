
#' Séquence de dates tous les 19 jours
#' @param start Date de départ (ex. "2024-12-16" ou objet Date)
#' @param end   Date de fin (par défaut: aujourd'hui en Europe/Paris)
#' @param at_close TRUE pour renvoyer des POSIXct à 19:59:59 Europe/Paris
#' @param tz    Fuseau horaire pour at_close (par défaut "Europe/Paris")
dates_every_19_days <- function(start = "2024-12-16",
                                end   = NULL,
                                at_close = FALSE,
                                tz = "Europe/Paris") {
  # calcule "aujourd'hui" dans le fuseau demandé
  if (is.null(end)) {
    end <- as.Date(lubridate::with_tz(Sys.time(), tz))
  }
  # normalisation des entrées
  start <- as.Date(start)
  end   <- as.Date(end)
  if (end < start) stop("La date de fin est antérieure à la date de début.")
  
  # séquence tous les 19 jours (inclus le start; s'arrête au plus tard à end)
  seq_dates <- seq.Date(from = start, to = end, by = "19 days")
  
  if (!at_close) return(seq_dates)
  
  # retourne des POSIXct à 19:59:59 dans le TZ demandé
  as.POSIXct(paste(seq_dates, "19:59:59"), tz = tz)
}

################## EXAMPLES ############################
# 1) Dates tous les 19 jours depuis 2024-12-16 jusqu'� aujourd'hui
# dates_every_19_days("2024-12-16")

# 2) M�me chose mais � l'heure de cl�ture 19:59:59 Europe/Paris
# dates_every_19_days("2024-12-16", at_close = TRUE)

# 3) Fen�tre explicite (par ex. jusqu'au 2025-06-01)
# dates_every_19_days("2024-12-16", end = "2025-06-01")
