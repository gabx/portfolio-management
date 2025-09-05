#' S�quence de dates tous les 19 jours (inclusif)
#' @param first_day Date de d�part ("YYYY-MM-DD" ou Date)
#' @param last_day  Date de fin ("YYYY-MM-DD" ou Date)
#' @param output    "Date" (par d�faut), "character" (YYYY-MM-DD), ou "string" (une ligne)
#' @return Un vecteur de Date / de caract�res / ou une seule cha�ne
dates_every_19_days <- function(first_day, last_day, output = c("Date","character","string")) {
  output <- match.arg(output)
  fd <- as.Date(first_day)
  ld <- as.Date(last_day)
  if (is.na(fd) || is.na(ld)) stop("first_day/last_day doivent �tre parsables en Date (YYYY-MM-DD).", call. = FALSE)
  if (ld < fd) stop("last_day doit �tre >= first_day.", call. = FALSE)
  
  ds <- seq.Date(from = fd, to = ld, by = "19 days")  # s'arr�te avant de d�passer last_day
  
  if (output == "Date")       return(ds)
  if (output == "character")  return(format(ds, "%Y-%m-%d"))
  paste(format(ds, "%Y-%m-%d"), collapse = ", ")
}

############ EXEMPLES ############################
# Vecteur de Date
# dates_every_19_days ("2024-12-16", Sys.Date())

# Vecteur de cha�nes "YYYY-MM-DD"
# dates_every_19_days ("2024-12-16", Sys.Date(), output = "character")

# Cha�ne unique format�e comme ton exemple
# dates_every_19_days ("2024-12-16", Sys.Date(), output = "string")
# "2024-12-16, 2025-01-04, 2025-01-23, 2025-02-11, ..."