# R/tokens.R
# Dépendances : readr, dplyr, tibble, stringr (optionnel)
# install.packages(c("readr","dplyr","tibble"))

# read .csv file with our token and return a list or tibble

load_assets<- function(
    file = "assets.csv",
    output = c("list", "tibble"),
    expected_dir = "/development/language/r/portfolio-management",
    drop_tokens  = c("FTMUSDC", "USDCUSDC"),   # blacklist initiale (ajuste si besoin)
    ensure_tokens = c("BTCUSDC")               # à forcer en sortie
) {
  output <- match.arg(output)
  
  # 1) Vérif du répertoire (strict)
  wd  <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  exp <- normalizePath(expected_dir, winslash = "/", mustWork = TRUE)
  if (!identical(wd, exp)) {
    stop(sprintf("Répertoire courant incorrect.\n- Actuel : %s\n- Attendu: %s\n", wd, exp), call. = FALSE)
  }
  
  # 2) Vérif fichier
  if (!file.exists(file)) {
    stop(sprintf("Fichier introuvable: %s (dans %s)", file, wd), call. = FALSE)
  }
  
  # 3) Lecture : 1 colonne sans en-tête
  df <- readr::read_csv(file, col_names = FALSE, show_col_types = FALSE, progress = FALSE)
  if (ncol(df) != 1L) stop(sprintf("Le CSV doit avoir exactement 1 colonne (trouvé: %d).", ncol(df)), call. = FALSE)
  
  # 4) Vectorisation + nettoyage
  tokens <- df[[1]]
  tokens <- tokens[!is.na(tokens)]
  tokens <- trimws(as.character(tokens))
  tokens <- tokens[tokens != ""]
  
  # 5) Normalisation suffixe → USDC (ex: BTC -> USDC, USDT -> USDC, conservant USDC tel quel)
  #   (on applique seulement si le suffixe est en fin de chaîne)
  tokens_usdc <- sub("(BTC|USDT)$", "USDC", tokens, perl = TRUE)
  
  # 6) Déduplication (préserve l'ordre d'apparition)
  tokens_usdc <- tokens_usdc[!duplicated(tokens_usdc)]
  
  # 7) Blacklist (FTMUSDC, USDCUSDC, etc.)
  if (length(drop_tokens)) {
    tokens_usdc <- tokens_usdc[!tokens_usdc %in% drop_tokens]
  }
  
  # 8) Ajouts forcés (ex. BTCUSDC toujours présent)
  for (tk in ensure_tokens) {
    if (!(tk %in% tokens_usdc)) tokens_usdc <- c(tokens_usdc, tk)
  }
  
  # 9) Format de sortie
  if (output == "tibble") {
    return(tibble::tibble(symbol = tokens_usdc))
  } else {
    return(as.list(tokens_usdc))
  }
}
