#' @title Funcoes Utilitarias do ADCnovosindicadores
#' @name utils
NULL

#' Normalizar texto para matching de contas contabeis
#'
#' Aplica lowercase e remocao de diacriticos via mapeamento de bytes UTF-8.
#' Funciona em qualquer locale (inclusive LC_CTYPE=C).
#'
#' @param x Vetor de caracteres.
#' @return Vetor normalizado.
#' @export
normalizar_texto <- function(x) {
  if (!is.character(x)) return(x)
  x <- gsub("\xc3[\x80-\x85\xa0-\xa5]", "a", x, useBytes = TRUE)
  x <- gsub("\xc3[\x87\xa7]", "c", x, useBytes = TRUE)
  x <- gsub("\xc3[\x88-\x8b\xa8-\xab]", "e", x, useBytes = TRUE)
  x <- gsub("\xc3[\x8c-\x8f\xac-\xaf]", "i", x, useBytes = TRUE)
  x <- gsub("\xc3[\x91\xb1]", "n", x, useBytes = TRUE)
  x <- gsub("\xc3[\x92-\x96\xb2-\xb6]", "o", x, useBytes = TRUE)
  x <- gsub("\xc3[\x99-\x9c\xb9-\xbc]", "u", x, useBytes = TRUE)
  x <- gsub("\xc3[\x9d\xbd\xbf]", "y", x, useBytes = TRUE)
  x <- tolower(x)
  x <- gsub("[^a-z0-9 _/]", "", x)
  trimws(x)
}

#' Converter numeros em formato brasileiro para numerico
#'
#' Trata separador de milhar (ponto) e decimal (virgula).
#' Aceita tambem formato internacional.
#'
#' @param x Vetor de caracteres ou numerico.
#' @return Vetor numerico.
#' @export
conv_br_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("[[:space:]]", "", x)

  eh_br <- grepl("\\.", x) & grepl(",", x)

  resultado <- rep(NA_real_, length(x))

  if (any(eh_br)) {
    br <- x[eh_br]
    br <- gsub("\\.", "", br)
    br <- gsub(",", ".", br)
    resultado[eh_br] <- suppressWarnings(as.numeric(br))
  }

  if (any(!eh_br)) {
    nbr <- x[!eh_br]
    nbr <- gsub(",", ".", nbr)
    resultado[!eh_br] <- suppressWarnings(as.numeric(nbr))
  }

  resultado
}

#' Divisao segura (previne divisao por zero)
#'
#' @param a Numerador.
#' @param b Denominador.
#' @param default Valor retornado quando b == 0. Padrao NA_real_.
#' @return Resultado da divisao ou default.
#' @export
safe_div <- function(a, b, default = NA_real_) {
  denom <- rep_len(b, length(a))
  bad <- denom == 0 | is.na(denom)
  out <- a / denom
  out[bad] <- default
  out
}

detectar_separador <- function(caminho) {
  linhas <- readLines(caminho, n = 5, warn = FALSE, encoding = "UTF-8")
  ponto_virgula <- sum(grepl(";", linhas))
  virgula <- sum(grepl(",", linhas))
  if (ponto_virgula > virgula) ";" else ","
}

detectar_encoding <- function(caminho) {
  bytes <- readBin(caminho, "raw", n = 3)
  if (length(bytes) >= 3 && bytes[1] == as.raw(0xEF) &&
      bytes[2] == as.raw(0xBB) && bytes[3] == as.raw(0xBF)) {
    return("UTF-8")
  }
  linhas <- tryCatch(
    readLines(caminho, n = 20, encoding = "UTF-8", warn = FALSE),
    error = function(e) NULL
  )
  if (!is.null(linhas) && all(validUTF8(linhas))) {
    return("UTF-8")
  }
  "latin1"
}

ler_csv_auto <- function(caminho, encoding = NULL) {
  if (!file.exists(caminho)) {
    stop("Arquivo nao encontrado: ", caminho, call. = FALSE)
  }
  if (is.null(encoding)) encoding <- detectar_encoding(caminho)
  sep <- detectar_separador(caminho)
  enc_arg <- if (encoding == "UTF-8" && identical(Sys.getlocale("LC_CTYPE"), "C")) "" else encoding
  if (sep == ";") {
    utils::read.csv2(caminho, stringsAsFactors = FALSE,
                     fileEncoding = enc_arg, check.names = FALSE)
  } else {
    utils::read.csv(caminho, stringsAsFactors = FALSE,
                    fileEncoding = enc_arg, check.names = FALSE)
  }
}

padronizar_nomes_com_mapa <- function(df, mapa) {
  nomes <- normalizar_texto(names(df))
  nomes <- gsub("[^a-z0-9_]", "_", nomes)
  nomes <- gsub("_+", "_", nomes)
  nomes <- gsub("^_|_$", "", nomes)
  for (i in seq_along(nomes)) {
    if (nomes[i] %in% names(mapa)) {
      nomes[i] <- mapa[nomes[i]]
    }
  }
  names(df) <- nomes
  df
}

validar_colunas_obrigatorias <- function(df, colunas, contexto = "") {
  presentes <- tolower(names(df))
  faltantes <- colunas[!tolower(colunas) %in% presentes]
  if (length(faltantes) > 0) {
    stop(
      sprintf(
        "Colunas obrigatorias ausentes%s: %s\nColunas encontradas: %s",
        if (nzchar(contexto)) paste0(" (", contexto, ")") else "",
        paste(faltantes, collapse = ", "),
        paste(names(df), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
