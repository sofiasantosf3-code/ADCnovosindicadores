#' @title Cotacoes da B3 via Yahoo Finance
#' @name cotacao_b3
#' @description Funcoes para buscar cotacoes, historico e dividendos de acoes
#'   listadas na B3 via Yahoo Finance (sem dependencias externas).
NULL

# --- helpers internos --------------------------------------------------------

yf_ticker <- function(ticker) {

  tk <- toupper(trimws(ticker))
  if (!grepl("\\.SA$", tk)) tk <- paste0(tk, ".SA")
  tk
}

yf_url <- function(ticker, range = "1d", interval = "1d") {
  sprintf(
    "https://query1.finance.yahoo.com/v8/finance/chart/%s?range=%s&interval=%s",
    yf_ticker(ticker), range, interval
  )
}

yf_get_json <- function(url) {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  resp <- tryCatch(
    utils::download.file(url, tmp, quiet = TRUE, method = "auto"),
    error = function(e) e
  )
  if (inherits(resp, "error") || resp != 0) {
    stop("Falha ao acessar Yahoo Finance. Verifique sua conexao e o ticker.",
         call. = FALSE)
  }
  linhas <- readLines(tmp, warn = FALSE, encoding = "UTF-8")
  json_text <- paste(linhas, collapse = "")
  yf_parse_json(json_text)
}

yf_parse_json <- function(txt) {
  txt <- trimws(txt)
  if (grepl('"error"\\s*:\\s*\\{\\s*"code"\\s*:', txt)) {
    msg <- sub('.*"description"\\s*:\\s*"([^"]*)".*', "\\1", txt)
    stop("Yahoo Finance: ", msg, call. = FALSE)
  }
  txt
}

yf_extract_number <- function(json, key) {
  pat <- sprintf('"%s"\\s*:\\s*([0-9eE.+-]+)', key)
  m <- regmatches(json, regexpr(pat, json))
  if (length(m) == 0 || !nzchar(m)) return(NA_real_)
  val <- sub(pat, "\\1", m)
  suppressWarnings(as.numeric(val))
}

yf_extract_array <- function(json, key) {
  pat <- paste0('"', key, '"\\s*:\\s*\\[([^\\]]*?)\\]')
  m <- regmatches(json, regexpr(pat, json, perl = TRUE))
  if (length(m) == 0 || !nzchar(m)) return(numeric(0))
  inner <- sub(pat, "\\1", m, perl = TRUE)
  inner <- gsub("null", "NA", inner)
  vals <- strsplit(inner, ",")[[1]]
  suppressWarnings(as.numeric(trimws(vals)))
}

yf_extract_string <- function(json, key) {
  pat <- sprintf('"%s"\\s*:\\s*"([^"]*)"', key)
  m <- regmatches(json, regexpr(pat, json))
  if (length(m) == 0 || !nzchar(m)) return(NA_character_)
  sub(pat, "\\1", m)
}

# --- funcoes publicas --------------------------------------------------------

#' Buscar cotacao atual de uma acao da B3
#'
#' Consulta o Yahoo Finance e retorna preco, variacao e volume.
#' O ticker pode ser informado com ou sem o sufixo .SA (ex: "PETR4" ou "PETR4.SA").
#'
#' @param ticker Codigo da acao na B3 (ex: "PETR4", "VALE3", "ITUB4").
#' @return Lista com ticker, nome, preco, abertura, maxima, minima,
#'   fechamento_anterior, variacao, variacao_pct, volume e moeda.
#' @export
cotacao_b3 <- function(ticker) {
  tk <- yf_ticker(ticker)
  json <- yf_get_json(yf_url(tk, "1d", "1d"))
  preco <- yf_extract_number(json, "regularMarketPrice")
  fech_ant <- yf_extract_number(json, "chartPreviousClose")
  if (is.na(fech_ant)) fech_ant <- yf_extract_number(json, "previousClose")
  variacao <- if (!is.na(preco) && !is.na(fech_ant)) preco - fech_ant else NA_real_
  variacao_pct <- safe_div(variacao, fech_ant) * 100
  list(
    ticker = tk,
    nome = yf_extract_string(json, "shortName"),
    preco = preco,
    abertura = yf_extract_number(json, "regularMarketOpen"),
    maxima = yf_extract_number(json, "regularMarketDayHigh"),
    minima = yf_extract_number(json, "regularMarketDayLow"),
    fechamento_anterior = fech_ant,
    variacao = variacao,
    variacao_pct = variacao_pct,
    volume = yf_extract_number(json, "regularMarketVolume"),
    moeda = yf_extract_string(json, "currency")
  )
}

#' Buscar historico de cotacoes de uma acao da B3
#'
#' @param ticker Codigo da acao na B3.
#' @param periodo Periodo do historico: "1mo", "3mo", "6mo", "1y", "2y", "5y", "max".
#' @param intervalo Intervalo: "1d" (diario), "1wk" (semanal), "1mo" (mensal).
#' @return data.frame com data, abertura, maxima, minima, fechamento, volume.
#' @export
historico_b3 <- function(ticker, periodo = "1y", intervalo = "1d") {
  tk <- yf_ticker(ticker)
  json <- yf_get_json(yf_url(tk, periodo, intervalo))
  timestamps <- yf_extract_array(json, "timestamp")
  if (length(timestamps) == 0) {
    stop("Nenhum dado historico retornado para ", tk, call. = FALSE)
  }
  datas <- as.Date(as.POSIXct(timestamps, origin = "1970-01-01", tz = "America/Sao_Paulo"))
  pad_vals <- function(key) {
    v <- yf_extract_array(json, key)
    if (length(v) == 0) rep(NA_real_, length(timestamps)) else v
  }
  data.frame(
    data = datas,
    abertura = pad_vals("open"),
    maxima = pad_vals("high"),
    minima = pad_vals("low"),
    fechamento = pad_vals("close"),
    volume = pad_vals("volume"),
    stringsAsFactors = FALSE
  )
}

#' Buscar cotacoes de multiplas acoes
#'
#' @param tickers Vetor de codigos (ex: c("PETR4", "VALE3", "ITUB4")).
#' @return data.frame com ticker, nome, preco, variacao_pct e volume.
#' @export
cotacoes_b3 <- function(tickers) {
  linhas <- lapply(tickers, function(tk) {
    cot <- tryCatch(cotacao_b3(tk), error = function(e) {
      list(ticker = yf_ticker(tk), nome = NA_character_, preco = NA_real_,
           variacao_pct = NA_real_, volume = NA_real_)
    })
    data.frame(
      ticker = cot$ticker, nome = cot$nome, preco = cot$preco,
      variacao_pct = cot$variacao_pct, volume = cot$volume,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, linhas)
}

#' Indicadores do investidor com cotacao automatica da B3
#'
#' Busca a cotacao atual via Yahoo Finance e calcula VPA, LPA, P/L e DPA.
#' Combina cotacao_b3() com indicadores_investidor().
#'
#' @param ag Lista agregada (balanco + DRE).
#' @param ticker Codigo da acao na B3.
#' @param n_acoes Numero de acoes emitidas.
#' @param dividendos Dividendos distribuidos no exercicio (opcional).
#' @return Lista com os indicadores do investidor mais dados da cotacao.
#' @export
indicadores_investidor_b3 <- function(ag, ticker, n_acoes, dividendos = NA_real_) {
  cot <- cotacao_b3(ticker)
  ind <- indicadores_investidor(ag, n_acoes, preco = cot$preco, dividendos = dividendos)
  c(ind, list(
    ticker = cot$ticker,
    nome_empresa = cot$nome,
    cotacao = cot$preco,
    variacao_dia = cot$variacao_pct,
    dividend_yield = safe_div(ind$DPA, cot$preco) * 100
  ))
}
