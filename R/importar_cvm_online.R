#' @title Importacao Online de Dados CVM
#' @name importar_cvm_online
#' @description Baixa dados contabeis diretamente do portal de dados abertos
#'   da CVM (dados.cvm.gov.br) e retorna lista agregada pronta para calculo
#'   de indicadores financeiros.
NULL

# --- helpers internos -------------------------------------------------------

.cvm_dt_refer <- function(ano, trimestre) {
  mes <- c("03", "06", "09", "12")[trimestre]
  dia <- c("31", "30", "30", "31")[trimestre]
  sprintf("%d-%s-%s", ano, mes, dia)
}

.cvm_parsear_periodo <- function(periodo) {
  periodo <- toupper(trimws(periodo))

  m <- regmatches(periodo, regexec("^([1-4])T(\\d{2,4})$", periodo))[[1]]
  if (length(m) == 3) {
    tri <- as.integer(m[2])
    ano <- as.integer(m[3])
    if (ano < 100) ano <- ano + 2000
    tipo <- if (tri == 4) "dfp" else "itr"
    return(list(ano = ano, trimestre = tri, tipo = tipo,
                dt_refer = .cvm_dt_refer(ano, tri)))
  }

  if (grepl("^\\d{4}$", periodo)) {
    ano <- as.integer(periodo)
    return(list(ano = ano, trimestre = 4, tipo = "dfp",
                dt_refer = sprintf("%d-12-31", ano)))
  }

  stop("Periodo invalido: '", periodo,
       "'. Use formato '1T25', '3T2024' ou '2024'.", call. = FALSE)
}

.cvm_baixar_zip <- function(ano, tipo) {
  if (tipo == "itr") {
    url <- sprintf(
      "https://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/ITR/DADOS/itr_cia_aberta_%d.zip", ano)
    prefixo <- "itr_cia_aberta"
  } else {
    url <- sprintf(
      "https://dados.cvm.gov.br/dados/CIA_ABERTA/DOC/DFP/DADOS/dfp_cia_aberta_%d.zip", ano)
    prefixo <- "dfp_cia_aberta"
  }

  cache_dir <- file.path(tempdir(), "adcnovi_cvm")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  zip_file <- file.path(cache_dir, basename(url))
  extract_dir <- file.path(cache_dir, sub("\\.zip$", "", basename(url)))

  if (!file.exists(zip_file)) {
    message("Baixando dados CVM (", toupper(tipo), " ", ano, ")... pode levar alguns segundos.")
    resp <- tryCatch(
      utils::download.file(url, zip_file, quiet = FALSE, mode = "wb"),
      error = function(e) e
    )
    if (inherits(resp, "error") || resp != 0) {
      unlink(zip_file)
      stop("Falha ao baixar dados da CVM. Verifique sua conexao.\nURL: ",
           url, call. = FALSE)
    }
  }

  if (!dir.exists(extract_dir)) {
    message("Extraindo arquivos...")
    dir.create(extract_dir, showWarnings = FALSE)
    utils::unzip(zip_file, exdir = extract_dir)
  }

  list(dir = extract_dir, prefixo = prefixo, ano = ano)
}

.cvm_ler_csvs <- function(info_zip, consolidado) {
  sufixo <- if (consolidado) "con" else "ind"
  tipos_arq <- c("BPA", "BPP", "DRE", "DFC_MI", "DVA")

  colunas_base <- c("CNPJ_CIA", "DENOM_CIA", "DT_REFER", "CD_CONTA",
                     "DS_CONTA", "VL_CONTA", "ORDEM_EXERC", "ESCALA_MOEDA")

  partes <- list()
  for (tp in tipos_arq) {
    arq <- sprintf("%s_%s_%s_%d.csv", info_zip$prefixo, tp, sufixo, info_zip$ano)
    caminho <- file.path(info_zip$dir, arq)
    if (file.exists(caminho)) {
      df <- utils::read.csv2(caminho, stringsAsFactors = FALSE,
                             fileEncoding = "latin1", check.names = FALSE)
      cols <- intersect(colunas_base, names(df))
      df <- df[, cols, drop = FALSE]
      df$.fonte <- tp
      partes[[tp]] <- df
    }
  }

  if (length(partes) == 0) {
    tipo_nome <- if (consolidado) "consolidadas" else "individuais"
    stop("Nenhum arquivo de demonstracoes ", tipo_nome, " encontrado.",
         call. = FALSE)
  }

  do.call(rbind, partes)
}

.cvm_buscar_empresa <- function(dados, empresa) {
  busca <- toupper(trimws(empresa))
  busca_limpa <- sub("\\d+$", "", busca)

  empresas <- unique(dados$DENOM_CIA)

  achados <- grep(busca, empresas, ignore.case = TRUE, value = TRUE)
  if (length(achados) == 0 && busca_limpa != busca) {
    achados <- grep(busca_limpa, empresas, ignore.case = TRUE, value = TRUE)
  }

  if (length(achados) == 0) {
    stop("Empresa nao encontrada para '", empresa,
         "'.\nUse listar_empresas_cvm() para ver empresas disponiveis.",
         call. = FALSE)
  }

  if (length(achados) > 1) {
    message("Multiplas empresas encontradas:")
    for (i in seq_along(achados)) message("  ", i, ". ", achados[i])
    message("Usando: ", achados[1])
  }

  achados[1]
}

.cvm_agregar <- function(df) {
  todas_cats <- c(categorias_bp, categorias_dre, categorias_dfc, categorias_dva)

  desc_norm <- normalizar_texto(df$DS_CONTA)
  cat_vals <- vapply(todas_cats, normalizar_texto, character(1))

  ordem <- order(nchar(cat_vals), decreasing = TRUE)

  classificacao <- rep(NA_character_, nrow(df))
  for (idx in ordem) {
    cat_nome <- names(cat_vals)[idx]
    matches <- grepl(cat_vals[idx], desc_norm, fixed = TRUE)
    classificacao[matches & is.na(classificacao)] <- cat_nome
  }

  resultado <- list()
  for (cat_nome in names(cat_vals)) {
    linhas <- which(classificacao == cat_nome)
    if (length(linhas) > 0) {
      cd_contas <- df$CD_CONTA[linhas]
      melhor <- linhas[which.min(nchar(cd_contas))]
      resultado[[cat_nome]] <- as.numeric(df$VL_CONTA[melhor])
    }
  }

  aliases <- c(
    CONTAS_RECEBER    = "CONTAS_RECEBER_CP",
    REALIZAVEL_LONGO_PRAZO = "REALIZAVEL_LP",
    CUSTO             = "CUSTO_SERVICOS"
  )
  for (novo in names(aliases)) {
    orig <- aliases[novo]
    if (!is.null(resultado[[orig]]) && is.null(resultado[[novo]])) {
      resultado[[novo]] <- resultado[[orig]]
    }
  }

  resultado
}

# --- funcoes publicas -------------------------------------------------------

#' Importar dados contabeis online da CVM
#'
#' Baixa os arquivos ITR (trimestral) ou DFP (anual) do portal de dados
#' abertos da CVM e retorna lista agregada pronta para todas as funcoes
#' de indicadores do pacote.
#'
#' @param empresa Nome da empresa ou ticker da B3 (ex: "VIVARA", "VIVA3",
#'   "PETROBRAS", "PETR4"). Busca parcial.
#' @param periodo Periodo no formato "1T25", "3T2024" (trimestral) ou
#'   "2024" (anual/DFP). Default: "1T25".
#' @param consolidado Logico. TRUE para demonstracoes consolidadas (padrao),
#'   FALSE para individuais.
#' @return Lista com:
#'   \item{empresa}{Nome completo da empresa na CVM.}
#'   \item{periodo}{Periodo solicitado.}
#'   \item{dt_referencia}{Data de referencia das demonstracoes.}
#'   \item{escala}{Escala monetaria (ex: "MIL" = R$ mil).}
#'   \item{agregado}{Lista nomeada pronta para indicadores_liquidez(), dupont(), etc.}
#'   \item{original}{data.frame com todos os dados brutos filtrados.}
#' @examples
#' \dontrun{
#' # Vivara 1o trimestre de 2025
#' viva <- importar_cvm_online("VIVA3", "1T25")
#' indicadores_liquidez(viva$agregado)
#' dupont(viva$agregado)
#' kanitz(viva$agregado)
#'
#' # Petrobras anual 2024
#' petro <- importar_cvm_online("PETROBRAS", "2024")
#' indicadores_rentabilidade(petro$agregado)
#' fleuriet(petro$agregado)
#' }
#' @export
importar_cvm_online <- function(empresa, periodo = "1T25", consolidado = TRUE) {
  info <- .cvm_parsear_periodo(periodo)

  info_zip <- .cvm_baixar_zip(info$ano, info$tipo)
  dados <- .cvm_ler_csvs(info_zip, consolidado)

  nome_empresa <- .cvm_buscar_empresa(dados, empresa)
  message("Empresa: ", nome_empresa)

  filtro <- dados$DENOM_CIA == nome_empresa &
    dados$DT_REFER == info$dt_refer &
    grepl("LTIMO", dados$ORDEM_EXERC, ignore.case = TRUE)

  df <- dados[filtro, ]

  if (nrow(df) == 0) {
    datas_disp <- unique(dados$DT_REFER[dados$DENOM_CIA == nome_empresa])
    datas_disp <- sort(datas_disp, decreasing = TRUE)
    stop("Nenhum dado encontrado para ", nome_empresa, " em ", info$dt_refer,
         ".\nDatas disponiveis: ", paste(head(datas_disp, 8), collapse = ", "),
         call. = FALSE)
  }

  escala <- unique(df$ESCALA_MOEDA)
  escala <- escala[!is.na(escala)][1]

  ag <- .cvm_agregar(df)

  tipo_demo <- if (consolidado) "Consolidado" else "Individual"
  message(tipo_demo, " | ", info$dt_refer, " | Escala: ", escala,
          " | Contas: ", length(ag[!is.na(ag)]))

  list(
    empresa       = nome_empresa,
    periodo       = periodo,
    dt_referencia = info$dt_refer,
    escala        = escala,
    agregado      = ag,
    original      = df
  )
}

#' Listar empresas disponiveis nos dados CVM
#'
#' Baixa os dados CVM para o periodo e retorna nomes das empresas.
#' Util para descobrir o nome correto antes de usar importar_cvm_online().
#'
#' @param periodo Periodo (ex: "1T25", "2024").
#' @param busca Texto para filtrar (opcional). Ex: "petro", "banco".
#' @param consolidado Logico. TRUE para consolidadas, FALSE para individuais.
#' @return Vetor de nomes de empresas.
#' @examples
#' \dontrun{
#' listar_empresas_cvm("1T25", "vivara")
#' listar_empresas_cvm("2024", "banco")
#' }
#' @export
listar_empresas_cvm <- function(periodo = "1T25", busca = NULL,
                                consolidado = TRUE) {
  info <- .cvm_parsear_periodo(periodo)
  info_zip <- .cvm_baixar_zip(info$ano, info$tipo)
  dados <- .cvm_ler_csvs(info_zip, consolidado)

  empresas <- sort(unique(dados$DENOM_CIA))

  if (!is.null(busca) && nzchar(busca)) {
    empresas <- grep(busca, empresas, ignore.case = TRUE, value = TRUE)
  }

  empresas
}

#' Analise completa de uma empresa da B3
#'
#' Baixa dados da CVM, calcula todos os indicadores e opcionalmente
#' busca a cotacao atual via Yahoo Finance.
#'
#' @param empresa Nome ou ticker da empresa (ex: "VIVA3", "PETROBRAS").
#' @param periodo Periodo (ex: "1T25").
#' @param n_acoes Numero de acoes emitidas (em milhares se escala = MIL).
#' @param cotacao Logico. TRUE para buscar cotacao via Yahoo Finance.
#' @param consolidado Logico. TRUE para consolidadas.
#' @return Lista com todos os indicadores calculados.
#' @examples
#' \dontrun{
#' resultado <- analise_completa("VIVA3", "1T25", n_acoes = 237619)
#' print(resultado)
#' }
#' @export
analise_completa <- function(empresa, periodo = "1T25", n_acoes = NA,
                             cotacao = TRUE, consolidado = TRUE) {
  dados <- importar_cvm_online(empresa, periodo, consolidado)
  ag <- dados$agregado

  resultado <- list(
    empresa   = dados$empresa,
    periodo   = dados$periodo,
    escala    = dados$escala,
    liquidez       = indicadores_liquidez(ag),
    endividamento  = indicadores_endividamento(ag),
    rentabilidade  = indicadores_rentabilidade(ag),
    dupont         = dupont(ag),
    cobertura_juros = cobertura_juros(ag),
    fleuriet       = fleuriet(ag),
    kanitz         = kanitz(ag),
    altman         = altman(ag),
    analise_vertical = analise_vertical(ag)
  )

  if (!is.na(n_acoes)) {
    ticker <- sub("\\d*$", "", toupper(trimws(empresa)))
    preco <- NA_real_
    if (cotacao) {
      preco <- tryCatch({
        cot <- cotacao_b3(empresa)
        message("Cotacao ", cot$ticker, ": R$ ", formatC(cot$preco, format = "f", digits = 2))
        cot$preco
      }, error = function(e) {
        message("Cotacao indisponivel: ", e$message)
        NA_real_
      })
    }
    resultado$investidor <- indicadores_investidor(ag, n_acoes = n_acoes, preco = preco)
  }

  class(resultado) <- "analise_completa"
  resultado
}

#' @export
print.analise_completa <- function(x, ...) {
  cat(sprintf("\n=== %s — %s (Escala: %s) ===\n", x$empresa, x$periodo, x$escala))

  .print_sec <- function(titulo, lst) {
    cat(sprintf("\n--- %s ---\n", titulo))
    for (nm in names(lst)) {
      val <- lst[[nm]]
      if (is.numeric(val) && length(val) == 1) {
        if (is.na(val)) {
          cat(sprintf("  %-25s     NA\n", nm))
        } else if (abs(val) >= 1000) {
          cat(sprintf("  %-25s %12.0f\n", nm, val))
        } else {
          cat(sprintf("  %-25s %8.4f\n", nm, val))
        }
      } else if (is.character(val)) {
        cat(sprintf("  %-25s %s\n", nm, val))
      }
    }
  }

  .print_sec("Liquidez", x$liquidez)
  .print_sec("Endividamento", x$endividamento)
  .print_sec("Rentabilidade", x$rentabilidade)
  .print_sec("DuPont", x$dupont)
  cat(sprintf("\n--- Cobertura de Juros ---\n  ICJ                       %8.4f\n",
              x$cobertura_juros))
  .print_sec("Fleuriet", x$fleuriet)
  .print_sec("Kanitz", x$kanitz)
  .print_sec("Altman", x$altman)
  if (!is.null(x$investidor)) .print_sec("Indicadores do Investidor", x$investidor)

  cat("\n")
  invisible(x)
}
