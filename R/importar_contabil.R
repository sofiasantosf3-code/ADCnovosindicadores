#' @title Importacao de Dados Contabeis (CVM/DFP/ITR)
#' @name importar_contabil
NULL

#' Tabelas de classificacao de contas (padrao CVM)
#' @export
categorias_bp <- list(
  ATIVO_TOTAL              = "ativo total",
  ATIVO_CIRCULANTE         = "ativo circulante",
  CAIXA_EQUIVALENTES       = "caixa e equivalente",
  APLICACOES_FINANCEIRAS   = "aplicacoes financeiras",
  CONTAS_RECEBER_CP        = "contas a receber",
  ESTOQUES                 = "estoques",
  ATIVO_NAO_CIRCULANTE     = "ativo nao circulante",
  REALIZAVEL_LP            = "realizavel a longo prazo",
  INVESTIMENTOS            = "investimentos",
  IMOBILIZADO              = "imobilizado",
  INTANGIVEL               = "intangivel",
  PASSIVO_CIRCULANTE       = "passivo circulante",
  EMPRESTIMOS_CP           = "emprestimos de curto prazo",
  FINANCIAMENTOS_CP        = "financiamentos de curto prazo",
  DEBENTURES_CP            = "debentures de curto prazo",
  FORNECEDORES             = "fornecedores",
  PASSIVO_NAO_CIRCULANTE   = "passivo nao circulante",
  EMPRESTIMOS_LP           = "emprestimos de longo prazo",
  FINANCIAMENTOS_LP        = "financiamentos de longo prazo",
  DEBENTURES_LP            = "debentures de longo prazo",
  PATRIMONIO_LIQUIDO       = "patrimonio liquido",
  CAPITAL_SOCIAL           = "capital social realizado",
  RESERVAS_LUCROS          = "reservas de lucros"
)

#' @rdname importar_contabil
#' @export
categorias_dre <- list(
  RECEITA_TOTAL            = "receita total",
  RECEITA_LIQUIDA          = "receita de venda de bens e/ou servicos",
  CUSTO_TOTAL              = "custo total",
  CUSTO_SERVICOS           = "custo dos bens e/ou servicos vendidos",
  LUCRO_BRUTO              = "lucro bruto",
  LUCRO_OPERACIONAL        = "lucro operacional",
  RESULTADO_BRUTO          = "resultado bruto",
  DESPESAS_OPERACIONAIS    = "despesas/receitas operacionais",
  DESPESAS_VENDAS          = "despesas com vendas",
  DESPESAS_ADMINISTRATIVAS = "despesas gerais e administrativas",
  RESULTADO_FINANCEIRO     = "resultado financeiro",
  RECEITAS_FINANCEIRAS     = "receitas financeiras",
  DESPESAS_FINANCEIRAS     = "despesas financeiras",
  RESULTADO_ANTES_IR       = "resultado antes do resultado financeiro e dos tributos",
  LUCRO_LIQUIDO            = "lucro ou prejuizo"
)

#' @rdname importar_contabil
#' @export
categorias_dfc <- list(
  FCO                      = "fluxo de caixa operacional",
  FCI                      = "fluxo de caixa de investimento",
  FCF                      = "fluxo de caixa de financiamento"
)

#' @rdname importar_contabil
#' @export
categorias_dva <- list(
  VA_DISTRIBUIR            = "valor adicionado a distribuir",
  PESSOAL                  = "pessoal",
  IMPOSTOS_TAXAS           = "impostos e taxas",
  REMUN_TERCEIROS          = "remuneracao de capital de terceiros",
  REMUN_PROPRIO            = "remuneracao de capital proprio"
)

#' Importar dados contabeis do formato DFP (Demonstracoes Financeiras Padronizadas)
#'
#' Le um arquivo CSV no formato de dados CVM/DFP e classifica as contas
#' de acordo com as categorias padrao do balanco patrimonial e DRE.
#'
#' @param caminho Caminho para o arquivo CSV.
#' @param tipo "bp" para Balanco Patrimonial, "dre" para DRE, "auto" para deteccao.
#' @param encoding Encoding do arquivo. NULL para deteccao automatica.
#' @return Lista com $original (dados brutos classificados) e $agregado (contas agregadas por categoria).
#' @export
importar_contabil_dfp <- function(caminho, tipo = "auto", encoding = NULL) {
  df <- ler_csv_auto(caminho, encoding)
  df <- padronizar_nomes_contabil(df)
  validar_colunas_obrigatorias(df, "descricao", "importar_contabil_dfp")

  if (tipo == "auto") {
    tipo <- detectar_tipo_contabil(df)
  }

  todas_cats <- c(categorias_bp, categorias_dre, categorias_dfc, categorias_dva)
  if (tipo == "bp") {
    categorias <- categorias_bp
  } else if (tipo == "dre") {
    categorias <- categorias_dre
  } else {
    categorias <- todas_cats
  }

  df$categoria <- classificar_conta(df$descricao, todas_cats)

  colunas_valor <- grep("^valor|^saldo|^\\d{4}", names(df), value = TRUE)
  for (col in colunas_valor) {
    if (!is.numeric(df[[col]])) {
      df[[col]] <- conv_br_numeric(df[[col]])
    }
  }

  agregado <- agregar_categorias(df, todas_cats, colunas_valor)

  list(original = df, agregado = agregado, tipo = tipo)
}

#' Importar dados contabeis trimestrais (ITR)
#'
#' Alias de importar_contabil_dfp. O formato ITR e identico ao DFP.
#'
#' @inheritParams importar_contabil_dfp
#' @return Lista com $original e $agregado.
#' @export
importar_contabil_itr <- importar_contabil_dfp

#' Validar dados contabeis importados
#'
#' @param dados Lista retornada por importar_contabil_dfp ou importar_contabil_itr.
#' @return Lista com $valido, $erros, $avisos.
#' @export
validar_dados_contabeis <- function(dados) {
  erros <- character(0)
  avisos <- character(0)

  if (!is.list(dados) || !all(c("original", "agregado") %in% names(dados))) {
    erros <- c(erros, "Formato invalido: esperado lista com $original e $agregado")
    return(list(valido = FALSE, erros = erros, avisos = avisos))
  }

  df <- dados$original
  if (nrow(df) == 0) {
    erros <- c(erros, "Nenhum registro encontrado")
  }

  n_class <- sum(!is.na(df$categoria))
  pct_class <- safe_div(n_class, nrow(df), 0)
  if (pct_class < 0.5) {
    avisos <- c(avisos, sprintf(
      "Apenas %.0f%% das contas foram classificadas. Verificar formato do arquivo.",
      pct_class * 100
    ))
  }

  ag <- dados$agregado
  if (dados$tipo == "bp") {
    if ("ATIVO_TOTAL" %in% names(ag) && "PATRIMONIO_LIQUIDO" %in% names(ag)) {
      at <- ag[["ATIVO_TOTAL"]]
      pl <- ag[["PATRIMONIO_LIQUIDO"]]
      pc <- ag[["PASSIVO_CIRCULANTE"]]
      pnc <- ag[["PASSIVO_NAO_CIRCULANTE"]]
      if (!is.na(at) && !is.na(pl) && !is.na(pc) && !is.na(pnc)) {
        diff <- abs(at - (pc + pnc + pl))
        if (diff > abs(at) * 0.01) {
          avisos <- c(avisos, sprintf(
            "Equacao patrimonial nao fecha: AT=%.0f vs PC+PNC+PL=%.0f (diff=%.0f)",
            at, pc + pnc + pl, diff
          ))
        }
      }
    }
  }

  list(valido = length(erros) == 0, erros = erros, avisos = avisos)
}

#' Classificar descricoes de contas em categorias padrao CVM
#'
#' @param descricoes Vetor de descricoes.
#' @param categorias Lista nomeada de categorias.
#' @return Vetor de nomes de categorias.
#' @export
classificar_conta <- function(descricoes, categorias) {
  desc_norm <- normalizar_texto(descricoes)
  desc_norm <- gsub("intagivel", "intangivel", desc_norm, fixed = TRUE)
  cat_vals <- vapply(categorias, normalizar_texto, character(1))

  ordem <- order(nchar(cat_vals), decreasing = TRUE)

  resultado <- rep(NA_character_, length(desc_norm))

  for (idx in ordem) {
    cat_nome <- names(cat_vals)[idx]
    matches <- grepl(cat_vals[idx], desc_norm, fixed = TRUE)
    resultado[matches & is.na(resultado)] <- cat_nome
  }

  resultado
}

#' Formatar valor monetario de forma compacta
#'
#' @param x Valor numerico.
#' @return String formatada.
#' @export
formatar_valor <- function(x) {
  if (is.na(x)) return("N/A")
  if (abs(x) >= 1e6) return(sprintf("%.1fM", x / 1e6))
  if (abs(x) >= 1e3) return(sprintf("%.1fK", x / 1e3))
  sprintf("%.0f", x)
}

# --- Funcoes internas ---

padronizar_nomes_contabil <- function(df) {
  mapa <- c(
    "cd_conta" = "conta_cvm",
    "codigo_conta" = "conta_cvm",
    "conta" = "conta_cvm",
    "ds_conta" = "descricao",
    "descricao_conta" = "descricao",
    "denominacao_cia" = "empresa",
    "denom_cia" = "empresa",
    "cnpj_cia" = "cnpj",
    "dt_refer" = "data_referencia",
    "vl_conta" = "valor",
    "valor_conta" = "valor"
  )
  padronizar_nomes_com_mapa(df, mapa)
}

agregar_categorias <- function(df, categorias, colunas_valor) {
  resultado <- list()

  col_val <- if (length(colunas_valor) > 0) colunas_valor[1] else "valor"

  if (!col_val %in% names(df)) {
    return(resultado)
  }

  for (cat_nome in names(categorias)) {
    linhas <- df$categoria == cat_nome & !is.na(df$categoria)
    if (any(linhas)) {
      vals <- df[[col_val]][linhas]
      resultado[[cat_nome]] <- sum(vals, na.rm = TRUE)
    } else {
      resultado[[cat_nome]] <- NA_real_
    }
  }

  resultado
}

detectar_tipo_contabil <- function(df) {
  if (!"descricao" %in% names(df)) return("bp")
  desc <- normalizar_texto(df$descricao)
  tem_ativo <- any(grepl("ativo total|ativo circulante", desc))
  tem_receita <- any(grepl("receita|lucro|custo total", desc))
  tem_fluxo <- any(grepl("fluxo de caixa", desc))

  if (tem_ativo && tem_receita) return("misto")
  if (tem_receita && !tem_ativo) return("dre")
  if (tem_fluxo) return("misto")
  "bp"
}
