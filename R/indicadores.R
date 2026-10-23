#' @title Indicadores Contabil-Financeiros
#' @name indicadores
#' @description Indicadores classicos (liquidez, endividamento, rentabilidade,
#'   DuPont, cobertura de juros), analise vertical/horizontal, modelo Fleuriet
#'   (CDG, NCG, ST, tipologia I-VI, efeito tesoura), prazos medios e ciclos,
#'   alavancagem (GAO, GAF, GAT), modelos de insolvencia (Kanitz, Altman,
#'   Elizabetsky, Matias) e indicadores do investidor (VPA, LPA, P/L, DPA).
#'
#'   Todas as funcoes operam sobre listas agregadas (nome -> valor) tal como
#'   retornadas por importar_contabil_dfp()$agregado.
NULL

# --- auxiliares internos -----------------------------------------------------

v_ag <- function(ag, nome) {
  x <- ag[[nome]]
  if (is.null(x) || length(x) == 0) NA_real_ else as.numeric(x)
}

soma_na <- function(...) {
  x <- c(...)
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

v_primeiro <- function(ag, nomes) {
  for (n in nomes) {
    x <- v_ag(ag, n)
    if (!is.na(x)) return(x)
  }
  NA_real_
}

var_pct <- function(atual, anterior) safe_div(atual - anterior, abs(anterior))

vendas_ag <- function(ag) v_primeiro(ag, c("RECEITA_LIQUIDA", "RECEITA_TOTAL"))
lajir_ag  <- function(ag) v_primeiro(ag, c("RESULTADO_ANTES_IR", "LUCRO_OPERACIONAL"))
cmv_ag    <- function(ag) abs(v_primeiro(ag, c("CUSTO_SERVICOS", "CUSTO_TOTAL")))
exigivel_total <- function(ag) soma_na(v_ag(ag, "PASSIVO_CIRCULANTE"),
                                       v_ag(ag, "PASSIVO_NAO_CIRCULANTE"))

# =============================================================================
# 1. Indicadores de liquidez
# =============================================================================

#' Liquidez corrente
#'
#' LC = AC / PC. Mede a capacidade de pagar obrigacoes de curto prazo.
#'
#' @param ag Lista agregada do balanco.
#' @return Valor numerico da liquidez corrente.
#' @export
liquidez_corrente <- function(ag) {
  safe_div(v_ag(ag, "ATIVO_CIRCULANTE"), v_ag(ag, "PASSIVO_CIRCULANTE"))
}

#' Liquidez seca
#'
#' LS = (AC - Estoques) / PC. Exclui estoques, menos liquidos.
#'
#' @inheritParams liquidez_corrente
#' @return Valor numerico.
#' @export
liquidez_seca <- function(ag) {
  est <- v_ag(ag, "ESTOQUES"); if (is.na(est)) est <- 0
  safe_div(v_ag(ag, "ATIVO_CIRCULANTE") - est, v_ag(ag, "PASSIVO_CIRCULANTE"))
}

#' Liquidez imediata
#'
#' LI = Disponivel / PC. Considera apenas caixa e equivalentes.
#'
#' @inheritParams liquidez_corrente
#' @return Valor numerico.
#' @export
liquidez_imediata <- function(ag) {
  safe_div(v_ag(ag, "CAIXA_EQUIVALENTES"), v_ag(ag, "PASSIVO_CIRCULANTE"))
}

#' Liquidez geral
#'
#' LG = (AC + RLP) / (PC + PNC). Compara realizaveis com exigiveis totais.
#'
#' @inheritParams liquidez_corrente
#' @return Valor numerico.
#' @export
liquidez_geral <- function(ag) {
  rlp <- v_ag(ag, "REALIZAVEL_LP"); if (is.na(rlp)) rlp <- 0
  safe_div(v_ag(ag, "ATIVO_CIRCULANTE") + rlp, exigivel_total(ag))
}

#' Todos os indicadores de liquidez
#'
#' @inheritParams liquidez_corrente
#' @return Lista com LC, LS, LI, LG.
#' @export
indicadores_liquidez <- function(ag) {
  list(
    LC = liquidez_corrente(ag),
    LS = liquidez_seca(ag),
    LI = liquidez_imediata(ag),
    LG = liquidez_geral(ag)
  )
}

# =============================================================================
# 2. Estrutura de capital / endividamento
# =============================================================================

#' Endividamento geral
#'
#' EG = Exigivel total / Ativo total. Proporcao do ativo financiada por terceiros.
#'
#' @param ag Lista agregada do balanco.
#' @return Valor numerico.
#' @export
endividamento_geral <- function(ag) {
  safe_div(exigivel_total(ag), v_ag(ag, "ATIVO_TOTAL"))
}

#' Composicao do endividamento
#'
#' CE = PC / Exigivel total. Proporcao das dividas que vencem no curto prazo.
#'
#' @inheritParams endividamento_geral
#' @return Valor numerico.
#' @export
composicao_endividamento <- function(ag) {
  safe_div(v_ag(ag, "PASSIVO_CIRCULANTE"), exigivel_total(ag))
}

#' Imobilizacao do patrimonio liquido
#'
#' IPL = (Imobilizado + Intangivel) / PL.
#'
#' @inheritParams endividamento_geral
#' @return Valor numerico.
#' @export
imobilizacao_pl <- function(ag) {
  imob <- v_ag(ag, "IMOBILIZADO"); if (is.na(imob)) imob <- 0
  intang <- v_ag(ag, "INTANGIVEL"); if (is.na(intang)) intang <- 0
  safe_div(imob + intang, v_ag(ag, "PATRIMONIO_LIQUIDO"))
}

#' Imobilizacao de recursos nao correntes
#'
#' IRNC = (Imobilizado + Intangivel) / (PL + PNC).
#'
#' @inheritParams endividamento_geral
#' @return Valor numerico.
#' @export
imobilizacao_rnc <- function(ag) {
  imob <- v_ag(ag, "IMOBILIZADO"); if (is.na(imob)) imob <- 0
  intang <- v_ag(ag, "INTANGIVEL"); if (is.na(intang)) intang <- 0
  safe_div(imob + intang,
           soma_na(v_ag(ag, "PATRIMONIO_LIQUIDO"), v_ag(ag, "PASSIVO_NAO_CIRCULANTE")))
}

#' Participacao de capital de terceiros
#'
#' PCT = Exigivel total / PL. Quanto de capital de terceiros para cada real proprio.
#'
#' @inheritParams endividamento_geral
#' @return Valor numerico.
#' @export
participacao_capital_terceiros <- function(ag) {
  safe_div(exigivel_total(ag), v_ag(ag, "PATRIMONIO_LIQUIDO"))
}

#' Todos os indicadores de endividamento
#'
#' @inheritParams endividamento_geral
#' @return Lista com EG, CE, IPL, IRNC, PCT.
#' @export
indicadores_endividamento <- function(ag) {
  list(
    EG   = endividamento_geral(ag),
    CE   = composicao_endividamento(ag),
    IPL  = imobilizacao_pl(ag),
    IRNC = imobilizacao_rnc(ag),
    PCT  = participacao_capital_terceiros(ag)
  )
}

# =============================================================================
# 3. Rentabilidade
# =============================================================================

#' Margem bruta
#'
#' MB = Lucro bruto / Receita liquida.
#'
#' @param ag Lista agregada (DRE).
#' @return Valor numerico.
#' @export
margem_bruta <- function(ag) {
  safe_div(v_ag(ag, "LUCRO_BRUTO"), vendas_ag(ag))
}

#' Margem operacional
#'
#' MO = LAJIR / Receita liquida.
#'
#' @inheritParams margem_bruta
#' @return Valor numerico.
#' @export
margem_operacional <- function(ag) {
  safe_div(lajir_ag(ag), vendas_ag(ag))
}

#' Margem liquida
#'
#' ML = Lucro liquido / Receita liquida.
#'
#' @inheritParams margem_bruta
#' @return Valor numerico.
#' @export
margem_liquida <- function(ag) {
  safe_div(v_ag(ag, "LUCRO_LIQUIDO"), vendas_ag(ag))
}

#' Retorno sobre o ativo (ROA)
#'
#' ROA = Lucro liquido / Ativo total.
#'
#' @param ag Lista agregada (balanco + DRE).
#' @return Valor numerico.
#' @export
roa <- function(ag) {
  safe_div(v_ag(ag, "LUCRO_LIQUIDO"), v_ag(ag, "ATIVO_TOTAL"))
}

#' Retorno sobre o patrimonio liquido (ROE)
#'
#' ROE = Lucro liquido / PL.
#'
#' @inheritParams roa
#' @return Valor numerico.
#' @export
roe <- function(ag) {
  safe_div(v_ag(ag, "LUCRO_LIQUIDO"), v_ag(ag, "PATRIMONIO_LIQUIDO"))
}

#' Giro do ativo
#'
#' GA = Receita liquida / Ativo total.
#'
#' @inheritParams roa
#' @return Valor numerico.
#' @export
giro_ativo <- function(ag) {
  safe_div(vendas_ag(ag), v_ag(ag, "ATIVO_TOTAL"))
}

#' Todos os indicadores de rentabilidade
#'
#' @inheritParams roa
#' @return Lista com MB, MO, ML, ROA, ROE, GA.
#' @export
indicadores_rentabilidade <- function(ag) {
  list(
    MB  = margem_bruta(ag),
    MO  = margem_operacional(ag),
    ML  = margem_liquida(ag),
    ROA = roa(ag),
    ROE = roe(ag),
    GA  = giro_ativo(ag)
  )
}

# =============================================================================
# 4. Decomposicao DuPont
# =============================================================================

#' Decomposicao DuPont
#'
#' ROE = Margem liquida x Giro do ativo x Multiplicador de alavancagem.
#' Multiplicador = AT / PL.
#'
#' @param ag Lista agregada (balanco + DRE).
#' @return Lista com ML, GA, multiplicador, ROE_dupont.
#' @export
dupont <- function(ag) {
  ml  <- margem_liquida(ag)
  ga  <- giro_ativo(ag)
  mult <- safe_div(v_ag(ag, "ATIVO_TOTAL"), v_ag(ag, "PATRIMONIO_LIQUIDO"))
  list(ML = ml, GA = ga, multiplicador = mult,
       ROE_dupont = ml * ga * mult)
}

# =============================================================================
# 5. Cobertura de juros
# =============================================================================

#' Indice de cobertura de juros
#'
#' ICJ = LAJIR / Despesas financeiras. Quantas vezes o resultado operacional
#' cobre os encargos financeiros.
#'
#' @param ag Lista agregada (DRE).
#' @return Valor numerico.
#' @export
cobertura_juros <- function(ag) {
  safe_div(lajir_ag(ag), abs(v_ag(ag, "DESPESAS_FINANCEIRAS")))
}

# =============================================================================
# 6. Analise vertical e horizontal
# =============================================================================

#' Analise vertical
#'
#' Participacao de cada conta sobre uma conta-base.
#'
#' @param ag Lista agregada de categorias.
#' @param base "ATIVO_TOTAL" (balanco) ou "RECEITA_LIQUIDA" (DRE).
#' @param contas Categorias a incluir. NULL = todas com valor.
#' @return data.frame (categoria, valor, pct).
#' @export
analise_vertical <- function(ag, base = "ATIVO_TOTAL", contas = NULL) {
  valor_base <- v_ag(ag, base)
  if (is.na(valor_base) || valor_base == 0) {
    stop("Conta-base ausente ou zero: ", base, call. = FALSE)
  }
  if (is.null(contas)) contas <- names(ag)
  vals <- vapply(contas, function(n) v_ag(ag, n), numeric(1))
  df <- data.frame(categoria = contas, valor = vals,
                   pct = safe_div(vals, valor_base) * 100,
                   stringsAsFactors = FALSE)
  rownames(df) <- NULL
  df[!is.na(df$valor), ]
}

#' Analise horizontal (numeros-indice)
#'
#' @param lista_ag Lista nomeada por ano: list("2024" = ag24, "2025" = ag25).
#' @param contas Categorias a incluir. NULL = todas.
#' @param ano_base Ano-base (= 100%). NULL = primeiro ano.
#' @return data.frame (categoria, ano, valor, indice, var_anual).
#' @export
analise_horizontal <- function(lista_ag, contas = NULL, ano_base = NULL) {
  anos <- sort(names(lista_ag))
  if (length(anos) < 2) stop("Necessario ao menos dois periodos.", call. = FALSE)
  if (is.null(ano_base)) ano_base <- anos[1]
  if (is.null(contas)) contas <- names(lista_ag[[ano_base]])

  linhas <- list()
  for (cta in contas) {
    base <- v_ag(lista_ag[[ano_base]], cta)
    ant <- NA_real_
    for (a in anos) {
      val <- v_ag(lista_ag[[a]], cta)
      linhas[[length(linhas) + 1]] <- data.frame(
        categoria = cta, ano = a, valor = val,
        indice = safe_div(val, base) * 100,
        var_anual = var_pct(val, ant) * 100,
        stringsAsFactors = FALSE)
      ant <- val
    }
  }
  df <- do.call(rbind, linhas)
  rownames(df) <- NULL
  df
}

# =============================================================================
# 7. Capital de giro - modelo Fleuriet (CCL, NIG, SD)
# =============================================================================

#' Reclassificar o balanco no modelo Fleuriet
#'
#' Ativo circulante financeiro (ACF) = caixa + aplicacoes financeiras.
#' Passivo circulante financeiro (PCF) = emprestimos + financiamentos +
#' debentures de curto prazo. O restante do circulante e operacional (ciclico).
#'
#' @param ag Lista agregada do balanco.
#' @return Lista com ACF, ACO, PCF, PCO, ANC, PNC_PL.
#' @export
reclassificar_fleuriet <- function(ag) {
  ac  <- v_ag(ag, "ATIVO_CIRCULANTE")
  pc  <- v_ag(ag, "PASSIVO_CIRCULANTE")
  acf <- soma_na(v_ag(ag, "CAIXA_EQUIVALENTES"), v_ag(ag, "APLICACOES_FINANCEIRAS"))
  pcf <- soma_na(v_ag(ag, "EMPRESTIMOS_CP"), v_ag(ag, "FINANCIAMENTOS_CP"),
                 v_ag(ag, "DEBENTURES_CP"))
  if (is.na(pcf)) pcf <- 0
  list(
    ACF = acf, ACO = ac - acf,
    PCF = pcf, PCO = pc - pcf,
    ANC = v_ag(ag, "ATIVO_NAO_CIRCULANTE"),
    PNC_PL = soma_na(v_ag(ag, "PASSIVO_NAO_CIRCULANTE"), v_ag(ag, "PATRIMONIO_LIQUIDO"))
  )
}

#' Variaveis do modelo Fleuriet
#'
#' CDG (= CCL) = AC - PC; NCG (= NIG) = ACO - PCO; ST (= SD) = ACF - PCF;
#' NTFP = NCG + ANC; termometro financeiro TF = |ST| / NCG.
#'
#' @param x Lista agregada do balanco OU resultado de reclassificar_fleuriet().
#' @return Lista com CDG, NCG, ST, NTFP, TF, tipo e situacao.
#' @export
fleuriet <- function(x) {
  r <- if (all(c("ACF", "ACO", "PCF", "PCO") %in% names(x))) x else reclassificar_fleuriet(x)
  cdg <- (r$ACF + r$ACO) - (r$PCF + r$PCO)
  ncg <- r$ACO - r$PCO
  st  <- r$ACF - r$PCF
  tip <- tipologia_fleuriet(cdg, ncg, st)
  list(CDG = cdg, NCG = ncg, ST = st,
       NTFP = soma_na(ncg, r$ANC),
       TF = safe_div(abs(st), ncg),
       tipo = tip$tipo, situacao = tip$situacao)
}

#' Tipologia de Marques e Braga (1995)
#'
#' @param cdg,ncg,st Valores de CDG (CCL), NCG (NIG) e ST (SD).
#' @return Lista com tipo (I a VI) e situacao.
#' @export
tipologia_fleuriet <- function(cdg, ncg, st) {
  if (any(is.na(c(cdg, ncg, st)))) return(list(tipo = NA_character_, situacao = NA_character_))
  s <- paste0(ifelse(cdg >= 0, "+", "-"), ifelse(ncg >= 0, "+", "-"), ifelse(st >= 0, "+", "-"))
  tabela <- c("+-+" = "I",  "+++" = "II", "++-" = "III",
              "-+-" = "IV", "---" = "V",  "--+" = "VI")
  rotulos <- c(I = "Excelente", II = "Solida", III = "Insatisfatoria",
               IV = "Pessima", V = "Muito ruim", VI = "Alto risco")
  tipo <- unname(tabela[s])
  if (is.na(tipo)) return(list(tipo = NA_character_, situacao = "Nao classificada"))
  list(tipo = tipo, situacao = unname(rotulos[tipo]))
}

#' Efeito tesoura
#'
#' Sinaliza os periodos em que a NCG cresce mais que o CDG e o saldo de
#' tesouraria fica negativo.
#'
#' @param cdg,ncg Vetores em ordem cronologica.
#' @param anos Rotulos dos periodos (opcional).
#' @return data.frame (ano, CDG, NCG, ST, tesoura).
#' @export
efeito_tesoura <- function(cdg, ncg, anos = seq_along(cdg)) {
  st <- cdg - ncg
  dncg <- c(NA, diff(ncg))
  dcdg <- c(NA, diff(cdg))
  data.frame(ano = anos, CDG = cdg, NCG = ncg, ST = st,
             tesoura = !is.na(dncg) & dncg > dcdg & st < 0,
             stringsAsFactors = FALSE)
}

# =============================================================================
# 8. Prazos medios e ciclos
# =============================================================================

#' Prazos medios, ciclo operacional e ciclo financeiro
#'
#' PMRV = Contas a receber / Vendas x dias; PMRE = Estoques / CMV x dias;
#' PMPC = Fornecedores / Compras x dias. Compras = CMV + EF - EI quando o
#' periodo anterior e informado; caso contrario usa o CMV como aproximacao.
#'
#' @param ag Lista agregada do periodo.
#' @param ag_ant Lista agregada do periodo anterior (opcional).
#' @param dias Base de dias (padrao 360).
#' @return Lista com PMRV, PMRE, PMPC, ciclo_operacional, ciclo_financeiro e rotacoes.
#' @export
prazos_medios <- function(ag, ag_ant = NULL, dias = 360) {
  vendas <- vendas_ag(ag)
  cmv <- cmv_ag(ag)
  est <- v_ag(ag, "ESTOQUES")
  compras <- cmv
  if (!is.null(ag_ant) && !is.na(est) && !is.na(v_ag(ag_ant, "ESTOQUES"))) {
    compras <- cmv + est - v_ag(ag_ant, "ESTOQUES")
  }
  pmrv <- safe_div(v_ag(ag, "CONTAS_RECEBER_CP"), vendas) * dias
  pmre <- safe_div(est, cmv) * dias
  if (is.na(est)) pmre <- 0
  pmpc <- safe_div(v_ag(ag, "FORNECEDORES"), compras) * dias
  co <- pmre + pmrv
  list(PMRV = pmrv, PMRE = pmre, PMPC = pmpc,
       ciclo_operacional = co, ciclo_financeiro = co - pmpc,
       rot_estoques = safe_div(dias, pmre),
       rot_receber = safe_div(dias, pmrv),
       rot_fornecedores = safe_div(dias, pmpc))
}

# =============================================================================
# 9. Alavancagem operacional, financeira e total
# =============================================================================

#' Graus de alavancagem
#'
#' GAO = var% LAJIR / var% Vendas; GAF = var% LL / var% LAJIR;
#' GAT = var% LL / var% Vendas (= GAO x GAF).
#'
#' @param ag Lista agregada do periodo atual.
#' @param ag_ant Lista agregada do periodo anterior.
#' @return Lista com GAO, GAF, GAT e as variacoes usadas.
#' @export
alavancagem <- function(ag, ag_ant) {
  dv <- var_pct(vendas_ag(ag), vendas_ag(ag_ant))
  dl <- var_pct(lajir_ag(ag), lajir_ag(ag_ant))
  dn <- var_pct(v_ag(ag, "LUCRO_LIQUIDO"), v_ag(ag_ant, "LUCRO_LIQUIDO"))
  list(GAO = safe_div(dl, dv), GAF = safe_div(dn, dl), GAT = safe_div(dn, dv),
       var_vendas = dv, var_lajir = dl, var_ll = dn)
}

# =============================================================================
# 10. Modelos de previsao de insolvencia
#     Atencao: modelos estimados para industria e comercio. Em concessoes
#     (intangivel elevado, PL negativo) os escores devem ser lidos com cautela.
# =============================================================================

#' Fator de insolvencia de Kanitz
#'
#' FI = 0,05 X1 + 1,65 X2 + 3,55 X3 - 1,06 X4 - 0,33 X5
#' X1 = LL/PL; X2 = (AC+RLP)/ET; X3 = (AC-Estoques)/PC; X4 = AC/PC; X5 = ET/PL.
#' Faixas: FI < -3 insolvente; -3 a 0 penumbra; > 0 solvente.
#'
#' @param ag Lista agregada (balanco + DRE).
#' @return Lista com escore, faixa e componentes.
#' @export
kanitz <- function(ag) {
  et <- exigivel_total(ag)
  pl <- v_ag(ag, "PATRIMONIO_LIQUIDO")
  ac <- v_ag(ag, "ATIVO_CIRCULANTE")
  pc <- v_ag(ag, "PASSIVO_CIRCULANTE")
  est <- v_ag(ag, "ESTOQUES"); if (is.na(est)) est <- 0
  rlp <- v_ag(ag, "REALIZAVEL_LP"); if (is.na(rlp)) rlp <- 0
  x <- c(X1 = safe_div(v_ag(ag, "LUCRO_LIQUIDO"), pl),
         X2 = safe_div(ac + rlp, et),
         X3 = safe_div(ac - est, pc),
         X4 = safe_div(ac, pc),
         X5 = safe_div(et, pl))
  fi <- sum(c(0.05, 1.65, 3.55, -1.06, -0.33) * x)
  list(escore = fi, faixa = classificar_kanitz(fi), componentes = x,
       aviso = if (!is.na(pl) && pl < 0) "PL negativo: X1 e X5 perdem significado" else NULL)
}

#' @rdname kanitz
#' @param fi Fator de insolvencia.
#' @export
classificar_kanitz <- function(fi) {
  if (is.na(fi)) return(NA_character_)
  if (fi < -3) "Insolvente" else if (fi <= 0) "Penumbra" else "Solvente"
}

#' Escore de Altman (versao brasileira, Z1)
#'
#' Z1 = -1,44 + 4,03 X2 + 2,25 X3 + 0,14 X4 + 0,42 X5
#' X2 = Reservas de lucros / AT; X3 = LAJIR / AT; X4 = PL / ET;
#' X5 = Vendas / AT. Z < 0 insolvente.
#'
#' @inheritParams kanitz
#' @export
altman <- function(ag) {
  at <- v_ag(ag, "ATIVO_TOTAL")
  x <- c(X2 = safe_div(v_ag(ag, "RESERVAS_LUCROS"), at),
         X3 = safe_div(lajir_ag(ag), at),
         X4 = safe_div(v_ag(ag, "PATRIMONIO_LIQUIDO"), exigivel_total(ag)),
         X5 = safe_div(vendas_ag(ag), at))
  z <- -1.44 + sum(c(4.03, 2.25, 0.14, 0.42) * x)
  list(escore = z, faixa = if (is.na(z)) NA_character_ else if (z < 0) "Insolvente" else "Solvente",
       componentes = x)
}

#' Escore de Elizabetsky
#'
#' Z = 1,93 X32 - 0,20 X33 + 1,02 X35 + 1,33 X36 - 1,12 X37
#' X32 = LL/Vendas; X33 = Disponivel/ANC; X35 = CR/AT; X36 = Estoques/AT;
#' X37 = PC/AT. Z < 0,5 insolvente.
#'
#' @inheritParams kanitz
#' @export
elizabetsky <- function(ag) {
  at <- v_ag(ag, "ATIVO_TOTAL")
  est <- v_ag(ag, "ESTOQUES"); if (is.na(est)) est <- 0
  x <- c(X32 = safe_div(v_ag(ag, "LUCRO_LIQUIDO"), vendas_ag(ag)),
         X33 = safe_div(v_ag(ag, "CAIXA_EQUIVALENTES"), v_ag(ag, "ATIVO_NAO_CIRCULANTE")),
         X35 = safe_div(v_ag(ag, "CONTAS_RECEBER_CP"), at),
         X36 = safe_div(est, at),
         X37 = safe_div(v_ag(ag, "PASSIVO_CIRCULANTE"), at))
  z <- sum(c(1.93, -0.20, 1.02, 1.33, -1.12) * x)
  list(escore = z, faixa = if (is.na(z)) NA_character_ else if (z < 0.5) "Insolvente" else "Solvente",
       componentes = x)
}

#' Escore de Matias
#'
#' Z = 23,79 X1 - 8,26 X2 - 9,868 X3 - 0,764 X4 - 0,535 X5 + 9,912 X6
#' X1 = PL/AT; X2 = Emprest./AT; X3 = Fornecedores/AT; X4 = AC/PC;
#' X5 = LO/LB; X6 = Disponivel/AT. Z < 0 insolvente.
#'
#' @inheritParams kanitz
#' @export
matias <- function(ag) {
  at <- v_ag(ag, "ATIVO_TOTAL")
  emp <- soma_na(v_ag(ag, "EMPRESTIMOS_CP"), v_ag(ag, "FINANCIAMENTOS_CP"),
                 v_ag(ag, "EMPRESTIMOS_LP"), v_ag(ag, "FINANCIAMENTOS_LP"))
  x <- c(X1 = safe_div(v_ag(ag, "PATRIMONIO_LIQUIDO"), at),
         X2 = safe_div(emp, at),
         X3 = safe_div(v_ag(ag, "FORNECEDORES"), at),
         X4 = safe_div(v_ag(ag, "ATIVO_CIRCULANTE"), v_ag(ag, "PASSIVO_CIRCULANTE")),
         X5 = safe_div(lajir_ag(ag), v_primeiro(ag, c("LUCRO_BRUTO", "RESULTADO_BRUTO"))),
         X6 = safe_div(v_ag(ag, "CAIXA_EQUIVALENTES"), at))
  z <- sum(c(23.79, -8.26, -9.868, -0.764, -0.535, 9.912) * x)
  list(escore = z, faixa = if (is.na(z)) NA_character_ else if (z < 0) "Insolvente" else "Solvente",
       componentes = x)
}

# =============================================================================
# 11. Indicadores do ponto de vista do investidor (Marion, 2013)
# =============================================================================

#' Indicadores do investidor
#'
#' VPA = PL / n de acoes; LPA = LL / n de acoes; P/L = preco / LPA;
#' DPA = dividendos / n de acoes.
#'
#' @param ag Lista agregada.
#' @param n_acoes Quantidade de acoes.
#' @param preco Cotacao da acao (opcional, B3).
#' @param dividendos Dividendos distribuidos no exercicio (opcional).
#' @export
indicadores_investidor <- function(ag, n_acoes, preco = NA_real_, dividendos = NA_real_) {
  lpa <- safe_div(v_ag(ag, "LUCRO_LIQUIDO"), n_acoes)
  list(VPA = safe_div(v_ag(ag, "PATRIMONIO_LIQUIDO"), n_acoes),
       LPA = lpa,
       PL_indice = safe_div(preco, lpa),
       DPA = safe_div(dividendos, n_acoes))
}
