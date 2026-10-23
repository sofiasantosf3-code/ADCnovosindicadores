test_that("alavancagem reproduz o exemplo da aula (GAO 2, GAF 2,5, GAT 5)", {
  ant <- list(RECEITA_LIQUIDA = 2000000, RESULTADO_ANTES_IR = 500000, LUCRO_LIQUIDO = 120000)
  atu <- list(RECEITA_LIQUIDA = 2400000, RESULTADO_ANTES_IR = 700000, LUCRO_LIQUIDO = 240000)
  a <- alavancagem(atu, ant)
  expect_equal(a$GAO, 2)
  expect_equal(a$GAF, 2.5)
  expect_equal(a$GAT, 5)
})

test_that("fleuriet reproduz o exemplo da Aula 07 (tipo III)", {
  r <- list(ACF = 200, ACO = 370, PCF = 230, PCO = 270, ANC = 300, PNC_PL = 500)
  f <- fleuriet(r)
  expect_equal(f$CDG, 70)
  expect_equal(f$NCG, 100)
  expect_equal(f$ST, -30)
  expect_equal(f$ST, f$CDG - f$NCG)
  expect_equal(f$NTFP, 400)
  expect_equal(f$TF, 0.3)
  expect_equal(f$tipo, "III")
  expect_equal(f$situacao, "Insatisfatoria")
})

test_that("tipologia_fleuriet cobre os seis tipos de Marques e Braga", {
  expect_equal(tipologia_fleuriet( 1, -1,  1)$tipo, "I")
  expect_equal(tipologia_fleuriet( 1,  1,  1)$tipo, "II")
  expect_equal(tipologia_fleuriet( 1,  1, -1)$tipo, "III")
  expect_equal(tipologia_fleuriet(-1,  1, -1)$tipo, "IV")
  expect_equal(tipologia_fleuriet(-1, -1, -1)$tipo, "V")
  expect_equal(tipologia_fleuriet(-1, -1,  1)$tipo, "VI")
})

test_that("reclassificar_fleuriet separa circulante financeiro e operacional", {
  ag <- list(ATIVO_CIRCULANTE = 570, CAIXA_EQUIVALENTES = 150, APLICACOES_FINANCEIRAS = 50,
             PASSIVO_CIRCULANTE = 500, EMPRESTIMOS_CP = 230,
             ATIVO_NAO_CIRCULANTE = 300, PASSIVO_NAO_CIRCULANTE = 200, PATRIMONIO_LIQUIDO = 300)
  r <- reclassificar_fleuriet(ag)
  expect_equal(r$ACF, 200)
  expect_equal(r$ACO, 370)
  expect_equal(r$PCF, 230)
  expect_equal(r$PCO, 270)
})

test_that("efeito_tesoura sinaliza NCG crescendo acima do CDG com ST negativo", {
  e <- efeito_tesoura(cdg = c(100, 110, 115), ncg = c(90, 120, 160), anos = 2023:2025)
  expect_equal(e$tesoura, c(FALSE, TRUE, TRUE))
})

test_that("analise_horizontal reproduz o Ativo Total da Aula 06", {
  l <- list("2010" = list(ATIVO_TOTAL = 7997),
            "2011" = list(ATIVO_TOTAL = 9458),
            "2012" = list(ATIVO_TOTAL = 11080))
  h <- analise_horizontal(l, contas = "ATIVO_TOTAL")
  expect_equal(round(h$indice, 1), c(100.0, 118.3, 138.6))
})

test_that("analise_vertical calcula participacao sobre a base", {
  ag <- list(ATIVO_TOTAL = 7997, ATIVO_CIRCULANTE = 5852)
  v <- analise_vertical(ag, "ATIVO_TOTAL")
  expect_equal(round(v$pct[v$categoria == "ATIVO_CIRCULANTE"], 1), 73.2)
})

test_that("prazos_medios calcula PMRV, PMRE, PMPC e ciclos", {
  ag <- list(RECEITA_LIQUIDA = 3600, CONTAS_RECEBER_CP = 750,
             CUSTO_SERVICOS = -1800, ESTOQUES = 345, FORNECEDORES = 445)
  p <- prazos_medios(ag)
  expect_equal(p$PMRV, 75)
  expect_equal(p$PMRE, 69)
  expect_equal(p$PMPC, 89)
  expect_equal(p$ciclo_operacional, 144)
  expect_equal(p$ciclo_financeiro, 55)
})

test_that("classificar_kanitz respeita o termometro de insolvencia", {
  expect_equal(classificar_kanitz(-4), "Insolvente")
  expect_equal(classificar_kanitz(-1), "Penumbra")
  expect_equal(classificar_kanitz(2), "Solvente")
})

test_that("kanitz avisa quando o PL e negativo", {
  ag <- list(ATIVO_CIRCULANTE = 100, PASSIVO_CIRCULANTE = 200, PASSIVO_NAO_CIRCULANTE = 900,
             PATRIMONIO_LIQUIDO = -100, LUCRO_LIQUIDO = -10)
  k <- kanitz(ag)
  expect_false(is.null(k$aviso))
})

test_that("indicadores_investidor calcula VPA, LPA e P/L", {
  i <- indicadores_investidor(list(PATRIMONIO_LIQUIDO = 1000, LUCRO_LIQUIDO = 100),
                              n_acoes = 100, preco = 20)
  expect_equal(i$VPA, 10)
  expect_equal(i$LPA, 1)
  expect_equal(i$PL_indice, 20)
})
