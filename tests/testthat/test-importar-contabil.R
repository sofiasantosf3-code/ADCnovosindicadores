test_that("categorias_bp tem contas esperadas sem duplicatas de texto", {
  expect_true("ATIVO_TOTAL" %in% names(categorias_bp))
  expect_true("PATRIMONIO_LIQUIDO" %in% names(categorias_bp))
  expect_true("EMPRESTIMOS_CP" %in% names(categorias_bp))
  expect_true("EMPRESTIMOS_LP" %in% names(categorias_bp))
  expect_true("APLICACOES_FINANCEIRAS" %in% names(categorias_bp))
  expect_true("RESERVAS_LUCROS" %in% names(categorias_bp))
  vals <- unlist(categorias_bp, use.names = FALSE)
  expect_equal(length(vals), length(unique(vals)))
})

test_that("categorias_dre tem contas esperadas", {
  expect_true("RECEITA_LIQUIDA" %in% names(categorias_dre))
  expect_true("LUCRO_BRUTO" %in% names(categorias_dre))
  expect_true("LUCRO_LIQUIDO" %in% names(categorias_dre))
})

test_that("categorias_dfc e dva existem", {
  expect_true("FCO" %in% names(categorias_dfc))
  expect_true("VA_DISTRIBUIR" %in% names(categorias_dva))
})

test_that("classificar_conta classifica descricoes corretamente", {
  todas_cats <- c(categorias_bp, categorias_dre, categorias_dfc, categorias_dva)
  desc <- c("Ativo Total", "Ativo Circulante", "Receita total", "xyz")
  cats <- classificar_conta(desc, todas_cats)
  expect_equal(cats[1], "ATIVO_TOTAL")
  expect_equal(cats[2], "ATIVO_CIRCULANTE")
  expect_equal(cats[3], "RECEITA_TOTAL")
  expect_true(is.na(cats[4]))
})

test_that("classificar_conta corrige typo intagivel", {
  cats <- classificar_conta("Intagível", categorias_bp)
  expect_equal(cats[1], "INTANGIVEL")
})

test_that("validar_dados_contabeis rejeita formato invalido", {
  v <- validar_dados_contabeis(list(x = 1))
  expect_false(v$valido)
})

test_that("importar_contabil_dfp da erro claro sem coluna de descricao", {
  f <- tempfile(fileext = ".csv")
  writeLines(c("codigo;gates", "GRU;80"), f)
  expect_error(importar_contabil_dfp(f), "Colunas obrigatorias ausentes")
})

test_that("formatar_valor formata corretamente", {
  expect_equal(formatar_valor(NA), "N/A")
  expect_equal(formatar_valor(1500000), "1.5M")
  expect_equal(formatar_valor(2500), "2.5K")
  expect_equal(formatar_valor(42), "42")
})
