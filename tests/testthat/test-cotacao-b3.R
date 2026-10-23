# --- Testes de cotacao B3 (Yahoo Finance) ------------------------------------

test_that("yf_ticker adiciona .SA quando necessario", {
  expect_equal(yf_ticker("PETR4"), "PETR4.SA")
  expect_equal(yf_ticker("petr4"), "PETR4.SA")
  expect_equal(yf_ticker("VALE3.SA"), "VALE3.SA")
  expect_equal(yf_ticker(" itub4 "), "ITUB4.SA")
})

test_that("yf_extract_number extrai numeros do JSON", {
  json <- '{"regularMarketPrice":28.45,"regularMarketVolume":12345678}'
  expect_equal(yf_extract_number(json, "regularMarketPrice"), 28.45)
  expect_equal(yf_extract_number(json, "regularMarketVolume"), 12345678)
  expect_true(is.na(yf_extract_number(json, "naoExiste")))
})

test_that("yf_extract_string extrai strings do JSON", {
  json <- '{"shortName":"PETROBRAS PN","currency":"BRL"}'
  expect_equal(yf_extract_string(json, "shortName"), "PETROBRAS PN")
  expect_equal(yf_extract_string(json, "currency"), "BRL")
  expect_true(is.na(yf_extract_string(json, "naoExiste")))
})

test_that("yf_extract_array extrai arrays numericos do JSON", {
  json <- '{"timestamp":[1000,2000,3000],"close":[10.5,11.0,10.8]}'
  expect_equal(yf_extract_array(json, "timestamp"), c(1000, 2000, 3000))
  expect_equal(yf_extract_array(json, "close"), c(10.5, 11.0, 10.8))
  expect_equal(length(yf_extract_array(json, "naoExiste")), 0)
})

test_that("yf_extract_array trata null como NA", {
  json <- '{"close":[10.5,null,10.8]}'
  vals <- yf_extract_array(json, "close")
  expect_equal(length(vals), 3)
  expect_true(is.na(vals[2]))
})

test_that("yf_parse_json detecta erros da API", {
  json_err <- '{"chart":{"result":null,"error":{"code":"Not Found","description":"No data found"}}}'
  expect_error(yf_parse_json(json_err), "No data found")
})

test_that("cotacao_b3 falha graciosamente sem conexao", {
  expect_error(cotacao_b3("TICKERINVALIDO99999"), "Falha|Yahoo|conexao|ticker")
})

test_that("indicadores_investidor_b3 combina cotacao com indicadores", {
  ag <- list(PATRIMONIO_LIQUIDO = 50000, LUCRO_LIQUIDO = 5000)
  expect_error(indicadores_investidor_b3(ag, "TICKERINVALIDO99999", 1000),
               "Falha|Yahoo|conexao|ticker")
})
