test_that("normalizar_texto remove diacriticos e normaliza", {
  expect_equal(normalizar_texto("Aeroporto"), "aeroporto")
  expect_equal(normalizar_texto("Ação Múltipla"), "acao multipla")
  expect_equal(normalizar_texto("São Paulo"), "sao paulo")
  expect_equal(normalizar_texto("  espacos  "), "espacos")
  expect_equal(normalizar_texto(123), 123)
})

test_that("conv_br_numeric converte formato brasileiro e internacional", {
  expect_equal(conv_br_numeric("1.234,56"), 1234.56)
  expect_equal(conv_br_numeric("42,0"), 42.0)
  expect_equal(conv_br_numeric("1234.56"), 1234.56)
  expect_equal(conv_br_numeric(42), 42)
  expect_true(is.na(conv_br_numeric("abc")))
})

test_that("safe_div previne divisao por zero e NA", {
  expect_equal(safe_div(10, 2), 5)
  expect_true(is.na(safe_div(10, 0)))
  expect_equal(safe_div(10, 0, 0), 0)
  expect_true(is.na(safe_div(10, NA)))
})

test_that("safe_div funciona com vetores", {
  expect_equal(safe_div(c(10, 20), 5), c(2, 4))
  expect_equal(safe_div(c(10, 20), c(5, 0), 0), c(2, 0))
})
