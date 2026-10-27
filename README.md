# ADCnovosindicadores

Pacote R para **Analise das Demonstracoes Contabeis** de empresas listadas na B3. Importa dados no formato CVM/DFP/ITR, classifica contas automaticamente e calcula indicadores financeiros utilizados nas disciplinas de Analise das Demonstracoes Contabeis e Contabilidade Financeira.

## Instalacao

```r
# Via remotes (GitHub)
remotes::install_github("sofiasantosf3-code/ADCnovosindicadores", force = TRUE)
```

## Funcionalidades

### 1. Importacao online de dados CVM

Baixa dados diretamente do portal de dados abertos da CVM — basta informar o nome da empresa (ou ticker) e o periodo.

```r
library(ADCnovosindicadores)

# Importar dados de qualquer empresa
viva <- importar_cvm_online("VIVA3", "1T25")
ag <- viva$agregado

indicadores_liquidez(ag)
indicadores_endividamento(ag)
indicadores_rentabilidade(ag)
dupont(ag)
cobertura_juros(ag)
fleuriet(ag)
kanitz(ag)

# Analise completa com todos os indicadores de uma vez
resultado <- analise_completa("PETROBRAS", "1T25", n_acoes = 12757851)
print(resultado)

# Listar empresas disponiveis
listar_empresas_cvm("1T25", "banco")
```

### 1b. Importacao de arquivo local (CSV)

Tambem aceita CSVs no formato padrao CVM, classificando automaticamente as contas do Balanco Patrimonial, DRE, DFC e DVA.

```r
resultado <- importar_contabil_dfp("dados_empresa.csv")
ag <- resultado$agregado   # lista: ATIVO_TOTAL, RECEITA_LIQUIDA, ...
```

**Categorias reconhecidas:**

| Demonstracao | Contas                                                                                  |
|:-------------|:----------------------------------------------------------------------------------------|
| **BP**       | Ativo Total/Circulante/Nao Circulante, Caixa, Aplicacoes Financeiras, Estoques, Contas a Receber, Imobilizado, Intangivel, Passivo Circulante/Nao Circulante, Emprestimos CP/LP, Financiamentos CP/LP, Debentures CP/LP, Fornecedores, PL, Capital Social, Reservas de Lucros |
| **DRE**      | Receita Total/Liquida, Custo Total/Servicos, Lucro Bruto/Operacional/Liquido, Despesas Operacionais/Vendas/Administrativas, Resultado Financeiro, Receitas/Despesas Financeiras, Resultado antes do IR |
| **DFC**      | FCO, FCI, FCF                                                                          |
| **DVA**      | Valor Adicionado a Distribuir, Pessoal, Impostos, Remuneracao de Terceiros/Proprio     |

### 2. Indicadores de liquidez

```r
# Individuais
liquidez_corrente(ag)   # AC / PC
liquidez_seca(ag)       # (AC - Estoques) / PC
liquidez_imediata(ag)   # Caixa / PC
liquidez_geral(ag)      # (AC + RLP) / (PC + PNC)

# Todos de uma vez
indicadores_liquidez(ag)
# $LC, $LS, $LI, $LG
```

### 3. Estrutura de capital / endividamento

```r
endividamento_geral(ag)             # ET / AT
composicao_endividamento(ag)        # PC / ET
imobilizacao_pl(ag)                 # (Imob + Intang) / PL
imobilizacao_rnc(ag)                # (Imob + Intang) / (PL + PNC)
participacao_capital_terceiros(ag)  # ET / PL

indicadores_endividamento(ag)
# $EG, $CE, $IPL, $IRNC, $PCT
```

### 4. Rentabilidade

```r
margem_bruta(ag)        # LB / RL
margem_operacional(ag)  # LAJIR / RL
margem_liquida(ag)      # LL / RL
roa(ag)                 # LL / AT
roe(ag)                 # LL / PL
giro_ativo(ag)          # RL / AT

indicadores_rentabilidade(ag)
# $MB, $MO, $ML, $ROA, $ROE, $GA
```

### 5. Decomposicao DuPont

```r
d <- dupont(ag)
# d$ML            — Margem liquida
# d$GA            — Giro do ativo
# d$multiplicador — AT / PL (alavancagem)
# d$ROE_dupont    — ML x GA x multiplicador (= ROE)
```

### 6. Cobertura de juros

```r
cobertura_juros(ag)  # LAJIR / |Despesas financeiras|
```

### 7. Analise vertical e horizontal

```r
# Analise vertical — participacao de cada conta sobre uma base
analise_vertical(ag, base = "ATIVO_TOTAL")

# Analise horizontal — numeros-indice entre periodos
analise_horizontal(
  list("2022" = ag22, "2023" = ag23, "2024" = ag24),
  contas = c("ATIVO_TOTAL", "RECEITA_LIQUIDA")
)
```

### 8. Modelo Fleuriet — capital de giro

Reclassifica o balanco em circulante financeiro e operacional e calcula CDG (CCL), NCG (NIG), ST (SD), NTFP e termometro financeiro.

```r
# Reclassificacao do balanco
r <- reclassificar_fleuriet(ag)
# r$ACF, r$ACO, r$PCF, r$PCO, r$ANC, r$PNC_PL

# Modelo completo com tipologia
f <- fleuriet(ag)
# f$CDG, f$NCG, f$ST, f$NTFP, f$TF
# f$tipo ("I" a "VI"), f$situacao ("Excelente" a "Alto risco")
```

**Tipologia de Marques e Braga (1995):**

| Tipo | CDG | NCG | ST | Situacao       |
|:----:|:---:|:---:|:--:|:---------------|
| I    |  +  |  -  |  + | Excelente      |
| II   |  +  |  +  |  + | Solida         |
| III  |  +  |  +  |  - | Insatisfatoria |
| IV   |  -  |  +  |  - | Pessima        |
| V    |  -  |  -  |  - | Muito ruim     |
| VI   |  -  |  -  |  + | Alto risco     |

```r
# Efeito tesoura — detecta NCG crescendo acima do CDG
efeito_tesoura(
  cdg = c(100, 110, 115),
  ncg = c(90, 120, 160),
  anos = 2022:2024
)
```

### 9. Prazos medios e ciclos

```r
p <- prazos_medios(ag, ag_anterior)
# p$PMRV  — Prazo Medio de Recebimento de Vendas
# p$PMRE  — Prazo Medio de Renovacao de Estoques
# p$PMPC  — Prazo Medio de Pagamento de Compras
# p$ciclo_operacional   — PMRV + PMRE
# p$ciclo_financeiro    — Ciclo operacional - PMPC
```

### 10. Alavancagem

```r
a <- alavancagem(ag_atual, ag_anterior)
# a$GAO — Grau de Alavancagem Operacional
# a$GAF — Grau de Alavancagem Financeira
# a$GAT — Grau de Alavancagem Total (= GAO x GAF)
```

### 11. Modelos de previsao de insolvencia

> **Nota:** Estes modelos foram estimados para industria e comercio. Em empresas de concessao (intangivel elevado, PL negativo), os escores devem ser interpretados com cautela.

```r
# Kanitz — termometro de insolvencia
k <- kanitz(ag)
# k$escore, k$faixa ("Solvente" / "Penumbra" / "Insolvente")
classificar_kanitz(-2)  # "Penumbra"

# Altman — versao brasileira (Z1)
altman(ag)    # Z < 0 → insolvente

# Elizabetsky
elizabetsky(ag)  # Z < 0.5 → insolvente

# Matias
matias(ag)    # Z < 0 → insolvente
```

### 12. Indicadores do investidor (Marion, 2013)

```r
i <- indicadores_investidor(ag, n_acoes = 1000000, preco = 25.50, dividendos = 500000)
# i$VPA       — Valor Patrimonial por Acao (PL / n acoes)
# i$LPA       — Lucro por Acao (LL / n acoes)
# i$PL_indice — Preco/Lucro (preco / LPA)
# i$DPA       — Dividendo por Acao
```

### 13. Cotacoes da B3 (Yahoo Finance)

Busca cotacoes em tempo real de acoes da B3, sem dependencias externas.

```r
# Cotacao atual de uma acao
cot <- cotacao_b3("PETR4")
# cot$preco, cot$variacao_pct, cot$volume, cot$nome

# Historico de cotacoes
hist <- historico_b3("VALE3", periodo = "1y", intervalo = "1d")
# data.frame: data, abertura, maxima, minima, fechamento, volume

# Varias acoes de uma vez
cotacoes_b3(c("PETR4", "VALE3", "ITUB4", "BBDC4"))

# Indicadores do investidor com cotacao automatica
i <- indicadores_investidor_b3(ag, ticker = "PETR4", n_acoes = 1000000)
# Busca preco da B3 automaticamente e calcula VPA, LPA, P/L, DPA
# Inclui: dividend_yield, cotacao, variacao_dia
```

## Utilitarios

```r
# Normalizar texto (remove acentos, lowercase)
normalizar_texto("Ativo Não Circulante")  # "ativo nao circulante"

# Converter numero em formato brasileiro
conv_br_numeric("1.234,56")  # 1234.56

# Divisao segura (sem erro em divisao por zero)
safe_div(100, 0)  # NA
safe_div(100, 0, default = 0)  # 0

# Classificar descricoes de contas
classificar_conta(c("Ativo Total", "Receita líquida"), categorias_bp)

# Formatar valores monetarios
formatar_valor(1500000)  # "1.5M"
```

## Estrutura do pacote

```
ADCnovosindicadores/
├── R/
│   ├── utils.R               # normalizar_texto, conv_br_numeric, safe_div
│   ├── importar_contabil.R   # categorias CVM, importar_contabil_dfp/itr
│   ├── indicadores.R         # todos os indicadores financeiros (classicos + avancados)
│   ├── cotacao_b3.R          # cotacoes da B3 via Yahoo Finance
│   └── importar_cvm_online.R # importacao online da CVM + analise completa
├── tests/testthat/
│   ├── test-utils.R          # 4 testes
│   ├── test-importar-contabil.R  # 8 testes
│   ├── test-indicadores.R    # 31 testes
│   └── test-cotacao-b3.R     # 8 testes
├── DESCRIPTION
├── NAMESPACE
└── LICENSE
```

## Testes

```r
devtools::test()
# 51 testes, todos passando
```

Os testes reproduzem os exemplos numericos das Aulas 06 e 07 do curso de Analise das Demonstracoes Contabeis.

## Colaboracao

Este pacote foi desenvolvido por **Sofia Santos Ferreira** como parte de projeto de bolsa de pesquisa na area de Contabilidade Financeira, com colaboracao do **Claude** (Anthropic) na implementacao de codigo, correcao de bugs e testes. Em particular, o modulo de importacao online de dados da CVM (`importar_cvm_online`) foi desenvolvido em conjunto (incluindo a resolucao de problemas de compatibilidade entre arquivos CSV da CVM e a correcao do algoritmo de classificacao de contas contabeis).

## Fontes
- **Aulas da cadeira de Análise de demonstrações contábeis**
- **Amorim, K.** — CCL/NIG/SD, Alavancagem, Indicadores combinados
- **Marion, J. C. (2013)** — Analise das Demonstracoes Contabeis
- **Marques, J. A. V. C.; Braga, R. (1995)** — Tipologia Fleuriet (tipos I–VI)
- **Assaf Neto, A.; Tibúrcio, C. A. (2011)** — Analise Financeira
- **ADContábil**

## Licenca

MIT
