# ==============================================================================
# AUTOR: JÔNATAS FERNANDES SILVA
# DATA: 09/02/2026
# LICENÇA: MIT
# ==============================================================================

# 1. Carregar Pacotes
pacotes <- c("tidyquant", "ggplot2", "dplyr", "forecast", "scales", "purrr", "tidyr", "patchwork")
if(!all(pacotes %in% installed.packages())) install.packages(pacotes)
lapply(pacotes, library, character.only = TRUE)

# 2. Configuração 
tickers <- c("HGBS11.SA", "HSML11.SA", "MULT3.SA", "IFIX.SA")
data_inicio <- "2020-01-01" 
data_fim    <- Sys.Date()

# 3. Coleta de Dados 
dados_brutos <- tq_get(tickers, from = data_inicio, to = data_fim) %>%
  filter(!is.na(adjusted) & adjusted > 0)

dividendos <- tryCatch({
  tq_get(tickers, get = "dividends", from = data_inicio)
}, error = function(e) { NULL })

# 4. Processamento de Médias Móveis e Total Return
if (is.null(dividendos) || nrow(dividendos) == 0) {
  dados_completos <- dados_brutos %>% mutate(value = 0)
} else {
  dados_completos <- dados_brutos %>%
    left_join(dividendos, by = c("symbol", "date")) %>%
    mutate(value = coalesce(value, 0))
}

dados_analise <- dados_completos %>%
  group_by(symbol) %>%
  mutate(
    ret_log = log(adjusted / lag(adjusted)),
    # Total Return Index (Base 100) para cálculo de CAGR
    tri = (adjusted + cumsum(value)) / first(adjusted),
    retorno_total_acum = tri - 1,
    # Médias de Curto (50d) e Longo Prazo (200d) -> exemplos
    ma50 = SMA(adjusted, n = 50),
    ma200 = SMA(adjusted, n = 200),
    # Drawdown Histórico
    peak = cummax(tri),
    drawdown = (tri / peak) - 1
  ) %>%
  ungroup()

# 5. Cálculo de Métricas de Longo Prazo
tabela_longo_prazo <- dados_analise %>%
  group_by(symbol) %>%
  summarise(
    Dias_Uteis = n(),
    # CAGR: Taxa de Crescimento Anual Composta
    CAGR = (last(tri)^(252/n())) - 1,
    # Yield on Cost: Total de dividendos / Preço inicial
    Yield_on_Cost_Total = sum(value) / first(adjusted),
    # Volatilidade e Sharpe
    Vol_Anual = sd(ret_log, na.rm = TRUE) * sqrt(252),
    Sharpe = (mean(ret_log, na.rm = TRUE) * 252) / Vol_Anual,
    Sortino = (mean(ret_log, na.rm = TRUE) * 252) / (sd(ret_log[ret_log < 0], na.rm = TRUE) * sqrt(252)),
    # Resiliência
    Max_Drawdown = min(drawdown),
    Recuperacao_Atual = last(drawdown)
  ) %>%
  arrange(desc(CAGR))

# 6. Automação de Previsão ARIMA
previsoes <- dados_analise %>%
  group_split(symbol) %>%
  map_df(function(df) {
    if(nrow(df) < 252) return(NULL)
    ts_ativo <- ts(df$adjusted, frequency = 252)
    modelo <- auto.arima(ts_ativo)
    prev <- forecast(modelo, h = 60) # Previsão estendida para 60 dias
    data.frame(
      date = seq(max(df$date) + 1, by = "day", length.out = 60),
      adjusted = as.numeric(prev$mean),
      lower_95 = as.numeric(prev$lower[,2]),
      upper_95 = as.numeric(prev$upper[,2]),
      symbol = unique(df$symbol)
    )
  })

# 7. Visualizações de Longo Prazo

# G1: Performance do Patrimônio (Total Return)
p1 <- ggplot(dados_analise, aes(x = date, y = retorno_total_acum, color = symbol)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = percent) +
  labs(title = "Evolução do Patrimônio (Total Return)", 
       subtitle = "Cota + Reinvestimento de Dividendos", x = NULL, y = "Retorno Acumulado") +
  theme_minimal()

# G2: Drawdown 
p2 <- ggplot(dados_analise, aes(x = date, y = drawdown, fill = symbol)) +
  geom_area(alpha = 0.4) +
  scale_y_continuous(labels = percent) +
  facet_wrap(~symbol) +
  labs(title = "Drawdown Histórico", 
       subtitle = "Profundidade das quedas em relação ao topo histórico", x = NULL, y = "Queda (%)") +
  theme_light() + theme(legend.position = "none")

# G3: Tendência Estrutural (MA200) e Previsão
p3 <- ggplot() +
  geom_line(data = dados_analise %>% filter(date > max(date) - 365), 
            aes(x = date, y = adjusted), color = "grey20") +
  geom_line(data = dados_analise %>% filter(date > max(date) - 365), 
            aes(x = date, y = ma200), color = "darkblue", size = 1) +
  geom_line(data = previsoes, aes(x = date, y = adjusted), color = "red", linetype = "dashed") +
  facet_wrap(~symbol, scales = "free_y") +
  labs(title = "Ciclos de Longo Prazo e Previsão", 
       subtitle = "Azul: Tendência | Vermelho: Projeção 60d", x = NULL, y = "Preço (R$)") +
  theme_bw()

# Exibição
print(tabela_longo_prazo)
(p1 / p2) 
print(p3)
