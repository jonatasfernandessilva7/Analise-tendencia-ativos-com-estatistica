# ==============================================================================
# AUTOR: JÔNATAS FERNANDES SILVA
# DATA: 09/02/2026
# LICENÇA: MIT
# ==============================================================================

# 1. Carregar Pacotes
pacotes <- c("tidyquant", "ggplot2", "dplyr", "forecast", "scales", "purrr", "tidyr")
if(!all(pacotes %in% installed.packages())) install.packages(pacotes)
lapply(pacotes, library, character.only = TRUE)

# 2. Configuração de Ativos
tickers <- c("HGBS11.SA", "HSML11.SA", "MULT3.SA", "IFIX.SA")
data_inicio <- "2019-01-01"
data_fim    <- "2025-12-31"

# 3. Coleta de Dados
dados_brutos <- tq_get(tickers, from = data_inicio, to = data_fim) %>%
  filter(!is.na(adjusted) & adjusted > 0)

# Coleta de dividendos com tratamento de erro
dividendos <- tryCatch({
  tq_get(tickers, get = "dividends", from = data_inicio)
}, error = function(e) { NULL })

# 4. Processamento e Tratamento do Erro de Join
if (is.null(dividendos) || nrow(dividendos) == 0) {
  # Se não houver dividendos, cria coluna de valor zero
  dados_processados <- dados_brutos %>% mutate(value = 0)
} else {
  # Se houver, une os dados e preenche dias sem dividendos com 0
  dados_processados <- dados_brutos %>%
    left_join(dividendos, by = c("symbol", "date")) %>%
    mutate(value = coalesce(value, 0))
}

# 5. Cálculo de Métricas de Performance e Risco
dados_analise <- dados_processados %>%
  group_by(symbol) %>%
  mutate(
    ret_log = log(adjusted / lag(adjusted)),
    # Total Return: Cota + Dividendos Acumulados
    retorno_total_acum = (adjusted + cumsum(value)) / first(adjusted) - 1,
    ma50 = SMA(adjusted, n = 50)
  ) %>%
  ungroup()

tabela_estatistica <- dados_analise %>%
  group_by(symbol) %>%
  summarise(
    Retorno_Anual = mean(ret_log, na.rm = TRUE) * 252,
    Volatilidade  = sd(ret_log, na.rm = TRUE) * sqrt(252),
    Sharpe        = Retorno_Anual / Volatilidade,
    # Sortino: Foca apenas na volatilidade negativa
    Sortino       = Retorno_Anual / (sd(ret_log[ret_log < 0], na.rm = TRUE) * sqrt(252)),
    # VaR 95%: Pior perda diária esperada com 95% de confiança
    VaR_95        = quantile(ret_log, 0.05, na.rm = TRUE),
    Max_Drawdown  = max(1 - (adjusted / cummax(adjusted)))
  ) %>%
  arrange(desc(Sharpe))

# 6. Automação de Previsão ARIMA 
previsoes_totais <- dados_analise %>%
  group_split(symbol) %>%
  map_df(function(df) {
    # precisa de pelo menos 2 períodos para o ARIMA
    if(nrow(df) < 30) return(NULL) 
    
    ts_ativo <- ts(df$adjusted, frequency = 252)
    modelo <- auto.arima(ts_ativo)
    prev <- forecast(modelo, h = 30)
    
    data.frame(
      date = seq(max(df$date) + 1, by = "day", length.out = 30),
      adjusted = as.numeric(prev$mean),
      lower_80 = as.numeric(prev$lower[,1]),
      upper_80 = as.numeric(prev$upper[,1]),
      symbol = unique(df$symbol)
    )
  })

# 7. Visualizações

# Graf 1: Matriz de Eficiência (Sortino vs Volatilidade)
p1 <- ggplot(tabela_estatistica, aes(x = Volatilidade, y = Retorno_Anual, label = symbol)) +
  geom_point(aes(size = Sortino, color = symbol)) +
  geom_text(vjust = -1.5, size = 3) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(title = "Eficiência de Ativos: Risco vs Retorno",
       subtitle = "O Índice de Sortino avalia o retorno pela volatilidade negativa",
       x = "Risco (Volatilidade Anual)", y = "Retorno Esperado (Anual)") +
  theme_minimal()

# Graf 2: Painel de Tendência e Previsão
p2 <- ggplot() +
  geom_line(data = dados_analise %>% filter(date > max(date) - 150), 
            aes(x = date, y = adjusted), color = "grey30") +
  geom_line(data = previsoes_totais, aes(x = date, y = adjusted), color = "red", size = 1) +
  geom_ribbon(data = previsoes_totais, aes(x = date, ymin = lower_80, ymax = upper_80), 
              fill = "red", alpha = 0.1) +
  facet_wrap(~symbol, scales = "free_y") +
  labs(title = "Tendência e Projeção (Próximos 30 dias)",
       subtitle = "Área vermelha: Margem de confiança do modelo",
       x = NULL, y = "Preço Ajustado (R$)") +
  theme_bw()

# Resultados
print(tabela_estatistica)
print(p1)
print(p2)
