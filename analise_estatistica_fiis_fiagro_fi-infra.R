# 1. Instalar e carregar pacotes
if(!require(tidyquant)) install.packages("tidyquant")
library(tidyquant)
library(ggplot2)
library(dplyr)

# 2. Definir os tickers e o período
tickers <- c("HGBS11.SA", "XPML11.SA", "VISC11.SA", "IFIX.SA") 

dados_fii <- tq_get(tickers,
                    from = "2023-01-01",
                    to = Sys.Date(),
                    get = "stock.prices")

# Calcular Retorno Acumulado 
retornos_acumulados <- dados_fii %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "daily",
               type = "log",
               col_rename = "retorno") %>%
  mutate(retorno_acum = cumsum(retorno))

# Visualização de Tendência (Média Móvel)
ggplot(dados_fii, aes(x = date, y = adjusted, color = symbol)) +
  geom_line(alpha = 0.5) +
  geom_ma(ma_fun = SMA, n = 50, linetype = "solid", size = 1) + 
  facet_wrap(~symbol, scales = "free_y") +
  labs(title = "Análise de Tendência: Preço vs Média Móvel (50 dias)",
       subtitle = "Se o preço está acima da linha sólida, a tendência é de alta",
       x = "Data", y = "Preço Ajustado", color = "Ativo") +
  theme_minimal()

# Gráfico de Comparação de Performance
ggplot(retornos_acumulados, aes(x = date, y = retorno_acum, color = symbol)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Comparativo de Retorno Acumulado",
       subtitle = "Base 100: Evolução percentual desde o início de 2023",
       x = "Data", y = "Retorno Acumulado") +
  theme_minimal()

# Coleta e Limpeza 
dados_limpos <- tq_get(tickers, from = "2023-01-01", to = "2025-12-01") %>%
  filter(!is.na(adjusted) & adjusted > 0) # Remove erros de base de dados

# Cálculo de Métricas Estatísticas (Sharpe e Volatilidade)
metricas <- dados_limpos %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "daily", col_rename = "ret") %>%
  summarise(
    Retorno_Anual = mean(ret) * 252,
    Volatilidade_Anual = sd(ret) * sqrt(252),
    Sharpe = Retorno_Anual / Volatilidade_Anual
  )

# Visualização de Risco vs Retorno 
ggplot(metricas, aes(x = Volatilidade_Anual, y = Retorno_Anual, label = symbol)) +
  geom_point(aes(size = Sharpe, color = symbol)) +
  geom_text(vjust = -1) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Eficiência dos FIIs: Risco vs Retorno",
       x = "Risco (Volatilidade Anualizada)",
       y = "Retorno Esperado (Anualizado)",
       size = "Índice de Sharpe") +
  theme_minimal()