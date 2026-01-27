# Análise Quantitativa de Investimentos: FIIs

Este projeto utiliza **Estatística** para transformar dados brutos da B3 em inteligência financeira. O objetivo é analisar a performance, o risco e a tendência de Fundos Imobiliários, Fundos de Infraestrutura (desenvolvimento) e Fundos Agro.

## Como Funciona o Pipeline

1. **Extração:** O script em R conecta-se à API do Yahoo Finance via pacote `tidyquant`.
2. **Transformação:** Cálculo de log-retornos para normalização estatística.
* Filtragem de dados inconsistentes.
* Cálculo de métricas de eficiência (Risco vs. Retorno).


3. **Carga:** Os dados processados são carregados diretamente no Power BI através do conector nativo de Script R.

## Principais Indicadores Calculados

* **Retorno Acumulado:** Evolução percentual base 100 desde o início do período.
* **Volatilidade:** Medida de dispersão (desvio padrão) que indica o risco do ativo.
* **Índice de Sharpe:** Relação retorno/risco. Quanto maior, melhor a gestão do fundo.
* **Tendência (SMA 50):** Identificação visual de ciclos de alta e baixa.
