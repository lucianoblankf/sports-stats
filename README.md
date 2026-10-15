# Metodos Estatisticos: gols vs xG de atacantes

Analise da razao gols/xG de atacantes (Understat, 2014/15 a 2024/25) para a disciplina de Metodos Estatisticos do mestrado.

## Como rodar

Abrir `Metodos_Estatisticos.Rproj` no RStudio e executar `00_run_all.R` (ou cada script na ordem).

| Script | O que faz |
|---|---|
| `01_download_data.R` | baixa a base bruta para `data/raw/` |
| `02_prepare_data.R` | limpa (`janitor`) e cria as bases de atacantes (>= 10 e >= 20 gols) |
| `03_descriptive.R` | descritivas e figuras |
| `04_tests_distribution.R` | normalidade, ajuste de distribuicoes, testes de hipotese, bootstrap |
| `05_selection_bias.R` | corte por gols minimos vs xG minimo + simulacao do vies |
| `06_regression.R` | regressoes com efeitos fixos (`fixest`, tabelas `texreg`) |
| `07_clusters.R` | perfis de finalizacao via k-means |

Funcoes reutilizaveis ficam em `R/`. Saidas em `tabs/` e `figs/`. Codigo antigo em `legacy/`.

## Principais resultados

- Base g10 (n = 1139): media gols/xG = 1,156, mediana 1,122, assimetria 1,17 (reproduz o roteiro).
- Com corte por **xG minimo** (nao enviesa pelo resultado) a media cai para ~0,99 e deixa de ser diferente de 1 (xG >= 5: p = 0,16). O "1,16" vem do vies de selecao.
- Simulacao com gols ~ Poisson(xG), sem nenhuma habilidade: o corte de 10 gols ja produz razao media ~1,22.
- Regressao de calibracao (xG >= 5): coeficiente de xG ~ 1,03, R2 ~ 0,78, estavel com efeitos fixos de liga e temporada.
- Clusters: k = 2 pela silhueta (0,29), separacao fraca; o principal eixo e criacao de jogadas (passes-chave, xA).
