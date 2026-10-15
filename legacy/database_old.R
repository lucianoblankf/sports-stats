
#1.Pacotes Inciais ----

#install.packages(c("tidyverse", "moments", "nortest", "fitdistrplus", "knitr"))
library(tidyverse)
library(moments)        
library(nortest)        
library(fitdistrplus)   
library(knitr)          

dir.create("dados",   showWarnings = FALSE)
dir.create("tabelas", showWarnings = FALSE)
dir.create("figuras", showWarnings = FALSE)

#2.Baixa a base ----

url <- "https://raw.githubusercontent.com/vibedatascience/understat_players_aggregated/main/understat_players_aggregated_2014_td.csv"

download.file(url, "dados/understat.csv", mode = "wb")

df <- read_csv("dados/understat.csv")

glimpse(df)      
summary(df)

#3.1Tratamento_1 ----

count(df, league)   
count(df, season)

pos_player <- "F"      #Escolhe filtro de posição    
goals_min_1 <- 10      #Escolhe Minimo de gols

atacantes_10 <- df |> 
  dplyr::filter(goals >= goals_min_1) |>                                
  dplyr::filter(primary_position == pos_player) |> 
  dplyr::mutate(
    goals_xG  = goals / xG,                       
    npg_npxG  = npg / npxG,                       
    goals_minus_xG = goals - xG,
    shots_90  = shots / time * 90,
    xG_90     = xG / time * 90
  )

nrow(atacantes_10)
count(atacantes_10, league)
count(atacantes_10, season)
summary(atacantes_10$goals_xG)

write_xlsx(atacantes_10, "atacantes_10.xlsx")


#3.2Tratamento 2 ----

pos_player <- "F"        
goals_min_2 <- 20      

atacantes_20 <- df |> 
  dplyr::filter(goals >= goals_min_2) |>                                
  dplyr::filter(primary_position == pos_player) |> 
  dplyr::mutate(
    goals_xG  = goals / xG,                       
    npg_npxG  = npg / npxG,                       
    goals_minus_xG = goals - xG,
    shots_90  = shots / time * 90,
    xG_90     = xG / time * 90
  )

nrow(atacantes_20)
count(atacantes_20, league)
count(atacantes_20, season)
summary(atacantes_20$goals_xG)

write_xlsx(atacantes_20, "atacantes_20.xlsx")

#4.Faz saidas (Trocar a base quando quiser para 10 ou 20) ---- 

base <- atacantes_10;  tag <- "g10"
# base <- atacantes_20;  tag <- "g20"

base <- base |> mutate(grupo = if_else(goals >= 20, "20 ou mais gols", "10 a 19 gols"))

# funcoes auxiliares para salvar saidas
salva_tab <- function(tab, nome, legenda, tab_tex = tab) {
  write_csv(tab, paste0("tabelas/", nome, "_", tag, ".csv"))
  tab_tex |>
    mutate(across(where(is.numeric), \(x) round(x, 3))) |>
    kable(format = "latex", booktabs = TRUE, caption = legenda,
          label = paste0(nome, "_", tag)) |>
    as.character() |>
    writeLines(paste0("tabelas/", nome, "_", tag, ".tex"))
}

salva_fig <- function(p, nome, w = 7, h = 4.5) {
  ggsave(paste0("figuras/", nome, "_", tag, ".pdf"), p, width = w, height = h)
  ggsave(paste0("figuras/", nome, "_", tag, ".png"), p, width = w, height = h, dpi = 300)
}

theme_set(theme_minimal(base_size = 12))

#5.Estatisticas descritivas ----

vars <- c("goals", "xG", "goals_xG", "npg_npxG", "goals_minus_xG", "shots_90", "xG_90")

desc_fun <- function(x) {
  q   <- as.numeric(quantile(x, c(.25, .75)))
  iqr <- q[2] - q[1]
  tibble(
    n          = length(x),
    media      = mean(x),
    mediana    = median(x),
    dp         = sd(x),
    cv         = sd(x) / mean(x),         
    min        = min(x),
    q1         = q[1],
    q3         = q[2],
    max        = max(x),
    iqr        = iqr,
    assimetria = skewness(x),
    curtose_ex = kurtosis(x) - 3,       
    outliers   = sum(x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr)
  )
}

tab_desc <- map_dfr(vars, \(v) bind_cols(variavel = v, desc_fun(base[[v]])))
print(tab_desc, width = Inf)
salva_tab(tab_desc, "desc", "Estatisticas descritivas")

# composicao da amostra
tab_liga <- base |> count(league, name = "n") |> mutate(pct = round(100 * n / sum(n), 1))
tab_temp <- base |> count(season, name = "n")
salva_tab(tab_liga, "n_liga", "Observacoes por liga")
salva_tab(tab_temp, "n_temporada", "Observacoes por temporada")

# melhores e piores finalizadores (gols/xG)
extremos <- bind_rows(
  base |> arrange(desc(goals_xG)) |> slice_head(n = 10) |> mutate(tipo = "Maiores"),
  base |> arrange(goals_xG)       |> slice_head(n = 10) |> mutate(tipo = "Menores")
) |>
  dplyr::select(tipo, player_name, season, team_title, league, goals, xG, goals_xG)

salva_tab(extremos, "extremos", "Maiores e menores razoes gols/xG")

# por liga (media e mediana de gols/xG)
tab_por_liga <- base |>
  group_by(league) |>
  summarise(n = n(), media = mean(goals_xG), mediana = median(goals_xG),
            dp = sd(goals_xG), .groups = "drop")
salva_tab(tab_por_liga, "gols_xg_liga", "Gols/xG por liga")

#6.Figuras ----

media_gxg   <- mean(base$goals_xG)
mediana_gxg <- median(base$goals_xG)
dp_gxg      <- sd(base$goals_xG)

#histograma + densidade + normal ajustada
p_hist <- ggplot(base, aes(goals_xG)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "grey80", color = "white") +
  geom_density(linewidth = 0.8) +
  stat_function(fun = dnorm, args = list(mean = media_gxg, sd = dp_gxg), linetype = "dashed") +
  geom_vline(xintercept = 1, color = "red") +
  geom_vline(xintercept = media_gxg, linetype = "dotted") +
  geom_vline(xintercept = mediana_gxg, linetype = "dotdash") +
  labs(x = "Gols / xG", y = "Densidade",
       subtitle = "Vermelha: razao = 1 | pontilhada: media | ponto-traco: mediana | tracejada: normal ajustada")
salva_fig(p_hist, "hist_goals_xG")

#QQ-plot
p_qq <- ggplot(base, aes(sample = goals_xG)) +
  stat_qq(alpha = 0.5) + stat_qq_line(color = "red") +
  labs(x = "Quantis teoricos (normal)", y = "Quantis amostrais")
salva_fig(p_qq, "qq_goals_xG")

#boxplot por liga
p_liga <- ggplot(base, aes(league, goals_xG)) +
  geom_boxplot(fill = "grey90") +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  labs(x = NULL, y = "Gols / xG")
salva_fig(p_liga, "box_liga")

#boxplot por temporada
p_temp <- ggplot(base, aes(season, goals_xG)) +
  geom_boxplot(fill = "grey90") +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  labs(x = NULL, y = "Gols / xG") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
salva_fig(p_temp, "box_temporada")

#dispersao xG x gols (linha de 45 graus = finalizacao "esperada")
p_disp <- ggplot(base, aes(xG, goals)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(x = "xG na temporada", y = "Gols na temporada")
salva_fig(p_disp, "dispersao_xG_gols")

#7.Teste de Normalidade ----

norm_test <- function(x, nome) {
  tibble(
    variavel      = nome,
    n             = length(x),
    shapiro_p     = if (length(x) <= 5000) shapiro.test(x)$p.value else NA_real_,
    anderson_p    = ad.test(x)$p.value,
    lilliefors_p  = lillie.test(x)$p.value,
    jarque_bera_p = jarque.test(x)$p.value
  )
}
tab_norm <- map_dfr(vars, \(v) norm_test(base[[v]], v))
print(tab_norm, width = Inf)
salva_tab(tab_norm, "normalidade", "Testes de normalidade (p-valores)",
          tab_tex = tab_norm |> mutate(across(ends_with("_p"), \(p) format.pval(p, digits = 3, eps = 1e-4))))

#8.Distribuicao ----

x <- base$goals_xG

ajustes <- list(
  normal    = fitdist(x, "norm"),
  lognormal = fitdist(x, "lnorm"),
  gamma     = fitdist(x, "gamma"),
  weibull   = fitdist(x, "weibull")
)
comp <- gofstat(ajustes, fitnames = names(ajustes))

tab_ajuste <- tibble(
  distribuicao = names(ajustes),
  aic = map_dbl(ajustes, "aic"),
  bic = map_dbl(ajustes, "bic"),
  ks  = unname(comp$ks),
  ad  = unname(comp$ad)
) |> arrange(aic)
print(tab_ajuste)
salva_tab(tab_ajuste, "ajuste_dist", "Comparacao de distribuicoes ajustadas a gols/xG")

tab_param <- map_dfr(ajustes, \(a) tibble(parametro = names(a$estimate),
                                          valor = unname(a$estimate)),
                     .id = "distribuicao")
salva_tab(tab_param, "parametros_dist", "Parametros estimados")

pdf(paste0("figuras/ajuste_", tag, ".pdf"), width = 9, height = 7)
par(mfrow = c(2, 2))
denscomp(ajustes, legendtext = names(ajustes))
qqcomp(ajustes,   legendtext = names(ajustes))
cdfcomp(ajustes,  legendtext = names(ajustes))
ppcomp(ajustes,   legendtext = names(ajustes))
dev.off()

pdf(paste0("figuras/cullen_frey_", tag, ".pdf"), width = 6, height = 5)
descdist(x, boot = 500)
dev.off()

#9.Teste de hipotese ----

linha <- function(h0, h1, res) {
  tibble(H0 = h0, H1 = h1, teste = res$method,
         estatistica = unname(res$statistic), p_valor = res$p.value)
}

tab_testes <- bind_rows(
  # os atacantes finalizam, em media, exatamente como o xG preve?
  linha("Media de gols/xG igual a 1", "Media diferente de 1",
        t.test(base$goals_xG, mu = 1)),
  linha("Mediana de gols/xG igual a 1", "Mediana diferente de 1",
        wilcox.test(base$goals_xG, mu = 1)),
  linha("Media de (gols menos xG) igual a 0", "Media diferente de 0",
        t.test(base$goals, base$xG, paired = TRUE)),
  linha("Mediana de (gols menos xG) igual a 0", "Mediana diferente de 0",
        wilcox.test(base$goals, base$xG, paired = TRUE)),
  linha("Proporcao de atacantes com gols acima do xG igual a 0,5", "Proporcao diferente de 0,5",
        binom.test(sum(base$goals > base$xG), nrow(base), p = 0.5)),
  # relacao entre xG e gols
  linha("Correlacao de Pearson entre xG e gols igual a 0", "Correlacao diferente de 0",
        cor.test(base$xG, base$goals)),
  linha("Correlacao de Spearman entre xG e gols igual a 0", "Correlacao diferente de 0",
        cor.test(base$xG, base$goals, method = "spearman", exact = FALSE)),
  # diferencas entre ligas e temporadas
  linha("Medias de gols/xG iguais entre as ligas", "Ao menos uma liga difere",
        oneway.test(goals_xG ~ league, data = base)),
  linha("Distribuicao de gols/xG igual entre as ligas", "Ao menos uma liga difere",
        kruskal.test(goals_xG ~ league, data = base)),
  linha("Variancias de gols/xG iguais entre as ligas", "Ao menos uma liga difere",
        fligner.test(goals_xG ~ league, data = base)),
  linha("Distribuicao de gols/xG igual entre as temporadas", "Ao menos uma temporada difere",
        kruskal.test(goals_xG ~ season, data = base))
)

# inclinacao da regressao gols ~ xG igual a 1 (xG "calibrado")
m  <- lm(goals ~ xG, data = base)
cf <- coef(summary(m))
t_b1 <- (cf["xG", "Estimate"] - 1) / cf["xG", "Std. Error"]
p_b1 <- 2 * pt(-abs(t_b1), df = df.residual(m))
tab_testes <- bind_rows(tab_testes, tibble(
  H0 = "Coeficiente de xG na regressao de gols igual a 1", H1 = "Coeficiente diferente de 1",
  teste = "Teste t para o coeficiente", estatistica = unname(t_b1), p_valor = unname(p_b1)))

# 10-19 gols vs 20+ gols (so faz sentido na base g10)
if (n_distinct(base$grupo) == 2) {
  tab_testes <- bind_rows(
    tab_testes,
    linha("Media de gols/xG igual entre os grupos 10-19 e 20+ gols", "Medias diferentes",
          t.test(goals_xG ~ grupo, data = base)),
    linha("Distribuicao de gols/xG igual entre os grupos 10-19 e 20+ gols", "Distribuicoes diferentes",
          wilcox.test(goals_xG ~ grupo, data = base))
  )
}

# sensibilidade: um valor por jogador (media das temporadas), para reduzir dependencia
por_jog <- base |> group_by(id) |> summarise(goals_xG = mean(goals_xG), .groups = "drop")
tab_testes <- bind_rows(tab_testes,
                        linha("Media de gols/xG igual a 1 (um valor por jogador)", "Media diferente de 1",
                              t.test(por_jog$goals_xG, mu = 1)))

print(tab_testes, n = Inf, width = Inf)
salva_tab(tab_testes, "testes", "Testes de hipoteses",
          tab_tex = tab_testes |> mutate(p_valor = format.pval(p_valor, digits = 3, eps = 1e-4)))

# comparacoes par a par entre ligas (Wilcoxon, ajuste de Holm)
pw <- pairwise.wilcox.test(base$goals_xG, base$league, p.adjust.method = "holm")
tab_pw <- as.data.frame(pw$p.value) |> rownames_to_column("liga")
salva_tab(tab_pw, "pareado_ligas", "P-valores pareados entre ligas (Wilcoxon, Holm)")

#10.Intervalo de confiança ----

set.seed(123)
b_media   <- replicate(5000, mean(sample(x, replace = TRUE)))
b_mediana <- replicate(5000, median(sample(x, replace = TRUE)))

tab_ic <- tibble(
  estatistica = c("media", "mediana"),
  estimativa  = c(mean(x), median(x)),
  ic_inf      = unname(c(quantile(b_media, .025), quantile(b_mediana, .025))),
  ic_sup      = unname(c(quantile(b_media, .975), quantile(b_mediana, .975)))
)
print(tab_ic)
salva_tab(tab_ic, "ic_bootstrap", "Intervalos de confianca bootstrap (95 por cento) para gols/xG")

message("Pronto. Veja as pastas tabelas/ e figuras/ (sufixo: ", tag, ").")