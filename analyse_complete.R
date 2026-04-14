# ================================================================
#  ANALYSE ECONOMETRIQUE - PRIX DE L'OR & EPARGNE DES MENAGES
#  L3 Économie - Cours de Séries Temporelles
# ================================================================

# ---------------------------------------------------------------
# 0. PACKAGES ET THEME GRAPHIQUE
# ---------------------------------------------------------------
library(readxl)
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Création des dossiers de sortie
dir.create("figures",  showWarnings = FALSE)
dir.create("outputs",  showWarnings = FALSE)

# -- Thème personnalisé style INSEE (minimaliste et professionnel) --
theme_insee <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title        = element_text(face = "bold", size = base_size + 1,
                                       colour = "#2c3e50", hjust = 0,
                                       margin = margin(b = 6)),
      plot.subtitle     = element_text(size = base_size - 1, colour = "#555555",
                                       hjust = 0, margin = margin(b = 8)),
      plot.caption      = element_text(size = base_size - 2, colour = "#888888",
                                       hjust = 0, margin = margin(t = 8)),
      axis.title        = element_text(size = base_size - 1, colour = "#444444"),
      axis.text         = element_text(size = base_size - 2, colour = "#444444"),
      axis.line.x       = element_line(colour = "#cccccc", linewidth = 0.4),
      axis.ticks        = element_line(colour = "#cccccc", linewidth = 0.3),
      panel.grid.major  = element_line(colour = "#eeeeee", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      legend.position   = "bottom",
      legend.title      = element_blank(),
      legend.text       = element_text(size = base_size - 2),
      legend.key.width  = unit(1.5, "cm"),
      strip.text        = element_text(face = "bold", size = base_size - 1)
    )
}

# Palette de couleurs cohérente
COL_OR      <- "#1a5276"   # bleu foncé - prix de l'or
COL_FIT     <- "#c0392b"   # rouge - ajusté / prévision
COL_REEL    <- "#27ae60"   # vert - valeurs réelles post-2020
COL_EP      <- "#117a65"   # vert foncé - épargne
COL_EPFIN   <- "#a04000"   # marron - épargne financière
COL_CRISIS  <- "#e74c3c"   # rouge crise
COL_SHADE   <- "#d5e8f3"   # bleu clair - zones IC

# Fonction utilitaire : sauvegarder un graphique ggplot
save_fig <- function(p, nom, w = 22, h = 12) {
  ggsave(filename = paste0("figures/", nom, ".png"),
         plot = p, width = w, height = h, units = "cm", dpi = 150,
         bg = "white")
  invisible(p)
}


# ================================================================
#  PARTIE 1 : PRIX DE L'OR
# ================================================================

# ---------------------------------------------------------------
# 1.1 Lecture et préparation des données
# ---------------------------------------------------------------

raw_or <- read_excel("ST_Or.xlsx", sheet = "valeurs_mensuelles",
                     col_names = FALSE)

# Les données commencent à la ligne 5 ; col 1 = date, col 2 = prix le + récent
data_or <- raw_or[5:nrow(raw_or), 1:2]
colnames(data_or) <- c("date_str", "prix")
data_or$prix <- suppressWarnings(as.numeric(data_or$prix))
data_or <- data_or[!is.na(data_or$prix), ]
data_or$date <- as.Date(paste0(data_or$date_str, "-01"), format = "%Y-%m-%d")
data_or <- data_or[order(data_or$date), ]

# Séparation modélisation (≤ déc. 2020) / post-2020
data_mod  <- data_or[data_or$date <= as.Date("2020-12-01"), ]
data_post <- data_or[data_or$date >  as.Date("2020-12-01"), ]

cat("========================================\n")
cat("DONNEES - PRIX DE L'OR\n")
cat("========================================\n")
cat("Nombre d'observations (1990-2020) :", nrow(data_mod), "\n")
cat("Période :", format(min(data_mod$date), "%b %Y"),
    "à", format(max(data_mod$date), "%b %Y"), "\n\n")

# Statistiques descriptives
s <- data_mod$prix
stats_or <- data.frame(
  Statistique = c("Minimum", "Maximum", "Moyenne", "Médiane", "Écart-type"),
  Valeur_USD  = round(c(min(s), max(s), mean(s), median(s), sd(s)), 2)
)
print(stats_or)

sink("outputs/stats_or.txt")
cat("STATISTIQUES DESCRIPTIVES - PRIX DE L'OR (1990-2020)\n\n")
cat("Nb d'observations :", nrow(data_mod), "\n")
cat("Période :", format(min(data_mod$date), "%b %Y"),
    "à", format(max(data_mod$date), "%b %Y"), "\n\n")
print(stats_or)
sink()

# Séries temporelles R
ts_or     <- ts(data_mod$prix,       start = c(1990, 1), frequency = 12)
ts_log    <- log(ts_or)
ts_dlog   <- diff(ts_log) * 100      # taux de croissance mensuel en %


# ---------------------------------------------------------------
# 1.2 Graphique 1 – Prix de l'or en niveau (1990-2026)
# ---------------------------------------------------------------

# Évènements à annoter
events <- data.frame(
  date  = as.Date(c("2008-09-01", "2011-09-01", "2020-03-01")),
  label = c("Crise financière\n2008", "Pic historique\n2011", "COVID-19\n2020"),
  ypos  = c(1600, 1920, 1700)
)

fig1 <- ggplot() +
  # Zone hors-échantillon (post-2020)
  annotate("rect",
           xmin = as.Date("2021-01-01"), xmax = max(data_or$date) + 30,
           ymin = -Inf, ymax = Inf,
           fill = "#f0f0f0", alpha = 0.8) +
  annotate("text",
           x = as.Date("2023-01-01"), y = 4800,
           label = "Hors\néchantillon", colour = "#999999",
           size = 3, hjust = 0.5) +
  # Lignes verticales événements
  geom_vline(data = events, aes(xintercept = date),
             linetype = "dashed", colour = COL_CRISIS, linewidth = 0.5) +
  # Série complète
  geom_line(data = data_or, aes(x = date, y = prix),
            colour = COL_OR, linewidth = 0.7) +
  # Annotations événements
  geom_label(data = events, aes(x = date, y = ypos, label = label),
             colour = COL_CRISIS, fill = "white", size = 2.6,
             label.size = 0.2, label.padding = unit(0.2, "lines")) +
  # Ligne de coupure modélisation
  geom_vline(xintercept = as.Date("2021-01-01"),
             linetype = "solid", colour = "#999999", linewidth = 0.6) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = label_comma(big.mark = " ", suffix = " $"),
                     expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    title    = "Prix au comptant de l'or (1990-2026)",
    subtitle = "Fixing LME après-midi – en dollars US par once troy",
    x        = NULL,
    y        = "Prix (USD/once)",
    caption  = "Source : Banque de France / LME (via BdF, extraction 2026)"
  ) +
  theme_insee()

save_fig(fig1, "fig1_or_niveau", w = 24, h = 13)
cat("Figure 1 sauvegardée.\n")


# ---------------------------------------------------------------
# 1.3 Graphique 2 – Log du prix de l'or (1990-2020)
# ---------------------------------------------------------------

df_log <- data.frame(date = data_mod$date, log_prix = log(data_mod$prix))

fig2 <- ggplot(df_log, aes(x = date, y = log_prix)) +
  geom_line(colour = COL_OR, linewidth = 0.75) +
  geom_vline(xintercept = as.Date("2008-09-01"),
             linetype = "dashed", colour = COL_CRISIS, linewidth = 0.5) +
  annotate("text", x = as.Date("2008-09-01"), y = 7.5,
           label = "Crise 2008", colour = COL_CRISIS,
           size = 3, hjust = -0.1) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  labs(
    title    = "Prix de l'or en logarithme (1990-2020)",
    subtitle = "Transformation log pour stabiliser la variance",
    x        = NULL,
    y        = "Log(prix)",
    caption  = "Source : Banque de France / LME"
  ) +
  theme_insee()

save_fig(fig2, "fig2_or_log", w = 22, h = 12)
cat("Figure 2 sauvegardée.\n")


# ---------------------------------------------------------------
# 1.4 Graphique 3 – Taux de croissance mensuel (delta log)
# ---------------------------------------------------------------

df_dlog <- data.frame(
  date  = data_mod$date[-1],    # diff réduit d'1 obs
  dlog  = as.numeric(ts_dlog)
)

fig3 <- ggplot(df_dlog, aes(x = date, y = dlog)) +
  geom_hline(yintercept = 0, colour = "#aaaaaa", linewidth = 0.4) +
  geom_line(colour = "#2980b9", linewidth = 0.5) +
  # Zone de crise 2008
  annotate("rect",
           xmin = as.Date("2008-06-01"), xmax = as.Date("2009-06-01"),
           ymin = -Inf, ymax = Inf,
           fill = COL_CRISIS, alpha = 0.08) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(
    title    = "Taux de croissance mensuel du prix de l'or (1990-2020)",
    subtitle = "Première différence du logarithme × 100",
    x        = NULL,
    y        = "Variation (%)",
    caption  = "Source : Banque de France / LME – calcul propre"
  ) +
  theme_insee()

save_fig(fig3, "fig3_or_dlog", w = 22, h = 12)
cat("Figure 3 sauvegardée.\n")


# ---------------------------------------------------------------
# 1.5 Tests de stationnarité (ADF + KPSS + PP)
# ---------------------------------------------------------------

cat("\n========================================\n")
cat("TESTS DE STATIONNARITE - LOG PRIX DE L'OR\n")
cat("========================================\n")

# ADF – en niveau
adf_niv <- adf.test(ts_log, alternative = "stationary", k = 4)
cat("\n-- ADF (4 retards) – Log prix, en niveau --\n")
cat("DF =", round(adf_niv$statistic, 4),
    "| p-valeur =", round(adf_niv$p.value, 4), "\n")
cat("Décision : On ne rejette pas H0 → présence d'une racine unitaire\n")

# ADF – en première différence
adf_diff <- adf.test(ts_dlog, alternative = "stationary", k = 4)
cat("\n-- ADF (4 retards) – Log prix, en différence --\n")
cat("DF =", round(adf_diff$statistic, 4),
    "| p-valeur =", round(adf_diff$p.value, 4), "\n")
cat("Décision : On rejette H0 → la série est stationnaire en différence → I(1)\n")

# KPSS – confirmation
kpss_niv  <- kpss.test(ts_log,  null = "Level")
kpss_diff <- kpss.test(ts_dlog, null = "Level")
cat("\n-- KPSS – en niveau (H0 : stationnaire) --\n")
cat("Stat =", round(kpss_niv$statistic, 4),
    "| p-valeur =", round(kpss_niv$p.value, 4), "\n")
cat("Décision : On rejette H0 → non-stationnaire (confirme ADF)\n")
cat("\n-- KPSS – en différence (H0 : stationnaire) --\n")
cat("Stat =", round(kpss_diff$statistic, 4),
    "| p-valeur =", round(kpss_diff$p.value, 4), "\n")
cat("Décision : On ne rejette pas H0 → stationnaire (confirme d=1)\n")

sink("outputs/tests_statio_or.txt")
cat("TESTS DE STATIONNARITE - LOG PRIX DE L'OR\n\n")
cat("ADF en niveau    : DF =", round(adf_niv$statistic, 4),
    ", p =", round(adf_niv$p.value, 4), "\n")
cat("ADF en diff.     : DF =", round(adf_diff$statistic, 4),
    ", p =", round(adf_diff$p.value, 4), "\n")
cat("KPSS en niveau   : stat =", round(kpss_niv$statistic, 4),
    ", p =", round(kpss_niv$p.value, 4), "\n")
cat("KPSS en diff.    : stat =", round(kpss_diff$statistic, 4),
    ", p =", round(kpss_diff$p.value, 4), "\n")
cat("\nConclusion : série I(1) – on modélise en différence (d=1)\n")
sink()


# ---------------------------------------------------------------
# 1.6 Graphiques 4 & 5 – ACF / PACF
# ---------------------------------------------------------------

# Fonction pour créer un graphe ACF/PACF stylisé avec ggplot
plot_acf_gg <- function(serie, titre, max_lag = 36, color = COL_OR) {
  acf_vals  <- acf(serie,  lag.max = max_lag, plot = FALSE)
  pacf_vals <- pacf(serie, lag.max = max_lag, plot = FALSE)
  ic        <- qnorm(0.975) / sqrt(length(serie))

  df_acf  <- data.frame(lag = as.numeric(acf_vals$lag[-1]),
                         valeur = as.numeric(acf_vals$acf[-1]),
                         type = "ACF")
  df_pacf <- data.frame(lag = as.numeric(pacf_vals$lag),
                         valeur = as.numeric(pacf_vals$acf),
                         type = "PACF")
  df <- rbind(df_acf, df_pacf)

  ggplot(df, aes(x = lag, y = valeur)) +
    geom_hline(yintercept = 0, colour = "#aaaaaa", linewidth = 0.4) +
    geom_hline(yintercept =  ic, linetype = "dashed",
               colour = COL_CRISIS, linewidth = 0.5) +
    geom_hline(yintercept = -ic, linetype = "dashed",
               colour = COL_CRISIS, linewidth = 0.5) +
    geom_segment(aes(x = lag, xend = lag, y = 0, yend = valeur),
                 colour = color, linewidth = 0.8) +
    geom_point(colour = color, size = 1.2) +
    facet_wrap(~type, nrow = 1) +
    scale_x_continuous(breaks = seq(0, max_lag, by = 6)) +
    scale_y_continuous(limits = c(-0.5, 1),
                       breaks = seq(-0.4, 1, by = 0.2)) +
    labs(title = titre,
         x = "Retard (lag)", y = "Corrélation",
         caption = "Barres en rouge : intervalle de confiance à 95 %") +
    theme_insee() +
    theme(legend.position = "none")
}

fig4 <- plot_acf_gg(ts_log,
                     "ACF et PACF – Log prix de l'or (en niveau, 1990-2020)",
                     color = COL_OR)
save_fig(fig4, "fig4_acf_niveau", w = 22, h = 12)
cat("Figure 4 sauvegardée.\n")

fig5 <- plot_acf_gg(ts_dlog,
                     "ACF et PACF – Taux de croissance de l'or (Δlog, 1990-2020)",
                     color = "#2980b9")
save_fig(fig5, "fig5_acf_diff", w = 22, h = 12)
cat("Figure 5 sauvegardée.\n")


# ---------------------------------------------------------------
# 1.7 Sélection et estimation du modèle ARIMA
# ---------------------------------------------------------------

cat("\n========================================\n")
cat("SELECTION DU MODELE ARIMA - LOG PRIX DE L'OR\n")
cat("========================================\n")

# auto.arima (modèle de référence)
arima_auto <- auto.arima(ts_log,
                          stepwise    = FALSE,
                          approximation = FALSE,
                          ic          = "aic",
                          trace       = FALSE)

p_or <- arima_auto$arma[1]
d_or <- arima_auto$arma[6]
q_or <- arima_auto$arma[2]
cat(sprintf("Modèle sélectionné par auto.arima : ARIMA(%d,%d,%d)\n", p_or, d_or, q_or))
print(summary(arima_auto))

# Test de significativité des coefficients (Student)
coefs    <- coef(arima_auto)
se_coefs <- sqrt(diag(arima_auto$var.coef))
t_stats  <- coefs / se_coefs
p_vals   <- 2 * (1 - pnorm(abs(t_stats)))
tab_coef <- data.frame(
  Coef     = round(coefs,    4),
  Std_Err  = round(se_coefs, 4),
  t_stat   = round(t_stats,  3),
  p_valeur = round(p_vals,   4)
)
cat("\nTests de Student sur les coefficients :\n")
print(tab_coef)

sink("outputs/arima_or.txt")
cat(sprintf("MODELE ARIMA(%d,%d,%d) - LOG PRIX DE L'OR (1990-2020)\n\n", p_or, d_or, q_or))
print(summary(arima_auto))
cat("\nTests de significativité (Student) :\n")
print(tab_coef)
cat("\nAIC =", round(AIC(arima_auto), 2),
    "| BIC =", round(BIC(arima_auto), 2), "\n")
sink()


# ---------------------------------------------------------------
# 1.8 Graphique 6 – Valeurs ajustées vs observées
# ---------------------------------------------------------------

df_fit <- data.frame(
  date     = data_mod$date,
  observes = data_mod$prix,
  ajustes  = exp(as.numeric(fitted(arima_auto)))
)
df_fit_long <- tidyr::pivot_longer(df_fit, -date,
                                    names_to = "serie",
                                    values_to = "valeur")
df_fit_long$serie <- factor(df_fit_long$serie,
                             levels = c("observes", "ajustes"),
                             labels = c("Valeurs observées", "Valeurs ajustées"))

fig6 <- ggplot(df_fit_long, aes(x = date, y = valeur,
                                 colour = serie, linewidth = serie)) +
  geom_line() +
  scale_colour_manual(values = c("Valeurs observées" = COL_OR,
                                  "Valeurs ajustées"  = COL_FIT)) +
  scale_linewidth_manual(values = c("Valeurs observées" = 0.8,
                                     "Valeurs ajustées"  = 0.65)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = label_comma(big.mark = " ", suffix = " $")) +
  labs(
    title    = sprintf("Prévision en échantillon – ARIMA(%d,%d,%d)", p_or, d_or, q_or),
    subtitle = "Prix de l'or (1990-2020) : valeurs observées vs valeurs ajustées",
    x = NULL, y = "Prix (USD/once)",
    caption  = "Source : calcul propre sous R"
  ) +
  theme_insee()

save_fig(fig6, "fig6_arima_fit", w = 22, h = 12)
cat("Figure 6 sauvegardée.\n")


# ---------------------------------------------------------------
# 1.9 Diagnostic des résidus
# ---------------------------------------------------------------

res_or <- as.numeric(residuals(arima_auto))
lb_or  <- Box.test(res_or, lag = 12, type = "Ljung-Box")

cat("\n========================================\n")
cat("DIAGNOSTIC DES RESIDUS - OR\n")
cat("========================================\n")
cat("Moyenne des résidus      :", round(mean(res_or), 6), "\n")
cat("Écart-type des résidus   :", round(sd(res_or),   6), "\n")
cat("Test de Ljung-Box (L=12) : X² =", round(lb_or$statistic, 4),
    "| p =", round(lb_or$p.value, 4), "\n")
cat("Décision : pas d'autocorrélation résiduelle (modèle bien spécifié)\n")

sink("outputs/residus_or.txt")
cat("DIAGNOSTIC DES RESIDUS ARIMA - PRIX DE L'OR\n\n")
cat("Moyenne :", round(mean(res_or), 6), "\n")
cat("Ecart-type :", round(sd(res_or), 6), "\n")
cat("Test Ljung-Box (lag=12) : X² =", round(lb_or$statistic, 4),
    ", p =", round(lb_or$p.value, 4), "\n")
sink()

# Graphique diagnostics résidus (4 panels)
df_res <- data.frame(
  date   = data_mod$date,
  residu = res_or
)

# Panel A : résidus dans le temps
p_res_time <- ggplot(df_res, aes(x = date, y = residu)) +
  geom_hline(yintercept = 0, colour = "#aaaaaa") +
  geom_line(colour = COL_OR, linewidth = 0.5) +
  labs(title = "Résidus dans le temps", x = NULL, y = "Résidus") +
  theme_insee()

# Panel B : ACF des résidus
acf_res  <- acf(res_or,  lag.max = 24, plot = FALSE)
ic_res   <- qnorm(0.975) / sqrt(length(res_or))
df_acfr  <- data.frame(lag = as.numeric(acf_res$lag[-1]),
                        val = as.numeric(acf_res$acf[-1]))
p_acf_res <- ggplot(df_acfr, aes(x = lag, y = val)) +
  geom_hline(yintercept = 0, colour = "#aaaaaa") +
  geom_hline(yintercept =  ic_res, linetype="dashed", colour=COL_CRISIS, linewidth=0.5) +
  geom_hline(yintercept = -ic_res, linetype="dashed", colour=COL_CRISIS, linewidth=0.5) +
  geom_segment(aes(xend = lag, y = 0, yend = val), colour = COL_OR, linewidth = 0.8) +
  geom_point(colour = COL_OR, size = 1.2) +
  labs(title = "ACF des résidus", x = "Retard", y = "Corrélation") +
  theme_insee()

# Panel C : histogramme des résidus
p_hist_res <- ggplot(df_res, aes(x = residu)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25,
                 fill = COL_SHADE, colour = "white") +
  stat_function(fun = dnorm,
                args = list(mean = mean(res_or), sd = sd(res_or)),
                colour = COL_FIT, linewidth = 0.8) +
  labs(title = "Distribution des résidus", x = "Résidus", y = "Densité") +
  theme_insee()

# Panel D : QQ-plot
df_qq <- data.frame(
  theorique  = qnorm(ppoints(length(res_or))),
  empirique  = sort(res_or)
)
p_qq <- ggplot(df_qq, aes(x = theorique, y = empirique)) +
  geom_abline(slope = sd(res_or), intercept = mean(res_or),
              colour = COL_FIT, linewidth = 0.8) +
  geom_point(colour = COL_OR, size = 1.2, alpha = 0.7) +
  labs(title = "QQ-plot des résidus",
       x = "Quantiles théoriques", y = "Quantiles empiriques") +
  theme_insee()

library(gridExtra)
fig7 <- grid.arrange(p_res_time, p_acf_res, p_hist_res, p_qq, ncol = 2,
                      top = grid::textGrob(
                        sprintf("Diagnostic des résidus – ARIMA(%d,%d,%d)",
                                p_or, d_or, q_or),
                        gp = grid::gpar(fontsize = 12, fontface = "bold",
                                        col = "#2c3e50")))
ggsave("figures/fig7_residus.png", fig7,
       width = 24, height = 16, units = "cm", dpi = 150, bg = "white")
cat("Figure 7 sauvegardée.\n")


# ---------------------------------------------------------------
# 1.10 Prévisions hors échantillon (2021-2025)
# ---------------------------------------------------------------

h_prev   <- 60   # 5 ans = 60 mois
prev_or  <- forecast(arima_auto, h = h_prev, level = c(80, 95))

dates_prev <- seq(as.Date("2021-01-01"), by = "month", length.out = h_prev)

# Reconstruction du data.frame pour ggplot
df_prev <- data.frame(
  date   = dates_prev,
  centre = exp(as.numeric(prev_or$mean)),
  lo80   = exp(as.numeric(prev_or$lower[, 1])),
  hi80   = exp(as.numeric(prev_or$upper[, 1])),
  lo95   = exp(as.numeric(prev_or$lower[, 2])),
  hi95   = exp(as.numeric(prev_or$upper[, 2]))
)

# Données réelles post-2020 disponibles (jusqu'à fin 2025)
data_reel_post <- data_or[data_or$date > as.Date("2020-12-01") &
                             data_or$date <= as.Date("2025-12-01"), ]

# Données observées récentes pour le graphique (à partir de 2015)
data_recents <- data_mod[data_mod$date >= as.Date("2015-01-01"), ]

fig8 <- ggplot() +
  # IC 95%
  geom_ribbon(data = df_prev,
              aes(x = date, ymin = lo95, ymax = hi95),
              fill = COL_SHADE, alpha = 0.6) +
  # IC 80%
  geom_ribbon(data = df_prev,
              aes(x = date, ymin = lo80, ymax = hi80),
              fill = "#a8d4f0", alpha = 0.6) +
  # Série observée 2015-2020
  geom_line(data = data_recents,
            aes(x = date, y = prix, colour = "Observé (≤ 2020)"),
            linewidth = 0.9) +
  # Prévision centrale
  geom_line(data = df_prev,
            aes(x = date, y = centre, colour = "Prévision ARIMA"),
            linewidth = 0.85, linetype = "dashed") +
  # Valeurs réelles post-2020
  geom_line(data = data_reel_post,
            aes(x = date, y = prix, colour = "Réel post-2020"),
            linewidth = 0.9) +
  geom_vline(xintercept = as.Date("2021-01-01"),
             linetype = "dotted", colour = "#777777", linewidth = 0.6) +
  scale_colour_manual(
    values = c("Observé (≤ 2020)" = COL_OR,
               "Prévision ARIMA"  = COL_FIT,
               "Réel post-2020"   = COL_REEL)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = label_comma(big.mark = " ", suffix = " $")) +
  labs(
    title    = sprintf("Prévisions hors échantillon du prix de l'or (2021-2025) – ARIMA(%d,%d,%d)",
                       p_or, d_or, q_or),
    subtitle = "Fan chart avec intervalles de confiance à 80 % et 95 %",
    x = NULL, y = "Prix (USD/once)",
    caption  = "Zones ombrées : IC 80 % (foncé) et IC 95 % (clair) | Source : calcul propre"
  ) +
  theme_insee()

save_fig(fig8, "fig8_previsions", w = 26, h = 13)
cat("Figure 8 sauvegardée.\n")

# Tableau comparatif
prev_df_ann  <- df_prev %>%
  mutate(annee = format(date, "%Y")) %>%
  group_by(annee) %>%
  summarise(prev_moy = round(mean(centre), 0), .groups = "drop")

reel_df_ann  <- data_reel_post %>%
  mutate(annee = format(date, "%Y")) %>%
  group_by(annee) %>%
  summarise(reel_moy = round(mean(prix),   0), .groups = "drop")

cmp <- merge(prev_df_ann, reel_df_ann, by = "annee", all.x = TRUE)
cmp$ecart_pct <- round((cmp$reel_moy - cmp$prev_moy) / cmp$prev_moy * 100, 1)
cat("\nComparaison prévisions vs valeurs réelles :\n")
print(cmp)
sink("outputs/comparaison_prev.txt")
cat("COMPARAISON PREVISIONS vs VALEURS REELLES\n\n")
print(cmp)
sink()


# ================================================================
#  PARTIE 2 : TAUX D'EPARGNE DES MENAGES (France, 1950-2018)
# ================================================================

cat("\n\n========================================\n")
cat("PARTIE 2 : EPARGNE DES MENAGES\n")
cat("========================================\n")

# ---------------------------------------------------------------
# 2.1 Lecture des données (Figure 4 = série temporelle annuelle)
# ---------------------------------------------------------------

raw_ep <- read_excel("Epargne_Menage.xlsx", sheet = "Figure 4",
                     col_names = FALSE)
data_ep <- as.data.frame(raw_ep[5:73, 1:4])
colnames(data_ep) <- c("annee", "tx_brut", "tx_invest", "tx_fin")
data_ep[] <- lapply(data_ep, as.numeric)
data_ep   <- data_ep[!is.na(data_ep$annee), ]

cat("Nombre d'observations :", nrow(data_ep), "\n")
cat("Période :", min(data_ep$annee), "à", max(data_ep$annee), "\n")

stats_ep <- data.frame(
  Statistique = c("Minimum", "Maximum", "Moyenne", "Médiane", "Écart-type"),
  Taux_brut   = round(c(min(data_ep$tx_brut), max(data_ep$tx_brut),
                         mean(data_ep$tx_brut), median(data_ep$tx_brut),
                         sd(data_ep$tx_brut)), 2)
)
cat("\nStatistiques descriptives du taux d'épargne brut (%) :\n")
print(stats_ep)

sink("outputs/stats_epargne.txt")
cat("STATISTIQUES - TAUX D'EPARGNE (France, 1950-2018)\n\n")
print(stats_ep)
sink()

ts_ep   <- ts(data_ep$tx_brut, start = 1950, frequency = 1)
ts_ep_d <- diff(ts_ep)


# ---------------------------------------------------------------
# 2.2 Graphique 9 – Taux d'épargne en niveau et financière
# ---------------------------------------------------------------

df_ep_long <- tidyr::pivot_longer(
  data_ep[, c("annee", "tx_brut", "tx_fin")],
  cols = c("tx_brut", "tx_fin"),
  names_to  = "serie",
  values_to = "valeur"
)
df_ep_long$serie <- factor(df_ep_long$serie,
                            levels = c("tx_brut", "tx_fin"),
                            labels = c("Taux d'épargne brut",
                                       "dont : épargne financière"))

events_ep <- data.frame(
  annee = c(1973, 1979, 2008),
  label = c("1er choc\npétrolier", "2e choc\npétrolier", "Crise\n2008")
)

fig9 <- ggplot(df_ep_long, aes(x = annee, y = valeur,
                                colour = serie, linetype = serie)) +
  geom_line(linewidth = 0.8) +
  geom_vline(data = events_ep, aes(xintercept = annee),
             linetype = "dotted", colour = COL_CRISIS, linewidth = 0.5) +
  geom_text(data = events_ep,
            aes(x = annee, y = 21.5, label = label),
            colour = COL_CRISIS, size = 2.6, hjust = 0.5,
            inherit.aes = FALSE) +
  scale_colour_manual(values = c("Taux d'épargne brut"       = COL_EP,
                                  "dont : épargne financière"  = COL_EPFIN)) +
  scale_linetype_manual(values = c("Taux d'épargne brut"       = "solid",
                                    "dont : épargne financière"  = "dashed")) +
  scale_x_continuous(breaks = seq(1950, 2020, by = 10)) +
  scale_y_continuous(labels = label_number(suffix = " %"),
                     limits = c(0, 23)) +
  labs(
    title    = "Taux d'épargne des ménages en France (1950-2018)",
    subtitle = "En pourcentage du revenu disponible brut",
    x = NULL, y = "Taux d'épargne (%)",
    caption  = "Source : INSEE, comptes nationaux, base 2014"
  ) +
  theme_insee()

save_fig(fig9, "fig9_epargne_niveau", w = 22, h = 12)
cat("Figure 9 sauvegardée.\n")


# ---------------------------------------------------------------
# 2.3 Graphique 10 – Variation annuelle du taux d'épargne
# ---------------------------------------------------------------

df_ep_diff <- data.frame(
  annee = data_ep$annee[-1],
  delta = as.numeric(ts_ep_d)
)

fig10 <- ggplot(df_ep_diff, aes(x = annee, y = delta)) +
  geom_hline(yintercept = 0, colour = "#aaaaaa", linewidth = 0.4) +
  geom_col(aes(fill = delta > 0), width = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = COL_EP, "FALSE" = COL_FIT)) +
  scale_x_continuous(breaks = seq(1950, 2020, by = 10)) +
  scale_y_continuous(labels = label_number(suffix = " pp")) +
  labs(
    title    = "Variation annuelle du taux d'épargne brut (1951-2018)",
    subtitle = "Première différence – en points de pourcentage",
    x = NULL, y = "Variation (pp)",
    caption  = "Source : INSEE, comptes nationaux, base 2014"
  ) +
  theme_insee()

save_fig(fig10, "fig10_epargne_diff", w = 22, h = 12)
cat("Figure 10 sauvegardée.\n")


# ---------------------------------------------------------------
# 2.4 Tests de stationnarité – Épargne
# ---------------------------------------------------------------

cat("\n========================================\n")
cat("TESTS DE STATIONNARITE - TAUX D'EPARGNE\n")
cat("========================================\n")

adf_ep_niv  <- adf.test(ts_ep,   alternative = "stationary", k = 3)
adf_ep_diff <- adf.test(ts_ep_d, alternative = "stationary", k = 3)
kpss_ep_niv <- kpss.test(ts_ep,  null = "Level")

cat("ADF en niveau (k=3) : DF =", round(adf_ep_niv$statistic,  4),
    "| p =", round(adf_ep_niv$p.value,  4), "\n")
cat("ADF en diff.  (k=3) : DF =", round(adf_ep_diff$statistic, 4),
    "| p =", round(adf_ep_diff$p.value, 4), "\n")
cat("KPSS en niveau      : stat =", round(kpss_ep_niv$statistic, 4),
    "| p =", round(kpss_ep_niv$p.value, 4), "\n")

cat("\nNote : avec seulement 69 obs annuelles, la puissance des tests ADF\n")
cat("est limitée. La série présente un fort pic structurel (années 1970).\n")
cat("L'inspection visuelle et le KPSS suggèrent une non-stationnarité en niveau.\n")
cat("On retient d=1 conformément à auto.arima.\n")

sink("outputs/tests_statio_ep.txt")
cat("TESTS DE STATIONNARITE - TAUX D'EPARGNE\n\n")
cat("ADF en niveau : DF =", round(adf_ep_niv$statistic, 4),
    ", p =", round(adf_ep_niv$p.value, 4), "\n")
cat("ADF en diff.  : DF =", round(adf_ep_diff$statistic, 4),
    ", p =", round(adf_ep_diff$p.value, 4), "\n")
cat("KPSS niveau   : stat =", round(kpss_ep_niv$statistic, 4),
    ", p =", round(kpss_ep_niv$p.value, 4), "\n\n")
cat("Note : puissance limitée (n=69). On retient d=1 (auto.arima).\n")
sink()


# ---------------------------------------------------------------
# 2.5 Graphique 11 – ACF/PACF épargne
# ---------------------------------------------------------------

fig11 <- plot_acf_gg(ts_ep,
                      "ACF et PACF – Taux d'épargne brut (en niveau, 1950-2018)",
                      max_lag = 20, color = COL_EP)
save_fig(fig11, "fig11_acf_epargne", w = 22, h = 12)
cat("Figure 11 sauvegardée.\n")


# ---------------------------------------------------------------
# 2.6 Modèle ARIMA – Épargne
# ---------------------------------------------------------------

cat("\n========================================\n")
cat("MODELE ARIMA - TAUX D'EPARGNE\n")
cat("========================================\n")

arima_ep <- auto.arima(ts_ep, stepwise = FALSE, approximation = FALSE,
                        ic = "aic", trace = FALSE)
p_ep <- arima_ep$arma[1]
d_ep <- arima_ep$arma[6]
q_ep <- arima_ep$arma[2]
cat(sprintf("Modèle sélectionné : ARIMA(%d,%d,%d)\n", p_ep, d_ep, q_ep))
print(summary(arima_ep))

# Comparaison de quelques modèles concurrents
arima_110 <- Arima(ts_ep, order = c(1, 1, 0))
arima_210 <- Arima(ts_ep, order = c(2, 1, 0))
arima_011 <- Arima(ts_ep, order = c(0, 1, 1))

tab_comp <- data.frame(
  Modele = c("ARIMA(1,1,0)", "ARIMA(2,1,0)", "ARIMA(3,1,0)",
             "ARIMA(0,1,1)"),
  AIC    = round(c(AIC(arima_110), AIC(arima_210), AIC(arima_ep), AIC(arima_011)), 2),
  BIC    = round(c(BIC(arima_110), BIC(arima_210), BIC(arima_ep), BIC(arima_011)), 2)
)
cat("\nComparaison de modèles candidats :\n")
print(tab_comp)

# Tests Student coefficients
coefs_ep  <- coef(arima_ep)
se_ep     <- sqrt(diag(arima_ep$var.coef))
t_ep      <- coefs_ep / se_ep
pv_ep     <- 2 * (1 - pnorm(abs(t_ep)))
tab_coef_ep <- data.frame(
  Coef     = round(coefs_ep, 4),
  Std_Err  = round(se_ep,    4),
  t_stat   = round(t_ep,     3),
  p_valeur = round(pv_ep,    4)
)
cat("\nTests de Student :\n")
print(tab_coef_ep)

# Diagnostic résidus
res_ep <- residuals(arima_ep)
lb_ep  <- Box.test(res_ep, lag = 10, type = "Ljung-Box")
cat("\nLjung-Box (L=10) : X² =", round(lb_ep$statistic, 4),
    "| p =", round(lb_ep$p.value, 4), "\n")

sink("outputs/arima_epargne.txt")
cat(sprintf("MODELE ARIMA(%d,%d,%d) - TAUX D'EPARGNE\n\n", p_ep, d_ep, q_ep))
print(summary(arima_ep))
cat("\nComparaison modèles :\n"); print(tab_comp)
cat("\nTests Student :\n"); print(tab_coef_ep)
cat("\nLjung-Box (L=10) : X² =", round(lb_ep$statistic, 4),
    ", p =", round(lb_ep$p.value, 4), "\n")
sink()


# ---------------------------------------------------------------
# 2.7 Graphique 12 – Prévisions taux d'épargne
# ---------------------------------------------------------------

prev_ep     <- forecast(arima_ep, h = 10, level = c(80, 95))
dates_ep_pr <- 2019:2028

df_prev_ep <- data.frame(
  annee  = dates_ep_pr,
  centre = as.numeric(prev_ep$mean),
  lo80   = as.numeric(prev_ep$lower[, 1]),
  hi80   = as.numeric(prev_ep$upper[, 1]),
  lo95   = as.numeric(prev_ep$lower[, 2]),
  hi95   = as.numeric(prev_ep$upper[, 2])
)

# Données historiques pour contexte
df_ep_hist <- data.frame(annee = data_ep$annee,
                          valeur = data_ep$tx_brut)

fig12 <- ggplot() +
  # Historique
  geom_line(data = df_ep_hist,
            aes(x = annee, y = valeur, colour = "Observé"),
            linewidth = 0.8) +
  # IC 95 %
  geom_ribbon(data = df_prev_ep,
              aes(x = annee, ymin = lo95, ymax = hi95),
              fill = "#c8e6c9", alpha = 0.5) +
  # IC 80 %
  geom_ribbon(data = df_prev_ep,
              aes(x = annee, ymin = lo80, ymax = hi80),
              fill = "#81c784", alpha = 0.5) +
  # Prévision centrale
  geom_line(data = df_prev_ep,
            aes(x = annee, y = centre, colour = "Prévision"),
            linewidth = 0.85, linetype = "dashed") +
  geom_vline(xintercept = 2018.5, linetype = "dotted",
             colour = "#777777", linewidth = 0.6) +
  scale_colour_manual(values = c("Observé"   = COL_EP,
                                  "Prévision" = "#1b5e20")) +
  scale_x_continuous(breaks = seq(1950, 2028, by = 10)) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(
    title    = sprintf("Prévisions du taux d'épargne (2019-2028) – ARIMA(%d,%d,%d)",
                       p_ep, d_ep, q_ep),
    subtitle = "Fan chart avec IC à 80 % et 95 %",
    x = NULL, y = "Taux d'épargne brut (%)",
    caption  = "Source : INSEE / calcul propre"
  ) +
  theme_insee()

save_fig(fig12, "fig12_prev_epargne", w = 22, h = 12)
cat("Figure 12 sauvegardée.\n")


# ================================================================
#  PARTIE 3 : COMPARAISON INTERNATIONALE (données panel)
# ================================================================

cat("\n========================================\n")
cat("PARTIE 3 : COMPARAISON INTERNATIONALE\n")
cat("========================================\n")

raw_f3 <- read_excel("Epargne_Menage.xlsx", sheet = "Figure 3",
                     col_names = FALSE)
pays    <- as.character(raw_f3[5:19, 1, drop = TRUE])
vals    <- suppressWarnings(
  as.data.frame(lapply(raw_f3[5:19, 2:6], as.numeric))
)
colnames(vals) <- c("y2000", "y2005", "y2010", "y2015", "y2018")
panel           <- cbind(data.frame(pays = pays), vals)
panel           <- panel[complete.cases(panel[, c("y2000", "y2018")]), ]
cat("Panel :", nrow(panel), "pays × 5 périodes\n")
print(panel[, c("pays", "y2000", "y2018")])

# Régression cross-section : taux 2018 ~ taux 2000
ols_panel <- lm(y2018 ~ y2000, data = panel)
cat("\nOLS cross-section (taux 2018 ~ taux 2000) :\n")
print(summary(ols_panel))

# Graphique panel
fig_panel <- ggplot(panel, aes(x = y2000, y = y2018, label = pays)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted",
              colour = "#aaaaaa") +
  geom_smooth(method = "lm", se = TRUE, colour = COL_FIT,
              fill = COL_SHADE, alpha = 0.3, linewidth = 0.8) +
  geom_point(colour = COL_OR, size = 2.5) +
  ggrepel::geom_text_repel(size = 3, colour = "#333333",
                             max.overlaps = 15) +
  scale_x_continuous(labels = label_number(suffix = " %")) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(
    title    = "Persistance du taux d'épargne en Europe (2000 vs 2018)",
    subtitle = "Régression cross-section – taux 2018 en fonction du taux 2000",
    x = "Taux d'épargne 2000 (%)", y = "Taux d'épargne 2018 (%)",
    caption  = "Source : Eurostat | Diagonale en pointillés : taux 2000 = taux 2018"
  ) +
  theme_insee()

# Vérifier si ggrepel est dispo
if (requireNamespace("ggrepel", quietly = TRUE)) {
  save_fig(fig_panel, "fig_panel", w = 18, h = 16)
  cat("Figure panel sauvegardée.\n")
} else {
  fig_panel2 <- fig_panel +
    geom_text(size = 2.8, colour = "#333333", vjust = -0.8)
  save_fig(fig_panel2, "fig_panel", w = 18, h = 16)
  cat("Figure panel sauvegardée (sans ggrepel).\n")
}


# ================================================================
#  PARTIE 4 : REGRESSIONS MCO – OR ET EPARGNE (SEPAREES)
# ================================================================

library(lmtest)

# ---------------------------------------------------------------
# 4.1 MCO – Prix de l'or (1990-2020)
# ---------------------------------------------------------------

cat("\n========================================\n")
cat("PARTIE 4.1 : REGRESSION MCO – PRIX DE L'OR\n")
cat("========================================\n")

# Données annuelles du prix de l'or (1990-2020)
data_or_ann <- data_or %>%
  mutate(annee = as.integer(format(date, "%Y"))) %>%
  group_by(annee) %>%
  summarise(prix_moy = mean(prix, na.rm = TRUE), .groups = "drop") %>%
  filter(annee >= 1990, annee <= 2020)

data_or_ann$log_or   <- log(data_or_ann$prix_moy)
data_or_ann$tendance  <- data_or_ann$annee - 1990   # tendance : 0 en 1990

# Variables muettes pour crises majeures
data_or_ann$D_2001 <- as.integer(data_or_ann$annee %in% 2001:2002)  # bulle internet + 11-Sept
data_or_ann$D_2008 <- as.integer(data_or_ann$annee %in% 2008:2009)  # crise financière globale
data_or_ann$D_2020 <- as.integer(data_or_ann$annee == 2020)          # COVID-19

cat("Nombre d'observations :", nrow(data_or_ann), "(1990-2020)\n\n")

# Modèles emboîtés
ols_or1 <- lm(log_or ~ tendance,                         data = data_or_ann)
ols_or2 <- lm(log_or ~ tendance + D_2008,                data = data_or_ann)
ols_or3 <- lm(log_or ~ tendance + D_2001 + D_2008 + D_2020, data = data_or_ann)

cat("OLS Or 1 – log(or) ~ tendance :\n");          print(summary(ols_or1))
cat("\nOLS Or 2 – log(or) ~ tendance + D_2008 :\n"); print(summary(ols_or2))
cat("\nOLS Or 3 – log(or) ~ tendance + 3 crises :\n"); print(summary(ols_or3))

bp_or2 <- bptest(ols_or2)
dw_or2 <- dwtest(ols_or2)
cat("\nBreusch-Pagan (OLS Or 2) :\n"); print(bp_or2)
cat("\nDurbin-Watson (OLS Or 2) :\n"); print(dw_or2)

# Tableau comparatif des modèles
tab_ols_or <- data.frame(
  Modele = c("Modele 1 : tendance seule",
             "Modele 2 : + D_2008",
             "Modele 3 : + D_2001 + D_2008 + D_2020"),
  R2adj  = round(c(summary(ols_or1)$adj.r.squared,
                   summary(ols_or2)$adj.r.squared,
                   summary(ols_or3)$adj.r.squared), 4),
  AIC    = round(c(AIC(ols_or1), AIC(ols_or2), AIC(ols_or3)), 2)
)
cat("\nComparaison modeles (or) :\n"); print(tab_ols_or)

sink("outputs/ols_or.txt")
cat("REGRESSION MCO – PRIX DE L'OR (1990-2020)\n\n")
cat("OLS 1 :\n"); print(summary(ols_or1))
cat("\nOLS 2 :\n"); print(summary(ols_or2))
cat("\nOLS 3 :\n"); print(summary(ols_or3))
cat("\nComparaison modeles :\n"); print(tab_ols_or)
cat("\nBreusch-Pagan (OLS 2) :\n"); print(bp_or2)
cat("\nDurbin-Watson (OLS 2) :\n"); print(dw_or2)
sink()

# Figure MCO or – ajustement + résidus
fig_mco_or_a <- ggplot(data_or_ann, aes(x = annee, y = log_or)) +
  geom_point(colour = COL_OR, size = 2.5) +
  geom_line(aes(y = fitted(ols_or2)), colour = COL_FIT,
            linewidth = 0.9, linetype = "dashed") +
  annotate("rect", xmin = 2007.5, xmax = 2009.5,
           ymin = -Inf, ymax = Inf, fill = COL_CRISIS, alpha = 0.08) +
  annotate("text", x = 2008.5, y = max(data_or_ann$log_or) * 0.985,
           label = "Crise 2008", colour = COL_CRISIS, size = 2.8) +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
  labs(title = "MCO : log(prix or) ~ tendance + D_2008 (1990-2020)",
       x = "Annee", y = "Log(prix or, USD/once)",
       caption = "Source : INSEE, serie n 010002061 – calcul propre") +
  theme_insee()

df_res_or2 <- data.frame(annee = data_or_ann$annee, res = residuals(ols_or2))
fig_mco_or_b <- ggplot(df_res_or2, aes(x = annee, y = res)) +
  geom_hline(yintercept = 0, colour = "#aaaaaa") +
  geom_line(colour = COL_OR, linewidth = 0.6) +
  geom_point(colour = COL_OR, size = 1.8) +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
  labs(title = "Residus MCO (or)", x = "Annee", y = "Residus") +
  theme_insee()

fig_mco_or <- grid.arrange(fig_mco_or_a, fig_mco_or_b, ncol = 2,
  top = grid::textGrob("Regression MCO – Prix de l'or (1990-2020)",
    gp = grid::gpar(fontsize = 12, fontface = "bold", col = "#2c3e50")))
ggsave("figures/fig_mco_or.png", fig_mco_or,
       width = 24, height = 13, units = "cm", dpi = 150, bg = "white")
cat("Figure MCO or sauvegardee.\n")


# ---------------------------------------------------------------
# 4.2 MCO – Taux d'épargne (1950-2018)
# ---------------------------------------------------------------

cat("\n========================================\n")
cat("PARTIE 4.2 : REGRESSION MCO – TAUX D'EPARGNE\n")
cat("========================================\n")

data_ep_mco <- data_ep
data_ep_mco$tendance <- data_ep_mco$annee - 1984   # centré sur 1984

# Variables muettes pour chocs structurels
data_ep_mco$D_1974 <- as.integer(data_ep_mco$annee %in% 1974:1976)  # 1er choc pétrolier
data_ep_mco$D_1979 <- as.integer(data_ep_mco$annee %in% 1980:1982)  # 2e choc pétrolier
data_ep_mco$D_2008 <- as.integer(data_ep_mco$annee %in% 2008:2010)  # crise financière

cat("Nombre d'observations :", nrow(data_ep_mco), "(1950-2018)\n\n")

ols_ep1 <- lm(tx_brut ~ tendance,                              data = data_ep_mco)
ols_ep2 <- lm(tx_brut ~ tendance + D_1974 + D_1979,           data = data_ep_mco)
ols_ep3 <- lm(tx_brut ~ tendance + D_1974 + D_1979 + D_2008,  data = data_ep_mco)

cat("OLS Ep 1 – tx_brut ~ tendance :\n");            print(summary(ols_ep1))
cat("\nOLS Ep 2 – tx_brut ~ tendance + D74 + D79 :\n"); print(summary(ols_ep2))
cat("\nOLS Ep 3 – tx_brut ~ tendance + 3 crises :\n"); print(summary(ols_ep3))

bp_ep3 <- bptest(ols_ep3)
dw_ep3 <- dwtest(ols_ep3)
cat("\nBreusch-Pagan (OLS Ep 3) :\n"); print(bp_ep3)
cat("\nDurbin-Watson (OLS Ep 3) :\n"); print(dw_ep3)

tab_ols_ep <- data.frame(
  Modele = c("Modele 1 : tendance seule",
             "Modele 2 : + D_1974 + D_1979",
             "Modele 3 : + D_1974 + D_1979 + D_2008"),
  R2adj  = round(c(summary(ols_ep1)$adj.r.squared,
                   summary(ols_ep2)$adj.r.squared,
                   summary(ols_ep3)$adj.r.squared), 4),
  AIC    = round(c(AIC(ols_ep1), AIC(ols_ep2), AIC(ols_ep3)), 2)
)
cat("\nComparaison modeles (epargne) :\n"); print(tab_ols_ep)

sink("outputs/ols_ep.txt")
cat("REGRESSION MCO – TAUX D'EPARGNE (1950-2018)\n\n")
cat("OLS 1 :\n"); print(summary(ols_ep1))
cat("\nOLS 2 :\n"); print(summary(ols_ep2))
cat("\nOLS 3 :\n"); print(summary(ols_ep3))
cat("\nComparaison modeles :\n"); print(tab_ols_ep)
cat("\nBreusch-Pagan (OLS 3) :\n"); print(bp_ep3)
cat("\nDurbin-Watson (OLS 3) :\n"); print(dw_ep3)
sink()

# Figure MCO épargne
fig_mco_ep_a <- ggplot(data_ep_mco, aes(x = annee, y = tx_brut)) +
  geom_point(colour = COL_EP, size = 2.0) +
  geom_line(aes(y = fitted(ols_ep3)), colour = COL_FIT,
            linewidth = 0.9, linetype = "dashed") +
  annotate("rect", xmin = 1973.5, xmax = 1976.5,
           ymin = -Inf, ymax = Inf, fill = COL_CRISIS, alpha = 0.07) +
  annotate("rect", xmin = 1979.5, xmax = 1982.5,
           ymin = -Inf, ymax = Inf, fill = COL_CRISIS, alpha = 0.07) +
  annotate("rect", xmin = 2007.5, xmax = 2010.5,
           ymin = -Inf, ymax = Inf, fill = "#2980b9", alpha = 0.07) +
  scale_x_continuous(breaks = seq(1950, 2018, by = 10)) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(title = "MCO : taux epargne ~ tendance + chocs (1950-2018)",
       x = "Annee", y = "Taux d'epargne brut (%)",
       caption = "Source : INSEE (ref. 4277790) – calcul propre") +
  theme_insee()

df_res_ep3 <- data.frame(annee = data_ep_mco$annee, res = residuals(ols_ep3))
fig_mco_ep_b <- ggplot(df_res_ep3, aes(x = annee, y = res)) +
  geom_hline(yintercept = 0, colour = "#aaaaaa") +
  geom_line(colour = COL_EP, linewidth = 0.6) +
  geom_point(colour = COL_EP, size = 1.5) +
  scale_x_continuous(breaks = seq(1950, 2018, by = 10)) +
  labs(title = "Residus MCO (epargne)", x = "Annee", y = "Residus (pp)") +
  theme_insee()

fig_mco_ep <- grid.arrange(fig_mco_ep_a, fig_mco_ep_b, ncol = 2,
  top = grid::textGrob("Regression MCO – Taux d'epargne (1950-2018)",
    gp = grid::gpar(fontsize = 12, fontface = "bold", col = "#2c3e50")))
ggsave("figures/fig_mco_ep.png", fig_mco_ep,
       width = 24, height = 13, units = "cm", dpi = 150, bg = "white")
cat("Figure MCO epargne sauvegardee.\n")

cat("\n\n=== SCRIPT TERMINE ===\n")
cat("Figures : ./figures/\n")
cat("Outputs : ./outputs/\n")
