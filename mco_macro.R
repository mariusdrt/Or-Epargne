# ================================================================
#  MCO AVEC VARIABLES MACROECONOMIQUES
#  Prix de l'or & Taux d'épargne des ménages
#  Sources : FRED/BLS (données US), INSEE/OCDE (données France)
# ================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(lmtest)
library(gridExtra)
library(readxl)

# Thème graphique (identique à analyse_complete.R)
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
      panel.grid.major  = element_line(colour = "#eeeeee", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      legend.position   = "bottom",
      legend.title      = element_blank(),
      legend.text       = element_text(size = base_size - 2)
    )
}

COL_OR    <- "#1a5276"
COL_FIT   <- "#c0392b"
COL_EP    <- "#117a65"
COL_SHADE <- "#d5e8f3"
COL_EPFIN <- "#a04000"

dir.create("figures", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

# ================================================================
#  PARTIE A : DONNÉES MACRO US – Prix de l'or (1990–2020)
# ================================================================

# ----------------------------------------------------------------
# Source 1 : Taux directeur de la Fed (Federal Funds Rate, %)
#            Source : FRED/Federal Reserve, série FEDFUNDS
#            Disponible : https://fred.stlouisfed.org/series/FEDFUNDS
# ----------------------------------------------------------------
annees_or <- 1990:2020

fed_funds <- c(
  8.10, 5.69, 3.52, 3.02, 4.21, 5.84, 5.30, 5.46, 5.35, 5.00,  # 1990-1999
  6.24, 3.88, 1.67, 1.13, 1.35, 3.22, 5.02, 5.02, 1.92, 0.24,  # 2000-2009
  0.18, 0.10, 0.14, 0.11, 0.09, 0.13, 0.40, 1.00, 1.83, 2.16,  # 2010-2019
  0.36                                                             # 2020
)

# ----------------------------------------------------------------
# Source 2 : Inflation US (CPI, variation annuelle %)
#            Source : BLS/FRED, série CPIAUCSL
#            Disponible : https://fred.stlouisfed.org/series/CPIAUCSL
# ----------------------------------------------------------------
inflation_us <- c(
  5.40, 4.20, 3.01, 2.99, 2.61, 2.81, 2.95, 2.34, 1.55, 2.19,  # 1990-1999
  3.38, 2.83, 1.59, 2.27, 2.68, 3.39, 3.23, 2.85, 3.84, -0.36, # 2000-2009
  1.64, 3.16, 2.07, 1.46, 1.62, 0.12, 1.26, 2.13, 2.44, 1.81,  # 2010-2019
  1.23                                                             # 2020
)

# ----------------------------------------------------------------
# Source 3 : Prix du pétrole brut (WTI, USD/baril, moyenne annuelle)
#            Source : EIA/FRED, série DCOILWTICO
#            Disponible : https://fred.stlouisfed.org/series/DCOILWTICO
# ----------------------------------------------------------------
wti <- c(
  24.47, 21.54, 19.25, 16.97, 15.82, 16.75, 22.02, 18.64, 10.87, 16.56, # 1990-1999
  28.26, 23.00, 22.81, 28.10, 37.66, 50.04, 61.08, 72.34, 99.67, 53.56, # 2000-2009
  77.45, 94.88, 94.05, 97.98, 93.17, 48.66, 43.29, 50.80, 64.94, 57.03, # 2010-2019
  39.68                                                                     # 2020
)

# ----------------------------------------------------------------
# Source 4 : Taux de change EUR/USD (moyenne annuelle)
#            Source : BCE/FRED, série DEXUSEU (à partir de 1999)
#            Note : avant 1999, on utilise le cours ECU/USD (prédécesseur)
#            Source pré-1999 : Banque de France / BCE archives
# ----------------------------------------------------------------
eur_usd <- c(
  0.803, 0.762, 0.771, 0.838, 0.832, 0.844, 0.859, 0.882, 0.903, 0.940, # 1990-1999 (ECU/USD approx)
  0.924, 0.896, 0.945, 1.131, 1.242, 1.245, 1.256, 1.370, 1.472, 1.395, # 2000-2009
  1.326, 1.393, 1.285, 1.328, 1.329, 1.110, 1.107, 1.130, 1.181, 1.120, # 2010-2019
  1.142                                                                     # 2020
)

# Assemblage du jeu de données or
# Lecture du prix de l'or depuis le fichier Excel
raw_or <- read_excel("ST_Or.xlsx", sheet = "valeurs_mensuelles", col_names = FALSE)
data_or <- raw_or[5:nrow(raw_or), 1:2]
colnames(data_or) <- c("date_str", "prix")
data_or$prix <- suppressWarnings(as.numeric(data_or$prix))
data_or <- data_or[!is.na(data_or$prix), ]
data_or$date <- as.Date(paste0(data_or$date_str, "-01"), format = "%Y-%m-%d")
data_or <- data_or[order(data_or$date), ]

# Agrégation en moyennes annuelles (1990-2020)
data_or_ann <- data_or %>%
  mutate(annee = as.integer(format(date, "%Y"))) %>%
  group_by(annee) %>%
  summarise(prix_moy = mean(prix, na.rm = TRUE), .groups = "drop") %>%
  filter(annee >= 1990, annee <= 2020)

# Fusion avec les variables macro
df_or <- data.frame(
  annee      = annees_or,
  log_or     = log(data_or_ann$prix_moy),
  fed_funds  = fed_funds,
  infl_us    = inflation_us,
  taux_reel  = fed_funds - inflation_us,  # taux d'intérêt réel US (approx.)
  log_wti    = log(wti),
  eur_usd    = eur_usd
)

cat("========================================\n")
cat("DONNEES OR + MACRO – Nb obs :", nrow(df_or), "\n")
cat("Période :", min(df_or$annee), "–", max(df_or$annee), "\n")
cat("========================================\n\n")

# ================================================================
#  MCO – PRIX DE L'OR
# ================================================================

cat("--- Modèles univariés (exploration) ---\n\n")

# Modèle 1 : taux directeur Fed seulement
ols_or_m1 <- lm(log_or ~ fed_funds, data = df_or)
cat("M1 – log(or) ~ taux_fed :\n"); print(summary(ols_or_m1))

# Modèle 2 : inflation US seulement
ols_or_m2 <- lm(log_or ~ infl_us, data = df_or)
cat("\nM2 – log(or) ~ inflation_us :\n"); print(summary(ols_or_m2))

# Modèle 3 : taux d'intérêt réel US
ols_or_m3 <- lm(log_or ~ taux_reel, data = df_or)
cat("\nM3 – log(or) ~ taux_reel_us :\n"); print(summary(ols_or_m3))

# Modèle 4 : prix du pétrole (WTI)
ols_or_m4 <- lm(log_or ~ log_wti, data = df_or)
cat("\nM4 – log(or) ~ log(WTI) :\n"); print(summary(ols_or_m4))

# Modèle 5 : taux de change EUR/USD
ols_or_m5 <- lm(log_or ~ eur_usd, data = df_or)
cat("\nM5 – log(or) ~ EUR/USD :\n"); print(summary(ols_or_m5))

cat("\n--- Modèles multivariés ---\n\n")

# Modèle 6 : taux réel + log(WTI)
ols_or_m6 <- lm(log_or ~ taux_reel + log_wti, data = df_or)
cat("M6 – log(or) ~ taux_reel + log(WTI) :\n"); print(summary(ols_or_m6))

# Modèle 7 : taux réel + log(WTI) + EUR/USD
ols_or_m7 <- lm(log_or ~ taux_reel + log_wti + eur_usd, data = df_or)
cat("\nM7 – log(or) ~ taux_reel + log(WTI) + EUR/USD :\n"); print(summary(ols_or_m7))

# Modèle 8 : modèle complet
ols_or_m8 <- lm(log_or ~ fed_funds + infl_us + log_wti + eur_usd, data = df_or)
cat("\nM8 – complet (fed + infl + WTI + EUR/USD) :\n"); print(summary(ols_or_m8))

# Tableau comparatif
tab_or <- data.frame(
  Modele = c("M1: taux_fed", "M2: infl_us", "M3: taux_reel",
             "M4: log(WTI)", "M5: EUR/USD",
             "M6: taux_reel + log(WTI)",
             "M7: M6 + EUR/USD",
             "M8: fed + infl + WTI + EUR/USD"),
  R2adj  = round(c(summary(ols_or_m1)$adj.r.squared, summary(ols_or_m2)$adj.r.squared,
                   summary(ols_or_m3)$adj.r.squared, summary(ols_or_m4)$adj.r.squared,
                   summary(ols_or_m5)$adj.r.squared, summary(ols_or_m6)$adj.r.squared,
                   summary(ols_or_m7)$adj.r.squared, summary(ols_or_m8)$adj.r.squared), 4),
  AIC    = round(c(AIC(ols_or_m1), AIC(ols_or_m2), AIC(ols_or_m3),
                   AIC(ols_or_m4), AIC(ols_or_m5), AIC(ols_or_m6),
                   AIC(ols_or_m7), AIC(ols_or_m8)), 2)
)
cat("\n--- Comparaison des modèles (or) ---\n")
print(tab_or)

# Tests diagnostiques – modèle retenu (M7)
bp_or7 <- bptest(ols_or_m7)
dw_or7 <- dwtest(ols_or_m7)
cat("\nBreusch-Pagan M7 :\n"); print(bp_or7)
cat("\nDurbin-Watson M7 :\n"); print(dw_or7)

# Matrice de corrélation des variables explicatives
mat_cor_or <- cor(df_or[, c("log_or", "taux_reel", "log_wti", "eur_usd", "fed_funds", "infl_us")])
cat("\nMatrice de corrélation :\n")
print(round(mat_cor_or, 3))

sink("outputs/mco_or.txt")
cat("MCO – PRIX DE L'OR (1990-2020)\n\n")
cat("Sources : FRED/BLS (taux Fed, CPI US), EIA (WTI), BCE (EUR/USD)\n\n")
for (i in 1:8) {
  m <- list(ols_or_m1, ols_or_m2, ols_or_m3, ols_or_m4,
            ols_or_m5, ols_or_m6, ols_or_m7, ols_or_m8)[[i]]
  cat(sprintf("Modele M%d :\n", i)); print(summary(m)); cat("\n")
}
cat("Comparaison modeles :\n"); print(tab_or)
cat("\nBreusch-Pagan M7 :\n"); print(bp_or7)
cat("\nDurbin-Watson M7 :\n"); print(dw_or7)
cat("\nMatrice de correlation :\n"); print(round(mat_cor_or, 3))
sink()


# ================================================================
#  GRAPHIQUES MCO – OR
# ================================================================

# Figure A – Scatter plots variables vs log(or)
p_or_a <- ggplot(df_or, aes(x = taux_reel, y = log_or)) +
  geom_point(colour = COL_OR, size = 2.2) +
  geom_smooth(method = "lm", se = TRUE, colour = COL_FIT,
              fill = COL_SHADE, alpha = 0.3) +
  labs(title = "Taux reel US vs log(or)",
       x = "Taux reel US (%)", y = "Log(prix or)") +
  theme_insee()

p_or_b <- ggplot(df_or, aes(x = log_wti, y = log_or)) +
  geom_point(colour = COL_OR, size = 2.2) +
  geom_smooth(method = "lm", se = TRUE, colour = COL_FIT,
              fill = COL_SHADE, alpha = 0.3) +
  labs(title = "Log(WTI) vs log(or)",
       x = "Log(prix WTI, USD/baril)", y = "Log(prix or)") +
  theme_insee()

p_or_c <- ggplot(df_or, aes(x = eur_usd, y = log_or)) +
  geom_point(colour = COL_OR, size = 2.2) +
  geom_smooth(method = "lm", se = TRUE, colour = COL_FIT,
              fill = COL_SHADE, alpha = 0.3) +
  labs(title = "EUR/USD vs log(or)",
       x = "Taux de change EUR/USD", y = "Log(prix or)") +
  theme_insee()

p_or_d <- ggplot(df_or, aes(x = infl_us, y = log_or)) +
  geom_point(colour = COL_OR, size = 2.2) +
  geom_smooth(method = "lm", se = TRUE, colour = COL_FIT,
              fill = COL_SHADE, alpha = 0.3) +
  labs(title = "Inflation US vs log(or)",
       x = "Inflation US (%, CPI)", y = "Log(prix or)") +
  theme_insee()

fig_scatteror <- grid.arrange(p_or_a, p_or_b, p_or_c, p_or_d, ncol = 2,
  top = grid::textGrob(
    "Relations entre le prix de l'or et les variables macro (1990-2020)",
    gp = grid::gpar(fontsize = 12, fontface = "bold", col = "#2c3e50")))
ggsave("figures/fig_scatter_or.png", fig_scatteror,
       width = 24, height = 16, units = "cm", dpi = 150, bg = "white")
cat("Figure scatter or sauvegardee.\n")

# Figure B – Ajustement + résidus modèle retenu (M7)
df_or$fitted_m7 <- fitted(ols_or_m7)
df_or$res_m7    <- residuals(ols_or_m7)

p_fit_or <- ggplot(df_or, aes(x = annee)) +
  geom_line(aes(y = log_or,     colour = "Observe"),  linewidth = 0.9) +
  geom_line(aes(y = fitted_m7,  colour = "Ajuste"),   linewidth = 0.85, linetype = "dashed") +
  scale_colour_manual(values = c("Observe" = COL_OR, "Ajuste" = COL_FIT)) +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
  labs(title = "Ajustement MCO M7 : log(or) observe vs ajuste",
       x = "Annee", y = "Log(prix or, USD/once)") +
  theme_insee()

p_res_or <- ggplot(df_or, aes(x = annee, y = res_m7)) +
  geom_hline(yintercept = 0, colour = "#aaaaaa", linewidth = 0.5) +
  geom_line(colour = COL_OR, linewidth = 0.7) +
  geom_point(colour = COL_OR, size = 1.8) +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5)) +
  labs(title = "Residus MCO M7",
       x = "Annee", y = "Residus") +
  theme_insee()

fig_fit_or <- grid.arrange(p_fit_or, p_res_or, ncol = 2,
  top = grid::textGrob(
    "MCO M7 – log(or) ~ taux_reel + log(WTI) + EUR/USD (1990-2020)",
    gp = grid::gpar(fontsize = 12, fontface = "bold", col = "#2c3e50")))
ggsave("figures/fig_mco_or.png", fig_fit_or,
       width = 24, height = 13, units = "cm", dpi = 150, bg = "white")
cat("Figure MCO or (ajustement+residus) sauvegardee.\n")


# ================================================================
#  PARTIE B : DONNEES MACRO FRANCE – Taux d'épargne (1970–2018)
# ================================================================

# Lecture du taux d'épargne depuis Excel
raw_ep <- read_excel("Epargne_Menage.xlsx", sheet = "Figure 4", col_names = FALSE)
data_ep <- as.data.frame(raw_ep[5:73, 1:4])
colnames(data_ep) <- c("annee", "tx_brut", "tx_invest", "tx_fin")
data_ep[] <- lapply(data_ep, as.numeric)
data_ep   <- data_ep[!is.na(data_ep$annee), ]

# Restriction à 1970-2018 (où les données macro sont disponibles)
data_ep70 <- data_ep[data_ep$annee >= 1970, ]

annees_ep <- 1970:2018

# ----------------------------------------------------------------
# Source 5 : Taux d'inflation France (CPI, variation annuelle %)
#            Source : INSEE (séries longues de l'IPC France)
#            Référence : INSEE, séries longues sur les indices de prix à la consommation
#            URL : https://www.insee.fr/fr/statistiques/2417384
# ----------------------------------------------------------------
inflation_fr <- c(
  5.24, 5.53, 5.87, 7.33, 13.67, 11.75, 9.62, 9.44, 9.09, 10.76, # 1970-1979
  13.63, 13.39, 11.96, 9.58,  7.44,  5.82,  2.70,  3.12, 2.71, 3.52, # 1980-1989
  3.37,  3.21,  2.37, 2.10,  1.68,  1.79,  1.97,  1.19, 0.74, 0.57,  # 1990-1999
  1.83,  1.78,  1.94, 2.17,  2.33,  1.90,  1.92,  1.61, 3.16, 0.09,  # 2000-2009
  1.74,  2.29,  2.17, 1.01,  0.62,  0.11,  0.33,  1.17, 2.11         # 2010-2018
)

# ----------------------------------------------------------------
# Source 6 : Taux de croissance du PIB réel France (%)
#            Source : INSEE (comptes nationaux, base 2014)
#            Référence : INSEE, séries longues comptes nationaux
#            URL : https://www.insee.fr/fr/statistiques/2832942
# ----------------------------------------------------------------
croissance_pib <- c(
  5.70, 4.80, 5.90, 5.40,  3.18, -0.93, 4.21, 3.12, 3.37, 3.23, # 1970-1979
  1.62, 1.05, 2.34, 1.34,  1.45,  1.99, 2.42, 2.31, 4.55, 4.40, # 1980-1989
  2.89, 1.05, 1.52,-0.61,  2.24,  2.11, 1.39, 2.25, 3.61, 3.39, # 1990-1999
  3.94, 2.00, 1.10, 0.83,  2.83,  1.74, 2.41, 2.35, 0.24,-2.87, # 2000-2009
  1.94, 2.19, 0.34, 0.62,  0.93,  1.07, 1.19, 2.29, 1.76        # 2010-2018
)

# ----------------------------------------------------------------
# Source 7 : Taux de chômage France (BIT, %)
#            Source : INSEE, enquête Emploi / comptes de l'emploi
#            URL : https://www.insee.fr/fr/statistiques/serie/001688526
# ----------------------------------------------------------------
taux_chomage <- c(
  2.50, 2.70, 2.80, 2.70, 2.80, 3.79, 4.42, 4.82, 5.28, 5.94, # 1970-1979
  6.16, 7.36, 8.07, 8.36, 9.74,10.25,10.54,10.68,10.18, 9.54, # 1980-1989
  8.87, 9.43,10.41,11.66,12.30,11.68,12.34,12.26,11.72,10.82, # 1990-1999
  9.27, 8.55, 8.89, 9.50, 9.92, 9.28, 9.25, 7.97, 7.79, 9.49, # 2000-2009
  9.73, 9.61,10.19,10.30,10.32,10.44,10.10, 9.38, 9.01        # 2010-2018
)

# ----------------------------------------------------------------
# Source 8 : Taux d'intérêt à long terme France (OAT 10 ans, %)
#            Source : Banque de France / BCE / OCDE
#            URL : https://www.banque-france.fr/statistiques/taux-et-cours
# ----------------------------------------------------------------
taux_lt <- c(
  8.49, 8.35, 8.36, 9.22,11.51,10.34,10.50,11.01,10.64,10.43, # 1970-1979
  13.73,15.68,15.68,14.38,13.35,11.94, 8.74, 9.47, 9.01, 8.77, # 1980-1989
  9.92, 9.04, 8.57, 6.87, 7.24, 7.53, 6.30, 5.58, 4.65, 4.61, # 1990-1999
  5.39, 5.03, 4.86, 4.15, 4.12, 3.42, 3.80, 4.30, 4.23, 3.65, # 2000-2009
  3.12, 3.32, 2.54, 2.20, 1.67, 0.84, 0.47, 0.82, 0.78        # 2010-2018
)

# Assemblage du jeu de données épargne
df_ep <- data.frame(
  annee      = annees_ep,
  tx_brut    = data_ep70$tx_brut,
  infl_fr    = inflation_fr,
  pib_croiss = croissance_pib,
  chomage    = taux_chomage,
  taux_lt    = taux_lt
)

cat("\n========================================\n")
cat("DONNEES EPARGNE + MACRO – Nb obs :", nrow(df_ep), "\n")
cat("Période : 1970–2018\n")
cat("========================================\n\n")

# ================================================================
#  MCO – TAUX D'ÉPARGNE
# ================================================================

cat("--- Modèles univariés (exploration) ---\n\n")

# Modèle 1 : inflation seule
ols_ep_m1 <- lm(tx_brut ~ infl_fr, data = df_ep)
cat("M1 – tx_brut ~ inflation_fr :\n"); print(summary(ols_ep_m1))

# Modèle 2 : croissance PIB
ols_ep_m2 <- lm(tx_brut ~ pib_croiss, data = df_ep)
cat("\nM2 – tx_brut ~ croissance_pib :\n"); print(summary(ols_ep_m2))

# Modèle 3 : taux de chômage
ols_ep_m3 <- lm(tx_brut ~ chomage, data = df_ep)
cat("\nM3 – tx_brut ~ taux_chomage :\n"); print(summary(ols_ep_m3))

# Modèle 4 : taux d'intérêt long terme
ols_ep_m4 <- lm(tx_brut ~ taux_lt, data = df_ep)
cat("\nM4 – tx_brut ~ taux_lt :\n"); print(summary(ols_ep_m4))

cat("\n--- Modèles multivariés ---\n\n")

# Modèle 5 : inflation + taux LT
ols_ep_m5 <- lm(tx_brut ~ infl_fr + taux_lt, data = df_ep)
cat("M5 – tx_brut ~ infl + taux_lt :\n"); print(summary(ols_ep_m5))

# Modèle 6 : taux LT + chômage
ols_ep_m6 <- lm(tx_brut ~ taux_lt + chomage, data = df_ep)
cat("\nM6 – tx_brut ~ taux_lt + chomage :\n"); print(summary(ols_ep_m6))

# Modèle 7 : taux LT + chômage + PIB
ols_ep_m7 <- lm(tx_brut ~ taux_lt + chomage + pib_croiss, data = df_ep)
cat("\nM7 – tx_brut ~ taux_lt + chomage + pib :\n"); print(summary(ols_ep_m7))

# Modèle 8 : modèle complet
ols_ep_m8 <- lm(tx_brut ~ infl_fr + pib_croiss + chomage + taux_lt, data = df_ep)
cat("\nM8 – complet :\n"); print(summary(ols_ep_m8))

# Tableau comparatif
tab_ep <- data.frame(
  Modele = c("M1: inflation_fr", "M2: croissance_pib", "M3: chomage",
             "M4: taux_lt", "M5: infl + taux_lt",
             "M6: taux_lt + chomage",
             "M7: taux_lt + chomage + pib",
             "M8: complet"),
  R2adj  = round(c(summary(ols_ep_m1)$adj.r.squared, summary(ols_ep_m2)$adj.r.squared,
                   summary(ols_ep_m3)$adj.r.squared, summary(ols_ep_m4)$adj.r.squared,
                   summary(ols_ep_m5)$adj.r.squared, summary(ols_ep_m6)$adj.r.squared,
                   summary(ols_ep_m7)$adj.r.squared, summary(ols_ep_m8)$adj.r.squared), 4),
  AIC    = round(c(AIC(ols_ep_m1), AIC(ols_ep_m2), AIC(ols_ep_m3),
                   AIC(ols_ep_m4), AIC(ols_ep_m5), AIC(ols_ep_m6),
                   AIC(ols_ep_m7), AIC(ols_ep_m8)), 2)
)
cat("\n--- Comparaison des modèles (epargne) ---\n")
print(tab_ep)

# Tests diagnostiques – modèle retenu
best_ep <- which.min(tab_ep$AIC)
cat("\nModele retenu : M", best_ep, "(AIC minimal)\n")
mlist_ep <- list(ols_ep_m1, ols_ep_m2, ols_ep_m3, ols_ep_m4,
                 ols_ep_m5, ols_ep_m6, ols_ep_m7, ols_ep_m8)
best_ep_model <- mlist_ep[[best_ep]]
cat("\nSummary modele retenu :\n"); print(summary(best_ep_model))
bp_ep_best <- bptest(best_ep_model)
dw_ep_best <- dwtest(best_ep_model)
cat("\nBreusch-Pagan modele retenu :\n"); print(bp_ep_best)
cat("\nDurbin-Watson modele retenu :\n"); print(dw_ep_best)

mat_cor_ep <- cor(df_ep[, c("tx_brut", "infl_fr", "pib_croiss", "chomage", "taux_lt")])
cat("\nMatrice de correlation :\n"); print(round(mat_cor_ep, 3))

sink("outputs/mco_ep.txt")
cat("MCO – TAUX D'EPARGNE (1970-2018)\n\n")
cat("Sources : INSEE (inflation, PIB, chomage), Banque de France (taux LT)\n\n")
for (i in 1:8) {
  cat(sprintf("Modele M%d :\n", i))
  print(summary(mlist_ep[[i]])); cat("\n")
}
cat("Comparaison modeles :\n"); print(tab_ep)
cat("\nBreusch-Pagan modele retenu :\n"); print(bp_ep_best)
cat("\nDurbin-Watson modele retenu :\n"); print(dw_ep_best)
cat("\nMatrice de correlation :\n"); print(round(mat_cor_ep, 3))
sink()


# ================================================================
#  GRAPHIQUES MCO – ÉPARGNE
# ================================================================

# Figure C – Scatter plots
p_ep_a <- ggplot(df_ep, aes(x = taux_lt, y = tx_brut)) +
  geom_point(colour = COL_EP, size = 2.0) +
  geom_smooth(method = "lm", se = TRUE, colour = COL_FIT,
              fill = "#c8e6c9", alpha = 0.3) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(title = "Taux LT vs epargne",
       x = "OAT 10 ans (%)", y = "Taux d'epargne (%)") +
  theme_insee()

p_ep_b <- ggplot(df_ep, aes(x = chomage, y = tx_brut)) +
  geom_point(colour = COL_EP, size = 2.0) +
  geom_smooth(method = "lm", se = TRUE, colour = COL_FIT,
              fill = "#c8e6c9", alpha = 0.3) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(title = "Chomage vs epargne",
       x = "Taux de chomage (%)", y = "Taux d'epargne (%)") +
  theme_insee()

p_ep_c <- ggplot(df_ep, aes(x = infl_fr, y = tx_brut)) +
  geom_point(colour = COL_EP, size = 2.0) +
  geom_smooth(method = "lm", se = TRUE, colour = COL_FIT,
              fill = "#c8e6c9", alpha = 0.3) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(title = "Inflation vs epargne",
       x = "Inflation France (%)", y = "Taux d'epargne (%)") +
  theme_insee()

p_ep_d <- ggplot(df_ep, aes(x = pib_croiss, y = tx_brut)) +
  geom_point(colour = COL_EP, size = 2.0) +
  geom_smooth(method = "lm", se = TRUE, colour = COL_FIT,
              fill = "#c8e6c9", alpha = 0.3) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(title = "Croissance PIB vs epargne",
       x = "Croissance PIB reel (%)", y = "Taux d'epargne (%)") +
  theme_insee()

fig_scatterep <- grid.arrange(p_ep_a, p_ep_b, p_ep_c, p_ep_d, ncol = 2,
  top = grid::textGrob(
    "Relations entre le taux d'epargne et les variables macro (1970-2018)",
    gp = grid::gpar(fontsize = 12, fontface = "bold", col = "#2c3e50")))
ggsave("figures/fig_scatter_ep.png", fig_scatterep,
       width = 24, height = 16, units = "cm", dpi = 150, bg = "white")
cat("Figure scatter epargne sauvegardee.\n")

# Figure D – Ajustement + résidus modèle retenu épargne
df_ep$fitted_best <- fitted(best_ep_model)
df_ep$res_best    <- residuals(best_ep_model)

p_fit_ep <- ggplot(df_ep, aes(x = annee)) +
  geom_line(aes(y = tx_brut,      colour = "Observe"),  linewidth = 0.9) +
  geom_line(aes(y = fitted_best,  colour = "Ajuste"),   linewidth = 0.85, linetype = "dashed") +
  scale_colour_manual(values = c("Observe" = COL_EP, "Ajuste" = COL_FIT)) +
  scale_x_continuous(breaks = seq(1970, 2018, by = 10)) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  labs(title = "Ajustement MCO : epargne observee vs ajustee",
       x = "Annee", y = "Taux d'epargne brut (%)") +
  theme_insee()

p_res_ep <- ggplot(df_ep, aes(x = annee, y = res_best)) +
  geom_hline(yintercept = 0, colour = "#aaaaaa", linewidth = 0.5) +
  geom_line(colour = COL_EP, linewidth = 0.7) +
  geom_point(colour = COL_EP, size = 1.5) +
  scale_x_continuous(breaks = seq(1970, 2018, by = 10)) +
  labs(title = "Residus MCO (epargne)", x = "Annee", y = "Residus (pp)") +
  theme_insee()

fig_fit_ep <- grid.arrange(p_fit_ep, p_res_ep, ncol = 2,
  top = grid::textGrob(
    paste0("MCO – taux d'epargne ~ modele retenu (1970-2018)"),
    gp = grid::gpar(fontsize = 12, fontface = "bold", col = "#2c3e50")))
ggsave("figures/fig_mco_ep.png", fig_fit_ep,
       width = 24, height = 13, units = "cm", dpi = 150, bg = "white")
cat("Figure MCO epargne (ajustement+residus) sauvegardee.\n")

cat("\n=== SCRIPT MCO TERMINE ===\n")
cat("Figures sauvegardees : fig_scatter_or.png, fig_mco_or.png\n")
cat("                       fig_scatter_ep.png, fig_mco_ep.png\n")
cat("Outputs : outputs/mco_or.txt, outputs/mco_ep.txt\n")
