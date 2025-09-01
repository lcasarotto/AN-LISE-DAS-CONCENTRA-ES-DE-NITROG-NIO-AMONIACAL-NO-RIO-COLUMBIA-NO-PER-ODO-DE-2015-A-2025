#======================================================================#
#                           0) PACOTES                                  #
#======================================================================#

# Lista de pacotes necessários
pkgs <- c("readxl", "MASS", "dplyr", "ggplot2", "ggcorrplot", "GGally", "tidyr", "survival")
inst <- pkgs[!pkgs %in% rownames(installed.packages())]
if(length(inst)) install.packages(inst, dependencies = TRUE)
lapply(pkgs, library, character.only = TRUE)

# (mantidos, conforme seu script)
pkgs_needed <- c("survival","survminer","dplyr","ggplot2")
inst <- pkgs_needed[!pkgs_needed %in% rownames(installed.packages())]
if(length(inst)) install.packages(inst, dependencies = TRUE)
lapply(pkgs_needed, library, character.only = TRUE)

pkgs_needed <- c("survival","dplyr","ggplot2")
inst <- pkgs_needed[!pkgs_needed %in% rownames(installed.packages())]
if(length(inst)) install.packages(inst, dependencies = TRUE)
invisible(lapply(pkgs_needed, library, character.only = TRUE))


#======================================================================#
#                     1) LEITURA E TRATAMENTO                          #
#======================================================================#

# Ler o data frame e verifica suas informações 
df <- read_excel("C:\\Users\\User\\Desktop\\Base.xlsx")
head(df)
tail(df)
str(df)

df$Date <- as.Date(df$Date)
df$Cens <- ifelse(df$Cens  == "U",0,1) 
df$Cens <- replace_na(df$Cens, 1)

df <- df %>% 
  mutate(across(!c("Date", "Cens", "Flow", "Station"), as.numeric))

df <- df %>%
  rename(
    Data = Date,
    Amonia_mgL = Ammonia,
    Pressao_mmHg = Pressure,
    O2_Dissolvido_mgL = `Dissolved Oxygen`,
    Coliformes_100ml = `Fecal Coliform`,
    Vazao_cfs = Flow,
    Nitrito_Nitrato_mgL = `Nitrate-Nitrite as N`,
    Ortofosfato_mgL = `Orthophosphate as P`,
    Fosforo_mgL = `Total Phosphorus, mixed forms as P`,
    Condutividade_uhmoscm = `Specific Conductivity`,
    Temperatura_C = Temperature,
    Persulfato_mgL = `Total Persulfate Nitrogen`,
    Solido_Suspendido_mgL = `Total Suspended Solids`,
    Turbidez_NTU = Turbidity,
    Estacao = Station
  )


#======================================================================#
#                    2) ANÁLISE EXPLORATÓRIA (EDA)                     #
#======================================================================#
# ===========================
# 0) Pacotes e diretórios
# ===========================
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggcorrplot)
  library(GGally)
  library(forcats)
  library(stringr)
})

# Define a pasta de saída fixa
out_dir <- "C:/Users/User/Desktop/Result"

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ===========================
# 1) Tema e paleta próprios
# ===========================
cores_status <- c("0" = "#E76F51",  # Censurado
                  "1" = "#2A9D8F")  # Medido

theme_meu <- function(base_size = 11){
  theme_minimal(base_size = base_size) +
    theme(
      plot.title      = element_text(face = "bold", hjust = 0.05, margin = margin(b = 6)),
      plot.subtitle   = element_text(color = "grey30", margin = margin(b = 8)),
      plot.caption    = element_text(color = "grey40", size = rel(0.85)),
      panel.grid.minor= element_blank(),
      panel.grid.major= element_line(color = "grey88"),
      axis.title.x    = element_text(margin = margin(t = 6)),
      axis.title.y    = element_text(margin = margin(r = 6))
    )
}

ggsave_pdf <- function(nome, plt, w = 9, h = 6){
  ggsave(file.path(out_dir, nome), plot = plt, width = w, height = h, device = cairo_pdf)
}

# ===========================
# 2) Sumário e faltantes
# ===========================
# Principais medidas
summary(df)

# Tabela de ausentes (reestilizada)
miss_tbl <- tibble(
  variavel   = names(df),
  n_ausentes = colSums(is.na(df)),
  n          = nrow(df)
) %>%
  mutate(pct = round(100 * n_ausentes / n, 1)) %>%
  arrange(desc(pct))

print(miss_tbl)

# ===========================
# 3) Proporção de censura
# ===========================
prop_cens <- 1 - mean(df$Cens, na.rm = TRUE) # 0 = censurado, 1 = medido
cat(
  "Proporção de observações CENSURADAS:", sprintf("%.1f%%", 100*prop_cens), "do total\n"
)

# ===========================
# 4) Gráfico: contagem de status
# ===========================
p_status <- df %>%
  mutate(Cens = factor(Cens, levels = c(0,1), labels = c("Censurado","Medido"))) %>%
  ggplot(aes(x = Cens, fill = Cens)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.35, size = 3.5) +
  scale_fill_manual(values = c("Censurado" = cores_status["0"], "Medido" = cores_status["1"])) +
  labs(
    title = "Panorama do Status das Observações",
    subtitle = "Contagem de registros censurados vs. medidos",
    x = "Status",
    y = "Frequência"
  ) +
  theme_meu() +
  theme(legend.position = "none")

ggsave_pdf("01_status_censura.pdf", p_status)

# ===========================
# 5) Amônia — histogramas e boxplots
# ===========================
# 5.1 Histograma simples (com log1p no eixo X)
p1 <- ggplot(df, aes(x = Amonia_mgL)) +
  geom_histogram(bins = 30, color = "grey20", fill = "grey75", alpha = 0.9, linewidth = 0.2) +
  scale_x_continuous(trans = "log1p") +
  labs(
    title = "Distribuição da Concentração de Amônia",
    subtitle = "Eixo em escala log1p para evidenciar a cauda à direita",
    x = "Amônia (mg/L) [log1p]",
    y = "Frequência"
  ) +
  theme_meu()

ggsave_pdf("02_hist_amonia.pdf", p1)

# 5.2 Histograma colorido por status de censura
p2 <- df %>%
  mutate(Cens = factor(Cens, levels = c(0,1), labels = c("Censurado","Medido"))) %>%
  ggplot(aes(x = Amonia_mgL, fill = Cens)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.65, color = "grey15", linewidth = 0.2) +
  scale_fill_manual(values = c("Censurado" = cores_status["0"], "Medido" = cores_status["1"])) +
  scale_x_continuous(trans = "log1p") +
  labs(
    title = "Distribuição de Amônia por Status de Censura",
    subtitle = "Sobreposição com transparência; eixo X em log1p",
    x = "Amônia (mg/L) [log1p]",
    y = "Frequência",
    fill = "Status"
  ) +
  theme_meu()

ggsave_pdf("03_hist_amonia_censura.pdf", p2)

# 5.3 Boxplot simples
p3 <- ggplot(df, aes(y = Amonia_mgL)) +
  geom_boxplot(fill = "white", color = "grey20", outlier.color = "#8E2DE2", outlier.alpha = 0.8) +
  scale_y_continuous(trans = "log1p") +
  labs(
    title = "Amplitude e Assimetria da Amônia",
    subtitle = "Boxplot com eixo Y em log1p",
    x = NULL, y = "Amônia (mg/L) [log1p]"
  ) +
  theme_meu()

ggsave_pdf("04_box_amonia.pdf", p3)

# 5.4 Boxplot por status
p4 <- df %>%
  mutate(Cens_f = factor(Cens, levels = c(0,1), labels = c("Censurado","Medido"))) %>%
  ggplot(aes(x = Cens_f, y = Amonia_mgL, fill = Cens_f)) +
  geom_boxplot(outlier.color = "grey20") +
  scale_fill_manual(values = c("Censurado" = "#F4A261", "Medido" = "#264653")) +
  scale_y_continuous(trans = "log1p") +
  labs(
    title = "Amônia por Status de Censura",
    subtitle = "Comparação de dispersão em escala log1p",
    x = NULL, y = "Amônia (mg/L) [log1p]", fill = NULL
  ) +
  theme_meu() +
  theme(legend.position = "none")

ggsave_pdf("05_box_amonia_por_status.pdf", p4)

# ===========================
# 6) Histogramas / Densidade / Boxplots para numéricas
# ===========================
num_vars <- df %>%
  select(where(is.numeric)) %>%
  select(-any_of(c("Amonia_mgL","Cens"))) %>%
  names()

dir_hist <- file.path(out_dir, "histogramas")
dir_box  <- file.path(out_dir, "boxplots")
if (!dir.exists(dir_hist)) dir.create(dir_hist, recursive = TRUE)
if (!dir.exists(dir_box))  dir.create(dir_box,  recursive = TRUE)

for (var in num_vars) {
  # hist contagem
  h1 <- ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(bins = 30, color = "white", fill = "#90CAF9", alpha = 0.9) +
    labs(title = paste0("Distribuição de ", var),
         x = var, y = "Frequência") +
    theme_meu()
  ggsave_pdf(paste0("hist_", var, ".pdf"), h1)

  # hist densidade + curva
  h2 <- ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white", fill = "#BDE0FE") +
    geom_density(linewidth = 0.8) +
    labs(title = paste0("Densidade Empírica de ", var),
         x = var, y = "Densidade") +
    theme_meu()
  ggsave_pdf(paste0("histdens_", var, ".pdf"), h2)

  # boxplot
  h3 <- ggplot(df, aes(y = .data[[var]])) +
    geom_boxplot(fill = "#E0FBFC", color = "grey30", outlier.color = "#3A0CA3") +
    labs(title = paste0("Boxplot de ", var), x = NULL, y = var) +
    theme_meu()
  ggsave_pdf(paste0("box_", var, ".pdf"), h3)
}

# ===========================
# 7) EDA por Estação
# ===========================
df <- df %>% mutate(Estacao = as.factor(Estacao))

# 7.1 Histograma da amônia por estação
p5 <- ggplot(df, aes(x = Amonia_mgL)) +
  geom_histogram(bins = 30, color = "white", fill = "grey65", alpha = 0.95) +
  scale_x_continuous(trans = "log1p") +
  facet_wrap(~ Estacao, labeller = label_both) +
  labs(
    title = "Amônia por Estação de Monitoramento",
    subtitle = "Distribuições por estação (eixo log1p)",
    x = "Amônia (mg/L) [log1p]", y = "Frequência"
  ) +
  theme_meu()
ggsave_pdf("06_hist_amonia_estacao.pdf", p5)

# 7.2 Histograma com censura por estação
p6 <- df %>%
  mutate(Cens = factor(Cens, levels = c(0,1), labels = c("Censurado","Medido"))) %>%
  ggplot(aes(x = Amonia_mgL, fill = Cens)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6, color = "grey20") +
  scale_fill_manual(values = c("Censurado" = cores_status["0"], "Medido" = cores_status["1"])) +
  scale_x_continuous(trans = "log1p") +
  facet_wrap(~ Estacao, labeller = label_both) +
  labs(
    title = "Amônia por Estação e Status de Censura",
    subtitle = "Sobreposição por estação (eixo log1p)",
    x = "Amônia (mg/L) [log1p]", y = "Frequência", fill = "Status"
  ) +
  theme_meu()
ggsave_pdf("07_hist_amonia_censura_estacao.pdf", p6)

# 7.3 Boxplot da amônia por estação
p7 <- ggplot(df, aes(y = Amonia_mgL)) +
  geom_boxplot(outlier.color = "#7B2CBF") +
  scale_y_continuous(trans = "log1p") +
  facet_wrap(~ Estacao, labeller = label_both) +
  labs(
    title = "Dispersão da Amônia por Estação",
    subtitle = "Boxplots com eixo log1p",
    x = NULL, y = "Amônia (mg/L) [log1p]"
  ) +
  theme_meu()
ggsave_pdf("08_box_amonia_estacao.pdf", p7)

# 7.4 Boxplot por censura e estação
p8 <- df %>%
  mutate(Cens = factor(Cens, levels = c(0,1), labels = c("Censurado","Medido"))) %>%
  ggplot(aes(x = Cens, y = Amonia_mgL, fill = Cens)) +
  geom_boxplot(outlier.color = "grey25") +
  scale_fill_manual(values = c("Censurado" = "#F4A261", "Medido" = "#264653")) +
  scale_y_continuous(trans = "log1p") +
  facet_wrap(~ Estacao, labeller = label_both) +
  labs(
    title = "Amônia por Censura × Estação",
    subtitle = "Comparações condicionadas (eixo log1p)",
    x = NULL, y = "Amônia (mg/L) [log1p]", fill = NULL
  ) +
  theme_meu() +
  theme(legend.position = "bottom")
ggsave_pdf("09_box_amonia_censura_estacao.pdf", p8)

# ===========================
# 8) Outras numéricas por estação
# ===========================
for (var in num_vars) {
  # Hist por estação
  h4 <- ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(bins = 30, color = "white", fill = "#9BF6FF") +
    facet_wrap(~ Estacao, labeller = label_both) +
    labs(title = paste("Distribuição de", var, "por Estação"),
         x = var, y = "Frequência") +
    theme_meu()
  ggsave_pdf(paste0("hist_", var, "_estacao.pdf"), h4)

  # Densidade por estação
  h5 <- ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, color = "white", fill = "#A0C4FF") +
    geom_density(linewidth = 0.8) +
    facet_wrap(~ Estacao, labeller = label_both) +
    labs(title = paste("Densidade de", var, "por Estação"),
         x = var, y = "Densidade") +
    theme_meu()
  ggsave_pdf(paste0("dens_", var, "_estacao.pdf"), h5)

  # Box por estação
  h6 <- ggplot(df, aes(y = .data[[var]])) +
    geom_boxplot(fill = "#E6F2FF", color = "grey25", outlier.color = "#3F37C9") +
    facet_wrap(~ Estacao, labeller = label_both) +
    labs(title = paste("Boxplot de", var, "por Estação"),
         x = NULL, y = var) +
    theme_meu()
  ggsave_pdf(paste0("box_", var, "_estacao.pdf"), h6)
}

# ===========================
# 9) Correlações e pares
# ===========================
num_df <- df %>%
  select(where(is.numeric)) %>%
  select(-any_of("Cens"))

corr_df <- cor(num_df, use = "pairwise.complete.obs")

corr_plot <- ggcorrplot(
  corr_df, lab = TRUE, outline.color = "white",
  colors = c("#264653", "white", "#E76F51")
) +
  labs(title = "Mapa de Correlações (Pearson)") +
  theme_meu()

ggsave_pdf("10_correlacao.pdf", corr_plot)

pairplot <- ggpairs(
  num_df,
  upper = list(continuous = "cor"),
  lower = list(continuous = "points")
) +
  theme_meu()

ggsave_pdf("11_pairplot.pdf", pairplot, w = 10.5, h = 8)

# ===========================
# 10) Série temporal (amônia)
# ===========================
serie <- df %>%
  mutate(Cens = factor(Cens, levels = c(0,1), labels = c("Censurado","Medido"))) %>%
  ggplot(aes(x = Data, y = Amonia_mgL, color = Cens)) +
  geom_line(alpha = 0.45, linewidth = 0.5) +
  geom_point(size = 1.6, alpha = 0.85) +
  scale_color_manual(values = c("Censurado" = cores_status["0"], "Medido" = cores_status["1"])) +
  scale_y_continuous(trans = "log1p") +
  labs(
    title = "Evolução Temporal da Amônia",
    subtitle = "Pontos indicam medições individuais; eixo Y em log1p",
    x = "Tempo", y = "Amônia (mg/L) [log1p]", color = "Status"
  ) +
  theme_meu()

ggsave_pdf("12_serie_temporal_amonia.pdf", serie)


#======================================================================#
#                           3) TRATAMENTO                              #
#======================================================================#
df <- df %>% filter(!is.na(Amonia_mgL))

df1 <- df[df$Estacao == "UMATILLA",]
df2 <- df[df$Estacao == "GRAND COULEE",]
df3 <- df[df$Estacao == "VERNITA",]
df4 <- df[df$Estacao == "NORTHPORT",]
library(mice)
imp1 <- mice(df1, m = 5, method = "pmm", maxit = 10, seed = 555)
imp2 <- mice(df2, m = 5, method = "pmm", maxit = 10, seed =555)
imp3 <- mice(df3, m = 5, method = "pmm", maxit = 10, seed =555)
imp4 <- mice(df4, m = 5, method = "pmm", maxit = 10, seed = 555)
df1_new <- complete(imp1,1)
df2_new <- complete(imp2,1)
df3_new <- complete(imp3,1)
df4_new <- complete(imp4,1)

df_new <- rbind(df1_new, df2_new, df3_new, df4_new)

num_df_new <- df_new %>% 
  select(where(is.numeric)) %>% 
  select(-c("Amonia_mgL","Cens"))

#====================================================#
#               4 -  Modelagem                       #
#====================================================#

# --- pasta de saída fixa ---
base_dir    <- "C:/Users/User/Desktop/Result"
modelos_dir <- file.path(base_dir, "modelos")

if (!dir.exists(modelos_dir)) dir.create(modelos_dir, recursive = TRUE)


# --- normalização das preditoras ---
library(caret)
preproc_z <- preProcess(num_df_new, method = "center")
standardized_df_new <- predict(preproc_z, num_df_new)

# --- dados do modelo ---
X     <- as.matrix(cbind(1, standardized_df_new))  # intercepto
delta <- df_new$Cens                               # 1=observado, 0=censurado-left
Y     <- df_new$Amonia_mgL
Ypos  <- pmax(as.numeric(Y), .Machine$double.eps)
n     <- length(Ypos)
p     <- ncol(X)

# --- Weibull via survreg (baseline) ---
library(survival)
surv_obj <- Surv(time = Y, event = delta, type = "left")
survreg_weibull <- survreg(surv_obj ~ X - 1, dist = "weibull")
sink(file.path(modelos_dir, "summary_survreg_weibull.txt"))
print(summary(survreg_weibull))
sink()

# parâmetros úteis do weibull AFT
weib_sigma  <- survreg_weibull$scale
weib_shape  <- 1 / weib_sigma
weib_beta   <- coef(survreg_weibull)

SCALE_MIN <- 1e-8; SCALE_MAX <- 1e8
clamp     <- function(x, lo, hi) pmin(pmax(x, lo), hi)
safe_exp  <- function(z) clamp(exp(clamp(z, log(SCALE_MIN), log(SCALE_MAX))), SCALE_MIN, SCALE_MAX)

weib_lambda <- function(Xm) safe_exp(as.numeric(Xm %*% weib_beta))
F_weib      <- function(t, Xm) pweibull(t, shape = weib_shape, scale = weib_lambda(Xm))
qmed_weib   <- function(Xm) qweibull(0.5, shape = weib_shape, scale = weib_lambda(Xm))

# --- diagnóstico de colinearidade nas preditoras ---
if (!requireNamespace("car", quietly = TRUE)) install.packages("car", dependencies = TRUE)
library(car)
X_df <- as.data.frame(standardized_df_new)
vif_fit_weibull <- lm(scale(Y) ~ ., data = X_df)
vifs_weibull <- car::vif(vif_fit_weibull)
write.csv(data.frame(Variavel = names(vifs_weibull), VIF = as.numeric(vifs_weibull)),
          file.path(modelos_dir, "vif_covariaveis.csv"), row.names = FALSE)

#====================================================#
#     4.1 PDFs/CDFs base e NLLs (left-censor)       #
#====================================================#

BIG <- 1e25

dgamma_aft <- function(x, shape, scale) dgamma(x, shape=shape, scale=scale)
pgamma_aft <- function(x, shape, scale) pgamma(x, shape=shape, scale=scale)

dlomax_base <- function(x, shape, scale) {
  z <- 1 + x/scale
  fx <- (shape/scale) * z^(-(shape + 1))
  fx[x <= 0] <- 0; fx[!is.finite(fx)] <- 0
  fx
}
plomax_base <- function(x, shape, scale) {
  z <- 1 + x/scale
  Fx <- 1 - z^(-shape)
  Fx[x <= 0] <- 0; clamp(Fx, 0, 1)
}

dnakagami_base <- function(x, m, omega) {
  logc <- log(2) + m*log(m) - lgamma(m) - m*log(omega)
  lx   <- log(pmax(x, .Machine$double.xmin))
  lpdf <- logc + (2*m - 1)*lx - (m/omega)*x^2
  fx <- exp(lpdf); fx[x <= 0] <- 0; fx[!is.finite(fx)] <- 0
  fx
}
pnakagami_base <- function(x, m, omega) {
  Fx <- pgamma(x^2, shape=m, scale=omega/m)
  Fx[x <= 0] <- 0; clamp(Fx, 0, 1)
}

# --- NLLs para censura à esquerda: delta=1 observado; 0=censurado ---
nll_gamma <- function(par) {
  log_k <- par[1]; beta <- par[-1]
  k <- exp(log_k); if (!is.finite(k) || k<=0) return(BIG)
  scale_i <- safe_exp(as.numeric(X %*% beta))
  f <- dgamma_aft(Ypos, shape=k, scale=scale_i)
  F <- pgamma_aft(Ypos, shape=k, scale=scale_i)
  if (!all(is.finite(f)) || !all(is.finite(F))) return(BIG)
  f <- pmax(f, .Machine$double.xmin)
  F <- pmin(pmax(F, 1e-12), 1-1e-12)
  -sum(delta*log(f) + (1-delta)*log(F))
}

nll_lomax <- function(par) {
  log_alpha <- par[1]; beta <- par[-1]
  alpha <- exp(log_alpha); if (!is.finite(alpha) || alpha<=0) return(BIG)
  lambda_i <- safe_exp(as.numeric(X %*% beta))
  f <- dlomax_base(Ypos, shape=alpha, scale=lambda_i)
  F <- plomax_base (Ypos, shape=alpha, scale=lambda_i)
  if (!all(is.finite(f)) || !all(is.finite(F))) return(BIG)
  f <- pmax(f, .Machine$double.xmin)
  F <- pmin(pmax(F, 1e-12), 1-1e-12)
  -sum(delta*log(f) + (1-delta)*log(F))
}

nll_nakagami <- function(par) {
  theta <- par[1]; beta <- par[-1]
  m <- 0.5 + exp(theta); if (!is.finite(m) || m<=0.5) return(BIG)
  omega_i <- safe_exp(as.numeric(X %*% beta))
  f <- dnakagami_base(Ypos, m=m, omega=omega_i)
  F <- pnakagami_base (Ypos, m=m, omega=omega_i)
  if (!all(is.finite(f)) || !all(is.finite(F))) return(BIG)
  f <- pmax(f, .Machine$double.xmin)
  F <- pmin(pmax(F, 1e-12), 1-1e-12)
  -sum(delta*log(f) + (1-delta)*log(F))
}

# --- otimizador robusto ---
fit_robust <- function(par0, fn) {
  out <- tryCatch(optim(par0, fn, method="Nelder-Mead",
                        control=list(maxit=10000, reltol=1e-8), hessian=TRUE),
                  error=function(e) e)
  if (inherits(out,"error") || !is.finite(out$value)) {
    out <- tryCatch(optim(par0, fn, method="L-BFGS-B",
                          lower=rep(-50, length(par0)), upper=rep(50, length(par0)),
                          control=list(maxit=15000, factr=1e7), hessian=TRUE),
                    error=function(e) e)
  }
  if (inherits(out,"error") || !is.list(out) || !is.finite(out$value)) return(NULL)
  out
}

# --- ajustes ---
fit_gamma <- fit_robust(c(log(1), rep(0, p)), nll_gamma)
fit_lom   <- fit_robust(c(log(1), rep(0, p)), nll_lomax)
fit_nak   <- fit_robust(c(log(1), rep(0, p)), nll_nakagami)

sink(file.path(modelos_dir, "summaries_optim.txt"))
cat("GAMMA:\n"); if (!is.null(fit_gamma)) print(fit_gamma) else cat("Falhou\n")
cat("\nLOMAX:\n"); if (!is.null(fit_lom))   print(fit_lom)   else cat("Falhou\n")
cat("\nNAKAGAMI:\n"); if (!is.null(fit_nak)) print(fit_nak)  else cat("Falhou\n")
sink()
 

# --- montar tabela ICs de forma limpa e correta ---
partes <- list(
  data.frame(distribuicao = "weibull",  logLik = ll_weib,  k = k_weib)
)

if (!is.null(fit_gamma))  partes <- c(partes, list(data.frame(distribuicao="gamma",   logLik=gam_loglik, k=gam_k)))
if (!is.null(fit_lom))    partes <- c(partes, list(data.frame(distribuicao="lomax",   logLik=lom_loglik, k=lom_k)))
if (!is.null(fit_nak))    partes <- c(partes, list(data.frame(distribuicao="nakagami",logLik=nak_loglik, k=nak_k)))

tab_ic <- dplyr::bind_rows(partes) %>%
  dplyr::mutate(
    AIC  = 2*k - 2*logLik,
    BIC  = log(n)*k - 2*logLik,
    AICc = ifelse(n > k + 1, AIC + (2*k*(k+1))/(n - k - 1), NA_real_)
  ) %>%
  dplyr::arrange(AIC)

write.csv(tab_ic, file.path(modelos_dir, "aic_bic_table.csv"), row.names = FALSE)
print(tab_ic)



# salvar e mostrar
write.csv(tab_ic, file.path(modelos_dir, "aic_bic_table.csv"), row.names = FALSE)
print(tab_ic)

 #====================================================#
#    5 GRÁFICOS por distribuição
#====================================================#

# helper de salvamento
save_pdf <- function(nome, plt, w = 7, h = 5){
  ggsave(file.path(modelos_dir, nome), plot = plt, width = w, height = h, device = cairo_pdf)
}

# garante clamps consistentes
clamp01 <- function(z) pmin(pmax(z, 1e-12), 1 - 1e-12)

# checa quais F(t|x) e quantis medianos estão disponíveis
model_list <- list(
  list(name = "weibull",  Ffun = if (exists("F_weib"))  F_weib  else NULL,
       qmed = if (exists("qmed_weib")) qmed_weib else NULL)
)

if (exists("F_gamma")   && exists("qmed_gamma"))   model_list <- c(model_list, list(list(name="gamma",    Ffun=F_gamma,   qmed=qmed_gamma)))
if (exists("F_lomax")   && exists("qmed_lomax"))   model_list <- c(model_list, list(list(name="lomax",    Ffun=F_lomax,   qmed=qmed_lomax)))
if (exists("F_nak")     && exists("qmed_nak"))     model_list <- c(model_list, list(list(name="nakagami", Ffun=F_nak,     qmed=qmed_nak)))

# status de observação (delta=1 observado)
is_exact <- (delta == 1)

for (mdl in model_list){
  if (is.null(mdl$Ffun) || is.null(mdl$qmed)) next
  nm   <- mdl$name
  Ffun <- mdl$Ffun
  qmed <- mdl$qmed

  # ---------- 1) Obs vs Ajustado (mediana prevista) ----------
  pred_med <- qmed(X)
  df_fit <- data.frame(Y = Ypos, pred = pred_med, exact = is_exact)
  cor_exatas <- with(subset(df_fit, exact),
                     suppressWarnings(cor(log10(pmax(Y, .Machine$double.eps)),
                                          log10(pmax(pred, .Machine$double.eps)),
                                          use="complete.obs")))
  g1 <- ggplot(subset(df_fit, exact), aes(x = pred, y = Y)) +
    geom_point(alpha = .75) +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(trans = "log10") +
    labs(title    = paste("Obs vs Ajustado —", nm),
         subtitle = paste0("Correlação em log10 (exatos): ", round(cor_exatas, 3)),
         x = "Ajustado (mediana prevista)", y = "Observado") +
    theme_minimal()
  save_pdf(sprintf("01_obs_vs_ajustado_%s.pdf", nm), g1)

  # ---------- 2) Cox–Snell (reverso) ----------
  Fi   <- clamp01(Ffun(Ypos, X))          # CDF(t|x) ∈ (0,1)
  r_cs <- -log(Fi)                        # transform reverso
  cs_fit <- survfit(Surv(r_cs, delta, type = "left") ~ 1)
  cs_sum <- summary(cs_fit)
  df_cs  <- data.frame(r = cs_sum$time, Fhat = cs_sum$surv)
  r_grid <- seq(0, max(df_cs$r, na.rm = TRUE), length.out = 200)
  df_exp <- data.frame(r = r_grid, Fexp = 1 - exp(-r_grid))

  g2 <- ggplot() +
    geom_step(data = df_cs, aes(x = r, y = Fhat), direction = "hv") +
    geom_line(data = df_exp, aes(x = r, y = Fexp), linetype = 2) +
    labs(title    = paste("Cox–Snell (reverso) —", nm),
         subtitle = "Tracejado: F teórica Exp(1) = 1 - exp(-r)",
         x = "r (Cox–Snell reverso)", y = "F(r)") +
    theme_minimal()
  save_pdf(sprintf("02_coxsnell_%s.pdf", nm), g2)

  # ---------- 3) KM-left (F̂) vs CDF média do modelo ----------
  km_fit <- survfit(Surv(Ypos, delta, type = "left") ~ 1)
  km_sum <- summary(km_fit)
  df_km  <- data.frame(time = km_sum$time, Fhat = km_sum$surv)  # para left, surv = F(t)

  t_grid <- seq(min(Ypos, na.rm = TRUE), max(Ypos, na.rm = TRUE), length.out = 200)
  F_model <- sapply(t_grid, function(tt) mean(Ffun(tt, X), na.rm = TRUE))
  df_mod  <- data.frame(time = t_grid, Fmodel = F_model)

  g3 <- ggplot() +
    geom_step(data = df_km,  aes(x = time, y = Fhat),   direction = "hv") +
    geom_line(data = df_mod, aes(x = time, y = Fmodel)) +
    labs(title    = paste("CDF empírica (KM-left) vs CDF média —", nm),
         subtitle = "Escada: F̂_KM(t) | Linha: média de F_model(t|x)",
         x = "t", y = "F(t)") +
    theme_minimal()
  save_pdf(sprintf("03_km_vs_modelo_%s.pdf", nm), g3)

  # ---------- 4) QQ-plot de Cox–Snell (somente observados) ----------
  r_ex <- r_cs[is_exact]
  r_ex <- r_ex[is.finite(r_ex) & r_ex > 0]
  if (length(r_ex) >= 10) {
    g4 <- ggplot(data.frame(x = r_ex), aes(sample = x)) +
      stat_qq(distribution = stats::qexp) +
      stat_qq_line(distribution = stats::qexp) +
      labs(title = paste("QQ-plot Cox–Snell (exatos) —", nm),
           x = "Quantis teóricos Exp(1)", y = "Quantis amostrais") +
      theme_minimal()
    save_pdf(sprintf("04_qq_coxsnell_%s.pdf", nm), g4, w = 6, h = 5)
  }
}

# ===== Exporta tabelas finais para XLSX (em C:\Users\User\Desktop\Result\modelos) =====
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
library(writexl)

# nomes das variáveis (mesmos de X = cbind(1, standardized_df_new))
var_names <- c("(Intercept)", colnames(standardized_df_new))

# betas por modelo
weib_beta <- as.numeric(coef(survreg_weibull)); names(weib_beta) <- var_names
weib_sigma <- survreg_weibull$scale; weib_shape <- 1/weib_sigma

gam_beta <- rep(NA_real_, length(var_names)); gam_shape <- NA_real_
if (!is.null(fit_gamma)) { gam_shape <- exp(fit_gamma$par[1]); b <- fit_gamma$par[-1]; names(b) <- var_names; gam_beta <- b }

lom_beta <- rep(NA_real_, length(var_names)); lom_alpha <- NA_real_
if (!is.null(fit_lom))   { lom_alpha <- exp(fit_lom$par[1]); b <- fit_lom$par[-1]; names(b) <- var_names; lom_beta <- b }

nak_beta <- rep(NA_real_, length(var_names)); nak_m <- NA_real_
if (!is.null(fit_nak))   { nak_m <- 0.5 + exp(fit_nak$par[1]); b <- fit_nak$par[-1]; names(b) <- var_names; nak_beta <- b }

# tabela larga (coeficientes)
tab_params_wide <- tibble::tibble(
  Variavel      = var_names,
  Weibull       = as.numeric(weib_beta),
  Gamma         = as.numeric(gam_beta),
  Lomax         = as.numeric(lom_beta),
  Nakagami      = as.numeric(nak_beta)
)

# tabela longa
tab_params_long <- tab_params_wide |>
  tidyr::pivot_longer(-Variavel, names_to = "Modelo", values_to = "Coeficiente") |>
  arrange(Variavel, Modelo)

# hiperparâmetros e ICs (AIC/BIC)
ll_weib <- as.numeric(logLik(survreg_weibull))
ll_gam  <- if (!is.null(fit_gamma))  -as.numeric(fit_gamma$value) else NA_real_
ll_lom  <- if (!is.null(fit_lom))    -as.numeric(fit_lom$value)   else NA_real_
ll_nak  <- if (!is.null(fit_nak))    -as.numeric(fit_nak$value)   else NA_real_

n <- length(Y)
k_weib <- length(weib_beta) + 1
k_gam  <- if (!is.null(fit_gamma)) length(fit_gamma$par) else NA_integer_
k_lom  <- if (!is.null(fit_lom))   length(fit_lom$par)   else NA_integer_
k_nak  <- if (!is.null(fit_nak))   length(fit_nak$par)   else NA_integer_

tab_hyper <- tibble::tibble(
  Modelo = c("Weibull","Gamma","Lomax","Nakagami"),
  Hiper  = c(weib_shape, gam_shape, lom_alpha, nak_m),
  logLik = c(ll_weib, ll_gam, ll_lom, ll_nak),
  k      = c(k_weib,  k_gam,  k_lom,  k_nak),
  AIC    = 2*k - 2*logLik,
  BIC    = log(n)*k - 2*logLik
)

# correlação entre vetores de betas (sem intercepto)
no_interc <- tab_params_wide |>
  filter(Variavel != "(Intercept)") |>
  select(-Variavel)
no_interc <- no_interc[, colSums(!is.na(no_interc)) > 0, drop = FALSE]
cor_models <- if (ncol(no_interc) >= 2) cor(no_interc, use = "pairwise.complete.obs") else matrix(NA_real_, 0, 0)
cor_df <- if (length(cor_models)) {
  df <- as.data.frame(as.table(cor_models)); names(df) <- c("Modelo_X","Modelo_Y","Cor"); df
} else {
  data.frame(Modelo_X=character(), Modelo_Y=character(), Cor=numeric())
}

# ranking de variáveis por |beta| médio e sinal consistente
tab_rank <- tab_params_wide |>
  filter(Variavel != "(Intercept)") |>
  mutate(
    media_abs = rowMeans(abs(across(where(is.numeric))), na.rm = TRUE),
    sinal_consistente = ifelse(
      apply(across(where(is.numeric)), 1, function(z){ z <- z[is.finite(z)]; length(z)>0 && (all(z>0) || all(z<0)) }),
      "Sim","Não"
    )
  ) |>
  arrange(desc(media_abs))

# arquivo xlsx
if (!dir.exists(modelos_dir)) dir.create(modelos_dir, recursive = TRUE)
xlsx_path <- file.path(modelos_dir, "tabelas_modelos.xlsx")
writexl::write_xlsx(
  list(
    parametros_largo = tab_params_wide,
    parametros_longo = tab_params_long,
    hiperparametros  = tab_hyper,
    correlacao_betas = cor_df,
    ranking_vars     = tab_rank
  ),
  path = xlsx_path
)

cat("OK:", xlsx_path, "\n")





