librerias <- c("tidyverse", "readxl","data.table",
               "knitr", "tables", "kableExtra",
               "moments", "ggplot2", "LSMRealOptions",
               "knitr", "tseries", "nortest", "patchwork", "dplyr",
               "quantmod", "tidyquant", "descr", "summarytools","MASS"
               ,"ggthemes","zoo","purrr","gridExtra","grid","tidyr","tibble","ggrepel")
for (pkg in librerias)
{
  if (!(pkg %in% installed.packages()))
    install.packages(pkg, repos = "http://cran.r-project.org")
  
  library(pkg, character.only = TRUE)
}
# 4. Resultados

# Descargar cada índice individualmente 
ibex35 <- tq_get("^IBEX", from = "2020-01-02", to = "2025-05-03") %>%
  dplyr::rename(Fecha = date, IBEX35 = close) %>%
  dplyr::select(Fecha, IBEX35)

stoxx50 <- tq_get("^STOXX50E", from = "2020-01-02", to = "2025-05-03") %>%
  dplyr::rename(Fecha = date, EUROSTOXX50 = close) %>%
  dplyr::select(Fecha, EUROSTOXX50)

nasdaq100 <- tq_get("^NDX", from = "2020-01-02", to = "2025-05-03") %>%
  dplyr::rename(Fecha = date, NASDAQ100 = close) %>%
  dplyr::select(Fecha, NASDAQ100)

hangseng <- tq_get("^HSI", from = "2020-01-02", to = "2025-05-03") %>%
  dplyr::rename(Fecha = date, HANGSENG = close) %>%
  dplyr::select(Fecha, HANGSENG)

nikkei225 <- tq_get("^N225", from = "2020-01-02", to = "2025-05-03") %>%
  dplyr::rename(Fecha = date, NIKKEI225 = close) %>%
  dplyr::select(Fecha, NIKKEI225)

gs_ibex <- ggplot(ibex35, aes(x = Fecha, y = IBEX35)) +
  geom_line(color = "steelblue") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab("Fecha") +
  ylab("Precio de cierre") +
  labs(title = "IBEX35") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"))

gs_stoxx <- ggplot(stoxx50, aes(x = Fecha, y = EUROSTOXX50)) +
  geom_line(color = "steelblue") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab("Fecha") +
  ylab("Precio de cierre") +
  labs(title = "EURO STOXX 50") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"))

gs_nasdaq <-ggplot(nasdaq100, aes(x = Fecha, y = NASDAQ100)) +
  geom_line(color = "steelblue") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab("Fecha") +
  ylab("Precio de cierre") +
  labs(title = "NASDAQ 100") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"))

gs_hangseng <- ggplot(hangseng, aes(x = Fecha, y = HANGSENG)) +
  geom_line(color = "steelblue") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab("Fecha") +
  ylab("Precio de cierre") +
  labs(title = "HANG SENG") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"))

gs_nikkei <- ggplot(nikkei225, aes(x = Fecha, y = NIKKEI225)) +
  geom_line(color = "steelblue") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab("Fecha") +
  ylab("Precio de cierre") +
  labs(title = "NIKKEI 225") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"))

gs_ibex
gs_stoxx
gs_nasdaq


# Función para normalizar precios a base 100
normalizar_precios <- function(df, precio_col) {
  df %>%
    mutate(Normalizado = 100 * .data[[precio_col]] / first(.data[[precio_col]]))
}

# Aplicar a cada índice
ibex35_n <- normalizar_precios(ibex35, "IBEX35")
stoxx50_n <- normalizar_precios(stoxx50, "EUROSTOXX50")
nasdaq100_n <- normalizar_precios(nasdaq100, "NASDAQ100")
hangseng_n <- normalizar_precios(hangseng, "HANGSENG")
nikkei225_n <- normalizar_precios(nikkei225, "NIKKEI225")


gsn_ibex <- ggplot(ibex35_n, aes(x = Fecha, y = Normalizado)) +
  geom_line(color = "steelblue") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab("Fecha") +
  ylab("Precio normalizado") +
  labs(title = "IBEX35") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"))

gsn_stoxx <- ggplot(stoxx50_n, aes(x = Fecha, y = Normalizado)) +
  geom_line(color = "steelblue") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab("Fecha") +
  ylab("Precio normalizado") +
  labs(title = "EURO STOXX 50") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"))

gsn_nasdaq <- ggplot(nasdaq100_n, aes(x = Fecha, y = Normalizado)) +
  geom_line(color = "steelblue") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab("Fecha") +
  ylab("Precio normalizado") +
  labs(title = "NASDAQ 100") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"))

gsn_hangseng <- ggplot(hangseng_n, aes(x = Fecha, y = Normalizado)) +
  geom_line(color = "steelblue") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab("Fecha") +
  ylab("Precio normalizado") +
  labs(title = "HANG SENG") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"))

gsn_nikkei <- ggplot(nikkei225_n, aes(x = Fecha, y = Normalizado)) +
  geom_line(color = "steelblue") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab("Fecha") +
  ylab("Precio normalizado") +
  labs(title = "NIKKEI 225") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"))

gsn_ibex
gsn_nikkei
gsn_nasdaq

# Lista nombrada de data frames
indices_lista <- list(
  IBEX35 = ibex35,
  EUROSTOXX50 = stoxx50,
  NASDAQ100 = nasdaq100,
  HANGSENG = hangseng,
  NIKKEI225 = nikkei225
)

# Aplicar descr() y convertir a data.frame
stats_lista <- map(indices_lista, ~ 
                     as.data.frame(descr(.x, transpose = FALSE)) %>% 
                     mutate_if(is.numeric, ~round(.x, 3))
)
estadisticos <- rownames(stats_lista[[1]])

# Unir todas las columnas en una tabla
tabla_estadistica <- stats_lista %>%
  map_dfc(~ .x[[1]]) %>%
  setNames(names(indices_lista)) %>%
  mutate(Estadístico = estadisticos) %>%
  relocate(Estadístico)

# Convertir toda la tabla a character (menos la columna de nombres de estadísticos)
tabla_estadistica_char <- tabla_estadistica
tabla_estadistica_char[,-1] <- tabla_estadistica_char[,-1] %>%
  mutate(across(everything(), ~ formatC(.x, format = "f", digits = 3)))  # mantiene formato decimal

# Convertir las dos últimas filas a enteros sin decimales
filas_a_convertir <- tabla_estadistica_char$Estadístico %in% c("N.Valid", "Pct.Valid")
tabla_estadistica_char[filas_a_convertir, -1] <- tabla_estadistica_char[filas_a_convertir, -1] %>%
  mutate(across(everything(), ~ as.character(as.integer(as.numeric(.x)))))

tabla_estadistica_char 

#Calcular los retornos logarítmicos
r_log <- diff(log(ibex35$IBEX35))  # logaritmo de la diferencia de precios
r_log <- c(NA, r_log)  # Añadir NA al primer valor, ya que no hay retorno logarítmico para el primer precio

# Crear el data frame con las fechas ajustadas y los retornos calculados en 'r'
retornos_log_ibex <- data.frame(Fecha = ibex35$Fecha[-1], Retornos_Log = r_log[-1])
retornos_log_ibex <- retornos_log_ibex %>% filter(!is.na(Retornos_Log))# Filtrar cualquier NA en la columna de Retornos

r_log_1 <- diff(log(stoxx50$EUROSTOXX50))  
r_log_1 <- c(NA, r_log_1)  
retornos_log_stoxx <- data.frame(Fecha = stoxx50$Fecha[-1], Retornos_Log = r_log_1[-1])
retornos_log_stoxx <- retornos_log_stoxx %>% filter(!is.na(Retornos_Log))


r_log_2 <- diff(log(nasdaq100$NASDAQ100))  
r_log_2 <- c(NA, r_log_2)  
retornos_log_nasdaq <- data.frame(Fecha = nasdaq100$Fecha[-1], Retornos_Log = r_log_2[-1])
retornos_log_nasdaq <- retornos_log_nasdaq %>% filter(!is.na(Retornos_Log))

r_log_3 <- diff(log(hangseng$HANGSENG))  
r_log_3 <- c(NA, r_log_3)  
retornos_log_hangseng <- data.frame(Fecha = hangseng$Fecha[-1], Retornos_Log = r_log_3[-1])
retornos_log_hangseng <- retornos_log_hangseng %>% filter(!is.na(Retornos_Log))

r_log_4 <- diff(log(nikkei225$NIKKEI225))  
r_log_4 <- c(NA, r_log_4)  
retornos_log_nikkei225 <- data.frame(Fecha = nikkei225$Fecha[-1], Retornos_Log = r_log_4[-1])
retornos_log_nikkei225 <- retornos_log_nikkei225 %>% filter(!is.na(Retornos_Log))

# Gráfico 1: IBEX 35
graf_ibex <- ggplot(retornos_log_ibex, aes(x = Fecha)) + 
  geom_line(aes(y = Retornos_Log), color = "firebrick") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab("Fecha") + ylab("Retornos logarítmicos") +
  labs(title = "IBEX 35") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )

# Gráfico 2: EURO STOXX 50
graf_stoxx <- ggplot(retornos_log_stoxx, aes(x = Fecha)) + 
  geom_line(aes(y = Retornos_Log), color = "firebrick") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab("Fecha") + ylab("Retornos logarítmicos") +
  labs(title = "EURO STOXX 50") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )

#Gráfico 3: Nasdaq 100
graf_nasdaq <- ggplot(retornos_log_nasdaq, aes(x = Fecha)) + 
  geom_line(aes(y = Retornos_Log), color = "firebrick") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab("Fecha") + ylab("Retornos logarítmicos") +
  labs(title = "NASDAQ 100") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )

# Grafico 4 HANG SENG
graf_hangseng <- ggplot(retornos_log_hangseng, aes(x = Fecha)) + 
  geom_line(aes(y = Retornos_Log), color = "firebrick") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab("Fecha") + ylab("Retornos logarítmicos") +
  labs(title = "HANG SENG") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )

# Graficar 5 NIKKEI 225
graf_nikkei <- ggplot(retornos_log_nikkei225, aes(x = Fecha)) + 
  geom_line(aes(y = Retornos_Log), color = "firebrick") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab("Fecha") + ylab("Retornos logarítmicos") +
  xlab("Fecha") + ylab("Retornos logarítmicos") +
  labs(title = "NIKKEI 225") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )
graf_ibex
graf_stoxx
graf_nasdaq

# Lista nombrada de data frames
indices_lista <- list(
  IBEX35 = retornos_log_ibex$Retornos_Log,
  EUROSTOXX50 = retornos_log_stoxx$Retornos_Log,
  NASDAQ100 = retornos_log_nasdaq$Retornos_Log,
  HANGSENG = retornos_log_hangseng$Retornos_Log,
  NIKKEI225 = retornos_log_nikkei225$Retornos_Log
)

# Aplicar descr() y convertir a data.frame
stats_lista <- map(indices_lista, ~ 
                     as.data.frame(descr(.x, transpose = FALSE)) %>% 
                     mutate_if(is.numeric, ~round(.x, 5))
)

# Obtener los nombres de estadísticos (iguales en todos)
estadisticos <- rownames(stats_lista[[1]])

# Unir todas las columnas en una tabla
tabla_estadistica_ret <- stats_lista %>%
  map_dfc(~ .x[[1]]) %>%
  setNames(names(indices_lista)) %>%
  mutate(Estadístico = estadisticos) %>%
  relocate(Estadístico)

tabla_estadistica_ret_char <- tabla_estadistica_ret

tabla_estadistica_ret_char[,-1] <- tabla_estadistica_ret_char[,-1] %>%
  mutate(across(everything(), ~ formatC(.x, format = "f", digits = 5)))

filas_finales <- tabla_estadistica_ret_char$Estadístico %in% c("N.Valid", "Pct.Valid")
tabla_estadistica_ret_char[filas_finales, -1] <- tabla_estadistica_ret_char[filas_finales, -1] %>%
  mutate(across(everything(), ~ as.character(as.integer(as.numeric(.x)))))

tabla_estadistica_ret_char 

#Cálculo de mu y sigma 
mu_ibex <- mean(r_log[-1])
sigma_ibex <- sd(r_log[-1])

mu_stoxx <- mean(r_log_1[-1])
sigma_stoxx <- sd(r_log_1[-1])

mu_nasdaq <- mean(r_log_2[-1])
sigma_nasdaq <- sd(r_log_2[-1])

mu_hangseng <- mean(r_log_3[-1])
sigma_hangseng <- sd(r_log_3[-1])

mu_nikkei <- mean(r_log_4[-1])
sigma_nikkei <- sd(r_log_4[-1])


#IBEX35 
hist(r_log[-1], breaks = 50, freq = FALSE,
     xlab = "Retornos logarítmicos", ylab = "Densidad", main = "IBEX 35",
     col = "grey90", border = "black",
     xlim = c(-0.15, 0.1), ylim = c(0, 45))
lines(density(r_log[-1]), col = "blue", lwd = 2)
lines(seq(-0.06,0.06,0.000001), dnorm(seq(-0.06,0.06,0.000001),mu_ibex, sigma_ibex),type = "l", col = "red", lwd=2)
legend("topright", legend = c("Densidad estimada", "Densidad Normal"),
       col = c("blue", "red"), lwd = 2, cex = 2)

#EUROSTOXX50
hist(r_log_1[-1], breaks = 50, freq = FALSE,
     xlab = "Retornos logarítmicos", ylab = "Densidad", main = "EURO STOXX 50",
     col = "grey90", border = "black",
     xlim = c(-0.15, 0.1), ylim = c(0, 45))
lines(density(r_log_1[-1]), col = "blue", lwd = 2)
lines(seq(-0.06,0.06,0.000001), dnorm(seq(-0.06,0.06,0.000001),mu_stoxx, sigma_stoxx), 
      type = "l", col = "red", lwd=2)
legend("topright", legend = c("Densidad estimada", "Densidad Normal"),
       col = c("blue", "red"), lwd = 2, cex = 2)

#Nasdaq 100
hist(r_log_2[-1], breaks = 50, freq = FALSE,
     xlab = "Retornos logarítmicos", ylab = "Densidad", main = "NASDAQ 100",
     col = "grey90", border = "black",
     xlim = c(-0.15, 0.1), ylim = c(0, 45))
lines(density(r_log_2[-1]), col = "blue", lwd = 2)
lines(seq(-0.06,0.06,0.000001), dnorm(seq(-0.06,0.06,0.000001),mu_nasdaq, sigma_nasdaq),type = "l", col = "red", lwd=2)
legend("topright", legend = c("Densidad estimada", "Densidad Normal"),
       col = c("blue", "red"), lwd = 2, cex = 2)

#HANGSENG
hist(r_log_3[-1], breaks = 50, freq = FALSE,
     xlab = "Retornos logarítmicos", ylab = "Densidad", main = " HANG SENG",
     col = "grey90", border = "black",
     xlim = c(-0.15, 0.1), ylim = c(0, 45))
lines(density(r_log_3[-1]), col = "blue", lwd = 2)
lines(seq(-0.06,0.06,0.000001), dnorm(seq(-0.06,0.06,0.000001),mu_hangseng, sigma_hangseng),type = "l", col = "red", lwd=2)
legend("topright", legend = c("Densidad estimada", "Densidad Normal"),
       col = c("blue", "red"), lwd = 2, cex = 2)

#Nikkei225
hist(r_log_4[-1], breaks = 50, freq = FALSE,
     xlab = "Retornos logarítmicos", ylab = "Densidad", main = " NIKKEI 225",
     col = "grey90", border = "black",
     xlim = c(-0.15, 0.1), ylim = c(0, 45))
lines(density(r_log_4[-1]), col = "blue", lwd = 2)
lines(seq(-0.06,0.06,0.000001), dnorm(seq(-0.06,0.06,0.000001),mu_nikkei, sigma_nikkei),type = "l", col = "red", lwd=2)
legend("topright", legend = c("Densidad estimada", "Densidad Normal"),
       col = c("blue", "red"), lwd = 2, cex = 2)



qqnorm(r_log[-1],
       xlab = "Quantiles teóricos", ylab = "Quantiles log-retornos",
       main = "IBEX 35", pch = 19, col = "steelblue", cex = 1.1)
qqline(r_log[-1], col = "red", lwd = 4)

qqnorm(r_log_1[-1],
       xlab = "Quantiles teóricos", ylab = "Quantiles log-retornos",
       main = "EURO STOXX 50", pch = 19, col = "steelblue", cex = 1.1)
qqline(r_log_1[-1], col = "red", lwd = 4)

qqnorm(r_log_2[-1],
       xlab = "Quantiles teóricos", ylab = "Quantiles log-retornos",
       main = "NASDAQ 100", pch = 19, col = "steelblue", cex = 1.1)
qqline(r_log_2[-1], col = "red", lwd = 4)

qqnorm(r_log_3[-1],
       xlab = "Quantiles teóricos", ylab = "Quantiles log-retornos",
       main = "HANG SENG", pch = 19, col = "steelblue", cex = 1.1)
qqline(r_log_3[-1], col = "red", lwd = 4)

qqnorm(r_log_4[-1],
       xlab = "Quantiles teóricos", ylab = "Quantiles log-retornos",
       main = "NIKKEI 225", pch = 19, col = "steelblue", cex = 1.1)
qqline(r_log_4[-1], col = "red", lwd = 4)


hacer_pruebas_normalidad <- function(nombre, retornos) {
  shapiro <- shapiro.test(retornos)
  jb      <- jarque.bera.test(retornos)
  ad      <- ad.test(retornos)
  
  data.frame(
    Indice = nombre,
    "Shapiro test" = shapiro$p.value,
    "Jarque Bera test" = jb$p.value,
    "Anderson Darling"   = ad$statistic,
    "P valor AD"  = case_when(
      ad$statistic > 1.035 ~ "***", # 0.1%
      ad$statistic > 1.035 ~ "**",  # 1%
      ad$statistic > 0.752 ~ "*",   # 5%
      TRUE ~ "ns"
    ))
}

resultados_normalidad <- dplyr::bind_rows(
  hacer_pruebas_normalidad("IBEX35",retornos_log_ibex$Retornos_Log),
  hacer_pruebas_normalidad("EUROSTOXX50", retornos_log_stoxx$Retornos_Log),
  hacer_pruebas_normalidad("NASDAQ100", retornos_log_nasdaq$Retornos_Log),
  hacer_pruebas_normalidad("HANGSENG", retornos_log_hangseng$Retornos_Log),
  hacer_pruebas_normalidad("NIKKEI225", retornos_log_nikkei225$Retornos_Log)
)

tabla_normalidad <- resultados_normalidad %>%
  rename(
    Shapiro_test = `Shapiro.test`,
    JB_test      = `Jarque.Bera.test`,
    AD_stat      = `Anderson.Darling`,
    AD_sig_5     = `P.valor.AD`
  ) %>%
  mutate(
    Shapiro_test = formatC(as.numeric(Shapiro_test), format = "e", digits = 2),
    JB_test      = formatC(as.numeric(JB_test), digits = 4),
    AD_stat      = sprintf("%.3f", as.numeric(AD_stat))
  )

colnames(tabla_normalidad) <- c("Indice", "Shapiro test", "Jarque Bera test", "Anderson Darling stat", "P-valor AD")

tabla_normalidad 


mu_ibex_anual <- mean(r_log[-1])*256
sigma_ibex_anual <- sd(r_log[-1])*sqrt(256)

mu_stoxx_anual <- mean(r_log_1[-1])*252
sigma_stoxx_anual <- sd(r_log_1[-1])*sqrt(252)

mu_nasdaq_anual <- mean(r_log_2[-1])*251
sigma_nasdaq_anual <- sd(r_log_2[-1])* sqrt(251)

mu_hangseng_anual <- mean(r_log_3[-1])*245
sigma_hangseng_anual <- sd(r_log_3[-1])*sqrt(245)

mu_nikkei_anual <- mean(r_log_4[-1])*244
sigma_nikkei_anual <- sd(r_log_4[-1])*sqrt(244)

#MBG
simular_mbg_t <- function(datos, nombre_columna_precio, mu, sigma, v, dias, T = 5.34) {
  N <- dias
  dt <- T / N
  S0 <- datos[[nombre_columna_precio]][1]
  
  df <- data.frame(t = numeric(dias + 1), 
                   Svalor = numeric(dias + 1), 
                   DeltaW = numeric(dias + 1))
  
  df[1, ] <- c(0, S0, 0)
  S <- S0
  
  for (i in 2:(dias + 1)) {
    t_actual <- (i - 1) * dt
    DeltaW <- (rt(1, df = v) / sqrt(v / (v - 2))) * sqrt(dt)
    S <- S * exp((mu - 0.5 * sigma^2) * dt + sigma * DeltaW)
    df[i, ] <- c(t_actual, S, DeltaW)
  }
  return(df[-1,])
}

#SIMULACIONES A TRAVÉS DEL MBG
generar_simulaciones_bucle <- function(
    datos, nombre_columna_precio, mu, sigma, dias, v, T = 5.34, num_simulaciones = 3000) 
{
  Fecha <- datos$Fecha
  
  simular <- function(i) {
    sim <- simular_mbg_t(datos, nombre_columna_precio, mu = mu, sigma = sigma, v = v, dias = dias, T = T)
    sim$Simulacion <- i
    sim$Fecha <- Fecha
    return(sim)
  }
  
  simulaciones <- lapply(1:num_simulaciones, simular)
  simulaciones_df <- dplyr::bind_rows(simulaciones)
  
  return(simulaciones_df)
}

# Función para simular o cargar desde .rds si ya existe
cargar_o_simular <- function(nombre_archivo, datos, nombre_columna_precio, mu, sigma, dias, v, T = 5.34, num_simulaciones = 3000) {
  if (file.exists(nombre_archivo)) {
    readRDS(nombre_archivo)
  } else {
    simulaciones <- generar_simulaciones_bucle(datos, nombre_columna_precio, mu, sigma, dias, v, T, num_simulaciones)
    saveRDS(simulaciones, nombre_archivo)
    simulaciones
  }
}

# Aplicación para cada índice
set.seed(965)
simulaciones_ibex <- cargar_o_simular("simulaciones_ibex.rds", ibex35, "IBEX35", mu_ibex_anual, sigma_ibex_anual, dias = nrow(ibex35), v = 3.579564)

set.seed(638)
simulaciones_stoxx <- cargar_o_simular("simulaciones_stoxx.rds", stoxx50, "EUROSTOXX50", mu_stoxx_anual, sigma_stoxx_anual, dias = nrow(stoxx50), v = 2.908278)

set.seed(1023)
simulaciones_nasdaq <- cargar_o_simular("simulaciones_nasdaq.rds", nasdaq100, "NASDAQ100", mu_nasdaq_anual, sigma_nasdaq_anual, dias = nrow(nasdaq100), v = 3.79)

set.seed(28)
simulaciones_hangseng <- cargar_o_simular("simulaciones_hangseng.rds", hangseng, "HANGSENG", mu_hangseng_anual, sigma_hangseng_anual, dias = nrow(hangseng), v = 4.419553)

set.seed(2020)
simulaciones_nikkei <- cargar_o_simular("simulaciones_nikkei.rds", nikkei225, "NIKKEI225", mu_nikkei_anual, sigma_nikkei_anual, dias = nrow(nikkei225), v = 4.4)

simulaciones_guardadas <- list(
  IBEX35 = simulaciones_ibex,
  EUROSTOXX50 = simulaciones_stoxx,
  NASDAQ100 = simulaciones_nasdaq,
  HANGSENG = simulaciones_hangseng,
  NIKKEI225 = simulaciones_nikkei
)


#Gráficos MonteCarlo
ibex_mc <- ggplot() +
  geom_line(data = simulaciones_ibex, 
            aes(x = Fecha, y = Svalor, group = Simulacion, 
                color = as.factor(Simulacion)), alpha = 0.3, linewidth = 0.2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  
  scale_y_continuous(labels = scales::label_comma()) +
  geom_line(data = ibex35, aes(x = Fecha, y = IBEX35), color = "black", linewidth = 1) +
  scale_color_viridis_d() +
  xlab("Fecha") + ylab("Precio de cierre") +
  labs(title = "IBEX 35", color = "Simulación") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5))

stoxx_mc <- ggplot() +
  geom_line(data = simulaciones_stoxx, 
            aes(x = Fecha, y = Svalor, group = Simulacion, color = as.factor(Simulacion)), 
            alpha = 0.3, linewidth = 0.2) +
  geom_line(data = stoxx50, aes(x = Fecha, y = EUROSTOXX50), color = "black", linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_color_viridis_d() +
  xlab("Fecha") + ylab("Precio de cierre") +
  labs(title = "EUROSTOXX 50") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5))

nasdaq_mc <- ggplot() +
  geom_line(data = simulaciones_nasdaq, 
            aes(x = Fecha, y = Svalor, group = Simulacion, color = as.factor(Simulacion)), 
            alpha = 0.3, linewidth = 0.2) +
  geom_line(data = nasdaq100, aes(x = Fecha, y = NASDAQ100), color = "black", linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_color_viridis_d() +
  xlab("Fecha") + ylab("Precio de cierre") +
  labs(title = "NASDAQ 100") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5))

hangseng_mc <- ggplot() +
  geom_line(data = simulaciones_hangseng, 
            aes(x = Fecha, y = Svalor, group = Simulacion, color = as.factor(Simulacion)), 
            alpha = 0.3, linewidth = 0.2) +
  geom_line(data = hangseng, aes(x = Fecha, y = HANGSENG), color = "black", linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_color_viridis_d() +
  xlab("Fecha") + ylab("Precio de cierre") +
  labs(title = "HANG SENG") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5))

nikkei_mc <- ggplot() +
  geom_line(data = simulaciones_nikkei, 
            aes(x = Fecha, y = Svalor, group = Simulacion, color = as.factor(Simulacion)), 
            alpha = 0.3, linewidth = 0.2) +
  geom_line(data = nikkei225, aes(x = Fecha, y = NIKKEI225), color = "black", linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_color_viridis_d() +
  xlab("Fecha") + ylab("Precio de cierre") +
  labs(title = "NIKKEI 225") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5))

ibex_mc 
stoxx_mc 
nasdaq_mc 
hangseng_mc 
nikkei_mc 


calcular_mejores_simulaciones <- function(simulaciones_df, datos_reales, nombre_col_real, 
                                          n_mejores = 1) {
  
  merged_df <- simulaciones_df %>%
    left_join(datos_reales %>% dplyr::select(Fecha, Real = all_of(nombre_col_real)), by = "Fecha")
  
  var_real <- var(datos_reales[[nombre_col_real]], na.rm = TRUE)
  
  ecm_df <- merged_df %>%
    group_by(Simulacion) %>%
    summarise(ECM = mean((Svalor - Real)^2, na.rm = TRUE) / var_real, .groups = "drop")
  
  mejores_simulaciones <- ecm_df %>% arrange(ECM) %>% slice(1:n_mejores)
  
  simulaciones_seleccionadas <- simulaciones_df %>%
    filter(Simulacion %in% mejores_simulaciones$Simulacion)
  
  sim_ids <- unique(simulaciones_seleccionadas$Simulacion)
  datos_reales_rep <- datos_reales %>% crossing(Simulacion = sim_ids)
  
  list(
    ecm_df = ecm_df,
    mejores_simulaciones = mejores_simulaciones,
    simulaciones_seleccionadas = simulaciones_seleccionadas,
    datos_reales_rep = datos_reales_rep
  )
}

resultados_ibex     <- calcular_mejores_simulaciones(simulaciones_ibex, ibex35, "IBEX35")
resultados_stoxx    <- calcular_mejores_simulaciones(simulaciones_stoxx, stoxx50, "EUROSTOXX50")
resultados_nasdaq   <- calcular_mejores_simulaciones(simulaciones_nasdaq, nasdaq100, "NASDAQ100")
resultados_hangseng <- calcular_mejores_simulaciones(simulaciones_hangseng, hangseng, "HANGSENG")
resultados_nikkei   <- calcular_mejores_simulaciones(simulaciones_nikkei, nikkei225, "NIKKEI225")

mejores_simulaciones_ibex     <- resultados_ibex$mejores_simulaciones
mejores_simulaciones_stoxx    <- resultados_stoxx$mejores_simulaciones
mejores_simulaciones_nasdaq   <- resultados_nasdaq$mejores_simulaciones
mejores_simulaciones_hangseng <- resultados_hangseng$mejores_simulaciones
mejores_simulaciones_nikkei   <- resultados_nikkei$mejores_simulaciones

mejores_simulaciones_ibex$Indice <- "IBEX 35"
mejores_simulaciones_stoxx$Indice <- "EURO STOXX 50"
mejores_simulaciones_nasdaq$Indice <- "NASDAQ 100"
mejores_simulaciones_hangseng$Indice <- "HANG SENG"
mejores_simulaciones_nikkei$Indice <- "NIKKEI 225"

mejores_simulaciones_todos <- dplyr::bind_rows(
  mejores_simulaciones_ibex,
  mejores_simulaciones_stoxx,
  mejores_simulaciones_nasdaq,
  mejores_simulaciones_hangseng,
  mejores_simulaciones_nikkei
)

# Reordenar índices y agrupar para aplicar formato
mejores_simulaciones_todos <- mejores_simulaciones_todos %>%
  mutate(
    Indice = factor(Indice, levels = c("IBEX 35", "EURO STOXX 50", "NASDAQ 100", "HANG SENG", "NIKKEI 225"))
  ) %>%
  arrange(Indice, ECM) %>%  # orden deseado + dentro de cada bloque por ECM
  group_by(Indice) %>%
  mutate(Indice = if_else(row_number() == 1, as.character(Indice), "")) %>%  # solo una vez por bloque
  ungroup() %>%
  relocate(Indice, .before = everything())  # poner como primera columna

mejores_simulaciones_todos 

#Graficar las  mejores simulaciones anteriormente seleccionadas
plot_mejores_simulaciones <- function(sim_seleccionadas, datos_reales, nombre_col_real, titulo = "") {
  sim_seleccionadas <- sim_seleccionadas %>% mutate(Simulacion = paste("Simulación", Simulacion))
  datos_reales <- datos_reales %>% mutate(Simulacion = paste("Simulación", Simulacion))
  
  ggplot() +
    geom_line(data = sim_seleccionadas, aes(x = Fecha, y = Svalor), color = "red", alpha = 0.6, linewidth = 0.25) +
    geom_line(data = datos_reales, aes(x = Fecha, y = !!sym(nombre_col_real)), color = "black", linewidth = 0.3) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    facet_wrap(~ Simulacion, ncol = 2) +
    labs(title = titulo, x = "Fecha", y = "Precio de cierre") +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
          strip.background = element_rect(fill = "white", color = "black"),
          strip.text = element_text(face = "bold", size = 15))
}

graf_sim1 <- plot_mejores_simulaciones(resultados_ibex$simulaciones_seleccionadas, resultados_ibex$datos_reales_rep, "IBEX35", "IBEX 35")
graf_sim2 <- plot_mejores_simulaciones(resultados_stoxx$simulaciones_seleccionadas, resultados_stoxx$datos_reales_rep, "EUROSTOXX50", "EURO STOXX 50")
graf_sim3 <- plot_mejores_simulaciones(resultados_nasdaq$simulaciones_seleccionadas, resultados_nasdaq$datos_reales_rep, "NASDAQ100", "NASDAQ 100")
graf_sim4 <- plot_mejores_simulaciones(resultados_hangseng$simulaciones_seleccionadas, resultados_hangseng$datos_reales_rep, "HANGSENG", "HANG SENG")
graf_sim5 <- plot_mejores_simulaciones(resultados_nikkei$simulaciones_seleccionadas, resultados_nikkei$datos_reales_rep, "NIKKEI225", "NIKKEI 225")


graf_sim1  
graf_sim2 
graf_sim3 
graf_sim4 
graf_sim5 


reestimar_parametros_log <- function(df, nombre_col, fecha_inicio = "2025-01-03", fecha_fin = "2025-05-03", dias_anuales) {
  precios <- df %>%
    filter(Fecha >= fecha_inicio & Fecha <= fecha_fin) %>% #filtro por fecha
    arrange(Fecha) %>% #ordenar por fecha ascendente
    pull(!!sym(nombre_col)) %>% #extraer columna de precios como vector
    na.omit() #eliminar NA
  
  retornos <- diff(log(precios)) 
  mu_anual <- mean(retornos) * dias_anuales
  sigma_anual <- sd(retornos) * sqrt(dias_anuales)
  
  tibble::tibble(mu = mu_anual, sigma = sigma_anual)
}

#PARTE 1: Simulación por índice con datos desde 2025
simular_y_evaluar_prediccion <- function(df_real, mu, sigma, nombre_col_real, ticker, v,
                                         fecha_inicio, fecha_fin, dias_anuales, T = 1) {
  escala <- sqrt(v / (v - 2))
  
  df_futuro <- tq_get(ticker, from = fecha_inicio, to = fecha_fin, get = "stock.prices") %>%
    dplyr::select(Fecha = date, Real = close) %>%
    mutate(Fecha = as.Date(Fecha))
  
  fechas_pred <- df_futuro$Fecha
  n_dias <- length(fechas_pred)
  dt <- T / dias_anuales  
  
  ultimo_precio <- df_real %>%
    filter(Fecha < as.Date(fecha_inicio)) %>%
    tail(1) %>%
    pull(!!sym(nombre_col_real))
  
  resultados_list <- vector("list", 3000)
  
  for (i in 1:3000) {
    S <- numeric(n_dias)
    S[1] <- ultimo_precio
    for (t in 2:n_dias) {
      DeltaW <- escala * rt(1, df = v) * sqrt(dt)
      S[t] <- S[t - 1] * exp((mu - 0.5 * sigma^2) * dt + sigma * DeltaW)
    }
    resultados_list[[i]] <- data.frame(
      Fecha = fechas_pred,
      Prediccion = S,
      Simulacion = paste0("Sim_", i)
    )
  }
  
  predicciones <- dplyr::bind_rows(resultados_list)
  
  resumen_predicciones <- predicciones %>%
    group_by(Fecha) %>%
    summarise(
      Media = mean(Prediccion),
      IC_2.5 = quantile(Prediccion, 0.025),
      IC_97.5 = quantile(Prediccion, 0.975),
      .groups = "drop"
    )
  
  list(
    predicciones = predicciones,
    datos_reales = df_futuro,
    resumen_predicciones = resumen_predicciones
  )
}

seleccionar_mejor_simulacion <- function(predicciones, reales) {
  merged <- predicciones %>%
    left_join(reales, by = "Fecha") %>%
    group_by(Simulacion) %>%
    summarise(
      ECM = mean((Prediccion - Real)^2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(ECM) %>%
    slice_min(ECM, n = 1) %>%
    pull(Simulacion)
  
  mejor_simulacion <- predicciones %>%
    filter(Simulacion == merged) %>%
    rename(Svalor = Prediccion)
  
  return(mejor_simulacion)
}

#PARTE 2: Bucle para simular por índice
resultados_evaluacion <- list()

indices <- list(
  list(nombre = "IBEX35",      df = ibex35,      col = "IBEX35",      ticker = "^IBEX",      dias = 256, v = 3.579564, semilla = 965),
  list(nombre = "EUROSTOXX50", df = stoxx50,     col = "EUROSTOXX50", ticker = "^STOXX50E",  dias = 252, v = 2.908278, semilla = 638),
  list(nombre = "NASDAQ100",   df = nasdaq100,   col = "NASDAQ100",   ticker = "^NDX",       dias = 251, v = 3.79,     semilla = 1023),
  list(nombre = "HANGSENG",    df = hangseng,    col = "HANGSENG",    ticker = "^HSI",       dias = 245, v = 4.419553, semilla = 28),
  list(nombre = "NIKKEI225",   df = nikkei225,   col = "NIKKEI225",   ticker = "^N225",      dias = 244, v = 4.4,      semilla = 2020)
)

for (indice in indices) {
  parametros <- reestimar_parametros_log(
    df = indice$df,
    nombre_col = indice$col,
    dias_anuales = indice$dias
  )
  
  set.seed(indice$semilla)
  
  evaluacion <- simular_y_evaluar_prediccion(
    df_real = indice$df,
    mu = parametros$mu[1],
    sigma = parametros$sigma[1],
    nombre_col_real = indice$col,
    ticker = indice$ticker,
    v = indice$v,
    fecha_inicio = "2025-05-05",
    fecha_fin = "2025-05-31",
    dias_anuales = indice$dias,
    T = 1
  )
  
  mejor_sim <- seleccionar_mejor_simulacion(
    predicciones = evaluacion$predicciones,
    reales = evaluacion$datos_reales
  )
  
  evaluacion$mejor_simulacion <- mejor_sim
  
  resultados_evaluacion[[indice$nombre]] <- evaluacion
}

mejores_trayectorias <- lapply(resultados_evaluacion, function(x) x$mejor_simulacion)

#PARTE 3: Grafico (añadiendo solo la parte válida de la mejor simulación)
plot_ic_vs_real <- function(resumen, reales = NULL, mejor = NULL, titulo = "") {
  if (!is.null(mejor)) {
    mejor <- mejor %>% filter(Fecha >= as.Date("2025-05-05") & Fecha <= as.Date("2025-05-31"))
  }
  
  p <- ggplot(resumen, aes(x = Fecha)) +
    geom_ribbon(aes(ymin = IC_2.5, ymax = IC_97.5), fill = "skyblue", alpha = 0.3) +
    geom_line(aes(y = Media), color = "blue", linewidth = 1) +
    labs(title = titulo, x = "Fecha", y = "Precio simulado") +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11))
  
  if (!is.null(mejor)) {
    p <- p + geom_line(data = mejor, aes(x = Fecha, y = Svalor), color = "red", linewidth = 0.6)
  }
  
  if (!is.null(reales) && "Real" %in% names(reales)) {
    p <- p + geom_line(data = reales, aes(x = Fecha, y = Real), color = "black", linewidth = 0.8)
  }
  
  return(p)
}

#PARTE 4: Generar gráficos individuales por índice
graficar_ic <- function(nombre, resultados, mejores) {
  plot_ic_vs_real(
    resumen = resultados[[nombre]]$resumen_predicciones,
    reales = resultados[[nombre]]$datos_reales,
    mejor = mejores[[nombre]],
    titulo = paste("IC 95% -", nombre)
  )
}

p1 <- graficar_ic("IBEX35", resultados = resultados_evaluacion, mejores = mejores_trayectorias) 
p2 <- graficar_ic("EUROSTOXX50", resultados = resultados_evaluacion, mejores = mejores_trayectorias) 
p3 <- graficar_ic("NASDAQ100", resultados = resultados_evaluacion, mejores = mejores_trayectorias) 
p4 <- graficar_ic("HANGSENG", resultados = resultados_evaluacion, mejores = mejores_trayectorias) 
p5 <- graficar_ic("NIKKEI225", resultados = resultados_evaluacion, mejores = mejores_trayectorias)



# Crear tabla comparativa para cada índice
comparar_indice <- function(nombre) {
  resumen <- resultados_evaluacion[[nombre]]$resumen_predicciones
  reales <- resultados_evaluacion[[nombre]]$datos_reales
  mejor_sim <- mejores_trayectorias[[nombre]] %>%
    dplyr::select(Fecha, Simulado = Svalor)
  
  resumen %>%
    left_join(reales, by = "Fecha") %>%
    left_join(mejor_sim, by = "Fecha") %>%  
    mutate(Indice = nombre,
           Error_Porc = abs((Real - Simulado) / Real) * 100
    ) %>%
    dplyr::select(Indice, Fecha, Real, Simulado, Error_Porc, Media, IC_2.5, IC_97.5)
}

orden_indices <- c("IBEX35", "EUROSTOXX50", "NASDAQ100", "HANGSENG", "NIKKEI225")
tabla_comparativa <- purrr::map_dfr(orden_indices, comparar_indice)

tabla_formateada <- tabla_comparativa %>%
  mutate(Indice = factor(Indice, levels = orden_indices)) %>%
  arrange(Indice, Fecha) %>%
  group_by(Indice) %>%
  slice(-1) %>%  # quitar primera fila si es redundante
  filter(row_number() <= 4 | row_number() > n() - 4) %>%
  mutate(Indice = if_else(row_number() == 1, as.character(Indice), "")) %>%
  ungroup()

tabla_formateada 

# VaR Mayo 2025 (2025-05-05 a 2025-05-31)
calcular_var_mayo <- function(pred_df, inversion = 10000) {
  pred_df %>%
    group_by(Simulacion) %>%
    summarise(Ret = last(Precio) / first(Precio) - 1, .groups = "drop") %>%
    summarise(
      VaR_Mayo_2025_95 = -inversion * quantile(Ret, 0.05, na.rm = TRUE),
      VaR_Mayo_2025_99 = -inversion * quantile(Ret, 0.01, na.rm = TRUE)
    )
}

# VaR a Futuro (10 días desde el último precio)
calcular_var_futuro <- function(mu, sigma, v, dias_habiles, semilla = NULL, inversion = 10000) {
  if (!is.null(semilla)) set.seed(semilla)
  
  dt <- 10 / dias_habiles
  ret_futuro <- exp(
    (mu - 0.5 * sigma^2) * dt +
      sigma * (rt(3000, df = v) / sqrt(v / (v - 2))) * sqrt(dt)
  ) - 1
  
  list(
    valores = tibble(
      VaR_Futuro_10d_95 = -inversion * quantile(ret_futuro, 0.05, na.rm = TRUE),
      VaR_Futuro_10d_99 = -inversion * quantile(ret_futuro, 0.01, na.rm = TRUE)
    ),
    retornos = ret_futuro
  )
}

lista_datos <- list(
  IBEX35 = ibex35,
  EUROSTOXX50 = stoxx50,
  NASDAQ100 = nasdaq100,
  HANGSENG = hangseng,
  NIKKEI225 = nikkei225
)

parametros <- data.frame(
  indice = c("IBEX35", "EUROSTOXX50", "NASDAQ100", "HANGSENG", "NIKKEI225"),
  mu = c(mu_ibex_anual, mu_stoxx_anual, mu_nasdaq_anual, mu_hangseng_anual, mu_nikkei_anual),
  sigma = c(sigma_ibex_anual, sigma_stoxx_anual, sigma_nasdaq_anual, sigma_hangseng_anual, sigma_nikkei_anual),
  v = c(3.579564, 2.908278, 3.79, 4.419553, 4.4),
  S0 = c(ibex35$IBEX35[1], stoxx50$EUROSTOXX50[1], nasdaq100$NASDAQ100[1], hangseng$HANGSENG[1], nikkei225$NIKKEI225[1]),
  semilla = c(965, 638, 1023, 28, 2020),
  dias_habiles = c(256, 252, 251, 245, 244)
)

resultados_finales_var <- list()
retornos_futuro_lista <- list()

for (i in seq_len(nrow(parametros))) {
  indice <- parametros$indice[i]
  
  #VaR Mayo
  sim_mayo <- resultados_evaluacion[[indice]]$predicciones
  
  # Verificamos que tenga columna "Prediccion" y renombramos a "Precio"
  if (!"Prediccion" %in% colnames(sim_mayo)) {
    stop(paste("Falta columna 'Prediccion' en el índice", indice))
  }
  sim_mayo <- sim_mayo %>% rename(Precio = Prediccion)
  var_mayo <- calcular_var_mayo(sim_mayo)
  
  resultado_futuro <- calcular_var_futuro(
    mu = parametros$mu[i],
    sigma = parametros$sigma[i],
    v = parametros$v[i],
    dias_habiles = parametros$dias_habiles[i],
    semilla = parametros$semilla[i]
  )
  
  # Verifica que retorne correctamente
  if (!is.null(resultado_futuro$retornos)) {
    retornos_futuro_lista[[indice]] <- tibble(
      Indice = indice,
      Retorno = resultado_futuro$retornos
    )
  }
  
  # Guardar el resultado del VaR
  var_futuro <- resultado_futuro$valores
  
  # Guardar los retornos simulados
  retornos_futuro_lista[[indice]] <- tibble(
    Indice = indice,
    Retorno = resultado_futuro$retornos
  )
  
  #Combinar 
  resultados_finales_var[[indice]] <- bind_cols(
    tibble(Indice = indice),
    var_mayo,
    var_futuro
  )
}

retornos_futuro_df <- bind_rows(retornos_futuro_lista)

tabla_var<- dplyr::bind_rows(resultados_finales_var)

tipos_cambio_mayo <- c(
  "IBEX35" = 1.13,
  "EUROSTOXX50" = 1.13,
  "NASDAQ100" = 1,
  "HANGSENG" = 0.13,
  "NIKKEI225" = 0.0069
)

tipos_cambio_futuro <- c(
  "IBEX35" = 1.14,
  "EUROSTOXX50" = 1.14,
  "NASDAQ100" = 1,
  "HANGSENG" = 0.13,
  "NIKKEI225" = 0.0069
)


tabla_var_usd <- tabla_var %>%
  rowwise() %>%
  mutate(
    VaR_Mayo_2025_95 = VaR_Mayo_2025_95 * tipos_cambio_mayo[Indice],
    VaR_Mayo_2025_99 = VaR_Mayo_2025_99 * tipos_cambio_mayo[Indice],
    VaR_Futuro_10d_95 = VaR_Futuro_10d_95 * tipos_cambio_futuro[Indice],
    VaR_Futuro_10d_99 = VaR_Futuro_10d_99 * tipos_cambio_futuro[Indice]
  ) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~ round(., 2)))


tabla_var 

retornos_futuro_df <- bind_rows(retornos_futuro_lista)

eficiencia_df <- retornos_futuro_df %>%
  group_by(Indice) %>%
  summarise(
    Retorno_Medio = mean(Retorno),
    VaR_10d_95 = -quantile(Retorno, 0.05),
    .groups = "drop"
  ) %>%
  mutate(
    Retorno_Medio_pct = Retorno_Medio * 100,
    VaR_10d_95_pct = VaR_10d_95 * 100
  )

n_var_r <- ggplot(eficiencia_df, aes(x = VaR_10d_95_pct, y = Retorno_Medio_pct, color = Indice)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = Indice), size = 5, fontface = "bold", box.padding = 0.5, max.overlaps = 100) +
  geom_hline(yintercept = mean(eficiencia_df$Retorno_Medio_pct), linetype = "dotted", color = "gray50") +
  geom_vline(xintercept = mean(eficiencia_df$VaR_10d_95_pct), linetype = "dotted", color = "gray50") +
  labs(
    x = "VaR al 95% a 10 días (%)",
    y = "Rentabilidad media esperada (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

n_var_r

# 1. Usamos los retornos ya generados de VaR a futuro
retornos_df <- retornos_futuro_df

graficos_lista1 <- lapply(orden_indices, function(indice_actual) {
  
  ret_indice <- retornos_df %>% filter(Indice == indice_actual)
  var_95 <- quantile(ret_indice$Retorno, probs = 0.05, na.rm = TRUE)
  
