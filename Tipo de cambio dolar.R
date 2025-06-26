library(tidyquant)
library(dplyr)

# Fechas
fecha_inicio <- "2025-06-02"
fecha_fin <- "2025-06-12"

# Descargar tipo de cambio EUR/USD
eurusd <- tq_get("EURUSD=X", from = fecha_inicio, to = fecha_fin)

# Descargar tipo de cambio HKD/USD
hkdusd <- tq_get("HKDUSD=X", from = fecha_inicio, to = fecha_fin)

# Descargar tipo de cambio JPY/USD
jpyusd <- tq_get("JPYUSD=X", from = fecha_inicio, to = fecha_fin)

cambio_promedios <- tibble(
  Moneda = c("EUR", "HKD", "JPY"),
  Cambio_a_USD = c(
    mean(eurusd$close, na.rm = TRUE),
    mean(hkdusd$close, na.rm = TRUE),
    mean(jpyusd$close, na.rm = TRUE)
  )
)

print(cambio_promedios)

tipos_cambio <- tibble(
  Indice = c("IBEX35", "EUROSTOXX50", "NASDAQ100", "HANGSENG", "NIKKEI225"),
  cambio_a_usd = c(1.11, 1.11, 1.00, 0.128, 0.00798)  # Tus promedios reales
)

