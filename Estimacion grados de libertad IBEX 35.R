
#####
# Función de densidad
ddt_adj <- function(x, mu, sigma, df) {
  dt((x - mu) / sigma, df = df, log = FALSE) / sigma
}

fit_fd <- fitdist(retornos_log$Retornos_Log, "dt_adj",
                  start = list(mu = mu_log, sigma = sigma_log, df = 5))

summary(fit_fd)

#####
#Multiarranque 
set.seed(123)

n_start <- 20 
multi_results <- matrix(NA, nrow = n_start, ncol = 4)
colnames(multi_results) <- c("m", "s", "df", "nll")

for(i in 1:n_start){
  # Generar valores iniciales alrededor de los estimadores muestrales
  m0 <- mu_log + rnorm(1, mean = 0, sd = 0.1*abs(mu_log))
  s0 <- sigma_log * runif(1, min = 0.8, max = 1.2)
  # Para df elegimos valores en un rango razonable (mayor que 2)
  df0 <- runif(1, min = 3, max = 15)
  
  init_params <- c(m = m0, s = s0, df = df0)
  
  # Ejecutar optimización con L-BFGS-B y límite inferior para df en 2.1
  fit_temp <- optim(init_params, neg_llik, 
                    data = retornos_log$Retornos_Log, 
                    method = "L-BFGS-B", 
                    lower = c(-Inf, 1e-8, 2.1),
                    hessian = TRUE)
  
  multi_results[i, ] <- c(fit_temp$par, fit_temp$value)
}

print(multi_results)

# Ordenar en función del valor mínimo de nll para ver cuál es la solución con mejor log-verosimilitud
best_fit <- multi_results[which.min(multi_results[, "nll"]), ]
cat("Mejor solución encontrada:\n")
print(best_fit)


#####
#Fijando df y optimizando mu y sigma
dfs  <- seq(2.1, 30, by = 0.1)
aics <- sapply(dfs, function(d){
  fit_tmp <- fitdist(retornos_log$Retornos_Log, ddt_adj,
                     start   = list(mu = mu_log, sigma = sigma_log),
                     fix.arg = list(df = d))
  AIC(fit_tmp)
})

best_df <- dfs[ which.min(aics) ]
best_df


#####
#Maximizando la log-verosimilitud
opt_df <- optimize(
  f = function(d){
    fit_tmp <- fitdist(retornos_log$Retornos_Log, ddt_adj,
                       start   = list(mu = mu_log, sigma = sigma_log),
                       fix.arg = list(df = d))
    fit_tmp$loglik
  },
  lower   = 2.1,
  upper   = 100,
  maximum = TRUE
)
opt_df


#Eligir el mejor df
dfs <- c(3.5785791897, 3.638158, 3.6, 3.579564)

fits <- lapply(dfs, function(d){
  fitdist(
    retornos_log$Retornos_Log,
    ddt_adj,
    start   = list(mu = mu_log, sigma = sigma_log), fix.arg = list(df = d)
  )
})

sapply(fits, class)

aics <- vapply(fits, function(fit) {
  k <- length(fit$estimate)
  -2 * fit$loglik + 2 * k
}, numeric(1))

idx_best <- which.min(aics)
df_best  <- dfs[idx_best]
df_best
