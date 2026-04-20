library(pwrss)
library(lmtest)
library(sandwich)
library(lme4)

# Effect sizes from literature (beta1)
effect_ntr_direct_noncog <- 0.1842
effect_ntr_indirect_noncog <- -0.0047
effect_teds_direct_noncog <- 0.0668
effect_teds_indirect_noncog <- 0.0924
effect_ntr_direct_cog <- 0.2330
effect_ntr_indirect_cog <- 0.0666
effect_teds_direct_cog <- 0.1431
effect_teds_indirect_cog <- 0.1252

effect <- list(effect_ntr_direct_noncog, effect_ntr_indirect_noncog, 
	effect_ntr_direct_cog, effect_ntr_indirect_cog,
	effect_teds_direct_noncog, effect_teds_indirect_noncog,
	effect_teds_direct_cog, effect_teds_indirect_cog)

# Number of predictors (k)
num_predictors_max <- 16

# Expected variance explained
ghirardi_adj_r2 <- 0.11

# Run the power analysis to get the required sample size
for (i in effect) {
print(paste0("Result for effect size ",i))
result_i <- pwrss.t.reg(beta1 = i, k = num_predictors_max, 
    r2 = ghirardi_adj_r2, power = 0.8, alpha = 0.05, alternative = "not equal")
}

###################################################

# Set parameters
set.seed(123)
beta_values <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30)
N_values <- c(3090, 4510)
cluster_map <- list("3090" = 1931, "4510" = 2469)  # Observed from your data
covariates <- 17
alpha <- 0.01
nsim <- 200  # Reduce for speed; increase later

# Store results
results <- data.frame()

# Loop over sample sizes and effect sizes
for (N in N_values) {
  clusters <- cluster_map[[as.character(N)]]
  obs_per_cluster <- ceiling(N / clusters)

  for (beta in beta_values) {
    powers <- numeric(nsim)
    iccs <- numeric(nsim)

    for (i in 1:nsim) {
      cluster_ids <- rep(1:clusters, each = obs_per_cluster)[1:N]

      PGI <- rnorm(N)
      covariate_matrix <- matrix(rnorm(N * covariates), ncol = covariates)
      colnames(covariate_matrix) <- paste0("cov", 1:covariates)

      # Cluster effects and residuals
      sigma_u <- 0.3
      cluster_effects <- rnorm(clusters, mean = 0, sd = sigma_u)
      u <- cluster_effects[cluster_ids]
      epsilon <- rnorm(N)

      y <- beta * PGI + rowSums(covariate_matrix) * 0.01 + u + epsilon
      data <- data.frame(y = y, PGI = PGI, cluster = cluster_ids, covariate_matrix)

      # Fit main model
      formula <- as.formula(paste("y ~ PGI +", paste(colnames(covariate_matrix), collapse = " + ")))
      model <- lm(formula, data = data)
      clustered_se <- coeftest(model, vcov = vcovCL(model, cluster = ~cluster))
      p_val <- clustered_se["PGI", "Pr(>|t|)"]
      powers[i] <- ifelse(p_val < alpha, 1, 0)

      # Estimate ICC
      lmer_model <- lmer(y ~ 1 + (1 | cluster), data = data)
      var_cluster <- as.numeric(VarCorr(lmer_model)$cluster)
      var_resid <- attr(VarCorr(lmer_model), "sc")^2
      icc <- var_cluster / (var_cluster + var_resid)
      iccs[i] <- icc
    }

    power_result <- mean(powers)
    mean_icc <- round(mean(iccs), 3)

    results <- rbind(results, data.frame(
      N = N,
      Clusters = clusters,
      Beta = beta,
      Mean_ICC = mean_icc,
      Power = round(power_result, 3)
    ))

    cat("N =", N, "| Clusters =", clusters, "| Beta =", beta,
        "| Mean ICC =", mean_icc, "| Power =", round(power_result, 3), "\n")
  }
}

# Display results
print(results)