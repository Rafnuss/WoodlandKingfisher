id <- pressurepath$label != "discard" & pressurepath$stap_id > 2

err <- pressurepath$pressure_tag[id] - pressurepath$pressure_era5_norm[id]

hist(err)
plot(err)

tmp <- acf(err, type = "covariance", lag.max = 2 * 24 * 2, plot = F)

dt <- as.numeric(median(diff(pressurepath$date)), units = "hours")


plot(tmp$lag * dt, tmp$acf)

# Example autocovariance vector
autocov_vector <- c(1.0, 0.7, 0.5, 0.3)

# Number of lags (length of autocovariance vector)
num_lags <- length(autocov_vector)

# Create an empty covariance matrix
cov_matrix <- matrix(0, nrow = num_lags, ncol = num_lags)

# Fill in the covariance matrix using autocovariance values
for (i in 1:num_lags) {
  for (j in 1:num_lags) {
    cov_matrix[i, j] <- tmp$acf[abs(i - j) + 1]
  }
}

# Print the covariance matrix
print(cov_matrix)


istap <- 10

mse <- tag$map_pressure_mse$data[[istap]]



# Number of sample
n <- tag$stap$nb_sample[istap]

# Log-linear pooling weight
w <- log_linear_pooling_weight(n)

# compute likelihood assume gaussian error distribution
likelihood <- (1 / (2 * pi * sd[istap]^2))^(n * w / 2) *
  exp(-w * n / (2 * sd[istap]^2) * tag$map_pressure_mse$data[[istap]])

# change water in NA
likelihood[is.na(likelihood)] <- 0
likelihood[tag$map_pressure_mse$mask_water] <- NA

# mask value of threshold
map_pressure[[istap]] <- likelihood
