#model validation
library(CausalQueries)


# Bayesian P value --------------------------------------------------------

# Number of simulations
num_simulations <- 1000

# Placeholders for test statistics
simulated_mean_BT <- numeric(num_simulations)
simulated_cov_S_I <- numeric(num_simulations)
simulated_cov_S_BT <- numeric(num_simulations)



# Function to calculate test statistics
calculate_test_statistics <- function(data) {
  mean_BT <- mean(data$BT)
  cov_S_I <- cov(data$S, data$I)
  cov_S_BT <- cov(data$S, data$BT)
  return(c(mean_BT, cov_S_I, cov_S_BT))
}

calculate_test_statistics(nat)

# Draw a parameter vector and simulate data repeatedly
for (i in 1:num_simulations) {
  param_vector <- m1 %>% CausalQueries:::get_parameters(param_type = 'posterior_draw')
  simulated_data <- simulate_data(model = m1, parameters = param_vector, n = nrow(nat))
  stats <- calculate_test_statistics(simulated_data)
  simulated_mean_BT[i] <- stats[1]
  simulated_cov_S_I[i] <- stats[2]
  simulated_cov_S_BT[i] <- stats[3]
}

# Calculate the test statistics using the real data
observed_stats <- calculate_test_statistics(nat)
observed_mean_BT <- observed_stats[1]
observed_cov_S_I <- observed_stats[2]
observed_cov_S_BT <- observed_stats[3]

# Assess how extreme the observed statistics are
p_value_mean_BT <- mean(simulated_mean_BT >= observed_mean_BT)
p_value_cov_S_I <- mean(simulated_cov_S_I >= observed_cov_S_I)
p_value_cov_S_BT <- mean(simulated_cov_S_BT >= observed_cov_S_BT)

print(paste("Observed Mean BT:", observed_mean_BT))
print(paste("P-value for Mean BT:", p_value_mean_BT))
print(paste("Observed Covariance S and I:", observed_cov_S_I))
print(paste("P-value for Covariance S and I:", p_value_cov_S_I))
print(paste("Observed Covariance S and BT:", observed_cov_S_BT))
print(paste("P-value for Covariance S and BT:", p_value_cov_S_BT))

# Create data frames for ggplot
data_mean_BT <- data.frame(value = simulated_mean_BT)
data_cov_S_I <- data.frame(value = simulated_cov_S_I)
data_cov_S_BT <- data.frame(value = simulated_cov_S_BT)

# Plot for Mean BT
plot_mean_BT <- ggplot(data_mean_BT, aes(x = value)) +
  geom_histogram(binwidth = 0.02, fill = "lightblue", color = "black") +
  geom_vline(xintercept = observed_mean_BT, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Simulated Mean BT",
       x = "Mean BT",
       y = "Frequency") +
  annotate("text", x = Inf, y = Inf, label = paste("P-value:", round(p_value_mean_BT, 3)), hjust = 1.1, vjust = 1.1) +
  theme_minimal()

# Plot for Covariance S and I
plot_cov_S_I <- ggplot(data_cov_S_I, aes(x = value)) +
  geom_histogram(binwidth = 0.02, fill = "lightblue", color = "black") +
  geom_vline(xintercept = observed_cov_S_I, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Simulated Covariance S and I",
       x = "Covariance S and I",
       y = "Frequency") +
  annotate("text", x = Inf, y = Inf, label = paste("P-value:", round(p_value_cov_S_I, 3)), hjust = 1.1, vjust = 1.1)+
  theme_minimal()

# Plot for Covariance S and BT
plot_cov_S_BT <- ggplot(data_cov_S_BT, aes(x = value)) +
  geom_histogram(binwidth = 0.02, fill = "lightblue", color = "black") +
  geom_vline(xintercept = observed_cov_S_BT, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Simulated Covariance S and BT",
       x = "Covariance S and BT",
       y = "Frequency") +
  annotate("text", x = Inf, y = Inf, label = paste("P-value:", round(p_value_cov_S_BT, 3)), hjust = 1.1, vjust = 1.1)+
  theme_minimal()

# Arrange plots in a single row
gridExtra::grid.arrange(plot_mean_BT, plot_cov_S_I, plot_cov_S_BT, ncol = 3)


