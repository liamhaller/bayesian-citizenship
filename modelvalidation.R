#model validation
library(CausalQueries)
library(pscl) 
library(gtsummary)
library(broom)
library(tidyverse)
library(rethinking)

confound.m3_status <- readRDS("m3_c.RDS")
confound.m3_ability <- readRDS("m3_ca.RDS")
m3 <- readRDS("m3.RDS")


# Sensitivity -------------------------------------------------------------


##Unobserved Confounding
lm(data = oap, formula = as.factor(overwhelmed)~as.factor(b_frequency)+
     as.factor(b_treatment)+
     as.factor(education)+
     as.factor(german_skills)) %>% summary()


lm(data = oap, formula = BT~T+F+E+L) %>% summary()


  tbl_regression(
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels() 




#Simiulate more able / unable 






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
  cov_S_I <- cov(data$S, data$O)
  cov_S_BT <- cov(data$S, data$BT)
  return(c(mean_BT, cov_S_I, cov_S_BT))
}

calculate_test_statistics(nat)

# Draw a parameter vector and simulate data repeatedly
for (i in 1:num_simulations) {
  param_vector <- m3 %>% CausalQueries:::get_parameters(param_type = 'posterior_draw') #!! model here
  simulated_data <- simulate_data(model = m3, parameters = param_vector, n = nrow(nat)) #!! model here
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




# What affects BT? --------------------------------------------------------


# Fit logistic regression model of Bureaucratic Trajectory
model <- glm(BT ~ O + F + T + S + E + L + C + Y, data = nat_full, family = binomial)

summary(model)

model %>% 
  tbl_regression(exponentiate = FALSE) %>% 
  as_flex_table()

# Calculate pseudo-R^2 measures
pseudo_r2 <- pscl::pR2(model)

# Display the results
pseudo_r2



# Markov Convergence ------------------------------------------------------


#some parameter draws to use for demonstration
x <- bayesplot::example_mcmc_draws(chains = 4, params = 6)
dim(x)
#> [1] 250   4   6
dimnames(x)
#> $Iteration
#> NULL
#> 
#> $Chain
#> [1] "chain:1" "chain:2" "chain:3" "chain:4"
#> 
#> $Parameter
#> [1] "alpha"   "sigma"   "beta[1]" "beta[2]" "beta[3]" "beta[4]"
#> 

# trace plots of the betas
color_scheme_set("viridis")
bayesplot::mcmc_trace(x, regex_pars = "beta")




grab(m3, object = 'stan_summary') %>% rethinking::trankplot()

grab(m3, object = 'stan_fit')

stan <- grab(m3, object = 'stan_objects') 
stan$stanfit %>% rethinking::trankplot()

