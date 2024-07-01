library(CausalQueries)
library(bayesplot)
library(tidyverse)
library(tidybayes)


# Functions ---------------------------------------------------------------



plot_halfeye <- function(model, variable, outcome = "N") {
  require(tidybayes)
  query1 <- paste(outcome, "[", variable, "=1]", sep = "")
  query0 <- paste(outcome, "[", variable, "=0]", sep = "")
  
  # Obtain the posterior distributions
  post_dist_1 <- query_distribution(model = model, query = query1, using = "posterior")
  post_dist_0 <- query_distribution(model = model, query = query0, using = "posterior")
  
  # Calculate the distribution of the difference
  post_dist_diff <- post_dist_1 - post_dist_0
  
  # Convert to a data frame
  post_df <- as.data.frame(post_dist_diff)
  
  # Plot using stat_halfeye with after_stat
  post_df %>%
    pivot_longer(cols = everything(), names_to = "Sample", values_to = "Value") %>%
    ggplot(aes(x = Value, y = 0)) +
    stat_halfeye(aes(fill = after_stat(cut_cdf_qi(cdf, .width = c(.5, .8, .95)))),
                 .width = c(.5, .8, .95), point_interval = mean_qi) +
    scale_fill_brewer() +
    ggtitle(paste("Posterior Distribution of the Difference", outcome, "[", variable, "=1] -", outcome, "[", variable, "=0]", sep = " ")) +
    xlab("Difference in N") +
    ylab("") +
    theme_minimal() +
    theme(legend.position = "none")
}



# Arbitray queries --------------------------------------------------------


evaluate_query <- function(model, outcome, query_vars, given_vars = NULL, print_query = FALSE) {
  # Construct the query strings
  query1 <- paste(outcome, "[", query_vars, "=1]", sep = "")
  query0 <- paste(outcome, "[", query_vars, "=0]", sep = "")
  
  # Initialize an empty data frame to store the combined results
  combined_df <- data.frame(Value = numeric(), Distribution = character(), Given = character(), stringsAsFactors = FALSE)
  
  if (is.null(given_vars)) {
    
    # Get the distributions for priors and posteriors
    prior_dist <- query_distribution(model, paste(query1, ">", query0), using = "priors")
    post_dist <- query_distribution(model, paste(query1, ">", query0), using = "posteriors")
    
    # Combine the distributions into a single data frame
    combined_df <- bind_rows(
      data.frame(Value = prior_dist, Distribution = "Prior", stringsAsFactors = FALSE),
      data.frame(Value = post_dist, Distribution = "Posterior", stringsAsFactors = FALSE)
    )
    
    colnames(combined_df) <- c('Value', "Distribution")
    
    # Filter out non-finite values
    combined_df <- combined_df %>% filter(is.finite(Value))
    
    # Optionally print the query used
    if (print_query) {
      print(paste("Query used:", paste(query1, ">", query0)))
    }
    
    # Plot the results
    ggplot(combined_df, aes(x = Value, fill = Distribution)) +
      geom_density(alpha = 0.4) +
      labs(
        title = paste("Prior and Posterior Distributions of Naturalization Probability (", query_vars, "=1 vs. 0)", sep = ""),
        x = "Difference in Naturalization Probability",
        y = "Density"
      ) +
      scale_fill_manual(values = c("Prior" = "skyblue", "Posterior" = "coral")) +
      theme_minimal() +
      theme(legend.position = "bottom") 
    
  } else {
    for (given_var in given_vars) {
      given_1 <- paste(given_var, "==1", sep = "")
      given_0 <- paste(given_var, "==0", sep = "")
      
      # Get the distributions for priors and posteriors with given conditions
      prior_dist_1 <- unlist(query_distribution(model, paste(query1, ">", query0), given = given_1, using = "priors"))
      prior_dist_0 <- unlist(query_distribution(model, paste(query1, ">", query0), given = given_0, using = "priors"))
      post_dist_1 <- unlist(query_distribution(model, paste(query1, ">", query0), given = given_1, using = "posteriors"))
      post_dist_0 <- unlist(query_distribution(model, paste(query1, ">", query0), given = given_0, using = "posteriors"))
      
      # Combine the distributions into a single data frame
      combined_df <- bind_rows(
        combined_df,
        data.frame(Value = prior_dist_1, Distribution = "Prior", Given = given_1, stringsAsFactors = FALSE),
        data.frame(Value = post_dist_1, Distribution = "Posterior", Given = given_1, stringsAsFactors = FALSE),
        data.frame(Value = prior_dist_0, Distribution = "Prior", Given = given_0, stringsAsFactors = FALSE),
        data.frame(Value = post_dist_0, Distribution = "Posterior", Given = given_0, stringsAsFactors = FALSE)
      )
    }
    
    colnames(combined_df) <- c('Value', "Distribution", "Given")
    
    # Filter out non-finite values
    combined_df <- combined_df %>% filter(is.finite(Value))
    # Determine colors based on the number of given variables
    if (length(unique(combined_df$Given)) == 2) {
      color_values <- c("black", "gray")
    } else {
      color_values <- rainbow(length(unique(combined_df$Given)))
    }
    # Calculate summary statistics for the plot
    plot_means <- combined_df %>%
      group_by(Distribution, Given) %>%
      summarize(mean = mean(Value), sd = sd(Value))
    
    # Optionally print the query used
    if (print_query) {
      print(paste("Query used:", paste(query1, ">", query0)))
    }
    
    # Plot the results with specific legend settings if there is only one given variable
    ggplot(combined_df, aes(x = Value, fill = Distribution, color = Given)) +
      geom_density(alpha = 0.4) +
      labs(
        title = paste("Prior and Posterior Distributions of Naturalization Probability (", query_vars, "=1 vs. 0)", sep = ""),
        x = "Difference in Naturalization Probability",
        y = "Density"
      ) +
      scale_fill_manual(values = c("Prior" = "skyblue", "Posterior" = "coral")) +
      scale_color_manual(values = if (length(unique(combined_df$Given)) == 2) setNames(c("black", "gray"), unique(combined_df$Given)) else rainbow(length(unique(combined_df$Given))) ) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(override.aes = list(fill = NA)))
  }
}

plot_query <- function(model, query, given_vars = NULL, print_query = FALSE) {
  require(CausalQueries)
  require(ggplot2)
  # Initialize an empty data frame to store the combined results
  combined_df <- data.frame(Value = numeric(), Distribution = character(), Given = character(), stringsAsFactors = FALSE)
  
  # Determine the x-axis label based on the query
  if (grepl(">", query)) {
    x_label <- "Probability Difference"
  } else {
    x_label <- "Average Treatment Effect (ATE)"
  }
  
  if (is.null(given_vars)) {
    # Get the distributions for priors and posteriors
    prior_dist <- query_distribution(model, query, using = "priors")
    post_dist <- query_distribution(model, query, using = "posteriors")
    
    # Combine the distributions into a single data frame
    combined_df <- bind_rows(
      data.frame(Value = prior_dist, Distribution = "Prior", stringsAsFactors = FALSE),
      data.frame(Value = post_dist, Distribution = "Posterior", stringsAsFactors = FALSE)
    )
    
    colnames(combined_df) <- c('Value', "Distribution")
    
    # Filter out non-finite values
    combined_df <- combined_df %>% filter(is.finite(Value))
    
    # Optionally print the query used
    if (print_query) {
      print(query_model(model, query, using = c("priors", "posteriors"), cred = 89))
    }
    
    # Plot the results
    ggplot(combined_df, aes(x = Value, fill = Distribution)) +
      geom_density(alpha = 0.4) +
      labs(
        title = paste("Prior and Posterior Distributions of", query),
        x = x_label,
        y = "Density"
      ) +
      scale_fill_manual(values = c("Prior" = "skyblue", "Posterior" = "coral")) +
      theme_minimal() +
      theme(legend.position = "bottom") 
    
  } else {
    for (given_var in given_vars) {
      given_1 <- paste(given_var, "==1", sep = "")
      given_0 <- paste(given_var, "==0", sep = "")
      
      # Get the distributions for priors and posteriors with given conditions
      prior_dist_1 <- unlist(query_distribution(model, query, given = given_1, using = "priors"))
      prior_dist_0 <- unlist(query_distribution(model, query, given = given_0, using = "priors"))
      post_dist_1 <- unlist(query_distribution(model, query, given = given_1, using = "posteriors"))
      post_dist_0 <- unlist(query_distribution(model, query, given = given_0, using = "posteriors"))
      
      # Combine the distributions into a single data frame
      combined_df <- bind_rows(
        combined_df,
        data.frame(Value = prior_dist_1, Distribution = "Prior", Given = given_1, stringsAsFactors = FALSE),
        data.frame(Value = post_dist_1, Distribution = "Posterior", Given = given_1, stringsAsFactors = FALSE),
        data.frame(Value = prior_dist_0, Distribution = "Prior", Given = given_0, stringsAsFactors = FALSE),
        data.frame(Value = post_dist_0, Distribution = "Posterior", Given = given_0, stringsAsFactors = FALSE)
      )
    }
    
    colnames(combined_df) <- c('Value', "Distribution", "Given")
    
    # Filter out non-finite values
    combined_df <- combined_df %>% filter(is.finite(Value))
    
    # Optionally print the query used
    if (print_query) {
      print(query_model(model,
                        query, 
                        using = c("priors", "posteriors"), 
                        given = c(given_1, given_0), 
                        expand_grid = TRUE, 
                        cred = 89))
    }
    
    # Determine colors based on the number of given variables
    if (length(unique(combined_df$Given)) == 2) {
      color_values <- c("black", "gray")
    } else {
      color_values <- rainbow(length(unique(combined_df$Given)))
    }
    
    # Plot the results with specific legend settings if there is only one given variable
    ggplot(combined_df, aes(x = Value, fill = Distribution, color = Given)) +
      geom_density(alpha = 0.4) +
      labs(
        title = paste("Prior and Posterior Distributions of", query),
        x = x_label,
        y = "Density"
      ) +
      scale_fill_manual(values = c("Prior" = "skyblue", "Posterior" = "coral")) +
      scale_color_manual(values = if (length(unique(combined_df$Given)) == 2) setNames(c("black", "gray"), unique(combined_df$Given)) else rainbow(length(unique(combined_df$Given)))) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(override.aes = list(fill = NA)))
  }
}
