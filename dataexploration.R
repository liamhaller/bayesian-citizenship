# 



# Alternitive Structures --------------------------------------------------

nat_test <- nat_full %>% select(-c(T,F)) %>% 
  rename(O = I, A = R) %>% 
 # select(-A) %>% 
  as.data.frame()

nat_matrix <- as.matrix(sapply(nat_test, as.numeric))

# Define the sufficient statistics
suffStat <- list(dm = nat_matrix, adaptDF = FALSE)
set.seed(514)
alpha <- 0.05# Significance level for conditional independence tests
m.max <- 3  # Limit the conditioning set size to simplify the graph

skel_fit <-  skeleton(suffStat, 
                indepTest = binCItest, #gaussCItest binCItest
                alpha = alpha,
                method = "original",
                # maj.rule = TRUE,
                labels = colnames(nat_matrix), verbose = TRUE)
plot(graph_from_adjacency_matrix(as(skel_fit@graph, "matrix"), mode = "undirected"))

pc_fit <- pc(suffStat, 
             indepTest = binCItest, #gaussCItest binCItest
             alpha = alpha,
             m.max = m.max, 
            # maj.rule = TRUE,
             labels = colnames(nat_matrix))

# Convert the result to an igraph object
graph_amat <- as(pc_fit@graph, "matrix")
g <- graph_from_adjacency_matrix(graph_amat, mode = "directed")

# Plot the resulting DAG
plot(g, main = "Learned DAG from PC Algorithm")

list(colnames(nat_matrix),colnames(nat_matrix))
fixedGaps <- matrix(data = FALSE, 
                    nrow = 5, 
                    ncol = 5,
                    dimnames =  list(colnames(nat_matrix),colnames(nat_matrix)))


# M4 ----------------------------------------------------------------------




#Model structure
M4_dag <- dagify(
  N ~ BT + S + O,
  S ~ BT,
  labels = c(N = "Naturalization", O = "Origin (HDI)", 
              BT = "Bureaucratic Trajectory", S = "Status"))



ggdag(M4_dag, 
      use_labels = "label", text = FALSE) +
  ggplot2::guides(fill = FALSE, color = FALSE) +  # Disable the legend
  theme_dag()




M4 <- make_model(M4_dag) 

M4 <- M4 %>%
  #Weakly incrasing (O does not decrease A)
  set_restrictions(decreasing("O","A")) 
  #Weakly incrasing (A does not decrease N)
  #set_restrictions(decreasing("A", "N")) 

grab(M4, 'nodal_types')


#M3 <- make_model("BT <-> A -> N; BT ->S ->N; O -> S; O->N") 

#saveRDS(m3, file = "m3.RDS")



m4.nat_test_collapsed <- collapse_data(data = nat_test, model = M4, summary = FALSE)

Sys.time()
#Update model with data
m4 <- M4 %>% 
  update_model(data = m4.nat_test_collapsed,
               data_type = 'compact',
               iter = 8000,
               keep_fit = TRUE,
               keep_event_probabilities = TRUE)
Sys.time()

saveRDS(m4, file = "m4.RDS")


# M3 ----------------------------------------------------------------------

ggdag_equivalent_dags(M3_dag)
#Model structure
M3_dag <- dagify(
  N ~ O + S + A,
  S ~ BT + O,
  A ~ S,
  
  labels = c(N = "Naturalization", O = "Origin (HDI)", 
             S = "Status",  BT = "Bureaucratic Trajectory", A = "Ability"))





ggdag(M3_dag, 
      use_labels = "label", text = FALSE) +
  ggplot2::guides(fill = FALSE, color = FALSE) +  # Disable the legend
  theme_dag_blank()
theme_dag_blank()
test <- make_model("X->Y")
testout <- test %>% set_restrictions(decreasing("X", "Y"))
testout$parameters_df




#M3 <- make_model("BT <-> A -> N; BT ->S ->N; O -> S; O->N") 
  
#saveRDS(m3, file = "m3.RDS")





str(M3)

M3$causal_types


# 
#   #Positive effect of interest on intention to apply
#   # set_restrictions(decreasing('I', 'N')) %>% 
#   #Positive effect of frequency on turbulant BT
#   set_restrictions(decreasing('F', 'BT')) %>% # could be stronger?
#   #Negitive effect of treatment on bureaucratic trajectory
#   set_restrictions(increasing('T', 'BT')) %>% # could be stronger
#   #positive effect of status (perm residency) on Requirements
#   set_restrictions(decreasing('S', 'R')) %>% 
#   #positive effect of requirments on intention to apply
#   set_restrictions(decreasing('R', 'N')) 


if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("graph", "RBGL", "Rgraphviz"))
library(pcalg)
library(Rgraphviz)

# Load necessary packages
library(pcalg)
library(igraph)

tnat <- nat %>% select(-c(T, F, R))

# Prepare your data
#nat_matrix <- as.matrix(nat)
nat_matrix <- as.matrix(sapply(tnat, as.numeric))

# Define the sufficient statistics
suffStat <- list(dm = nat_matrix, adaptDF = FALSE)

alpha <- 0.01  # Significance level for conditional independence tests
m.max <- 3  # Limit the conditioning set size to simplify the graph
pc_fit <- pc(suffStat, indepTest = binCItest,
             alpha = alpha,
             m.max = m.max, 
             labels = colnames(nat_matrix))

# Convert the result to an igraph object
graph_amat <- as(pc_fit@graph, "matrix")
g <- graph_from_adjacency_matrix(graph_amat, mode = "directed")

# Plot the resulting DAG
plot(g, main = "Learned DAG from PC Algorithm")

# -------------------------------------------------------------------------



#Model structure
M9_dag <- dagify(
  N ~ I + S,
  S ~ BT + I,
  labels = c(N = "Naturalization", I = "Origin (HDI)", 
             R = "Requirements", S = "Status", 
             BT = "Bureaucratic Trajectory", T = "Treatment",
             F = "Frequency"))
ggdag(M9_dag, 
      use_labels = "label", text = FALSE) +
  ggplot2::guides(fill = 'none', color = 'none') +  # Disable the legend
  theme_dag()


 M9 <- make_model(M9_dag) 
#   #positive effect of status (perm residency) on Requirements
#   set_restrictions(decreasing('S', 'R')) %>% 
#   #positive effect of requirments on intention to apply
#   set_restrictions(decreasing('R', 'N')) 
### Data Formatting
#Fit data to model types so that it can be used to update model
M9.nat_collapsed <- collapse_data(data = tnat, model = M9, summary = FALSE)

#Update model with data
m9 <- M9 %>% 
  update_model(data = M9.nat_collapsed,
               data_type = 'compact',
               iter = 6000,
               keep_fit = TRUE,
               keep_event_probabilities = TRUE)

m9 %>% 
  query_model("N[I=1] - N[I = 0]", 
              using = c("priors", "posterior"),
              expand_grid = TRUE)




m1 %>% 
  query_model("N[BT=1] > N[BT = 0]", 
              using = c("posterior"), 
              expand_grid = TRUE)

plot_query(m1, "N[BT=1] - N[BT=0]", given_vars = "S", print_query = TRUE)

plot_query(m1, "N[BT=1, I = 0] > N[BT=0, I =0]", given_vars = "S", print_query = TRUE)


plot_query(m1, "N[BT=1] - N[BT=0]", given_vars = "I", print_query = TRUE)

plot_query(m1, "N[BT=1] > N[BT=0]", given_vars = "I", print_query = TRUE)










evaluate_query(model = m9, outcome = "N", query_vars = "S", given_vars = "BT", print_query = FALSE)

evaluate_query(model = m9, outcome = "N", query_vars = "BT", given_vars = "I", print_query = FALSE)


evaluate_query(model = m9, outcome = "N", query_vars = "R", given_vars = "BT", print_query = FALSE)


plot_query(m1, "N[S=1] > N[S=0]", print_query = TRUE)
plot_query(m2, "N[S=1] > N[S=0]", print_query = TRUE)


plot_query(m9, "N[S=1] - N[S=0]", print_query = TRUE)
plot_query(m9, "N[BT=1] - N[BT=0]", print_query = TRUE)


plot_query(m9, "N[BT=1] - N[BT=0]", given_vars = c("S"), print_query = TRUE)



plot_query(m9, "N[BT=1] - N[BT=0]", print_query = TRUE)
plot_query(m9, "N[BT=1] - N[BT=0]", print_query = TRUE)


debugonce(plot_query_distributions)
plot_query(m9, "N[I=1] - N[I=0]", given_vars = c("BT"), print_query = TRUE)
plot_query(m9, "N[S=1] - N[S=0]", given_vars = c("BT"), print_query = TRUE)


plot_query(m9, "N[BT=1] - N[BT=0]", print_query = TRUE)

plot_query(m9, "N[BT=1] > N[BT=0]", given_vars = c("I"), print_query = TRUE)

plot_query(m9, "N[BT=0] > N[BT=1]", given_vars = c("I"), print_query = TRUE)



query_model(m1, "N[BT=0] > N[BT=1]", given = "BT == 1 & N == 1")

# -------------------------------------------------------------------------


# Generate posterior predictive samples
posterior_predictive_samples <- simulate_data(m1)

# Calculate the observed test statistic
observed_statistic <- mean(nat$N)

# Calculate the test statistic for each posterior predictive sample
posterior_predictive_statistics <- mean(posterior_predictive_samples$N, 2, mean)

# Calculate the Bayesian p-value
bayesian_p_value <- mean(posterior_predictive_statistics >= observed_statistic)

# Print the Bayesian p-value
print(bayesian_p_value)

query_model(m1, "S=1")

get_query_types(m1,"N==1")$types

get_type_prob(m1)
CausalQueries:::get_event_probabilities(m1)

weighted.mean(get_query_types(m1,"N[BT=1] > N[BT=0]")$types, CausalQueries:::get_type_prob(m1))



plot_query(m1, "N[BT=1] > N[BT=0]", given_vars = "S", print_query = TRUE)



# Sample data
nat <- data.frame(
  I = c(1, 1, 1, 1, 1, 1),
  S = c(1, 1, 1, 1, 1, 1),
  BT = c(1, 0, 0, 1, 0, 1),
  N = c(1, 0, 0, 1, 0, 1)
)

# How often BT is 1 when both F and T are 1
both_FT_1 <- sum(nat$I == 1 & nat$S == 1 & nat$BT == 1)
both_FT_total <- sum(nat$I == 1 & nat$S == 1)
both_FT_ratio <- both_FT_1 / both_FT_total

# How often BT is 1 when either F or T is 1
either_FT_1 <- sum((nat$I == 1 | nat$S == 1) & nat$BT == 1)
either_FT_total <- sum(nat$I == 1 | nat$S == 1)
either_FT_ratio <- either_FT_1 / either_FT_total

# Print the results
cat("BT is 1 when both F and T are 1:", both_FT_ratio, "\n")
cat("BT is 1 when either F or T is 1:", either_FT_ratio, "\n")


# descriptive overview ----------------------------------------------------


# Load the necessary packages
library(dplyr)
library(gt)
library(tidyr)

# Assuming your data frame is named 'nat_test'
# Summarize the data
summary_table <- nat_test %>%
  summarise(
    sum_O = sum(O),
    mean_O = mean(O),
    sum_S = sum(S),
    mean_S = mean(S),
    sum_A = sum(A),
    mean_A = mean(A),
    sum_BT = sum(BT),
    mean_BT = mean(BT),
    sum_N = sum(N),
    mean_N = mean(N)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Function", "Variable"),
    names_pattern = "(sum|mean)_(.*)"
  ) %>%
  pivot_wider(
    names_from = Variable,
    values_from = value
  )

# Create a GT table
gt_table <- summary_table %>%
  gt() %>%
  tab_header(
    title = "Descriptive Overview of Binary Data"
  ) %>%
  cols_label(
    Function = "Function"
  ) %>%
  fmt_number(
    columns = c(O, S, A, BT, N),
    decimals = 2
  )

# Print the GT table
print(gt_table)

# Load the necessary packages
library(corrplot)

# Assuming your data frame is named 'nat_test'
# Calculate the correlation matrix
cor_matrix <- cor(nat_test)
# Create a correlation plot
col<- colorRampPalette(c("red", "grey", "blue"))(20)
corrplot(cor_matrix,
         method = "number",
         type = "upper", 
         order = "hclust", 
         col = col,  
         tl.col="black",
         tl.srt = 0  # Rotate the labels 45 degrees
         )
# Print the correlation plot
print(cor_plot)

# Add number of observations as annotations
for (i in 1:length(num_obs)) {
  mtext(side = 3, at = i, line = 0, text = paste(names(num_obs)[i], " (n=", num_obs[i], ")", sep = ""), cex = 0.8, col = "black")
}





# Older Models ------------------------------------------------------------

nat <- nat_full %>% 
  select(-c(T,F,R)) %>% 
  as.data.frame() #this is necessary in order for CausalQueries::collapse_data function to work 

#Data set that only includes indivduals who have fufilled the requirements to naturalize
nat_isable <- nat_full %>% 
  filter(R==1) %>% 
  select(-c(T,F,R)) %>% 
  as.data.frame() #this is necessary in order for CausalQueries::collapse_data function to work 



#Model structure
M1_dag <- dagify(
  N ~ O + S,
  S ~ BT + O,
  labels = c(N = "Naturalization", O = "Origin (HDI)", 
             S = "Status",  BT = "Bureaucratic Trajectory"))

M2_dag <- dagify(
  N ~ I + S,
  S ~  I,
  labels = c(N = "Naturalization", I = "Origin (HDI)", 
             S = "Status",  BT = "Bureaucratic Trajectory"))



ggdag(M1_dag, 
      use_labels = "label", text = FALSE) +
  ggplot2::guides(fill = FALSE, color = FALSE) +  # Disable the legend
  theme_dag()

ggdag(M2_dag, 
      use_labels = "label", text = FALSE) +
  ggplot2::guides(fill = FALSE, color = FALSE) +  # Disable the legend
  theme_dag()



#Base model
M1 <- make_model(M1_dag) 
#model without BT
M2 <- make_model(M2_dag) 



#Formatted data for base model
m1.nat_collapsed <- collapse_data(data = nat, model = M1, summary = FALSE)
#Formatted data for base model with only indivduals who are able
m1.nat_isable_collapsed <- collapse_data(data = nat_isable, model = M1, summary = FALSE)
#formatted data for model without bt
m2.nat_collapsed <- collapse_data(data = nat, model = M2, summary = FALSE)



### Model building
#Update base model with data
m1 <- M1 %>% 
  update_model(data = m1.nat_collapsed,
               data_type = 'compact',
               iter = 6000,
               keep_fit = TRUE,
               keep_event_probabilities = TRUE)

m1_isable <- M1 %>% 
  update_model(data = m1.nat_isable_collapsed,
               data_type = 'compact',
               iter = 6000,
               keep_fit = TRUE,
               keep_event_probabilities = TRUE)



#Update model with data
m2 <- M2 %>% 
  update_model(data = m2.nat_collapsed,
               data_type = 'compact',
               iter = 6000,
               keep_fit = TRUE,
               keep_event_probabilities = TRUE)


pc_fit <- pc(suffStat, 
             indepTest = binCItest,
             alpha = alpha,
             m.max = m.max, 
             labels = colnames(nat_matrix))

# Convert the result to an igraph object
graph_amat <- as(pc_fit@graph, "matrix")
g <- graph_from_adjacency_matrix(graph_amat, mode = "directed")

# Plot the resulting DAG
plot(g, main = "Learned DAG from PC Algorithm")


