#model development

source('dataprocessing.R')
library(CausalQueries)
#Plotting
library(ggdag)
library(dagitty)
library(tidyverse)
library(parallel)
options(mc.cores = parallel::detectCores())

## For 
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install(c("graph", "RBGL", "Rgraphviz"))
library(pcalg)
library(igraph)


# Draw DAG ----------------------------------------------------------------

nat_matrix <- as.matrix(sapply(nat, as.numeric))

# Define the sufficient statistics
suffStat <- list(dm = nat_matrix, adaptDF = FALSE)

alpha <- 0.01  # Significance level for conditional independence tests
m.max <- 3  # Limit the conditioning set size to simplify the graph
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



# -------------------------------------------------------------------------


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



# Prepare Model ------------------------------------------------------------
#Set restrictions (~1 hour)

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





# Saved models ------------------------------------------------------------

saveRDS(m2, file = "m2.RDS")
saveRDS(m1, file = "m1.RDS")
#m1 <- readRDS("m1.RDS")
#m2 <- readRDS("m2.RDS")

