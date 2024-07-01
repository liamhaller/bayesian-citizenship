#The sheet is used to develop and compile the network models

# 1.`pcalg` is used to run the PC algorithim to develop the structure 
#of the dag

#2. The structre of the DAG is translated to ggdag which can be used for
#plotting and imported into CausalQueries 

#3. Initialize paramters with make_model function and set the restrictions/
#confounding for each model

#4. Data is then transformed into the appropriate format and used to update
#the priors to build the probabilistic model

#5. The output is then saved to a RDS object to avoid recompiling 

source('dataprocessing.R')
library(CausalQueries)
library(ggdag)
library(dagitty)
library(tidyverse)
library(parallel) 
options(mc.cores = parallel::detectCores())

#The following two packages need to be installed from BioManager
#How to install
## For 
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install(c("graph", "RBGL", "Rgraphviz"))
library(pcalg) #PC algorithim for skelton of Dag
library(igraph) #Plot skelton 


# Draw DAG ----------------------------------------------------------------

nat_matrix <- as.matrix(sapply(nat, as.numeric))

# Define the sufficient statistics
suffStat <- list(dm = nat_matrix, adaptDF = FALSE)

alpha <- 0.01  # Significance level for conditional independence tests
m.max <- 3  # Limit the conditioning set size to simplify the graph

skel_fit <-  skeleton(suffStat, 
                      indepTest = binCItest, #gaussCItest binCItest
                      alpha = alpha,
                      method = "original",
                      # maj.rule = TRUE,
                      labels = colnames(nat_matrix), verbose = TRUE)
plot(graph_from_adjacency_matrix(as(skel_fit@graph, "matrix"), mode = "undirected"))




# -------------------------------------------------------------------------


M3_dag <- dagify(
  N ~ O + S + A,
  S ~ BT + O,
  A ~ S,
  
  coords = list(
    x = c(N = 2, O = 1, S = 2, BT = 1.5, A = 3),
    y = c(N = 1, O = 3, S = 2, BT = 4, A = 3)
  ),
  labels = c(N = "Naturalization", O = "Origin (HDI)", 
             S = "Status", BT = "Bureaucratic Trajectory", A = "Ability"))


# Create the third DAG plot with manual layout
ggdag(M3_dag, text = FALSE, layout = "manual", node_size = 12) +
  geom_dag_label_repel(aes(label = label), size = 3, force = 12) +  # Add repelled labels
  ggplot2::guides(fill = FALSE, color = FALSE) +  # Disable the legend
  geom_dag_node(size = 12) +  # Nodes without HC highlighting
  theme_dag_blank() +
  geom_dag_edges() +  # Plot edges last to have them above the text
  ggtitle(expression(italic("m")))  # Title with italic m



# Prepare Model ------------------------------------------------------------

#Create Model
M3 <- CausalQueries::make_model(M3_dag) 

#Set Restrictiosn
M3 <- M3 %>% 
  #Weakly incrasing (S does not decrease A)
  set_restrictions(decreasing("S","A")) %>%
  #Weakly incrasing (A does not decrease N)
  set_restrictions(decreasing("A", "N")) %>%
  #Weakly incrasing (O does not decrease S)
  set_restrictions(decreasing("O", "S"))

#Set confounding for Robustness checks
M3_c <- M3 %>%  set_confound("BT <-> S")
M3_ca <- M3 %>%  set_confound("BT <-> A")


## Collapse Data
m3.nat_collapsed <- collapse_data(data = nat, model = M3, summary = FALSE)
m3_c.nat_collapsed <- collapse_data(data = nat, model = M3_c, summary = FALSE)
m3_ca.nat_collapsed <- collapse_data(data = nat, model = M3_ca, summary = FALSE)


## Run Models (~15-20 minutes each)
Sys.time()
#Update model with data
m3 <- M3 %>% 
  update_model(data = m3.nat_collapsed,
               data_type = 'compact',
               iter = 8000,
               keep_fit = TRUE,
               keep_event_probabilities = TRUE)
Sys.time()

Sys.time()
#Update model with data
m3_c <- M3_c %>% 
  update_model(data = m3_c.nat_collapsed,
               data_type = 'compact',
               iter = 8000,
               keep_fit = TRUE,
               keep_event_probabilities = TRUE)
Sys.time()


Sys.time()
#Update model with data
m3_ca <- M3_ca %>% 
  update_model(data = m3_ca.nat_collapsed,
               data_type = 'compact',
               iter = 8000,
               keep_fit = TRUE,
               keep_event_probabilities = TRUE)
Sys.time()


# Save models ------------------------------------------------------------

# saveRDS(m2, file = "m2.RDS") # 
# saveRDS(m1, file = "m1.RDS") #
saveRDS(m3, file = "m3.RDS")
saveRDS(m3_c, file = "m3_c.RDS")
saveRDS(m3_ca, file = "m3_ca.RDS")


