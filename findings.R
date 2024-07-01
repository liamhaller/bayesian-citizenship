#Figures


library(corrplot) #Needed for Figure 2
library(ggdag) #to Plot the dag in Figure 1
library(tidyverse)
library(patchwork) #needed for figure 6 to plot combined dags

confound.m3_status <- readRDS("m3_c.RDS")
confound.m3_ability <- readRDS("m3_ca.RDS")
m3 <- readRDS("m3.RDS")


# Figure 1: DAG -----------------------------------------------------------


ggdag(M3_dag, text = FALSE, layout = "fr", node_size = 12) +
  geom_dag_label_repel(aes(label = label), size =3, force =8) +  # Add repelled labels
  ggplot2::guides(fill = FALSE, color = FALSE) +  # Disable the legend
  theme_dag_blank()



# Figure 2: Correlations between variables included in model --------------

# Load the necessary packages

# Assuming your data frame is named 'nat_test'
# Calculate the correlation matrix
cor_matrix <- cor(nat)
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


# H1a ---------------------------------------------------------------------

#Base Model
h1a_m3 <- plot_query(m3, "N[BT=1] - N[BT=0]", print_query = TRUE)

#Model with confounding between BT & S
h1a_m3_status <- plot_query(confound.m3_status, "N[BT=1] - N[BT=0]", print_query = TRUE)

#Model with confounding between BT & A
h1a_m3_ability <- plot_query(confound.m3_ability, "N[BT=1] - N[BT=0]", print_query = TRUE)


h1a_m3
h1a_m3_status
h1a_m3_ability

# H1b ---------------------------------------------------------------------

#Base Model
query_model(m3, "N[BT=1] > N[BT=0]", using = "posteriors")
h1b_m3 <- plot_query(m3, "N[BT=1] > N[BT=0]", print_query = TRUE)

#Model with confounding between BT & S
h1b_m3_status <- plot_query(confound.m3_status, "N[BT=1] > N[BT=0]", print_query = TRUE)

#Model with confounding between BT & A
h1b_m3_ability <- plot_query(confound.m3_ability, "N[BT=1] > N[BT=0]", print_query = TRUE)


h1b_m3
h1b_m3_status
h1b_m3_ability

# H2 & Figure 4 -----------------------------------------------------------


#Base Model
query_model(m3, "N[S=1] - N[S=0]", using = "posteriors")
h2_m3 <- plot_query(m3, "N[BT=1] > N[BT=0]", given_vars = "S", print_query = TRUE)

#Model with confounding between BT & S
h2_m3_status <- plot_query(confound.m3_status, "N[BT=1] > N[BT=0]", given_vars = "S", print_query = TRUE)

#Model with confounding between BT & A
h2_m3_ability <- plot_query(confound.m3_ability, "N[BT=1] > N[BT=0]", given_vars = "S", print_query = TRUE)

h2_m3
h2_m3_status
h2_m3_ability

# H3 & Figure 5 -----------------------------------------------------------

#Base Model
query_model(m3, "N[O=1] - N[O=0]", using = "posteriors")
h3_m3 <- plot_query(m3, "N[BT=1] > N[BT=0]", given_vars = "O", print_query = TRUE)

#Model with confounding between BT & S
h3_m3_status <-plot_query(confound.m3_status, "N[BT=1] > N[BT=0]", given_vars = "O", print_query = TRUE)

#Model with confounding between BT & A
h3_m3_ability <- plot_query(confound.m3_ability, "N[BT=1] > N[BT=0]", given_vars = "O", print_query = TRUE)

h3_m3
h3_m3_status
h3_m3_ability

# Figure 6 ----------------------------------------------------------------
# Define DAGs with specified coordinates
M3_ability_dag <- dagify(
  N ~ O + S + A,
  S ~  O + BT,
  BT ~ HC,
  A ~ S + HC,
  
  coords = list(
    x = c(N = 2, O = 1, S = 2, BT = 1.5, A = 3, HC = 2.5),
    y = c(N = 1, O = 3, S = 2, BT = 4, A = 3, HC = 4)
  ),
  labels = c(N = "Naturalization", O = "Origin (HDI)", 
             S = "Status", BT = "Bureaucratic Trajectory", A = "Ability", HC = "Human Capital"))

M3_status_dag <- dagify(
  N ~ O + S + A,
  S ~  O + BT + HC,
  BT ~ HC,
  A ~ S,
  
  coords = list(
    x = c(N = 2, O = 1, S = 2, BT = 1.5, A = 3, HC = 2.5),
    y = c(N = 1, O = 3, S = 2, BT = 4, A = 3, HC = 4)
  ),
  labels = c(N = "Naturalization", O = "Origin (HDI)", 
             S = "Status", BT = "Bureaucratic Trajectory", A = "Ability", HC = "Human Capital"))

# Create the first DAG plot with manual layout
m_prime <- ggdag(M3_ability_dag, text = FALSE, layout = "manual", node_size = 12) +
  geom_dag_label_repel(aes(label = label), size = 3, force = 2) +  # Add repelled labels
  ggplot2::guides(fill = FALSE, color = FALSE) +  # Disable the legend
  geom_dag_node(aes(color = name == "HC"), size = 12) +  # Different color for HC
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +  # Define colors
  theme_dag_blank() +
  geom_dag_edges() +  # Plot edges last to have them above the text
  ggtitle(expression(italic("m'")))  # Title with italic m'

# Create the second DAG plot with manual layout
m_double_prime <- ggdag(M3_status_dag, text = FALSE, layout = "manual", node_size = 12) +
  geom_dag_label_repel(aes(label = label), size = 3, force = 2) +  # Add repelled labels
  ggplot2::guides(fill = FALSE, color = FALSE) +  # Disable the legend
  geom_dag_node(aes(color = name == "HC"), size = 12) +  # Different color for HC
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +  # Define colors
  theme_dag_blank() +
  geom_dag_edges() +  # Plot edges last to have them above the text
  ggtitle(expression(italic("m''")))  # Title with italic m''

# Combine the plots side by side
combined_plot <- m_prime + m_double_prime

# Display the combined plot
print(combined_plot)








# Misc --------------------------------------------------------------------


plot_query(m3, "N[A=1] - N[A=0]", print_query = TRUE)

plot_query(m3, "N[BT=1] > N[BT=0]", given_vars = "A", print_query = TRUE)

plot_query(m3, "S[BT=1] > S[BT=0]", print_query = TRUE)


library(ggdag)
library(patchwork)



