#model development

library(CausalQueries)

library(ggdag)
library(dagitty)
library(ggplot2)



# Draw DAG ----------------------------------------------------------------

model_statement <- (" 
  I -> N ;
  A -> N;
  BT -> N;
  F -> BT;
  T -> BT
  S -> A
 ")

M1 <- dagify(
  N ~ I + A + BT,
  BT ~ T + F,
  F ~ A,
  A ~ S,
  labels = c(N = "Naturalization", I = "Interest", 
             A = "Ability", S = "Status", 
             BT = "Bureaucratic Trajectory", T = "Treatment",
             F = "Frequency"),
  
  exposure = c("BT", "I", "T", "F"),
  outcome = c('A', "S")
)

ggdag_status(M1, layout = "nicely",
             use_labels = "label", text = FALSE) +
  guides(fill = FALSE, color = FALSE) +  # Disable the legend
  theme_dag()




make_model(M1)
CausalQueries::plot_model(make_model(model_statement)) 
