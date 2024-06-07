# model comparision 

#H1
plot_query(m1, "N[BT=1] - N[BT=0]", print_query = TRUE)
plot_query(m1_isable, "N[BT=1] - N[BT=0]", print_query = TRUE)

#H2
plot_query(m1, "N[BT=1] > N[BT=0]", print_query = TRUE)
plot_query(m1_isable, "N[BT=1] > N[BT=0]", print_query = TRUE)

#H3a
plot_query(m1, "N[BT=1] > N[BT=0]", given_vars = "S", print_query = TRUE)
plot_query(m1_isable, "N[BT=1] > N[BT=0]", given_vars = "S", print_query = TRUE)

#H3b
plot_query(m1, "N[BT=1] > N[BT=0]", given_vars = "I", print_query = TRUE)
plot_query(m1_isable, "N[BT=1] > N[BT=0]", given_vars = "I", print_query = TRUE)
