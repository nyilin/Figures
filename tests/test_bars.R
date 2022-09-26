data(df_compas_shapley_vic, package = "ShapleyVIC")
library(magrittr)
df_bars <- df_compas_shapley_vic %$% ShapleyVIC::summarise_shapley_vic(
  val = shapley_vic_val, val_sd = sage_sd, var_names = var_names
)
df_bars %$% Figures::draw_bars(
  val = val, var_names = Variable, x_lab = "ShapleyVIC values"
)
