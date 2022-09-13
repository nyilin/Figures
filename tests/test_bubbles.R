library(Figures)
data(df_rank)
head(df_rank)
bubble_for_rank(data = df_rank, y_name = "Variable", x_name = "method",
                rank_name = "mean_rank", highlight_name = "sig")
bubble_for_rank(data = df_rank,
                y_name = "Variable", x_name = "method", rank_name = "mean_rank",
                highlight_name = "sig",
                arrange_rows_by = "Linear XGBoost (AUC-based)")
bubble_for_rank(data = df_rank,
                y_name = "Variable", x_name = "method", rank_name = "mean_rank",
                highlight_name = "sig",
                arrange_rows_by = "Linear XGBoost (AUC-based)",
                x_order = c("Logistic", "Linear XGBoost (loss-based)",
                            "Linear XGBoost (AUC-based)", "Linear SVM"),
                vline_at = 1.5)
