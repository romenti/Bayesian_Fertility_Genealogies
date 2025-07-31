#### Code to produce validation tables ####


#### upload all the relevant packages ####

source('code/upload_packages.R')

#### upload relevant data sets ####

# Bayesian model results
load('Results/bayesian_model_results.RData')

# Indirect estimation results
load('Results/indirect_TFR_results.RData')

# Historical TFR values
load('Data/historical_TFR.RData')



table_bTFR_error = mcmc.out_TFR_final %>%
  left_join(hist_data) %>%
  filter(!(is.na(TFR_true))) %>%
  group_by(country) %>%
  mutate(pred=(TFR_median-TFR_true)/TFR_true) %>%
  summarise(mean_error_bTFR=mean(abs(pred)),
            median_error_bTFR=median(abs(pred)),
            rmse_bTFR=sqrt(mean(pred^2)))




table_iTFR_error = data_iTFR %>%
  left_join(hist_data) %>%
  filter(!(is.na(TFR_true))) %>%
  group_by(country) %>%
  mutate(pred=(iTFR_star-TFR_true)/TFR_true) %>%
  summarise(mean_error_iTFR=mean(abs(pred)),
            median_error_iTFR=median(abs(pred)),
            rmse_iTFR=sqrt(mean(pred^2)))


table_xTFR_error = data_xTFR %>%
  left_join(hist_data) %>%
  filter(!(is.na(TFR_true))) %>%
  group_by(country) %>%
  mutate(pred=(xTFR_star-TFR_true)/TFR_true) %>%
  summarise(mean_error_xTFR=mean(abs(pred)),
            median_error_xTFR=median(abs(pred)),
            rmse_xTFR=sqrt(mean(pred^2)))



table1_a = table_bTFR_error %>%
  left_join(table_iTFR_error) %>%
  left_join(table_xTFR_error) %>%
  select(country,rmse_bTFR,rmse_iTFR,rmse_xTFR,mean_error_bTFR,mean_error_iTFR,mean_error_xTFR,
         median_error_bTFR,median_error_iTFR,median_error_xTFR)
table1_a[,-1] <- round(table1_a[,-1],3)

table1_a_latex = kable(table1_a, format = "latex", booktabs = F,
      align = c("l", rep("r", 9)),
      linesep='',
      col.names = c("Country", rep(c("$bTFR^*$", "$iTFR^*$", "$xTFR^*$"), 3))) %>%
  add_header_above(c(" " = 1, "RMSE" = 3, "Mean Abs. Error" = 3, "Median Abs. Error" = 3)) %>%
  kable_styling(latex_options = "hold_position")

writeLines(table1_a_latex, "Tables/table1_a.tex")


#### (b) ####


table1_b = mcmc.out_TFR_final %>%
  rename(years=year) %>%
  left_join(hist_data) %>%
  filter(!is.na(TFR_true)) %>%
  mutate(coverage=ifelse(TFR_true>=TFR_lower & TFR_true<=TFR_upper,1,0),
         below=ifelse(TFR_true<TFR_lower,1,0),
         above=ifelse(TFR_true>TFR_upper,1,0)) %>%
  group_by(country) %>%
  summarise(coverage=mean(coverage),
            below=mean(below),
            above=mean(above))
  
table1_b[,-1] <- round(table1_b[,-1],3)


table1_b_latex = kable(table1_b, format = "latex", booktabs = F,
      align = c("l", rep("r", 3)),
      linesep='',
      col.names = c("Country",c("Coverage", "Below", "Above"))) %>%
  add_header_above(c(" " = 1, "RMSE" = 3, "Mean Abs. Error" = 3, "Median Abs. Error" = 3)) %>%
  kable_styling(latex_options = "hold_position")



writeLines(table1_b_latex, "Tables/table1_b.tex")
