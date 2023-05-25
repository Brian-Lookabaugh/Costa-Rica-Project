############################################################################
##########-------------Synthetic Control Estimation--------------###########
############################################################################

# Observing the Contribution of the Donor Pool and Predictor Importance
synth %>% plot_weights()

# Predictor Values Between Real and Synthetic Costa Rica
synth %>% grab_balance_table()

# (Main Result) Trend in Outcome Between Real and Synthetic Costa Rica
ts.plot <-
  synth %>% plot_trends() + 
  labs(title = "Trends in Democracy Between Costa Rica and Synthetic Control", caption = "") + 
  ylab("Democracy") + xlab("Year")

ggsave(
  "ts.plot.png",
  width = 8,
  height = 6,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/Costa Rica Project/Graphics"
)

# Gap Between Real and Synthetic Costa Rica
diff.plot <-
  synth %>% plot_differences() + 
  labs(title = "Difference in Democracy Between Costa Rica and Synthetic Control", caption = "") + 
  ylab("Difference in Democracy") + xlab("Year")

ggsave(
  "diff.plot",
  width = 8,
  height = 6,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/Costa Rica Project/Graphics"
)

# In-Space Placebo Test
s.placebo <- 
  synth %>% plot_placebos() + 
  labs(title = "In-Space Placebo Test", caption = "") + 
  ylab("Difference in Democracy") + xlab("Year")

ggsave(
  "s.pplot",
  width = 8,
  height = 6,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/Costa Rica Project/Graphics"
)

# Leave-One-Out Robustness Check

# RMSPE Visualization
mspe.plot <- synth %>% plot_mspe_ratio() + 
  labs(title = "Ratio of Pre/Post-Treatment MSPE")

ggsave(
  "mspe.plot",
  width = 6,
  height = 8,
  path = "C:/Users/brian/Desktop/Peacebuilding Dissertation/Costa Rica Project/Graphics"
)

# Estimating Results for Alternative Pre-Treatment Outcome Specifications

# Statistical Inference for Alternative Pre-Treatment Outcome Specifications