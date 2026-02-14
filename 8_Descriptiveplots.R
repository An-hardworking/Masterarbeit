options(bitmapType = "cairo")


#Use this: Lines with CI -----------------------------------------------------------

summary_stats <- function(x) {
  n <- length(x)
  mean_val <- mean(x)
  se <- sd(x) / sqrt(n)
  sd <- sd(x)
  ci <- qt(0.975, df = n - 1) * se
  return(data.frame(mean = mean_val, stddev= sd, lower = mean_val - ci, upper = mean_val + ci))
}

# Compute summary statistics for Verification and Trust
verification_summary <- final_data %>%
  group_by(Exposure, Reliability) %>%
  summarise(
    stats = list(summary_stats(Verification))
  ) %>%
  unnest(stats)

trust_summary <- final_data %>%
  group_by(Exposure, Reliability) %>%
  summarise(
    stats = list(summary_stats(Trust))
  ) %>%
  unnest(stats)

# Compute overall means with confidence intervals
verification_overall <- final_data %>%
  group_by(Exposure) %>%
  summarise(
    stats = list(summary_stats(Verification))
  ) %>%
  unnest(stats) %>%
  mutate(Reliability = "Overall")

trust_overall <- final_data %>%
  group_by(Exposure) %>%
  summarise(
    stats = list(summary_stats(Trust))
  ) %>%
  unnest(stats) %>%
  mutate(Reliability = "Overall")

# Combine group-specific and overall summaries
verification_combined <- bind_rows(verification_summary, verification_overall)
trust_combined <- bind_rows(trust_summary, trust_overall)
size <- 20
####theme####
theme_custom <- theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", size = size),
    plot.title = element_text(size = size, face = "bold", hjust = 0.5),
    axis.title = element_text(size = size),
    #axis.text = element_text(size = 12),
    plot.background = element_rect(fill = "white", color = NA), # White background
    #panel.grid.major = element_blank(),                         # Remove major grid lines
    #panel.grid.minor = element_blank(),                         # Remove minor grid lines
    #panel.border = element_blank(),                              # Remove border
    legend.title = element_text(size = size), #here size for guide of legend
    legend.text = element_text(size = size),    
    legend.position = c(0.8, 0.85),
    legend.background = element_rect(fill = "white", color = NA)
  )
par(mfrow = c(2, 1))  # Optional, reset to single plot layout
####Plots####
# Plot for Verification
(verification_plot <- ggplot(verification_summary, aes(x = Exposure, y = mean, 
                                                       color = Reliability, group = Reliability)) +
  geom_line(size = 0.8) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + #just from min to max is fine, no puffer needed
  scale_y_continuous(limits = c(0, 4)) +
  #scale_y_continuous(breaks=seq(1, 4, by = 1)) +
  #scale_y_break(c(0, 1), scales = "free") +
  labs(title = "Mean Verification by Exposure and Reliability (95% CI)",
       x = "Exposure",
       y = "Mean Number of Verified Parameters",
       color = "Reliability") +
  theme_custom +
  #scale_fill_manual(values = c("High" = "red", "Low" = "blue")) +
  scale_color_manual(values = c("High" = "red", "Low" = "blue")) +
  guides(color = guide_legend(title = "Reliability Groups"))
)

# Plot for Trust
(trust_plot <- ggplot(trust_summary, aes(x = Exposure, y = mean, 
                                         color = Reliability, group = Reliability)) +
  geom_line(size = 0.8) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Trust by Exposure and Reliability (95% CI)",
       x = "Exposure",
       y = "Trust Rating",
       color = "Reliability") +
  theme_custom +
  #scale_fill_manual(values = c("High" = "red", "Low" = "blue")) +
  scale_color_manual(values = c("High" = "red", "Low" = "blue")) +
  guides(color = guide_legend(title = "Reliability Groups"))
)

print(trust_plot)
print(verification_plot)

