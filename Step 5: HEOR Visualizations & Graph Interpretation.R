ggplot(claims_clean, aes(x = total_cost, fill = treatment)) +
  geom_density(alpha = 0.4) +
  scale_x_continuous(labels = scales::dollar_format()) +
  theme_minimal() +
  labs(
    title = "Figure 1: Distribution of Total Annual Health Claims Cost",
    subtitle = "Demonstrating right-skewness common in healthcare economic data",
    x = "Total Annual Claims Cost ($)",
    y = "Density",
    fill = "Treatment Group"
  )