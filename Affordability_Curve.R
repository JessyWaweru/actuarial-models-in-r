# Load required libraries
library(ggplot2)
library(scales)

# 1. Simulate Income Brackets (Ksh 4,000 to Ksh 40,000)
incomes <- seq(4000, 40000, by = 1000)

# 2. Apply the SHIF Math and Affordability Rules
# Assuming a conservative 80% of income goes to basic survival (Food, Rent)
survival_rate <- 0.80
disposable_incomes <- incomes * (1 - survival_rate)

# SHIF Contribution (Max of 2.75% or 300)
shif_contributions <- pmax(incomes * 0.0275, 300)

# Affordability Burden (% of Disposable Income)
affordability_burden <- (shif_contributions / disposable_incomes) * 100

# 3. Create DataFrame
df_affordability <- data.frame(Income = incomes, Burden = affordability_burden)

# 4. Generate the Plot (Figure 4.4)
affordability_plot <- ggplot(df_affordability, aes(x = Income, y = Burden)) +
  geom_line(color = "darkred", linewidth = 1.2) +
  
  # Add the Catastrophic Expenditure Threshold (10%)
  geom_hline(yintercept = 10, linetype = "dashed", color = "black", linewidth = 1) +
  annotate("text", x = 30000, y = 11.5, label = "Catastrophic Threshold (10%)", fontface = "bold") +
  
  # Highlight the Ksh 10,909 Statutory Floor Tipping Point
  geom_vline(xintercept = 10909, linetype = "dotted", color = "steelblue", linewidth = 1) +
  annotate("text", x = 11800, y = 30, label = "Ksh 10,909 Statutory Floor", color = "steelblue", angle = 90, fontface="bold") +
  
  # Formatting the axes
  scale_x_continuous(labels = scales::comma, breaks = seq(5000, 40000, by = 5000)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 40, by = 5)) +
  
  # Titles and Labels
  labs(
    title = "Figure 4.3: The Regressive Tax Effect of the Ksh 300 Minimum",
    subtitle = "Affordability burden spikes exponentially for earners below the Ksh 10,909 floor",
    x = "Gross Monthly Income (Ksh)",
    y = "SHIF Premium as % of Disposable Income"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

# Display in VS Code
print(affordability_plot)

# Save the high-resolution image to your folder
ggsave("Affordability_Curve.png", plot = affordability_plot, width = 8, height = 5, dpi = 300, bg="white")

print("Analysis complete. Check your folder for Affordability_Curve.png")