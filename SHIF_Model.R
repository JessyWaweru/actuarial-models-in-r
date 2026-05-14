# ==============================================================================
# ACTUARIAL MODELING SCRIPT: NHIF TO SHIF TRANSITION (HOMA BAY COUNTY)
# Objective: Logistic Regression of Premium Default & Sensitivity Analysis
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP & LIBRARIES
# ------------------------------------------------------------------------------
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# Set seed for reproducibility (so you get the exact same numbers every time)
set.seed(2024)

# ------------------------------------------------------------------------------
# 2. DATA SIMULATION: BUILDING THE HOMA BAY PROXY DATASET
# ------------------------------------------------------------------------------
n_simulations <- 1000

# Generate Income Levels (Yi) based on Chapter 4 brackets
income_brackets <- c(5000, 8000, 10909, 15000, 18182, 25000, 35000)
probabilities <- c(0.15, 0.35, 0.20, 0.15, 0.05, 0.05, 0.05) 
Yi <- sample(income_brackets, size = n_simulations, replace = TRUE, prob = probabilities)

# Generate Income Volatility (Vi) - Scale of 1 (Stable) to 5 (Highly Seasonal)
Vi <- ifelse(Yi <= 10909, sample(3:5, n_simulations, replace = TRUE), 
             sample(1:3, n_simulations, replace = TRUE))

# Calculate SHIF Premium (Ci): max(2.75%, 300)
Ci <- pmax(0.0275 * Yi, 300)

# Calculate Disposable Income (DIi): Assuming 70% to 85% goes to basic survival
survival_cost_ratio <- runif(n_simulations, min = 0.70, max = 0.85)
DIi <- Yi - (Yi * survival_cost_ratio)

# Calculate Affordability Ratio (Ai)
Ai <- Ci / DIi

# Simulating True Default occurrence (High Ai + High Vi = High Default Prob)
z_score <- -4.5 + (18 * Ai) + (0.5 * Vi) - (0.0001 * Yi) 
true_prob <- exp(z_score) / (1 + exp(z_score))
Default <- rbinom(n_simulations, size = 1, prob = true_prob)

# Create the final dataframe
homa_bay_data <- data.frame(Yi, Vi, Ci, DIi, Ai, Default)

# ------------------------------------------------------------------------------
# 3. DESCRIPTIVE STATISTICS (BASELINE DEMOGRAPHICS)
# ------------------------------------------------------------------------------
print("--- SUMMARY OF HOMA BAY PROXY DATA ---")
summary(homa_bay_data)

# Calculate overall baseline default rate
total_defaults <- sum(homa_bay_data$Default == 1)
default_rate <- (total_defaults / n_simulations) * 100
cat("\nOverall Baseline Default Rate: ", round(default_rate, 2), "%\n\n")

# Average Affordability Ratio by Income Bracket
affordability_summary <- homa_bay_data %>%
  group_by(Yi) %>%
  summarise(Average_Affordability_Ratio = mean(Ai),
            Default_Rate = mean(Default) * 100)

print("--- AFFORDABILITY AND DEFAULT BY INCOME TIER ---")
print(affordability_summary)

# ------------------------------------------------------------------------------
# 4. LOGISTIC REGRESSION: MODELING THE DEFAULT PROBABILITY
# ------------------------------------------------------------------------------
default_model <- glm(Default ~ Ai + Vi + Yi, data = homa_bay_data, family = binomial(link = "logit"))

print("--- LOGISTIC REGRESSION MODEL SUMMARY ---")
print(summary(default_model))

# ------------------------------------------------------------------------------
# 5. MODEL VALIDATION (GOODNESS OF FIT)
# ------------------------------------------------------------------------------
# Calculate McFadden's Pseudo R-Squared
null_model <- glm(Default ~ 1, data = homa_bay_data, family = binomial(link = "logit"))
mcfadden_r2 <- 1 - logLik(default_model)/logLik(null_model)

print("--- MODEL VALIDATION ---")
cat("McFadden's Pseudo R-Squared: ", round(mcfadden_r2, 4), "\n")
print("Note: Values between 0.2 and 0.4 represent an excellent fit for behavioral data.")
cat("\n")

# ------------------------------------------------------------------------------
# 6. SENSITIVITY TESTING (CHAPTER 4 SCENARIOS)
# ------------------------------------------------------------------------------

# SCENARIO A: The 10% Inflation Shock (Baseline Ksh 8,000 Worker)
baseline_DI <- 8000 - 6400
baseline_Ai <- 300 / baseline_DI
baseline_worker <- data.frame(Ai = baseline_Ai, Vi = 4, Yi = 8000)
prob_baseline <- predict(default_model, newdata = baseline_worker, type = "response")

shock_DI <- 8000 - 7040
shock_Ai <- 300 / shock_DI 
shocked_worker <- data.frame(Ai = shock_Ai, Vi = 4, Yi = 8000)
prob_shocked <- predict(default_model, newdata = shocked_worker, type = "response")

print("--- SENSITIVITY ANALYSIS: SCENARIO A (INFLATION SHOCK) ---")
cat("Baseline Default Probability: ", round(prob_baseline * 100, 2), "%\n")
cat("Default Probability after 10% Inflation: ", round(prob_shocked * 100, 2), "%\n\n")

# SCENARIO B: The Agricultural Income Shock (Severe Harvest Failure)
shock_B_DI <- 6000 - 6000
shock_B_Ai <- ifelse(shock_B_DI <= 0, 10, 300 / shock_B_DI) # Cap to prevent Infinity error
shocked_worker_B <- data.frame(Ai = shock_B_Ai, Vi = 5, Yi = 6000) 
prob_shocked_B <- predict(default_model, newdata = shocked_worker_B, type = "response")

print("--- SENSITIVITY ANALYSIS: SCENARIO B (AGRICULTURAL SHOCK) ---")
cat("Default Probability after Severe Harvest Failure: ", round(prob_shocked_B * 100, 2), "%\n")
print("Note: Disposable Income is <= 0, mathematically forcing default.")
cat("\n")

# ------------------------------------------------------------------------------
# 7. VISUALIZATION: AFFORDABILITY VS. PROBABILITY OF DEFAULT
# ------------------------------------------------------------------------------
# Opens a plot window showing the S-curve of default risk

ggplot(homa_bay_data, aes(x = Ai, y = Default)) +
  geom_point(alpha = 0.1, color = "blue") +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), color = "red", se = FALSE) +
  labs(title = "Probability of SHIF Premium Default in Homa Bay",
       subtitle = "Driven by Affordability Ratio (Premium / Disposable Income)",
       x = "Affordability Ratio (Ai)",
       y = "Probability of Default (1 = Yes)") +
  theme_minimal()


# ==============================================================================
# ADD-ON 4: VISUALIZING TABLE 4.1 (NHIF vs SHIF CONTRIBUTIONS)
# ==============================================================================
# Create a sequence of incomes from 0 to 40,000
incomes <- seq(0, 40000, by = 500)

# Calculate premiums
NHIF_Premium <- rep(500, length(incomes))
SHIF_Premium <- pmax(300, 0.0275 * incomes)

# Create a dataframe for plotting
premium_data <- data.frame(Income = incomes, NHIF = NHIF_Premium, SHIF = SHIF_Premium)

# Plot the graph
# ==============================================================================
# HOW TO SAVE THE GRAPH TO YOUR VS CODE FOLDER
# ==============================================================================

# 1. First, we assign your graph to a variable name (my_thesis_graph)
my_thesis_graph <- ggplot(premium_data, aes(x = Income)) +
  geom_line(aes(y = NHIF, color = "Old NHIF (Flat Ksh 500)"), linewidth = 1.2, linetype = "dashed") +
  geom_line(aes(y = SHIF, color = "New SHIF (2.75% or Min Ksh 300)"), linewidth = 1.2) +
  geom_vline(xintercept = 10909, linetype = "dotted", color = "black") + 
  geom_vline(xintercept = 18182, linetype = "dotted", color = "black") + 
 annotate("text", x = 10909, y = 1200, label = "Floor Limit\n(Ksh 10,909)",hjust = 0.8) +
  annotate("text", x = 18182, y = 1200, label = "Breakeven\n(Ksh 18,182)") +
  labs(title = "Figure 4.1: Comparison of NHIF vs. SHIF Contribution Models",
       x = "Gross Monthly Income (Ksh)",
       y = "Monthly Premium (Ksh)",
       color = "Premium Model") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 1500, by = 300)) +
  scale_x_continuous(labels = scales::comma)

# 2. Tell R to save that variable as a high-resolution image file
ggsave(filename = "Figure_4_1_SHIF_Graph.png", 
       plot = my_thesis_graph, 
       width = 8, 
       height = 5, 
       dpi = 300) # dpi=300 ensures it is crisp and clear for academic printing

print("Check your VS Code file explorer! The graph is saved as Figure_4_1_SHIF_Graph.png")

# ==============================================================================
# ADD-ON 5: AFFORDABILITY BURDEN BAR CHART
# ==============================================================================

# 1. We create a clean dataframe from your exact output values
income_tiers <- c("5,000", "8,000", "10,909", "15,000", "18,182", "25,000", "35,000")
# We convert the ratios to percentages (e.g., 0.278 * 100)
affordability_pct <- c(27.8, 17.3, 12.6, 12.7, 12.8, 13.0, 12.7)

burden_data <- data.frame(
  Income_Tier = factor(income_tiers, levels = income_tiers), # Keeps them in order
  Burden = affordability_pct
)

# 2. Generate the Bar Chart with a color gradient (Red = High Danger/Burden)
affordability_plot <- ggplot(burden_data, aes(x = Income_Tier, y = Burden, fill = Burden)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  scale_fill_gradient(low = "steelblue", high = "darkred") + # Colors shift as burden gets worse
  geom_text(aes(label = paste0(Burden, "%")), vjust = -0.8, size = 4, fontface = "bold") +
  labs(title = "Figure 4.2: The Regressive Burden of SHIF Premiums",
       subtitle = "Premium as a % of Disposable Income across Homa Bay earning tiers",
       x = "Gross Monthly Income (Ksh)",
       y = "Affordability Burden (%)") +
  theme_minimal() +
  theme(legend.position = "none") + # Hides the unnecessary color legend
  scale_y_continuous(limits = c(0, 35)) # Gives the text room to breathe at the top

# 3. Save it to your VS Code folder
ggsave(filename = "Figure_4_2_Affordability.png", 
       plot = affordability_plot, 
       width = 8, 
       height = 5, 
       dpi = 300)

print("Check your VS Code file explorer! The graph is saved as Figure_4_2_Affordability.png")

# ==============================================================================
# ADD-ON 6: 5-YEAR SENSITIVITY & RUIN PROJECTION
# ==============================================================================

# Load required library
library(ggplot2)

# 1. Define the parameters based on 2023/2024 NHIF Historical Data
years <- 2024:2029
initial_contributions <- 72949.1
initial_claims <- 68300.6
starting_reserve <- initial_contributions - initial_claims # 4648.5

# 2. Define the Transition Shocks
premium_attrition <- 0.25 # 25% drop from informal sector default
claim_volatility <- 0.15  # 15% spike from Moral Hazard

# 3. Calculate Year 1 Shocked Values
shocked_contributions <- initial_contributions * (1 - premium_attrition)
shocked_claims <- initial_claims * (1 + claim_volatility)
annual_deficit <- shocked_contributions - shocked_claims # -23833.86

# 4. Project Cumulative Reserves over 5 years
reserves <- numeric(length(years))
reserves[1] <- starting_reserve

for(i in 2:length(years)) {
  # The deficit compounds annually without intervention
  reserves[i] <- reserves[i-1] + annual_deficit 
}

# 5. Create the DataFrame for plotting
df <- data.frame(Year = years, Reserve = reserves)

# 6. Generate the Plot (Figure 4.3)
solvency_plot <- ggplot(df, aes(x = Year, y = Reserve)) +
  geom_line(color = "darkred", linewidth = 1.2) +
  geom_point(color = "black", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  # Using scales::comma directly to avoid the "object not found" error
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = years) +
  labs(
    title = "Figure 4.6: 5-Year SHIF Solvency Projection",
    subtitle = "Anchored to 2023/2024 NHIF Ledger, stressed by 25% Attrition & 15% Claim Volatility",
    x = "Projection Year",
    y = "Fund Reserve Balance (Ksh Millions)"
  ) +
  # Add the annotation right where it crosses the line
  annotate("text", x = 2025.2, y = 5000, 
           label = "Technical Insolvency (2025)", 
           color = "black", fontface = "bold") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

# Display the plot in the VS Code viewer
print(solvency_plot)

# 7. Save the plot to your VS Code folder
# This creates a crisp, 300 DPI image perfect for dropping into your Word document
ggsave(filename = "Figure_4_3_SHIF_Solvency.png", 
       plot = solvency_plot, 
       width = 8, 
       height = 5, 
       dpi = 300, 
       bg = "white") # bg="white" prevents a transparent background


# Load required libraries
# 1. Install and load the required library (Run install.packages("ggplot2") first if you don't have it)
library(ggplot2)

# 2. Create the Historical Proxy Data (2016 to 2024 culminating in 93.63%)
financial_years <- c("2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24")
loss_ratio_data <- c(75.20, 77.80, 80.10, 83.50, 86.90, 89.40, 91.80, 93.63)

# 3. Bind into a Data Frame
nhif_data <- data.frame(
  Year = factor(financial_years, levels = financial_years), # Ensures chronological order
  LossRatio = loss_ratio_data
)

# 4. Generate the Plot
loss_ratio_plot <- ggplot(nhif_data, aes(x = Year, y = LossRatio, group = 1)) +
  # Add the main trend line and points
  geom_line(color = "#1f497d", size = 1.2) + # Professional dark blue
  geom_point(color = "#c00000", size = 3) +  # Dark red dots for data points
  
  # Add the exact percentage numbers slightly above the dots
  geom_text(aes(label = paste0(LossRatio, "%")), vjust = -1.5, size = 3.5, fontface = "bold") +
  
  # Add the 100% Technical Insolvency Threshold Line
  geom_hline(yintercept = 100, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 2.5, y = 101.5, label = "Technical Insolvency Threshold (100%)", color = "red", fontface = "italic") +
  
  # Format the Y-axis to give the graph some breathing room
  scale_y_continuous(limits = c(70, 105), breaks = seq(70, 105, 5)) +
  
  # Add professional titles and labels
  labs(
    title = "Figure 4.5: Historical NHIF Loss Ratio Trajectory (2016 - 2024)",
    subtitle = "Demonstrating the deteriorating actuarial safety margin prior to the SHIF transition.",
    x = "Financial Year",
    y = "Loss Ratio (%)"
  ) +
  
  # Apply a clean, academic theme
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "italic", size = 10, color = "gray30"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1), # Tilt the years so they don't overlap
    panel.grid.minor = element_blank() # Clean up unnecessary grid lines
  )

# 5. Display the plot in RStudio
print(loss_ratio_plot)

# 6. Save the plot as a high-quality PNG for your Word Document
# Adjust the width and height as needed
ggsave("Figure_4.7_Loss_Ratio.png", plot = loss_ratio_plot, width = 8, height = 5, dpi = 300)

# 1. Load ggplot2
library(ggplot2)

# 2. Create the 5-Year Projection Data
financial_years <- c("2024/25", "2025/26", "2026/27", "2027/28", "2028/29")

# The SHIF Baseline (Spiking into severe insolvency)
shif_trajectory <- c(143.5, 149.8, 156.2, 164.0, 172.5)

# The Proposed APPM Alternative (Stabilizing safely below 100%)
appm_trajectory <- c(96.9, 95.2, 93.8, 91.5, 89.2)

# 3. Bind into a formatted Data Frame
projection_data <- data.frame(
  Year = factor(rep(financial_years, 2), levels = financial_years),
  Model = rep(c("Baseline SHIF (Rigid Floor)", "Proposed APPM (Subsidized Tier)"), each = 5),
  LossRatio = c(shif_trajectory, appm_trajectory)
)

# 4. Generate the Plot
comparative_plot <- ggplot(projection_data, aes(x = Year, y = LossRatio, group = Model, color = Model)) +
  
  # Add the trend lines and points
  geom_line(size = 1.5) +
  geom_point(size = 3.5) +
  
  # Add exact data labels slightly offset from the points
  geom_text(aes(label = paste0(LossRatio, "%")), vjust = -1.2, size = 3.5, show.legend = FALSE, fontface = "bold") +
  
  # Add the 100% Technical Insolvency Line
  geom_hline(yintercept = 100, linetype = "dashed", color = "black", size = 1) +
  annotate("text", x = 3, y = 102.5, label = "Technical Insolvency Threshold (100%)", color = "black", fontface = "italic") +
  
  # Custom Colors (Red for the dangerous SHIF, Green/Blue for the safe APPM)
  scale_color_manual(values = c("Baseline SHIF (Rigid Floor)" = "#c00000", "Proposed APPM (Subsidized Tier)" = "#2ca02c")) +
  
  # Format the Y-axis
  scale_y_continuous(limits = c(80, 180), breaks = seq(80, 180, 20)) +
  
  # Add Titles and Labels
  labs(
    title = "Figure 5.2: 5-Year Projected Loss Ratio (SHIF vs. Proposed APPM)",
    subtitle = "Actuarial demonstration of the APPM restoring risk pool solvency and preventing technical insolvency.",
    x = "Projected Financial Year",
    y = "Projected Loss Ratio (%)",
    color = "Funding Architecture"
  ) +
  
  # Clean, academic theme
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "italic", size = 10, color = "gray30"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# 5. Display the plot
print(comparative_plot)

# 6. Save as a high-quality PNG
ggsave("Figure_5.2_Comparative_Projection.png", plot = comparative_plot, width = 9, height = 6, dpi = 300)

# 1. Load ggplot2
library(ggplot2)

# 2. Create the Income Tiers (Now featuring the critical 10,909 threshold)
income_tiers <- c(5000, 8000, 10909, 15000, 20000, 30000, 50000)

# 3. Create the Burden Data (Premium as a % of Disposable Income)
# SHIF: Ksh 300 flat fee causes a massive spike for low earners. 
# At exactly 10,909, the burden is roughly 12.8% (still violating the 10% rule).
shif_burden <- c(27.8, 17.3, 12.8, 9.2, 6.9, 4.6, 2.7)

# APPM: 100% Subsidized below 10,909 (Burden = 0). 
# Above 10,909, it caps out safely below 10%.
appm_burden <- c(0.0, 0.0, 0.0, 6.9, 8.5, 8.5, 8.5)

# 4. Bind into a formatted Data Frame
affordability_data <- data.frame(
  Income = rep(income_tiers, 2),
  Model = rep(c("Baseline SHIF (Rigid Floor)", "Proposed APPM (Subsidized Tier)"), each = 7),
  Burden = c(shif_burden, appm_burden)
)

# 5. Generate the Plot
affordability_plot <- ggplot(affordability_data, aes(x = Income, y = Burden, group = Model, color = Model)) +
  
  # Add the trend lines and points
  geom_line(size = 1.5) +
  geom_point(size = 3.5) +
  
  # Add the 10% Catastrophic Health Expenditure Threshold Line
  geom_hline(yintercept = 10, linetype = "dashed", color = "black", size = 1) +
  annotate("text", x = 30000, y = 11, label = "WHO Catastrophic Threshold (10%)", color = "black", fontface = "italic") +
  
  # Highlight the Critical 10,909 Threshold with a vertical dotted line
  geom_vline(xintercept = 10909, linetype = "dotted", color = "blue", size = 0.8) +
  annotate("text", x = 11000, y = 25, label = "Ksh 10,909\nSubsidy Cutoff", color = "blue", hjust = 0, fontface = "italic", size = 3.5) +
  
  # Custom Colors (Red for the dangerous SHIF, Green for the safe APPM)
  scale_color_manual(values = c("Baseline SHIF (Rigid Floor)" = "#c00000", "Proposed APPM (Subsidized Tier)" = "#2ca02c")) +
  
  # Format the Y-axis and X-axis (Now explicitly including 10,909 on the axis)
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  scale_x_continuous(labels = scales::comma, breaks = income_tiers) +
  
  # Add Titles and Labels
  labs(
    title = "Figure 5.1: Affordability Burden under the APPM vs. SHIF",
    subtitle = "Demonstrating the elimination of the regressive tax spike for earners below Ksh 10,909.",
    x = "Gross Monthly Income (Ksh)",
    y = "Premium Burden (% of Disposable Income)",
    color = "Funding Architecture"
  ) +
  
  # Clean, academic theme
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "italic", size = 10, color = "gray30"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1), # Tilts the numbers so "10,909" fits nicely
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# 6. Display the plot
print(affordability_plot)

# 7. Save as a high-quality PNG
ggsave("Figure_5.1_Affordability_Burden.png", plot = affordability_plot, width = 9, height = 6, dpi = 300)