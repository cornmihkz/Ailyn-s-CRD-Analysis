library(ggplot2)
library(dplyr)
library(agricolae)

# ==============================
# 2. DATA INPUT
# ==============================
treatment <- c(
  rep("SA",3),
  rep("AgNPs",3),
  rep("Harzianum",3),
  rep("Virens",3),
  rep("Viride",3),
  rep("Negative",3),
  rep("Positive",3)
)

inhibition <- c(
  0,0,0,
  8,7,7,
  0,0,0,
  0,0,0,
  0,0,0,
  0,0,0,
  13,14,14
)

data <- data.frame(treatment, inhibition)

# Order treatments for better plotting
data$treatment <- factor(data$treatment,
                         levels = c("SA","Harzianum","Virens","Viride","Negative","AgNPs","Positive"))

# ==============================
# 3. ANOVA (CRD)
# ==============================
model <- aov(inhibition ~ treatment, data = data)
summary(model)

# ==============================
# 4. POST-HOC TESTS
# ==============================
# Tukey
TukeyHSD(model)

# LSD
LSD.test(model, "treatment", p.adj="none")

# ==============================
# 5. SUMMARY STATISTICS
# ==============================
summary_data <- data %>%
  group_by(treatment) %>%
  summarise(
    mean = mean(inhibition),
    sd = sd(inhibition)
  )

print(summary_data)

# ==============================
# 6. COLORED BOXPLOT
# ==============================
ggplot(data, aes(x=treatment, y=inhibition, fill=treatment)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "none") +
  labs(title="Inhibition Zone (24 hrs)",
       x="Treatment",
       y="Zone of Inhibition (mm)")

# ==============================
# 7. BAR GRAPH WITH ERROR BARS
# ==============================
ggplot(summary_data, aes(x=treatment, y=mean, fill=treatment)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "none") +
  labs(title="Mean Inhibition Zone (24 hrs)",
       x="Treatment",
       y="Mean Zone (mm)")

# ==============================
# 8. CUSTOM COLOR GRAPH (OPTIONAL)
# ==============================
custom_colors <- c(
  "SA" = "#999999",
  "Harzianum" = "#66c2a5",
  "Virens" = "#8da0cb",
  "Viride" = "#e78ac3",
  "Negative" = "#fc8d62",
  "AgNPs" = "#a6d854",
  "Positive" = "#e31a1c"
)

ggplot(summary_data, aes(x=treatment, y=mean, fill=treatment)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "none") +
  labs(title="Mean Inhibition Zone",
       x="Treatment",
       y="Mean Zone (mm)")
