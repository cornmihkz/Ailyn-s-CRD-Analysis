# ==============================
# 1. INSTALL & LOAD PACKAGES
# ==============================
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
  rep("Positive_2days",3),
  rep("Positive_1week",2),
  "Positive_1week3days",
  rep("Uninoculated",3)
)

incidence <- c(
  20,40,0,
  10,20,0,
  30,30,10,
  40,10,20,
  40,30,10,
  60,40,60,
  40,20,50,
  20,30,
  20,
  0,0,0
)

data <- data.frame(treatment, incidence)

# Convert to factor with order
data$treatment <- factor(data$treatment,
                         levels = c("Uninoculated","AgNPs","SA",
                                    "Positive_1week3days","Positive_1week",
                                    "Harzianum","Virens","Viride",
                                    "Positive_2days","Negative"))

# ==============================
# 3. CRD ANOVA
# ==============================
model <- aov(incidence ~ treatment, data = data)
summary(model)

# ==============================
# 4. POST-HOC TESTS
# ==============================
# Tukey
tukey <- TukeyHSD(model)
print(tukey)

# LSD (for grouping letters)
lsd <- LSD.test(model, "treatment", p.adj="none")
print(lsd$groups)

letters <- data.frame(
  treatment = rownames(lsd$groups),
  letter = lsd$groups$groups
)

# ==============================
# 5. SUMMARY STATISTICS
# ==============================
summary_data <- data %>%
  group_by(treatment) %>%
  summarise(
    mean = mean(incidence),
    sd = sd(incidence),
    n = n(),
    se = sd/sqrt(n),
    .groups="drop"
  )

summary_data <- merge(summary_data, letters, by="treatment")

print(summary_data)

# ==============================
# 6. COLORED BAR GRAPH (MEAN + SE + LETTERS)
# ==============================
p1 <- ggplot(summary_data, aes(x=treatment, y=mean, fill=treatment)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2) +
  geom_text(aes(label=letter, y=mean+se+3), size=5) +
  scale_fill_brewer(palette="Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  labs(title="Disease Incidence by Treatment",
       x="Treatment",
       y="Mean % Disease Incidence")

print(p1)

# ==============================
# 7. COLORED BOXPLOT
# ==============================
p2 <- ggplot(data, aes(x=treatment, y=incidence, fill=treatment)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none") +
  labs(title="Distribution of Disease Incidence",
       x="Treatment",
       y="% Disease Incidence")

print(p2)

# ==============================
# 8. AUTO SAVE (300 DPI)
# ==============================
ggsave("CRD_barplot.png", plot=p1, dpi=300, width=9, height=6)
ggsave("CRD_barplot.tiff", plot=p1, dpi=300, width=9, height=6)

ggsave("CRD_boxplot.png", plot=p2, dpi=300, width=9, height=6)
ggsave("CRD_boxplot.tiff", plot=p2, dpi=300, width=9, height=6)

# ==============================
# 9. SAVE RESULTS TABLE
# ==============================
write.csv(summary_data, "CRD_summary_results.csv", row.names=FALSE)