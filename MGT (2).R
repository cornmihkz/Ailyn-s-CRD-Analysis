# ==============================
# 1. DATA INPUT
# ==============================
treatment <- c(
  rep("SA",3),
  rep("AgNPs",3),
  rep("Harzianum",3),
  rep("Virens",3),
  rep("Viride",3),
  rep("Negative",3),
  rep("Positive_2days",3),
  rep("Positive_1week",3),
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
  20,30,20,
  0,0,0
)

data <- data.frame(treatment, incidence)

# ==============================
# 2. ANOVA (CRD)
# ==============================
model <- aov(incidence ~ treatment, data = data)
summary(model)

# ==============================
# 3. ASSUMPTION CHECKS
# ==============================
shapiro.test(residuals(model))     # Normality
bartlett.test(incidence ~ treatment, data=data)  # Homogeneity

# ==============================
# 4. POST-HOC TEST (Tukey)
# ==============================
TukeyHSD(model)

# ==============================
# 5. LSD TEST (Agricolae)
# ==============================
install.packages("agricolae")
library(agricolae)

LSD.test(model, "treatment", p.adj="none")

# ==============================
# 6. MEANS
# ==============================
aggregate(incidence ~ treatment, data, mean)


# ==============================
# 7. BOXPLOT
# ==============================
boxplot(incidence ~ treatment, data=data,
        xlab="Treatment",
        ylab="% Disease Incidence",
        las=2)

# ==============================
# 8. BAR PLOT (MEANS)
# ==============================
means <- aggregate(incidence ~ treatment, data, mean)

barplot(means$incidence,
        names.arg=means$treatment,
        las=2,
        ylab="% Disease Incidence",
        main="Treatment Means")

# ==============================
# 9. OPTIONAL: ggplot (cleaner)
# ==============================

library(ggplot2)
library(dplyr)

# ==============================
# DATA
# ==============================
treatment <- c(
  rep("SA",3),
  rep("AgNPs",3),
  rep("Harzianum",3),
  rep("Virens",3),
  rep("Viride",3),
  rep("Negative",3),
  rep("Positive_2days",3),
  rep("Positive_1week",3),
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
  20,30,20,
  0,0,0
)

data <- data.frame(treatment, incidence)

# Order treatments (optional for nicer display)
data$treatment <- factor(data$treatment,
                         levels = c("Uninoculated","AgNPs","SA","Positive_1week",
                                    "Harzianum","Virens","Viride","Positive_2days","Negative"))

# ==============================
# 1. COLORED BOXPLOT
# ==============================
ggplot(data, aes(x=treatment, y=incidence, fill=treatment)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "none") +
  labs(title="Disease Incidence by Treatment",
       x="Treatment",
       y="% Disease Incidence")

# ==============================
# 2. COLORED BAR GRAPH (MEANS)
# ==============================
means <- data %>%
  group_by(treatment) %>%
  summarise(mean_incidence = mean(incidence))

ggplot(means, aes(x=treatment, y=mean_incidence, fill=treatment)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "none") +
  labs(title="Mean Disease Incidence",
       x="Treatment",
       y="Mean % Disease Incidence")

# ==============================
# 3. BAR GRAPH WITH ERROR BARS (SD)
# ==============================
summary_data <- data %>%
  group_by(treatment) %>%
  summarise(
    mean = mean(incidence),
    sd = sd(incidence)
  )

ggplot(summary_data, aes(x=treatment, y=mean, fill=treatment)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2) +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "none") +
  labs(title="Mean Disease Incidence with SD",
       x="Treatment",
       y="Mean % Disease Incidence")

# ==============================
# 4. OPTIONAL: BETTER COLORS (MANUAL)
# ==============================
custom_colors <- c(
  "Uninoculated" = "#1b9e77",
  "AgNPs" = "#66a61e",
  "SA" = "#7570b3",
  "Positive_1week" = "#e6ab02",
  "Harzianum" = "#a6761d",
  "Virens" = "#1f78b4",
  "Viride" = "#b2df8a",
  "Positive_2days" = "#fb9a99",
  "Negative" = "#e31a1c"
)

ggplot(means, aes(x=treatment, y=mean_incidence, fill=treatment)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "none") +
  labs(title="Mean Disease Incidence (Custom Colors)",
       x="Treatment",
       y="Mean % Disease Incidence")