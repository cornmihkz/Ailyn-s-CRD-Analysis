# ==============================
# 1. PACKAGES
# ==============================

library(ggplot2)
library(dplyr)
library(agricolae)

# ==============================
# 2. DATA INPUT
# ==============================
data <- data.frame(
  isolate = c(
    rep("VSU",9),
    rep("CAB01",9),
    rep("CAB02",9),
    rep("CAB03",9)
  ),
  variety = rep(c("Avatar","Dmax","Jewel"), each=3, times=4),
  incidence = c(
    # VSU
    100,100,100, 100,100,100, 100,100,100,
    # CAB01
    20,20,10, 20,10,30, 70,60,20,
    # CAB02
    20,30,10, 20,0,20, 10,20,10,
    # CAB03
    60,10,20, 20,20,30, 80,40,50
  )
)

# ==============================
# 3. FUNCTION FOR CRD ANALYSIS
# ==============================
run_crd <- function(df, isolate_name){
  
  cat("\n========================\n")
  cat("ISOLATE:", isolate_name, "\n")
  cat("========================\n")
  
  # ANOVA
  model <- aov(incidence ~ variety, data = df)
  print(summary(model))
  
  # LSD test
  lsd <- LSD.test(model, "variety", p.adj="none")
  print(lsd$groups)
  
  letters <- data.frame(
    variety = rownames(lsd$groups),
    letter = lsd$groups$groups
  )
  
  # Summary stats
  summary_df <- df %>%
    group_by(variety) %>%
    summarise(
      mean = mean(incidence),
      sd = sd(incidence),
      n = n(),
      se = sd/sqrt(n),
      .groups="drop"
    )
  
  summary_df <- merge(summary_df, letters, by="variety")
  
  print(summary_df)
  
  # ==============================
  # BAR GRAPH (COLORED + LETTERS)
  # ==============================
  p1 <- ggplot(summary_df, aes(x=variety, y=mean, fill=variety)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2) +
    geom_text(aes(label=letter, y=mean+se+3), size=5) +
    scale_fill_brewer(palette="Set2") +
    theme_minimal() +
    theme(legend.position="none") +
    labs(title=paste("Disease Incidence -", isolate_name),
         x="Tomato Variety",
         y="Mean % Disease Incidence")
  
  print(p1)
  
  # ==============================
  # BOXPLOT
  # ==============================
  p2 <- ggplot(df, aes(x=variety, y=incidence, fill=variety)) +
    geom_boxplot() +
    scale_fill_brewer(palette="Set3") +
    theme_minimal() +
    theme(legend.position="none") +
    labs(title=paste("Distribution -", isolate_name),
         x="Tomato Variety",
         y="% Disease Incidence")
  
  print(p2)
  
  # ==============================
  # SAVE FILES (300 DPI)
  # ==============================
  ggsave(paste0(isolate_name, "_barplot.png"), p1, dpi=300, width=7, height=5)
  ggsave(paste0(isolate_name, "_barplot.tiff"), p1, dpi=300, width=7, height=5)
  
  ggsave(paste0(isolate_name, "_boxplot.png"), p2, dpi=300, width=7, height=5)
  ggsave(paste0(isolate_name, "_boxplot.tiff"), p2, dpi=300, width=7, height=5)
  
  # Save summary
  write.csv(summary_df,
            paste0(isolate_name, "_summary.csv"),
            row.names=FALSE)
}

# ==============================
# 4. RUN ANALYSIS PER ISOLATE
# ==============================
isolates <- unique(data$isolate)

for(i in isolates){
  df_subset <- subset(data, isolate == i)
  run_crd(df_subset, i)
}