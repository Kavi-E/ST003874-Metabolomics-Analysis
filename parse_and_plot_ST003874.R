library(ggplot2)

# 1. Read the file from line 79 to get the SUBJECT_SAMPLE_FACTORS data
file_lines <- readLines("metabolomics_workbench.txt", warn = FALSE)

df_raw <- read.delim(
  text = file_lines[79:length(file_lines)],
  sep = "\t",
  header = TRUE,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# 2. Create a data frame with abundance values and sample metadata
plot_data <- data.frame(
  Abundance = suppressWarnings(as.numeric(df_raw[, 2])),
  MetaData  = df_raw[, 4],
  stringsAsFactors = FALSE
)

# 3. Remove missing values and blank samples
plot_data <- plot_data[!is.na(plot_data$Abundance), ]
plot_data <- plot_data[!is.na(plot_data$MetaData), ]
plot_data <- plot_data[!grepl("BLANK", plot_data$MetaData, ignore.case = TRUE), ]

# 4. Extract the treatment group from the sample metadata
plot_data$Treatment <- sub(".*Treatment_Group:([^|]+).*", "\\1", plot_data$MetaData)
plot_data$Treatment <- trimws(plot_data$Treatment)

# 5. Keep only the three treatment groups used in the study
plot_data <- plot_data[plot_data$Treatment %in% c("Placebo", "Low Dose 2FL", "High Dose 2FL"), ]

# 6. Set the treatment groups in the desired order
plot_data$Treatment <- factor(
  plot_data$Treatment,
  levels = c("Placebo", "Low Dose 2FL", "High Dose 2FL")
)

# 7. Remove zero, negative, and unusually high abundance values
plot_data <- plot_data[plot_data$Abundance > 0 & plot_data$Abundance < 1e6, ]

# 8. Create a boxplot with different colors for each treatment group
p <- ggplot(plot_data, aes(x = Treatment, y = Abundance, fill = Treatment)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(
    width = 0.15,
    alpha = 0.7,
    size = 2,
    aes(color = Treatment),
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c(
    "Placebo" = "#808080",
    "Low Dose 2FL" = "#4E79A7",
    "High Dose 2FL" = "#E15759"
  )) +
  scale_color_manual(values = c(
    "Placebo" = "#404040",
    "Low Dose 2FL" = "#2B4C7E",
    "High Dose 2FL" = "#A22B2D"
  )) +
  theme_bw(base_size = 14) +
  labs(
    title = "Metabolite Abundance by Intervention Group",
    subtitle = "Study ST003874: 2′-FL Prebiotic Intervention in Older Adults",
    x = "Treatment Arm",
    y = "Peak Intensity / Abundance"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Display the plot
print(p)

# Save the plot as a high-resolution PNG image
ggsave(
  "ST003874_Metabolite_Boxplot_Final.png",
  plot = p,
  width = 7,
  height = 5,
  dpi = 300
)
