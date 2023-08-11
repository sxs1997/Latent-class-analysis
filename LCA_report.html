setwd("/rds/projects/g/gkoutosg-variant-prediction/MScData/Shivani")
options(bitmapType = "cairo")


# Subsetting the data from the original data
clustering_data <- subset(DiseasesAndTrajectories, select = c("eid","Category", "Disease"))
clustering_data

# Cleaning the data of "Unknown"
clustering_data <- clustering_data[!grepl("Unknown", clustering_data$Category, ignore.case = TRUE), ]
# Cleaning the data of NA values
clustering_data <- clustering_data[!is.na(clustering_data$Disease), ]

clustering_unclassed <- clustering_data %>%
  mutate(Values = 1) %>%
  tidyr::pivot_wider( id_cols = c(eid, Category), names_from = Disease, values_from = Values) %>% #values.fill = 0 but not working 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate_all(as.factor) %>%
  janitor::clean_names()

clustering_unclassed

#===========================================
train <- clustering_unclassed
#===========================================

# Set the number of latent classes to evaluate
min_classes <- 2
max_classes <- 6

# Create data frames to store AIC and BIC values
AIC <- data.frame()
BIC <- data.frame()
lca <- list()

clustering_unclassed1 <- clustering_unclassed %>% 
  dplyr::select(-c(eid, category)) 
clustering_unclassed1

pdf(paste0(IncludeFigHere, "/Results.pdf"))

# Running LCA for different numbers of latent classes
for (i in min_classes:max_classes) {
  # Define the formula for LCA
  formula <- as.formula(paste("cbind(", paste(names(clustering_unclassed1), collapse = ", "), ") ~ 1"))
  
  
  # Run LCA with the specified number of classes
  lca[[i]] <- poLCA(formula, data = train, nclass = i, nrep = 5, verbose = TRUE, graphs = TRUE) #tol = 0.001
  
  # Store AIC and BIC values
  AIC <- rbind(AIC, data.frame(Classes = i, AIC = lca[[i]]$aic))
  BIC <- rbind(BIC, data.frame(Classes = i, BIC = lca[[i]]$bic))
}

dev.off()

# Combine AIC and BIC values
results <- merge(AIC, BIC, by = "Classes")

# Print the results
results

# Save the plot as a PDF in the specified directory

pdf(paste0(IncludeFigHere, "/AIC_BIC_plot.pdf"))

print(# Plot AIC and BIC values
  ggplot(results, aes(x = Classes)) +
    geom_line(aes(y = AIC, color = "AIC")) +
    geom_line(aes(y = BIC, color = "BIC")) +
    labs(x = "Number of Latent Classes", y = "Value", color = "Metric") +
    ggtitle("AIC and BIC for Latent Class Analysis"))

dev.off()


# Calculate the AIC values for each number of classes
AIC <- AIC[order(AIC$AIC), ]
AIC

optimal_classes <- AIC$Classes[1]  # Select the class with the lowest AIC value
optimal_classes

save(lca,optimal_classes, file = paste0(IncludeFigHere, "/Data.RData"))

#since the analysis is completed
load("/rds/projects/g/gkoutosg-variant-prediction/MScData/Shivani/Attempt2_20230731_2147/Data.RData")


pdf("/rds/projects/g/gkoutosg-variant-prediction/MScData/Shivani/Attempt2_20230731_2147/See.pdf", 14, 10)

for (i in 2:6) {
  
  plot(lca[[i]])
  
}

dev.off()


best_model <- lca[[optimal_classes]]
best_model

#class probabiities
class_proportions <- best_model$probs
class_proportions

all_probs_class_1 <- list()

for (class_name in names(class_proportions)) {
  probs_class_1 <- class_proportions[[class_name]][, "Pr(1)"]
  all_probs_class_1[[class_name]] <- probs_class_1
}

Probabilities_disease <- as.data.frame(all_probs_class_1)

Probabilities_disease <- t(Probabilities_disease)
Probabilities_disease <- as.data.frame(Probabilities_disease)
Probabilities_disease$Diseases <- rownames(Probabilities_disease)
rownames(Probabilities_disease) <- NULL

str(Probabilities_disease)

prob_ranges <- c(0, 0.25, 0.50, 0.75, 0.99, 1.00)  # Adjusted to include right endpoint '1.00'

Probabilities_disease$Diseases <- factor(Probabilities_disease$Diseases, levels = unique(Probabilities_disease$Diseases))

Probabilities_disease_long <- gather(Probabilities_disease, class, value, -Diseases)

# Create prob_category based on probability ranges using findInterval
Probabilities_disease_long$prob_category <- findInterval(Probabilities_disease_long$value, prob_ranges)

str(Probabilities_disease_long)

unique(Probabilities_disease_long$prob_category)

unique(Probabilities_disease_long$value)


library(openxlsx)

write.xlsx(Probabilities_disease, file = "/rds/projects/g/gkoutosg-variant-prediction/MScData/Shivani/excel_sheets/Disease_class_prob.xlsx")

# Load required packages
library(ggplot2)
library(RColorBrewer)

# Convert the 'value' column to numeric (continuous)
Probabilities_disease_long$value <- as.numeric(as.character(Probabilities_disease_long$value))

# Define custom labels and colors for the probability categories
prob_labels <- c("0.00-0.25", "0.25-0.50", "0.50-0.75", "0.75-0.90", "0.90-1.00")

# Define the RColorBrewer color palette
color_palette <- brewer.pal(5, "Set1")  # Replace "Set1" with the desired palette name and number of colors

# Create the point plot with reduced point sizes and RColorBrewer colors
plot1 <- ggplot(Probabilities_disease_long, aes(x = class, y = Diseases, group = class, color = factor(prob_category), size = prob_category)) +
  geom_point(alpha = 0.8) +
  theme_bw() +
  theme(
    axis.line = element_line(colour = "grey80", size = 1),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_line(),
    axis.text.y = element_text(size = 8, hjust = 1.0, family = "sans"),  # Change the font family and size for y-axis labels
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"),  # Change the font family and size for x-axis labels
    axis.title = element_text(family = "sans"),  # Change the font family for axis titles
    plot.title = element_text(family = "sans", face = "bold", hjust = 0.5, size = 16),  # Center-align the plot title and reduce size
    legend.text = element_text(family = "sans"),  # Change the font family for legend text
    legend.title = element_text(family = "sans", face = "bold")  # Change the font family and style for legend title
  ) +
  labs(y = "Diseases", x = "Class", title = "Class Probabilities by Class and Disease") +
  scale_color_manual(name = "Probability Category", labels = prob_labels, values = color_palette) +  # Set custom colors using RColorBrewer palette
  scale_size_continuous(range = c(1, 3), guide = "none") +  # Adjust the range for smaller point sizes here (e.g., c(0.5, 2)) and remove size guide legend
  guides(color = guide_legend(title = "Probability Category", override.aes = list(size = 2))) +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5", "6")) +  # Set custom x-axis labels
  coord_cartesian(clip = "off", expand = 0.2)  # Increase plot height by expanding the y-axis limits

# Save the plot as a PDF with a larger size
ggsave("LCAplot[1].pdf", plot, width = 12, height = 8)

plot1


#===========================================
train <- clustering_unclassed #(covariate)
#===========================================

# Set the number of latent classes to evaluate
min_classes <- 2
max_classes <- 6

# Create data frames to store AIC and BIC values
AIC <- data.frame()
BIC <- data.frame()
lca <- list()

clustering_unclassed3 <- clustering_unclassed %>% 
  dplyr::select(-c(eid)) 

pdf(paste0(IncludeFigHere, "/Results_cov.pdf"))

# Running LCA for different numbers of latent classes
for (i in min_classes:max_classes) {
  # Define the formula for LCA
  formula <- as.formula(paste("cbind(", paste(names(clustering_unclassed3), collapse = ", "), ") ~ 1"))
  
  
  # Run LCA with the specified number of classes
  lca[[i]] <- poLCA(formula, data = train, nclass = i, nrep = 5, verbose = TRUE, graphs = TRUE) #tol = 0.001
  
  # Store AIC and BIC values
  AIC <- rbind(AIC, data.frame(Classes = i, AIC = lca[[i]]$aic))
  BIC <- rbind(BIC, data.frame(Classes = i, BIC = lca[[i]]$bic))
}

dev.off()

# Combine AIC and BIC values
results <- merge(AIC, BIC, by = "Classes")

# Print the results
results

# Save the plot as a PDF in the specified directory

pdf(paste0(IncludeFigHere, "/AIC_BIC_plot_cov.pdf"))

print(# Plot AIC and BIC values
  ggplot(results, aes(x = Classes)) +
    geom_line(aes(y = AIC, color = "AIC")) +
    geom_line(aes(y = BIC, color = "BIC")) +
    labs(x = "Number of Latent Classes", y = "Value", color = "Metric") +
    ggtitle("AIC and BIC for Latent Class Analysis"))

dev.off()


# Calculate the AIC values for each number of classes
AIC <- AIC[order(AIC$AIC), ]
AIC

optimal_classes <- AIC$Classes[1]  # Select the class with the lowest AIC value
optimal_classes

save(lca,optimal_classes, file = paste0(IncludeFigHere, "/Data_cov.RData"))

load("/rds/projects/g/gkoutosg-variant-prediction/MScData/Shivani/Analysis_plots_20230801_2019/Data_cov.RData")

best_model <- lca[[optimal_classes]]
best_model

#class probabiities
class_probs <- best_model$probs
class_probs

#posterior probabilities
posterior_probs <- best_model$posterior
head(posterior_probs)

max_posterior_probs <- apply(posterior_probs, 1, max, na.rm = TRUE)
max_posterior_probs

ClusterUnderstanding <- clustering_unclassed %>% 
  add_column(PredClass = best_model$predclass, 
             PredClassProb = max_posterior_probs,
             check = best_model$predclass)

ClusterUnderstanding


# Load required packages
library(ggplot2)
library(RColorBrewer)

# Convert the 'value' column to numeric (continuous)
Probabilities_disease_long$value <- as.numeric(as.character(Probabilities_disease_long$value))

# Define custom labels and colors for the probability categories
prob_labels <- c("0.00-0.25", "0.25-0.50", "0.50-0.75", "0.75-0.90", "0.90-1.00")

# Define the RColorBrewer color palette
color_palette <- brewer.pal(5, "Set1")  # Replace "Set1" with the desired palette name and number of colors

# Create the point plot with reduced point sizes and RColorBrewer colors
plot <- ggplot(Probabilities_disease_long, aes(x = class, y = Diseases, group = class, color = factor(prob_category), size = prob_category)) +
  geom_point(alpha = 0.8) +
  theme_bw() +
  theme(
    axis.line = element_line(colour = "grey80", size = 1),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_line(),
    axis.text.y = element_text(size = 8, hjust = 1.0, family = "sans"),  # Change the font family and size for y-axis labels
    axis.text.x = element_text(angle = 45, hjust = 1, family = "sans"),  # Change the font family and size for x-axis labels
    axis.title = element_text(family = "sans"),  # Change the font family for axis titles
    plot.title = element_text(family = "sans", face = "bold", hjust = 0.5, size = 16),  # Center-align the plot title and reduce size
    legend.text = element_text(family = "sans"),  # Change the font family for legend text
    legend.title = element_text(family = "sans", face = "bold")  # Change the font family and style for legend title
  ) +
  labs(y = "Diseases", x = "Class", title = "Class Probabilities by Class and Disease (covariate)") +
  scale_color_manual(name = "Probability Category", labels = prob_labels, values = color_palette) +  # Set custom colors using RColorBrewer palette
  scale_size_continuous(range = c(1, 3), guide = "none") +  # Adjust the range for smaller point sizes here (e.g., c(0.5, 2)) and remove size guide legend
  guides(color = guide_legend(title = "Probability Category", override.aes = list(size = 2))) +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5", "6")) +  # Set custom x-axis labels
  coord_cartesian(clip = "off", expand = 0.2)  # Increase plot height by expanding the y-axis limits

# Save the plot as a PDF with a larger size
ggsave("LCAplot[2].pdf", plot, width = 12, height = 8)

plot