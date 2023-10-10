# Helper function to generate factor scores
generate_scores <- function(mean, sd, n) {
  rnorm(n, mean, sd)
}

# Number of samples per group
n <- 1000
set.seed(123)
# Group 1: Scores distinctly different from Group 2
group1_scores <- generate_scores(0.01, 1, n)
  

# Group 2: Standard normal distribution
group2_scores <- generate_scores(0, 1, n)

# Group 3: Scores close to Group 2
group3_scores <- generate_scores(-0.1, 1, n)

# Create a dataframe
df <- data.frame(
  Scores = c(group1_scores, group2_scores, group3_scores),
  Group = factor(rep(1:3, each=n))
)


# Rename the levels of the Group factor
df$Group <- factor(df$Group, 
                   levels=c(1, 2, 3), 
                   labels=c("New-Form: Anchored","Old Form", "New Form: Non-Anchored"))

# Calculate means for each group
group_means <- tapply(df$Scores, df$Group, mean)

cor_value <- cor(group2_scores, group3_scores)

mean_colors <- c("#66c2a5", "#8da0cb", "#ffd92f", "#fc8d62") 

# Adjustments for the labels
label_shift <- c(0.5, -0.5, 0.5,-.5) 

# Common x location for all mean labels
label_x <- max(df$Scores)/2 +.25

# Create the plot
EquateGraph <- ggplot(df, aes(x=Scores, y=Group)) +
  geom_jitter(aes(color=Group), width=0, height=0.2, size=3, alpha=0.2) +
  scale_color_manual(values=mean_colors, name="Form") +
  
  geom_vline(aes(xintercept=group_means["New-Form: Anchored"]), color=mean_colors[1], linetype="dashed", linewidth=1) +
 
  geom_vline(aes(xintercept=group_means["Old Form"]), color=mean_colors[2], linetype="dashed", linewidth=1, alpha = .5) +
  geom_vline(aes(xintercept=group_means["New Form: Non-Anchored"]), color=mean_colors[3], linetype="dashed", linewidth=1) +
  
  geom_text(aes(x=label_x, y="New-Form: Anchored", label=sprintf("Mean: %.2f", group_means["New-Form: Anchored"])), color=mean_colors[1], hjust=0, nudge_y = -0.5) +
  geom_text(aes(x=label_x, y="Old Form", label=sprintf("Mean: %.2f", group_means["Old Form"])), color=mean_colors[2], hjust=0, nudge_y = -0.5) +
  geom_text(aes(x=label_x, y="New Form: Non-Anchored", label=sprintf("Mean: %.2f", group_means["New Form: Non-Anchored"])), color=mean_colors[3], hjust=0, nudge_y = -0.5) +
  
  theme_minimal() +
  labs(title="Score Differences Before and After Equating", x="Scores", y="") +
  theme(legend.position="top")

  EquateGraph <- EquateGraph + theme(plot.title = element_text(hjust = 0.5))
  EquateGraph
  
  