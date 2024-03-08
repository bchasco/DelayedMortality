g2 <- list()
#Plot the ratio of
icnt <- 1
for(i in c("MCN", "BON_J", "BON_A")){
  g2[[icnt ]] <- rbind(do.call(rbind, r_phi),do.call(rbind,r_p)) %>%
    mutate(dam = factor(dam, level = c("MCN", "BON_J", "BON_A"))) %>%
    mutate(var = factor(var, level = c("low","med","high"))) %>%
    filter(dam == i) %>%
    ggplot(aes(x = par, fill = parName)) +
    geom_density(aes(y = ..scaled..), alpha = 0.5) +
    facet_grid(var ~ dam + parName, scales = "free_x") +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +  # Set y-axis limits to 0 and 1
    theme_minimal() +
    ylab("") +
    xlab("") +
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))#   mutate(n = n/1000) %>%
  icnt <- icnt + 1
}

# Install and load gridExtra package
library(gridExtra)
# Plot the list of ggplots
g <- grid.arrange(grobs = g2, ncol = 3, )
print(g)
ggsave(filename = "parDistribution.png", g, width = 9, height = 6)
