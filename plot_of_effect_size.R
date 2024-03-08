#Plot the ratio of
g1 <- do.call(rbind, p) %>%
  mutate(dam = factor(dam, level = c("RI","MCN", "BON_J", "BON_A"))) %>%
  # mutate(delayedMortalityLocations = factor(delayedMortalityLocations, level = c("MCN", "BON_J", "BON_A"))) %>%
  mutate(var = factor(var, level = c("low", "med", "high"))) %>%
  mutate(n = n/1000) %>%
  filter(damCol == TRUE, delay == "BON_J") %>%
  filter(d_phi %in% c(0.05, 0.1, 0.2, 0.25, 0.3)) %>%
  pivot_wider(names_from =  loc, values_from = returns) %>%
  group_by(d_phi,n,sim,nStrata,var,dam) %>%
  summarise(p.value = t.test(above,below,alternative = "less")$p.value) %>%
  group_by(d_phi,n,nStrata,var,dam) %>%
  summarise(n.p = sum(p.value<=0.05)/nsim) %>%
  mutate(g.col = ifelse(n.p>=0.8,"#999999", '#0000FF')) %>%
  mutate(g.alpha = ifelse(n.p>=0.8,0.3, 0.2)) %>%
  # pivot_wider(names_from =  dam, values_from = p.value)
  ggplot(aes(x = factor(n), y = n.p)) +
  geom_col(aes(fill = as.factor(dam), color = as.factor(dam), alpha = g.alpha), position = "dodge", width = 0.75) +
  facet_grid(var + nStrata ~ d_phi, scales = "free") +
  coord_cartesian(ylim = c(0, 1)) +
  xlab("Number of fish per replicate (1000)") +
  ylab("Percent of simulations that correctly detect delayed mortality") +
  theme_bw() +
  scale_fill_manual(values = c(RI = "#999999", MCN = "#999999", BON_J = "#0000FF", BON_A = "#999999")) +
  scale_color_manual(values = c(RI = "#999999", MCN = "#999999", BON_J = "#0000FF", BON_A = "#999999")) +
  scale_alpha_continuous(range = c(0.1, 1))+
  theme(legend.position = "left") +  # Position legend on the left
  guides(color = guide_legend(override.aes = list(color = NA))) +  # Remove fill from legend
  labs(fill = "Detection location")  # Change legend title

print(g1)
ggsave(g1, filename = "plot_of_effect_size_BON_J.png", height = 8, width = 10, units = "in")
