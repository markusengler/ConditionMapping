plot_glht <- function(glht_confint_tidy) {
  glht_confint_tidy %>% 
  mutate(Hypothesis = paste(lhs, "==", rhs)) %>% 
  ggplot(aes(x = Hypothesis, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_pointrange() +
  expand_limits(y = 0) +
  coord_flip() +
  ylab("Estimate of the difference with 95% CI\n(adjusted with the single-step method)")
}
