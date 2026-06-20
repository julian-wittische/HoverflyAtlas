df <- data.frame(
  mois = factor(
    c("Jan", "Fev", "Mar", "Avr", "May", "June",
      "July", "Aug", "Sept", "Oct", "Nov", "Dec"),
    levels = c("Jan", "Fev", "Mar", "Avr", "May", "June",
               "July", "Aug", "Sept", "Oct", "Nov", "Dec")
  ),
  n = c(0,0,0,0,24,92,280,237,148,0,0,0)
)
ggplot(df, aes(mois, n)) +
  geom_col(
    fill = "black",
    colour = "black",
    linewidth = 0.3,
    width = 1
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank()
  )
