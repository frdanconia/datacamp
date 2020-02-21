ggsave(ggplot(flips1, aes(x = coinFlip)) +
  geom_histogram(bins = 2, aes(fill =..count..)) +
  labs(title="Histogram for Coin Flip - Trial One") +
  labs(x="Heads Or Tails", y="Count") +
  theme(plot.title = 
          element_text(
            family = "Trebuchet MS", 
            color = "#666666", 
            face = "bold",
            size = 22,
            hjust = 0.5)) +
  theme(axis.title = 
          element_text(
            family = "Trebuchet MS",
            color = "#666666",
            face = "bold", 
            size = 18,
            hjust = 0.5)),
  file = "trialOne.png", 
  path = "images/", width = 17, height = 8.8, dpi = 72)

  ggsave(ggplot(flips2, aes(x = coinFlip)) +
  geom_histogram(bins = 2, aes(fill =..count..)) +
  labs(title="Histogram for Coin Flip - Trial Two") +
  labs(x="Heads Or Tails", y="Count") +
  theme(plot.title = 
          element_text(
            family = "Trebuchet MS", 
            color = "#666666", 
            face = "bold",
            size = 22,
            hjust = 0.5)) +
  theme(axis.title = 
          element_text(
            family = "Trebuchet MS",
            color = "#666666",
            face = "bold", 
            size = 18,
            hjust = 0.5)), 
  file = "trialTwo.png", 
  path = "images/", width = 17, height = 8.8, dpi = 72)