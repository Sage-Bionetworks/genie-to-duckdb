library(here)
library(tidyverse)

# the manual_vs_ai csv comes from inputting my report and the text from claude into gemini.  Let the AI rot continue.
comp <- readr::read_csv(
  here('db', 'manual_vs_ai.csv')
) %>%
  rename(
    manual = `Uploaded Report (%)`,
    ai = `Report B (%)`
  )

comp <- comp %>%
  mutate(across(.cols = c(manual, ai), as.numeric)) %>%
  mutate(`Cancer Type` = forcats::fct_inorder(`Cancer Type`))


gg <- comp %>%
  pivot_longer(cols = c(manual, ai), names_to = "method", values_to = "pct") %>%
  ggplot(aes(x = pct, y = `Cancer Type`, color = method)) +
  annotate(
    "rect",
    ymin = which(levels(comp$`Cancer Type`) == "Renal (RCC)") - 0.4,
    ymax = which(levels(comp$`Cancer Type`) == "Renal (RCC)") + 0.4,
    xmin = -Inf,
    xmax = Inf,
    fill = "grey80",
    alpha = 0.3
  ) +
  annotate(
    "rect",
    ymin = which(levels(comp$`Cancer Type`) == "Total") - 0.4,
    ymax = which(levels(comp$`Cancer Type`) == "Total") + 0.4,
    xmin = -Inf,
    xmax = Inf,
    fill = "grey80",
    alpha = 0.3
  ) +
  geom_point(size = 3) +
  labs(
    x = "Off-label (%)",
    y = NULL,
    color = "Method",
    title = "Percentage of exposures that are off label - Pretty close! (kinda)"
  ) +
  ggsci::scale_color_bmj() +
  theme_minimal() +
  theme(
    plot.title.position = 'plot'
  )

ggsave(gg, height = 2, width = 6, filename = 'ai_manual_compare.png')

comp %>%
  pivot_longer(cols = c(manual, ai), names_to = "method", values_to = "pct") %>%
  ggplot(aes(x = pct, y = `Cancer Type`, color = method)) +
  geom_point(size = 3) +
  labs(x = "Off-label (%)", y = NULL, color = "Method") +
  ggsci::scale_color_bmj() +
  theme_minimal()
