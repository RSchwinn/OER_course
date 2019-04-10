library(tidyverse)
ucb_admit = read_csv("data/ucb-admit.csv")
ucb_admit
glimpse(ucb_admit)

ucb_admit %>%
    count(Gender,
          Admit) %>%
    group_by(Gender) %>%
    mutate(prop_admit = n / sum(n))

ggplot(data = ucb_admit,
       mapping = aes(x = Gender, fill = Admit)) +
    geom_bar(position = "fill") +
    labs(y = "", title = "Admit by gender")

ggplot(ucb_admit, mapping = aes(x = Gender, fill = Admit)) +
    geom_bar(position = "fill") +
    facet_grid(. ~ Dept) +
    labs(x = "Gender",
         y = "",
         fill = "Admission",
         title = "Admit by gender by department")
backslash <- function(x, y)
    