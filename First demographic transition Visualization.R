#Google disk with data: https://drive.google.com/drive/folders/1sqrfrhu9rBovb_fHHujmbhA2bZ-Rx03J?usp=sharing

library(tidyverse)
library(ggplot2)
library(sysfonts)
library(showtext)

#Import data
load("Crude_birth_rate.RData")
load("Crude_death_rate.RData")

#Import font
sysfonts::font.add.google(name = "Roboto",
                          family = "Roboto",
                          regular.wt = 300)
showtext_auto()
showtext_opts(dpi = 320)

#Process data
cbr <- cbr |>
  select(!Germany) |>
  filter(!is.na(Year)) |> 
  pivot_longer(!"Year", names_to = "Country", values_to = "cbr_value") |> 
  mutate(cbr_value = as.numeric(cbr_value))
cdr <- cdr |> 
  select(!Germany) |>
  filter(!is.na(Year)) |>
  pivot_longer(!"Year", names_to = "Country", values_to = "cdr_value") |> 
  mutate(cdr_value = as.numeric(cdr_value))

final <- left_join(cbr, cdr) 

dataset <- final |> 
  filter(!is.na(cbr_value)&!is.na(cdr_value)) |> 
  pivot_longer(cols = c("cbr_value", "cdr_value"),
                                names_to = "Type", values_to = "Values") |>
  mutate(Type = ifelse(Type == "cbr_value", "a", "b")) |> 
  rename("x" = "Year", "y" = "Values", "f" = "Type")

df_A <- dataset |> filter(Country == "Austria") |> select(!Country)
df_F <- dataset |> filter(Country == "France") |> select(!Country)
df_E <- dataset |> filter(Country == "England") |> select(!Country)
df_S <- dataset |> filter(Country == "Sweden") |> select(!Country)

ribbonize <- function(.data, .x, .y, .f) {
  # Calculate the ribbons required for geom_ribbon().
  # For more info, visit nsgrantham.com/fill-between-two-lines-ggplot2
  #
  # Usage:
  # df <- tibble(
  #   x = c(1:8, 1:8),
  #   y = c(1, 5, 6, 4, 1, 1, 3, 2, 1, 4, 5, 4, 2, 2, 2, 2),
  #   f = c(rep("a", 8), rep("b", 8))
  # )
  #
  # ribbons <- ribbonize(df, x, y, f)
  #
  # ggplot(df) +
  #   geom_line(aes(x, y, linetype = f)) +
  #   geom_ribbon(data = ribbons, aes(x, ymin = ymin, ymax = ymax, fill = fill))
  
  # Check there are only 2 level in .f
  levels <- .data %>%
    pull({{ .f }}) %>%
    unique()
  
  stopifnot(length(levels) == 2)
  
  # Check that there is exactly 1 observation per level in .f at every .x
  level_counts_by_x <- .data %>%
    filter(!is.na({{ .y }})) %>%
    group_by({{ .x }}) %>%
    count() %>%
    pull(n)
  
  stopifnot(all(level_counts_by_x == 2))
  
  bounds <- .data %>%
    mutate({{ .f }} := recode({{ .f }}, a = levels[1], b = levels[2])) %>%
    pivot_wider(names_from = {{ .f }}, values_from = {{ .y }}) %>%
    mutate(
      ymax = pmax(a, b),
      ymin = pmin(a, b),
      fill = a >= b
    )
  
  intervals <- bounds %>%
    filter(ymax > ymin) %>%
    select(-a, -b)
  
  intersections <- bounds %>%
    mutate(lag_fill = lag(fill), lead_fill = lead(fill)) %>%
    filter(ymax == ymin) %>%
    select(-a, -b, -fill) %>%
    pivot_longer(lag_fill:lead_fill, names_to = NULL, values_to = "fill") %>%
    filter(!is.na(fill)) %>%
    distinct()
  
  other_intersections <- bounds %>%
    transmute(
      x1 = {{ .x }},       y1 = a,
      x2 = lead({{ .x }}), y2 = lead(a),
      x3 = {{ .x }},       y3 = b,
      x4 = lead({{ .x }}), y4 = lead(b)
    ) %>%
    filter(((y1 > y3) & (y2 < y4)) | ((y1 < y3) & (y2 > y4))) %>%
    mutate(
      d = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4),
      u = x1 * y2 - y1 * x2,
      v = x3 * y4 - y3 * x4,
      x = (u * (x3 - x4) - v * (x1 - x2)) / d,
      y = (u * (y3 - y4) - v * (y1 - y2)) / d
    ) %>%
    select(x, ymax = y, ymin = y)
  
  bind_rows(
    intervals,
    intersections,
    mutate(other_intersections, fill = TRUE),
    mutate(other_intersections, fill = FALSE)
  ) %>%
    arrange({{ .x }})
}
ribbons_A <- ribbonize(df_A, x, y, f) |> mutate (Country = "Austria")
ribbons_F <- ribbonize(df_F, x, y, f) |> mutate (Country = "France")
ribbons_E <- ribbonize(df_E, x, y, f) |> mutate (Country = "England")
ribbons_S <- ribbonize(df_S, x, y, f) |> mutate (Country = "Sweden")

ribbons <- bind_rows(ribbons_A, ribbons_F, ribbons_E, ribbons_S)

#Plot
dataset |> 
  filter(Country %in% c("England", "France", "Austria", "Sweden")) |>
  ggplot() +
  geom_line(aes(x, y, color = f), lwd = 0.6) +
  scale_color_manual(values = c("#00BFC4", "#F8766D"), labels = c("Crude birth rate", "Crude death rate"))+
  geom_ribbon(data = ribbons, aes(x, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.4) +
  facet_wrap(~Country, scales = "free_x")+
  scale_x_continuous(breaks = seq(1750, 2000, 50))+
  geom_vline(aes(xintercept = 1914), color = "grey65", lwd = 0.5) +
  geom_vline(aes(xintercept = 1939), color = "grey65", lwd = 0.5) +
  theme_minimal()+
  labs(x = NULL, y = NULL,
       title = "First Demographic Transition in European Countries",
       subtitle = "Births and deaths per 1000 people per year",
       caption = "Source: Jean-Claude Chesnais. La transition démographique. Figure: Elizaveta Minaeva")+
  scale_fill_discrete(labels = c("Population decrease", "Population increase"))+
  annotate(x=1914,y=+Inf,label="WWI",vjust=1,geom="label", size = 2.5, color = "grey45")+
  annotate(x=1939,y=+Inf,label="WWII",vjust=1,geom="label", size = 2.5, color = "grey45")+
  theme(text=element_text(family = "Roboto"),
        plot.title.position = "plot",
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major =  element_line(color = "#F2F2F2", linewidth = 0.3),
        panel.grid.minor = element_line(color = "#F2F2F2", linewidth = 0.3))
  
#ggsave("Minaeva Visualization.png", width = 8, height = 5, dpi = 320, bg = "white")




