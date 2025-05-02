# Nearshore data
nearshore <- bioRaw %>%
  as_tibble() %>%
  filter(
    SourceCode == 2 & GearCode == 1 |
      SourceCode == 4 & GearCode %in% c(21, 70) & Year %in% 1995:2000 #&
      #StatArea == 23
  ) %>%
  select(
    Year, Month, Region, StatArea, Group, Section, LocationCode, LocationName,
    Sample, SourceCode, GearCode, Fish, Length, Weight, Sex, MaturityCode, Age
  ) %>%
  na.omit() %>%
  left_join(y = tGear %>% select(GearCode, Gear), by = "GearCode") %>%
  mutate(Period = ifelse(Year %in% 1995:2000, "Early","Recent"))

# Years for nearshore data
yrsNearshore <- nearshore %>%
  pull(Year) %>%
  unique()

# Number of nearshore samples
nearshore %>%
  group_by(Period, Year, StatArea) %>%
  summarise(NSamp = length(unique(Sample))) %>%
  ungroup()

# Number of samples
near_num_samp <- nearshore %>%
  group_by(Year, StatArea, Gear) %>%
  summarise(Num = length(unique(Sample))) %>%
  ungroup()

plot_num_fish <- ggplot(
  data = nearshore, mapping = aes(x = Year, fill = Gear)
) +
  geom_bar() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_viridis_d() +
  labs(y = "Number of fish") +
  facet_grid(StatArea ~ ., labeller = "label_both")
ggsave(filename = "NumFish.png")
print(plot_num_fish)

plot_num_samp <- ggplot(
  data = near_num_samp, mapping = aes(x = Year, y = Num, fill = Gear)
) +
  geom_col() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_fill_viridis_d() +
  labs(y = "Number of samples") +
  facet_grid(StatArea ~ ., labeller = "label_both")
ggsave(filename = "NumSamp.png")
print(plot_num_samp)

plot_age <- ggplot(
  data = nearshore, mapping = aes(x = Year, y = Age, group = Year)
) +
  geom_violin(fill = "darkgrey") + 
  scale_x_continuous(breaks = pretty_breaks()) +
  facet_grid(StatArea ~ ., labeller = "label_both")
ggsave(filename = "Age.png")
print(plot_age)

plot_length_age <- ggplot(
  data = nearshore, mapping = aes(x = Age, y = Length, group = Age)
) +
  geom_violin(fill = "darkgrey") + 
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(y = "Length (mm)") +
  facet_grid(StatArea ~ Period, labeller = "label_both")
ggsave(filename = "LengthAge.png")
print(plot_length_age)

plot_weight_age <- ggplot(
  data = nearshore, mapping = aes(x = Age, y = Weight, group = Age)
) +
  geom_violin(fill = "darkgrey") + 
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(y = "Weight (g)") +
  facet_grid(StatArea ~ Period, labeller = "label_both")
ggsave(filename = "WeightAge.png")
print(plot_weight_age)

plot_length_weight <- ggplot(
  data = nearshore, mapping = aes(x = Length, y = Weight, colour = Age)
) +
  geom_point() + 
  scale_colour_viridis_c() +
  labs(x = "Length (mm)", y = "Weight (g)") +
  facet_grid(StatArea ~ Period, labeller = "label_both")
ggsave(filename = "LengthWeight.png")
print(plot_length_weight)
