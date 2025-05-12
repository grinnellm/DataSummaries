# Controls
print_figs <- FALSE
out_folder <- "Nearshore"

# CRS
st_crs(bioRaw) <- st_crs(shapes$sections)

time_periods <- list(Early = 1995:2000, Middle = 2001:2014, Recent = 2014:2024)

# Grab nearshore data
nearshore <- bioRaw %>%
  # as_tibble() %>%
  filter(
    SourceCode == 2 & GearCode == 1 |
      SourceCode == 4 & GearCode %in% c(21, 70) & Year %in% time_periods$Early
  ) %>%
  mutate(Type = "Nearshore")

# # Years for nearshore data
# yrsNearshore <- nearshore %>%
#   pull(Year) %>%
#   unique()

# Grab seine data
seine <- bioRaw %>%
  # as_tibble() %>%
  filter(GearCode == 29, Year %in% unlist(time_periods)) %>%
  mutate(Type = "Seine test")

# Combine nearshore and seine data
all_dat <- bind_rows(nearshore, seine) %>%
  left_join(y = tGear %>% select(GearCode, Gear), by = "GearCode") %>%
  left_join(
    y = tSource %>% select(SourceCode, SampleSource), by = "SourceCode"
  ) %>%
  rename(MonthCode = Month, Source = SampleSource) %>%
  mutate(
    Period = ifelse(Year %in% time_periods$Early, "Early",
                    ifelse(Year %in% time_periods$Middle, "Middle", "Recent")),
    Month = month(MonthCode, label = TRUE)
  ) %>%
  filter(Year < 2014 | Representative == 1) %>%
  select(
    Year, Month, Period, Region, StatArea, Group, Section, LocationCode,
    LocationName, Sample, Source, Gear, Type, Fish, Length, Weight, Sex,
    MaturityCode, Age
  ) %>%
  na.omit() %>%
  st_crop(y = reg_bbox)  # TODO: This might remove some samples that should stay

# Number of samples
num_samp <- all_dat %>%
  group_by(Type, Year, StatArea, Gear) %>%
  summarise(Num = length(unique(Sample))) %>%
  ungroup()

# Number of samples
samples <- all_dat %>%
  group_by(
    Year, Month, Period, Region, StatArea, Group, Section, LocationCode,
    LocationName, Type
  ) %>%
  summarise(Number = length(unique(Sample))) %>%
  ungroup()

# Proportion-at-age
prop_age <- all_dat %>%
  group_by(Type, Year, StatArea, Age) %>%
  summarise(Number = n()) %>%
  mutate(Proportion = Number / sum(Number)) %>%
  ungroup() %>%
  group_by(Type, Year, StatArea) %>%
  mutate(Mean = weighted.mean(x = Age, w = Proportion)) %>%
  ungroup() %>%
  complete(Type, Year, StatArea, Age)

plot_map <- RegionMap + 
  geom_sf(
    data = samples, alpha = 0.85, size = 3, show.legend = "point",
    mapping = aes(colour = Number, shape = Month)
  ) +
  scale_colour_viridis_c() +
  labs(colour = "Number of\nsamples") +
  facet_grid(Type ~ Period, labeller = "label_both") +
  coord_sf(expand = FALSE) +
  theme(legend.position = "right")
if(print_figs)  ggsave(filename = here(out_folder, "Map.png"))
print(plot_map)

plot_num_fish <- ggplot(
  data = all_dat, mapping = aes(x = Year, fill = Gear)
) +
  geom_bar() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_viridis_d() +
  labs(y = "Number of fish") +
  facet_grid(StatArea ~ Type, labeller = "label_both") +
  theme(legend.position = "top")
if(print_figs)  ggsave(filename = here(out_folder, "NumFish.png"))
print(plot_num_fish)

plot_num_samp <- ggplot(
  data = num_samp, mapping = aes(x = Year, y = Num, fill = Gear)
) +
  geom_col() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_fill_viridis_d() +
  labs(y = "Number of samples") +
  facet_grid(StatArea ~ Type, labeller = "label_both") +
  theme(legend.position = "top")
if(print_figs)  ggsave(filename = here(out_folder, "NumSamp.png"))
print(plot_num_samp)

plot_prop_age <- ggplot(
  data = prop_age,
  mapping = aes(x = Year, y = Age, size = Proportion, fill = Type)
) +
  geom_point(
    shape = 21, color = "black", position = position_dodge(0.7), alpha = 0.7
  ) + 
  geom_line(mapping = aes(x = Year, y = Mean, colour = Type), size = 1) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_fill_viridis_d() +
  scale_colour_viridis_d() +
  scale_size(range = c(1, 5), breaks = 1:7/10) +
  facet_grid(StatArea ~ ., labeller = "label_both")
if(print_figs)  ggsave(filename = here(out_folder, "PropAge.png"))
print(plot_prop_age)

plot_length_age <- ggplot(
  data = all_dat,
  mapping = aes(x = Age, y = Length, group = interaction(Age, Type),
                fill = Type)
) +
  geom_violin() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_fill_viridis_d() +
  labs(y = "Length (mm)") +
  facet_grid(StatArea ~ Period, labeller = "label_both") +
  theme(legend.position = "top")
if(print_figs)  ggsave(filename = here(out_folder, "LengthAge.png"))
print(plot_length_age)

plot_weight_age <- ggplot(
  data = all_dat,
  mapping = aes(x = Age, y = Weight, group = interaction(Age, Type),
                fill = Type)
) +
  geom_violin() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_fill_viridis_d() +
  labs(y = "Weight (g)") +
  facet_grid(StatArea ~ Period, labeller = "label_both") +
  theme(legend.position = "top")
if(print_figs)  ggsave(filename = here(out_folder, "WeightAge.png"))
print(plot_weight_age)

plot_length_weight <- ggplot(
  data = all_dat,
  mapping = aes(x = Length, y = Weight, group = Type, colour = Type)
) +
  # geom_point() + 
  geom_density2d(alpha = 0.6, linewidth = 1) +
  scale_colour_viridis_d() +
  labs(x = "Length (mm)", y = "Weight (g)") +
  facet_grid(StatArea ~ Period, labeller = "label_both") +
  theme(legend.position = "top")
if(print_figs)  ggsave(filename = here(out_folder, "LengthWeight.png"))
print(plot_length_weight)
