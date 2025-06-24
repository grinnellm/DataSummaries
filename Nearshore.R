# CRS
st_crs(bioRaw) <- st_crs(shapes$sections)

# Time periods: CC
if(region == "CC") {
  time_periods <- list(Recent = 2017:2024)
  time_years <- list(Recent = paste(range(time_periods$Recent), collapse = "-"))
}

# Time periods: WCVI
if(region == "WCVI") {
  time_periods <- list(
    Early = 1995:2000, Middle = 2001:2014, Recent = 2014:2025
  )
  time_years <- list(Early = paste(range(time_periods$Early), collapse = "-"),
                     Middle = paste(range(time_periods$Middle), collapse = "-"),
                     Recent = paste(range(time_periods$Recent), collapse = "-"))
}

# Grab nearshore data
nearshore <- bioRaw %>%
  # as_tibble() %>%
  filter(
    SourceCode == 2 & GearCode == 1 |
      SourceCode == 4 & GearCode %in% c(21, 70) & Year %in% time_periods$Early
  ) %>%
  mutate(Type = "Nearshore")

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
    Period = ifelse(Year %in% time_periods$Early,
                    paste0("Early (", time_years$Early, ")"),
                    ifelse(Year %in% time_periods$Middle,
                           paste0("Middle (", time_years$Middle, ")"), 
                           paste0("Recent (", time_years$Recent, ")"))),
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
  ungroup() #%>%
  # group_by(Type, Year, StatArea) %>%
  # mutate(Mean = weighted.mean(x = Age, w = Proportion)) %>%
  # ungroup() #%>%
  # complete(Type, Year, StatArea, Age)

plot_near_map <- RegionMap + 
  geom_sf(
    data = samples, alpha = 0.85, size = 2, show.legend = "point",
    mapping = aes(colour = Number, shape = Month)
  ) +
  scale_colour_viridis_c() +
  labs(colour = "Number of\nsamples") +
  coord_sf(expand = FALSE) +
  guides(shape = guide_legend(nrow = 2), fill = guide_legend(nrow = 2)) +
  theme(
    legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1)
  )
if(region %in% c("CC")) {
  plot_near_map <- plot_near_map +
    facet_grid(Period ~ Type, labeller = "label_both")
} else {
  plot_near_map <- plot_near_map +
    facet_grid(Type ~ Period, labeller = "label_both")
}
ggsave(
  plot_near_map, filename = here(regName, "NearMap.png"),
  width = figWidth, height = 6, dpi = figRes
)

plot_near_num_fish <- ggplot(
  data = all_dat, mapping = aes(x = Year, fill = Gear)
) +
  geom_bar() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_viridis_d() +
  labs(y = "Number of fish") +
  facet_grid(StatArea ~ Type, labeller = "label_both") +
  theme(legend.position = "top")
ggsave(
  plot_near_num_fish, filename = here(regName, "NearNumFish.png"),
  width = figWidth, height = 5, dpi = figRes
)

plot_near_num_samp <- ggplot(
  data = num_samp, mapping = aes(x = Year, y = Num, fill = Gear)
) +
  geom_col() + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_fill_viridis_d() +
  labs(y = "Number of samples") +
  facet_grid(StatArea ~ Type, labeller = "label_both") +
  theme(legend.position = "top")
ggsave(
  plot_near_num_samp, filename = here(regName, "NearNumSamp.png"),
  width = figWidth, height = 5, dpi = figRes
)

plot_near_prop_age <- ggplot(
  data = prop_age,
  mapping = aes(x = Year, y = Age, size = Proportion, fill = Type)
) +
  geom_point(
    shape = 21, color = "black", position = position_dodge(0.7), alpha = 0.7
  ) + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_fill_viridis_d() +
  scale_colour_viridis_d() +
  scale_size(range = c(0.5, 2), breaks = 1:6/10) +
  facet_grid(StatArea ~ ., labeller = "label_both") +
  theme(legend.position = "top")
ggsave(
  plot_near_prop_age, filename = here(regName, "NearPropAge.png"),
  width = figWidth, height = 5, dpi = figRes
)

plot_near_length_age <- ggplot(
  data = all_dat,
  mapping = aes(x = Age, y = Length, group = interaction(Age, Type),
                fill = Type)
) +
  geom_boxplot(outlier.colour = "black", size = 0.25) + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_fill_viridis_d() +
  labs(y = "Length (mm)") +
  facet_grid(StatArea ~ Period, labeller = "label_both") +
  theme(legend.position = "top")
ggsave(
  plot_near_length_age, filename = here(regName, "NearLengthAge.png"),
  width = figWidth, height = 5, dpi = figRes
)

plot_near_weight_age <- ggplot(
  data = all_dat,
  mapping = aes(x = Age, y = Weight, group = interaction(Age, Type),
                fill = Type)
) +
  geom_boxplot(outlier.colour = "black", size = 0.25) + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_fill_viridis_d() +
  labs(y = "Weight (g)") +
  facet_grid(StatArea ~ Period, labeller = "label_both") +
  theme(legend.position = "top")
ggsave(
  plot_near_weight_age, filename = here(regName, "NearWeightAge.png"),
  width = figWidth, height = 5, dpi = figRes
)

plot_near_length_weight <- ggplot(
  data = all_dat,
  mapping = aes(x = Length, y = Weight, group = Type, colour = Type)
) +
  geom_density2d(alpha = 0.6, linewidth = 1) +
  scale_colour_viridis_d() +
  labs(x = "Length (mm)", y = "Weight (g)") +
  facet_grid(StatArea ~ Period, labeller = "label_both") +
  theme(legend.position = "top")
ggsave(
  plot_near_length_weight, filename = here(regName, "NearLengthWeight.png"),
  width = figWidth, height = 5, dpi = figRes
)
