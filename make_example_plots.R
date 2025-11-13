library(tidyverse)
library(naniar)
library(palmerpenguins)

#exploratory plot

vis_miss(penguins_raw) + 
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(size = 15))

#Violin Plot
penguins %>% 
  drop_na() %>% 
  ggplot(aes(y = flipper_length_mm, 
             fill = sex, 
             x = species)) + 
  geom_violin() + 
  theme_bw() + 
  theme(legend.position = "bottom", 
        text = element_text(size = 20)) +
  scale_fill_viridis_d(option = "viridis", end = "0.6")

penguins %>% 
  drop_na() %>% 
  ggplot(aes(y = flipper_length_mm, 
             fill = sex,
             group = interaction(species, sex),
             x = species)) + 
  geom_boxplot(color = 'darkgray', outliers = FALSE, ) +
  geom_jitter(color = 'darkgray', alpha = 0.8, position = position_jitterdodge()) +
  theme_bw() + 
  theme(legend.position = "bottom", 
        text = element_text(size = 20)) +
  scale_fill_viridis_d(option = "viridis", end = "0.6")


# Forest Plot
## Remove rows with missing values
penguins_clean <- na.omit(penguins)
## Run separate linear models for each species
adelie_model <- lm(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, 
                   data = penguins_clean %>% filter(species == "Adelie"))

chinstrap_model <- lm(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, 
                      data = penguins_clean %>% filter(species == "Chinstrap"))

gentoo_model <- lm(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, 
                   data = penguins_clean %>% filter(species == "Gentoo"))
## Extract model results for Adelie penguins
adelie_results <- data.frame(
  species = "Adelie",
  term = names(coef(adelie_model))[-1], # Exclude the intercept
  estimate = coef(adelie_model)[-1],
  confint(adelie_model)[-1,]
) %>%
  rename(conf.low = X2.5.., conf.high = X97.5..)

## Extract model results for Chinstrap penguins
chinstrap_results <- data.frame(
  species = "Chinstrap",
  term = names(coef(chinstrap_model))[-1],
  estimate = coef(chinstrap_model)[-1],
  confint(chinstrap_model)[-1,]
) %>%
  rename(conf.low = X2.5.., conf.high = X97.5..)

## Extract model results for Gentoo penguins
gentoo_results <- data.frame(
  species = "Gentoo",
  term = names(coef(gentoo_model))[-1],
  estimate = coef(gentoo_model)[-1],
  confint(gentoo_model)[-1,]
) %>%
  rename(conf.low = X2.5.., conf.high = X97.5..)

## Combine all results into a single data frame
caterpillar_data <- rbind(adelie_results, chinstrap_results, gentoo_results) %>%
  arrange(estimate) %>%
  unite(col = "label", c("species", "term"), sep = "-", remove = FALSE) %>%
  mutate(label = factor(label, levels = label))

## Create the forest plot
ggplot(caterpillar_data, aes(y = label, x = estimate, xmin = conf.low, xmax = conf.high, color = species, shape = term)) +
  # Add the horizontal line for each confidence interval
  geom_errorbarh(height = 0.2, position = position_dodge(width = 0.5)) +
  # Add the point estimate
  geom_point(position = position_dodge(width = 0.5)) +
  # Add a vertical "line of no effect"
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  # Use different colors for each species
  scale_color_manual(values = c("Adelie" = "darkorange", "Chinstrap" = "purple", "Gentoo" = "cyan4")) +
  # Set titles and labels
  labs(
    title = "Caterpillar Plot: Body Mass Predictors by Penguin Species",
    x = "Coefficient Estimate",
    y = "Predictor Variable",
    color = "Species"
  ) +
  theme_minimal()

library(networkD3)

# 1. Load the data and remove missing values for the selected columns
# Sankey diagrams don't handle NAs well, so we filter them out
penguins_sankey_data <- penguins %>%
  filter(!is.na(species), !is.na(island), !is.na(sex))

# 2. Prepare the data for Sankey format (source, target, value)
# Create flows from species -> island and from island -> sex

# Flow 1: species to island
links_species_island <- penguins_sankey_data %>%
  group_by(source = species, target = island) %>%
  summarise(value = n(), .groups = 'drop') %>%
  mutate(source = as.character(source), target = as.character(target))

# Flow 2: island to sex
links_island_sex <- penguins_sankey_data %>%
  group_by(source = island, target = sex) %>%
  summarise(value = n(), .groups = 'drop') %>%
  mutate(source = as.character(source), target = as.character(target))

# Combine the two flows into one links data frame
links_df <- bind_rows(links_species_island, links_island_sex)

# 3. Create a nodes data frame
nodes_df <- data.frame(
  name = unique(c(links_df$source, links_df$target)),
  stringsAsFactors = FALSE
)

# 4. Map the source and target names to their 0-based index in the nodes data frame
links_df$source_id <- match(links_df$source, nodes_df$name) - 1
links_df$target_id <- match(links_df$target, nodes_df$name) - 1

# 5. Create the interactive Sankey diagram
sankeyNetwork(Links = links_df, Nodes = nodes_df,
              Source = "source_id", Target = "target_id",
              Value = "value", NodeID = "name",
              fontSize = 18, nodeWidth = 30)

#Can I add numbers to the diagram? https://stackoverflow.com/questions/78057769/how-to-add-counts-to-sankey-diagram-when-axis-categories-differ

# Waterfall plot
avg_body_mass <- mean(penguins$body_mass_g, na.rm = TRUE)

penguin_waterfall <- penguins %>%
  dplyr::filter(!is.na(body_mass_g)) %>% # Remove missing values
  mutate(dif_avg_mass = avg_body_mass - body_mass_g) %>%
  arrange(dif_avg_mass) %>% # Arrange in ascending order 
  mutate(sample_id = 1:nrow(.))

#average - actual
penguin_waterfall %>%
  ggplot(aes(x = sample_id, y = dif_avg_mass, fill = sex)) + 
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(
    title = "Body Mass compared to average by Penguin sex",
    y = "Difference of Body Mass (g) from Average",
    x = "Penguins"
  ) +
  scale_fill_viridis_d(end = 0.6)

#actual - average
penguin_waterfall %>%
  mutate(dif_b = body_mass_g - avg_body_mass) %>%
  arrange(dif_b) %>%
  mutate(sample_id_b = 1:nrow(.)) %>%
  ggplot(aes(x = sample_id_b, y = dif_b, fill = sex)) + 
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(
    title = "Body Mass compared to average by Penguin sex",
    y = "Difference of observed Body Mass (g) and Average",
    x = "Penguins"
  ) +
  scale_fill_viridis_d(end = 0.6)
  

# Heatmap

penguins %>%
  drop_na() %>%
  group_by(sex) %>%
  arrange(body_mass_g) %>%
  ungroup() %>%
  mutate(sample_id = 1:nrow(.)) %>%
  ggplot(aes(x = sex, y=sample_id, fill = body_mass_g)) +
  geom_tile() +
  theme_bw() +
  scale_fill_viridis_c(end = 0.6)

# Stacked bar chart
penguins %>%
  drop_na() %>%
  group_by(island) %>%
  mutate(total_island_penguins = n()) %>%
  group_by(island, sex) %>%
  mutate(count_sex_on_island = n()) %>%
  ungroup() %>%
  mutate(percentage_sex_on_island = (count_sex_on_island / total_island_penguins) * 100) %>%
  distinct(island, sex, percentage_sex_on_island) %>%
  ggplot(aes(x = island, y = percentage_sex_on_island, fill = sex)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Percentage of Sex on Each Island (Palmer Penguins)",
    x = "Island",
    y = "Percentage (%)",
    fill = "Sex"
  ) +
  theme_bw() +
  geom_hline(color = "#FDE725FF", yintercept = 50, linetype = "dashed", size =2) +
  scale_fill_viridis_d(end = 0.6)
  
# Ridgeline 

library(ggridges)

# Create the ridgeline plot
penguins %>%
  drop_na() %>%
  group_by(species, sex) %>%
  mutate(
    group_penguins_united = cur_group() %>%
      unite(col = "united_key", sep = "+") %>%
      pull(united_key)
  ) %>%
  ungroup() %>%
  ggplot(aes(x = flipper_length_mm, y = group_penguins_united, fill = sex)) +
  geom_density_ridges(alpha = 0.8) +
  labs(
    title = "Flipper Length Distribution",
    x = "Flipper Length (mm)",
    y = "Species + Sex"
  ) +
  theme_ridges() + # A theme optimized for ridgeline plots
  theme(
    legend.position = "none" # Hide the legend as fill is mapped to species on y-axis
  ) +
  scale_fill_viridis_d(end = 0.6)

# Create the ridgeline plot
penguins %>%
  drop_na() %>%
  group_by(species, island, sex) %>%
  mutate(
    group_penguins_united = cur_group() %>%
    unite(col = "united_key", sep = "+") %>%
    pull(united_key)
  ) %>%
  ungroup() %>%
  ggplot(aes(x = flipper_length_mm, y = group_penguins_united, fill = sex)) +
  geom_density_ridges(alpha = 0.8) +
  labs(
    title = "Flipper Length Distribution",
    x = "Flipper Length (mm)",
    y = "Species, Island, Sex Group"
  ) +
  theme_ridges() + # A theme optimized for ridgeline plots
  theme(
    legend.position = "none" # Hide the legend as fill is mapped to species on y-axis
  ) +
  scale_fill_viridis_d(end = 0.6)


#raincloud plots

library(ggrain)

penguins %>%
  ggplot(aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_rain(alpha = 0.7) +
  labs(
    title = "Raincloud Plot of Flipper Length",
    x = "Species",
    y = "Flipper Length (mm)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip() +
  scale_fill_viridis_d(end = 0.6)

penguins %>%
  drop_na() %>%
  group_by(species, sex) %>%
  mutate(
    group_penguins_united = cur_group() %>%
      unite(col = "united_key", sep = "+") %>%
      pull(united_key)
  ) %>%
  ungroup() %>%
  ggplot(aes(x = group_penguins_united, y = flipper_length_mm, fill = species)) +
  geom_rain(alpha = 0.7) +
  labs(
    title = "Raincloud Plot of Flipper Length",
    x = "Species + Sex Group",
    y = "Flipper Length (mm)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip() +
  scale_fill_viridis_d(end = 0.6)

# expository plot example
library(janitor)
df_2017 <- read.csv("~/Downloads/candyhierarchy2017.csv", fileEncoding = "ISO-8859-1") %>% clean_names()

non_candy <- c("Bonkers  the board game",
               "Box o Raisins",
               "Broken glow stick",
               "Cash  or other forms of legal tender",
               "Chardonnay",
               "Creepy Religious comics Chick Tracts",
               "Dental paraphenalia",
               "Generic Brand Acetaminophen",
               "Glow sticks",
               "Healthy Fruit",
               "Hugs  actual physical hugs",
               "JoyJoy  Mit Iodine",
               "Kale smoothie",
               "Green Party M M s",
               "Independent M M s",
               "Abstained from M M ing",
               "Minibags of chips",
               "Pencils",
               "Real Housewives of Orange County Season 9 Blue Ray",
               "Sandwich sized bags filled with BooBerry Crunch",
               "Spotted Dick",
               "Trail Mix",
               "Vials of pure high fructose corn syrup  for main lining into your vein",
               "Vicodin", 
               "White Bread", 
               "Whole Wheat anything")

df_2017_jd <- df_2017 %>% 
  select(starts_with("Q6")) %>%
  rename_with(~ str_remove(., "Q6...")) %>%
  rename_with(~ str_replace_all(., "\\.", " ")) %>%
  rename_with(~ str_trim(.x, side = "both")) %>%
  select(-all_of(non_candy)) %>%
  pivot_longer(everything(), names_to = "column_name", values_to = "value") %>%
  group_by(column_name, value) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(proportion = count / total)


df_2017_jd[df_2017_jd==""]<- NA

to_plot <- df_2017_jd %>%
  replace_na(list(value = "Not Answered")) %>%
  pivot_wider(id_cols = column_name,
              names_from = value,
              values_from = proportion) %>%
  arrange(JOY) %>%
  mutate(label = "")
  
to_plot[nrow(to_plot), "label"] <- "Any full-sized candy bar"
to_plot[nrow(to_plot)-1, "label"] <- "Reese's Peanut Butter Cups"
to_plot[1, "label"] <- "Candy that is clearly just the stuff given out for free at restaurants"
to_plot[2, "label"] <- "Gum from baseball cards"
to_plot[10, "label"] <- "Circus peanuts"
to_plot$Chocolate <- "No"
to_plot$Chocolate[c(13, 22, 26, 28, 34, 44, 46, 47, 49, 50, 51, 52, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,77 )] <- "Yes"


to_plot %>%
  ggplot(aes(x=JOY, y = DESPAIR, label = label, color = Chocolate, shape = Chocolate)) +
  geom_point(alpha=0.6, size = 2.5) +
  xlim(0, 1) +
  ylim(1, 0) +
  theme_bw() +
  ggtitle("Chocolate is among the most liked candies in 2017") +
  labs(x = "Fraction of Respondents\nWho Reported Joy",
       y = "Fraction of Respondents\nWho Reported Despair",
       caption = "Here each candy is plotted based on the fraction of respondents which viewed that candy with JOY\nvs the fraction of respondents which viewed that candy with DESPAIR.\nThe upper right corner of the plot represents the most liked candies (lots of joy, little despair),\nwhile the bottom left corner represents the least liked candies (more despair, less joy).\nCandy color and shape is used to separate\nchocolate candy (brown triangles) from non-chocolate candy (blue circles).\nChocolate candy seems to be among the most liked candy\nwith any full-sized candy bar having the most joy and least despair reported.") +
  geom_text_repel(show.legend = FALSE) +
  coord_cartesian(clip = "off") +
  scale_color_manual(values = c("Yes" = "#654321", "No" = "#00008B")) +
  theme(text = element_text(size = 16),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(size = 12, hjust = 0))

  

