source("Rcode/00-preamble.R")

# load data ---------------------------------------------------------------

d <- read_delim("Data/redlist_rothmaler_t.csv", 
                delim = ";", escape_double = FALSE, trim_ws = TRUE)
names(d)

# analysis for overview paragraph -----------------------------------------

# how man species are cultivated in Germany?
d %>% filter(cultivated == "yes") %>% group_by(taxon_name) %>% count() %>% arrange(desc(n))
d %>% filter(cultivated == "yes") %>% nrow()
# 3407 species cultivated

# how many as ornamental plants, how many as crops?
d %>% filter(cultivated == "yes") %>% count(ornamental, crop)

# how many of these species are native, how many non-native?
d <- d %>% mutate(native = ifelse(is.na(native) == T , "non-native", native))
d %>% filter(cultivated == "yes") %>% count(native)
# 152 + 13 species = 165 non-native species are established from a total
# of 2531 + 165 non-native species cultivated
165 / (2524 + 165)
# c. 6 %
total <- d %>% filter(cultivated == "yes") %>% nrow()
d %>% filter(cultivated == "yes") %>% count(native) %>% 
  mutate(percentage = n / total)
# 21% of the cultivated flora is native, 79% consists of non-native plants

# how much is that relative to the German native flora?
d %>% filter(native == "native") %>% nrow()
# 3584 wild native species in Germany

# taking out apomictic genera Rubus and Hieracium
d %>% filter(native == "native") %>% 
  filter(!grepl("\\b(Rubus|Hieracium)\\b", taxon_name)) %>% nrow()
# 3031 wild native species without the apomictic genera Rubus and Hieracium

# this suggests the native flora is enriched by an additional 2689 species
# cultivated in gardens. That is 
2689 / 3512

# how many established non-natives are there in Germany
d %>% filter(native == "established non-native") %>% nrow
# compare that to the original file
rl <- read_excel("Data/02_Datentabelle_RL_Farn-_und_Bluetenpflanzen_2018_Deutschland_20210317-1607.xlsx")
rl %>% filter(Status == "N") %>% 
  filter(Arten == "Arten") %>% 
  nrow
# that's an overall reduction of about 25 species, this is likely due to
# matching the taxonomy to WCVP

# 361 non-native species are established in Germany (BfN estimates 470 established
# non-natives, important here to say that we only count species, and no trees)
165/361


# how the cultivation frequency is distributed across species -------------

d %>% filter(cultivated == "yes") %>% count(frequency)
# small correction
d %>% filter(cultivated == "yes") %>% filter(frequency == "c")
d <- d %>% mutate(frequency = ifelse(frequency == "c", "v", frequency))

# make these levels more meaningful
# Replace frequency letters with translations
d$frequency[d$frequency == "s"] <- "Rare"
d$frequency[d$frequency == "z"] <- "Scattered"
d$frequency[d$frequency == "v"] <- "Common"

d %>% 
  filter(cultivated == "yes") %>% 
  filter(native == "native") %>% 
  count(frequency) %>% 
  summarize(total= sum(n))

d %>% 
  filter(cultivated == "yes") %>% 
  filter(native == "native") %>% 
  count(frequency) %>% 
  mutate(n / 718)


# study the relationship between rl and cultivation -----------------------

dt <- d %>% filter(native == "native") %>% 
  mutate(cultivated = ifelse(is.na(cultivated) == T, 
                             "Not cultivated", "Cultivated")) %>% 
  select(-wild)

dt %>% group_by(cultivated) %>% count
dt_overview <- dt %>% group_by(cultivated) %>% count(threat_2018)

# define a vector of endangered categories
endangered_categories <- c("0", "1", "2", "3", "G")
not_endangered_categories <- c("V", "R", "*")

# create a new column indicating whether the category is endangered
dt_overview <- dt_overview %>%
  mutate(endangered = ifelse(threat_2018 %in% endangered_categories, 
                             "Endangered", 
                             ifelse(threat_2018 %in% not_endangered_categories,
                                    "Not endangered", "Data Deficient"))) %>% 
  filter(endangered != "Data Deficient")

custom_order <- c("0", "1", "2", "3", "G", "V", "R", "*")
dt_overview$threat_2018 <- factor(dt_overview$threat_2018, levels = custom_order)

# plot the raw data
(ggplot(dt_overview, aes(x = threat_2018, y = n, fill = endangered)) +
    geom_chicklet(width = 0.7,
                  position = ggplot2::position_stack(reverse = FALSE)) +
    labs(x = "Threat Categories", y = "Count", fill = "") +
    scale_fill_manual(values = c("Endangered" = "#fbcbe0", 
                                 "Not endangered" = "#e8e9eb")) +
    theme_ipsum(axis_text_size = 14, 
                axis_title_size = 14,
                strip_text_size = 14,
                grid = "Y",
                axis_title_just = "mm") +
    labs(x = "Red List categories", y = "Number of species") +
    facet_wrap(~ cultivated, ncol = 1) -> fig1a)



# proportions endangered / not endangered for cultivated yes / no
proportions <- dt_overview %>%
  group_by(cultivated, endangered) %>%
  summarize(total_count = sum(n)) %>%
  group_by(cultivated) %>%
  mutate(proportion = total_count / sum(total_count) * 100)
proportions

# visualize
custom_order <- c("Cultivated", "Not cultivated")
proportions$cultivated <- factor(proportions$cultivated, levels = custom_order)
unique_totals <- proportions %>%  group_by(cultivated) %>% 
  summarise(total = sum(total_count)) %>% mutate(endangered = "endangered")

(ggplot(proportions, aes(x = cultivated, y = proportion, fill = endangered)) +
    geom_chicklet(width = 0.7,
                  position = ggplot2::position_stack(reverse = FALSE)) +
    geom_text(aes(label = paste0(round(proportion, 1), "%")), 
              position = position_stack(vjust = 0.5),size = 5,
              family = "Arial Narrow", fontface = "italic") +
    geom_text(data = unique_totals, aes(label = paste0("n = ", total), y = 100), 
              vjust = -.1, size = 4, color = "black",
              family = "Arial Narrow", fontface = "italic") +
    scale_fill_manual(values = c("Endangered" = "#fbcbe0", 
                                 "Not endangered" = "#e8e9eb")) +
    theme_ipsum(axis_text_size = 14, grid = "",
                axis_title_just = "mm") +
    labs(x = "", y = "", fill = "") +
    scale_x_discrete(position = "top") +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(colour = "black")) -> fig1b)


# reshape proportions for fisher test
proportions_wide <- proportions %>% 
  select(-proportion) %>% 
  filter(endangered != "Data Deficient") %>% 
  pivot_wider(names_from = endangered, values_from = total_count) %>% 
  mutate(total = Endangered +  `Not endangered`)

# perform fisher's exact test
fisher.test(proportions_wide[, c("Endangered", "Not endangered")])

# do this per cultivation frequency
# calculate proportions
proportions <- dt %>%
  mutate(frequency = ifelse(
    cultivated == "yes" & is.na(frequency) == T,
    "Unknown",
    ifelse(cultivated == "Cultivated", frequency, "Not cultivated")
  )) %>%
  mutate(endangered = ifelse(
    threat_2018 %in% endangered_categories,
    "Endangered",
    ifelse(
      threat_2018 %in% not_endangered_categories,
      "Not endangered",
      "Data Deficient"
    )
  )) %>%
  group_by(frequency, endangered) %>%
  count() %>% 
  filter(endangered != "Data Deficient") %>%
  filter(frequency != "Unknown")

totals <- proportions %>% group_by(frequency) %>% summarize(total = sum(n))
proportions <- left_join(proportions, totals) %>% mutate(proportion = (n/total*100))
proportions

# visualize
custom_order <- c("Common", "Scattered", "Rare", "Not cultivated")
proportions$frequency <- factor(proportions$frequency, levels = custom_order)
unique_totals <- proportions[!duplicated(proportions$frequency), ]

(ggplot(proportions, aes(x = frequency, y = proportion, fill = endangered)) +
    geom_chicklet(width = 0.7,
                  position = ggplot2::position_stack(reverse = FALSE)) +
    geom_text(aes(label = paste0(round(proportion, 1), "%")), 
              position = position_stack(vjust = 0.5),size = 5,
              family = "Arial Narrow", fontface = "italic") +
    geom_text(data = unique_totals, aes(label = paste0("n = ", total), y = 100), 
              vjust = -.1, size = 4, color = "black",
              family = "Arial Narrow", fontface = "italic") +
    scale_fill_manual(values = c("Endangered" = "#fbcbe0", 
                                 "Not endangered" = "#e8e9eb")) +
    theme_ipsum(axis_text_size = 14, grid = "",
                axis_title_just = "mm") +
    labs(x = "", y = "", fill = "") +
    scale_x_discrete(position = "top") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "black")) -> fig1c)

# convert to wide format for pairwise fisher tests
proportions_wide <- proportions %>% 
  select(-proportion) %>% 
  pivot_wider(names_from = endangered, values_from = n) %>% 
  select(-total)

fisher.test(
  (proportions_wide %>%
    filter(frequency == "Common" |
             frequency == "Not cultivated"))[, c("Endangered", 
                                                 "Not endangered")])
fisher.test(
  (proportions_wide %>%
     filter(frequency == "Scattered" |
              frequency == "Not cultivated"))[, c("Endangered", 
                                                  "Not endangered")])
fisher.test(
  (proportions_wide %>%
     filter(frequency == "Rare" |
              frequency == "Not cultivated"))[, c("Endangered", 
                                                  "Not endangered")])

# create multi-panel plot
# Customize theme for each plot
fig1a <- fig1a + theme(plot.margin = margin(0, 0, 0, 0), 
                       plot.tag = element_text(face = "bold", family = "Arial Narrow"))
fig1b <- fig1b + scale_y_continuous(limits = c(0, 105)) +
  theme(plot.margin = margin(0, 0, 0, 0), 
                       plot.tag = element_text(face = "bold", family = "Arial Narrow"))
fig1c <- fig1c + scale_y_continuous(limits = c(0, 105)) +
  theme(plot.margin = margin(0, 0, 0, 0), 
                       plot.tag = element_text(face = "bold", family = "Arial Narrow"))

(fig1a) + (fig1b/fig1c) +
  plot_layout(guides = "collect") &
  plot_annotation(tag_levels = "a", 
                  title = "Cultivated plants are less likely to be endangered",
                  theme = theme(plot.title = element_text(size = 14, 
                                                          family = "Arial Narrow",
                                                          face = "bold"))) &
  theme(legend.position = "bottom", legend.text=element_text(size=14))

showtext_opts(dpi=600)
ggsave(width = 10, height = 7.5, bg = "white",
       file = "Figures/fig1.png",
       dpi = 600)
showtext_opts(dpi=96)


# population trend data ---------------------------------------------------

dt_trend <- dt %>% select(taxon_name,
                          cultivated,
                          frequency,
                          trend,
                          threat_1998,
                          threat_2018) %>% 
  mutate(frequency = ifelse(
    cultivated == "Cultivated" & is.na(frequency) == T,
    "Unknown",
    ifelse(is.na(frequency) == F, frequency, "Not cultivated")
  )) %>%
  mutate(endangered = ifelse(
    threat_2018 %in% endangered_categories,
    "Endangered",
    ifelse(
      threat_2018 %in% not_endangered_categories,
      "Not Endangered",
      "Data Deficient"
    )
  ))

# test long-term population trend data
unique(dt_trend$trend)

# long-term
# modify trend cats, since these are too many to easily comprehend
dt_trend$trend[dt_trend$trend == "(<)"] <- "<"
dt_trend1 <- dt_trend %>% filter(trend != "?") %>% filter(!is.na(trend))

# calculate totals and proportions for bar chart
proportions <- dt_trend1 %>% 
  group_by(frequency) %>% 
  count(trend) %>% 
  filter(frequency != "Unknown")

totals <- proportions %>% group_by(frequency) %>% summarize(total = sum(n))
proportions <- left_join(proportions, totals) %>% mutate(proportion = n/total*100)
proportions

custom_order <- c("Common", "Scattered", "Rare", "Not cultivated")
proportions$frequency <- factor(proportions$frequency, levels = custom_order)
unique_totals <- proportions[!duplicated(proportions$frequency), ]

# visualize
ggplot(proportions, aes(x = frequency, y = proportion, fill = trend)) +
  geom_chicklet(width = 0.7,
                position = ggplot2::position_stack(reverse = FALSE)) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 4,
            family = "Arial Narrow", fontface = "italic") +
  geom_text(data = unique_totals, aes(label = paste0("n = ", total), y = 100), 
            vjust = -.1, size = 4, color = "black",
            family = "Arial Narrow", fontface = "italic") +
  scale_fill_manual(values = c("<" = "#fbcbe0", 
                               "<<" = "#fab3d1",
                               "<<<" = "#f89cc3",
                               "=" = "#e8e9eb",
                               ">" = "#adffe4")) +
  theme_ipsum(axis_text_size = 14, grid = "", plot_title_size = 14,
              axis_title_just = "mm") +
  labs(x = "", y = "") +
  scale_x_discrete(position = "top") +
  theme(legend.position = "top",
        legend.box = "horizontal",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,-20,-5,-15),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "black")) +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5)) +
  labs(fill = "Long-term population trend", 
       title = "Less negative population trends for cultivated plants")

showtext_opts(dpi=600)
ggsave(width = 8, height = 6, bg = "white",
       file = "Figures/fig2.png",
       dpi = 600)
showtext_opts(dpi=96)

# fisher tests
proportions$trend[proportions$trend == "<"] <- "decline" 
proportions$trend[proportions$trend == "<<"] <- "decline" 
proportions$trend[proportions$trend == "<<<"] <- "decline" 

decline <- proportions %>% 
  filter(trend == "decline") %>% 
  group_by(frequency, trend) %>% 
  summarise(n= sum(n), total = first(total)) %>% 
  mutate(perc = n/total) %>% 
  filter(frequency == "Scattered" | frequency == "Not cultivated") %>% 
  mutate(notn = total -n)
fisher.test(decline[, c("n", "notn")])

stable <- proportions %>% 
  filter(trend == "=") %>% 
  filter(frequency == "Common" | frequency == "Not cultivated") %>% 
  mutate(notn = total -n)
fisher.test(stable[, c("n", "notn")])

increase <- proportions %>% filter(trend == ">") %>% 
  filter(frequency == "Common" | frequency == "Not cultivated") %>% 
  mutate(notn = total -n)
fisher.test(increase[, c("n", "notn")])



# do cultivated species even transition threat cats -----------------------

# use the transitions from 1998 to 2018 in rl cat to
# visualize the proportion of improvements for plants
# that are cultivated and their frequency vs those not cultivated
  
# map threat levels to numeric values
threat_levels <- c("0", "1", "2", "3", "G", "R", "V", "*")

dt_trend2 <- dt_trend %>%
  mutate(threat_1998_num = match(threat_1998, threat_levels),
         threat_2018_num = match(threat_2018, threat_levels)) %>% 
  mutate(change = ifelse(threat_2018_num > threat_1998_num, "Improvement",
                         ifelse(threat_1998_num == threat_2018_num, "Stable", 
                                "Decline")
  )) 

# count species that change threat categories
changes <- dt_trend2 %>%
  group_by(threat_1998, threat_2018) %>%
  tally() %>%
  mutate(threat_1998_num = match(threat_1998, threat_levels),
         threat_2018_num = match(threat_2018, threat_levels)) %>%
  filter(!is.na(threat_1998_num)) %>%
  filter(!is.na(threat_2018_num)) %>% 
  filter(!threat_1998 == threat_2018)
changes

(ggplot(changes, aes(x = factor(threat_1998, levels = threat_levels),
                    y = factor(threat_2018, levels = threat_levels))) +
  geom_point(aes(col = ifelse(threat_2018_num > threat_1998_num,
                              "Improvement", 
                              ifelse(threat_2018_num == threat_1998_num,
                                     "Stable", "Decline")), size = n),
             alpha = 0.7) +
  geom_text(aes(label = n),
            color = "black", size = 3, fontface = "bold", alpha = 0.8) +
  scale_colour_manual(values = c("Decline" = "#fbcbe0", 
                               "Improvement" = "#adffe4"), 
                      name = "Red List transition") +
  scale_size_continuous(range = c(5, 15)) + 
  labs(x = "Red List category 1998", y = "Red List category 2018") +
  theme_ipsum(axis_text_size = 14, grid = "", axis_title_size = 14,
              axis_title_just = "mm") +
    theme(legend.position = "top", 
          legend.box = "horizontal", 
          legend.text=element_text(size=14),
          legend.title = element_text(size = 14, family = "Arial Narrow")) +
    guides(size = "none",
           colour = guide_legend(title.position="top", title.hjust = 0.5)) -> fig3a)


# what is the transition likelihood per cultivation frequency
# calculate totals and proportions for bar chart
proportions <- dt_trend2 %>% 
  filter(change != "Stable") %>% 
  group_by(frequency) %>% 
  count(change) %>% 
  filter(!is.na(change)) %>% 
  filter(frequency != "Unknown")
totals <- proportions %>% group_by(frequency) %>% summarize(total = sum(n))
proportions <- left_join(proportions, totals) %>% mutate(proportion = n/total*100)
proportions

custom_order <- c("Common", "Scattered", "Rare", "Not cultivated")
proportions$frequency <- factor(proportions$frequency, levels = custom_order)
custom_order <- c("Decline", "Stable", "Improvement")
proportions$change <- factor(proportions$change, levels = custom_order)
unique_totals <- proportions[!duplicated(proportions$frequency), ]

(ggplot(proportions, aes(x = frequency, y = proportion, fill = change)) +
  geom_chicklet(width = 0.7,
                position = ggplot2::position_stack(reverse = FALSE)) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 4,
            family = "Arial Narrow", fontface = "italic") +
  geom_text(data = unique_totals, aes(label = paste0("n = ", total), y = 100), 
            vjust = -.1, size = 4, color = "black",
            family = "Arial Narrow", fontface = "italic") +
  scale_fill_manual(values = c("Decline" = "#fbcbe0", 
                               "Stable" = "#e8e9eb",
                               "Improvement" = "#adffe4")) +
  theme_ipsum(axis_text_size = 14, grid = "", plot_title_size = 14,
              axis_title_just = "mm") +
  scale_y_continuous(limits = c(0, 105)) +
  labs(x = "", y = "") +
  scale_x_discrete(position = "top") +
  theme(legend.position = "top",
        legend.box = "horizontal",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,-20,-5,-15),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "black")) +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5)) +
    theme(legend.position = "none") -> fig3b)


# customize theme for each plot
fig3a <- fig3a + theme(plot.margin = margin(0, 0, 0, 0), 
                       plot.tag = element_text(face = "bold", family = "Arial Narrow"))
fig3b <- fig3b + theme(plot.margin = margin(0, 0, 0, 0), 
                       plot.tag = element_text(face = "bold", family = "Arial Narrow"))
(fig3a) / (fig3b) +
  plot_annotation(tag_levels = "a", 
                  title = "Commonly cultivated plants more often witness improvements in Red List category",
                  theme = theme(plot.title = element_text(size = 14, 
                                                          family = "Arial Narrow",
                                                          face = "bold")))

# save plot
showtext_opts(dpi=600)
ggsave(width = 8.2, height = 10, bg = "white",
       file = "Figures/fig3.png",
       dpi = 600)
showtext_opts(dpi=96)


# test for differences
test_frame <- proportions %>% 
  select(-proportion) %>% 
  pivot_wider(names_from = change, values_from = n) %>% 
  filter(frequency == "Common" | frequency == "Not cultivated")
  
fisher.test(test_frame[, c("Decline", "Improvement")])



# some filtering for disucssion -------------------------------------------

# examples of commonly cultivated species endangered in 2018
dt_trend2 %>% filter(endangered == "Endangered") %>% 
  filter(frequency == "Common") %>% View()


dt_trend2 %>% filter(change == "Decline") %>% 
  filter(frequency == "Common") %>% View()

