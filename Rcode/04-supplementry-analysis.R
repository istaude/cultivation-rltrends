source("Rcode/00-preamble.R")


# load data ---------------------------------------------------------------
# data from sMon occupancy trend data over 60yrs in Germany,
# only include the trends they include
eichenberg <- read_excel("Data/eichenberg.xlsx") %>% filter(`Included for trend analysis` == 1)

# my dataset
d <- read_delim("Data/redlist_rothmaler_t.csv", 
                delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  filter(native == "native") %>% 
  mutate(cultivated = ifelse(is.na(cultivated) == T, 
                             "Non-cultivated", "Cultivated")) %>% 
  select(-wild)
d$frequency[d$frequency == "s"] <- "Rare"
d$frequency[d$frequency == "z"] <- "Scattered"
d$frequency[d$frequency == "v"] <- "Common"

# join --------------------------------------------------------------------
dt <- left_join(d, eichenberg %>% rename(taxon_name = TaxonName))

dt <- dt %>% select(taxon_name, frequency, cultivated, `t3-t1`) 
dt <- dt %>% mutate(frequency = ifelse(cultivated == "Non-cultivated", "Non-cultivated", frequency))

dt$frequency <- factor(dt$frequency, levels = c("Common", "Scattered", "Rare", "Non-cultivated"))


# viz ---------------------------------------------------------------------
# calculate sample size for each 'frequency'
dt <- dt %>% na.omit()

statistics <- dt %>%
  group_by(frequency) %>%
  summarize(n = n(), mean = mean(`t3-t1`)) %>%
  ungroup()

# plot
ggplot(dt, aes(x = frequency, y = `t3-t1`)) +
  geom_boxplot2(width = 0.5, width.errorbar = 0) +
  #geom_jitter(width =0.1, alpha = 0.4, col = "grey") +
  geom_point(data = statistics, aes(x = frequency, y = mean), colour="magenta", 
              size=3, show.legend=FALSE) + 
  geom_line(data = statistics, aes(x = frequency, y = mean, group = 1), 
            colour = "magenta", size = 1) + 
  geom_text(data = statistics, aes(x = frequency, y = Inf, label = n), 
            position = position_dodge(width = 0.8), vjust = -0.5) +
  theme_ipsum(axis_text_size = 14, 
              axis_title_size = 14,
              strip_text_size = 14,
              grid = "Y",
              axis_title_just = "mm") +
  labs(x = "Cultivation frequency", 
       y = "Percentage change in occupancy across Germany\n  1997–2017 vs. 1960–1987") 

# apply the Yeo-Johnson transformation
dt$transformed <- predict(preProcess(dt[, "t3-t1", drop = FALSE], 
                                     method = "YeoJohnson"), 
                          dt[, "t3-t1", drop = FALSE]) %>% pull


(ggplot(dt, aes(x = frequency, y = transformed)) +
  geom_jitter(width =0.1, alpha = 0.1, col = "blue") +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha =0.4) +
  stat_summary(fun=mean, colour="magenta", geom="point", 
               size=3, show.legend=FALSE) +
    geom_text(data = statistics, aes(x = frequency, y = -230, label = paste("n =", n)), 
              position = position_dodge(width = 0.8), vjust = -0.5, family = "Arial Narrow",
              fontface = 3) +
  theme_ipsum(axis_text_size = 14, 
              axis_title_size = 14,
              strip_text_size = 14,
              grid = "Y",
              axis_title_just = "mm") +
  labs(x = "Cultivation frequency", 
       y = "Percentage change in occupancy\n between 1960 and 2017\n (Yeo-Johnson transformed)") -> figsupp1a)


# model -------------------------------------------------------------------
mod_supp <- lm(transformed ~ frequency, dt)
plot(mod_supp)


(confint(pairs(emmeans(mod_supp, "frequency"))) %>%  as.data.frame() %>% 
    filter(str_detect(contrast, "(Non-cultivated)")) %>% # only comps to ref level non-cultivated
  mutate(contrast = factor(contrast, levels = c("Common - (Non-cultivated)",
                                                "Scattered - (Non-cultivated)",
                                                "Rare - (Non-cultivated)"))) %>% 
  ggplot(aes(x = estimate, y = contrast, xmin = lower.CL, xmax = upper.CL)) +
  geom_errorbar(width = 0.1, col = "blue")+
  geom_point(col = "magenta", size = 4) +
  geom_vline(xintercept = 1, linetype = "dotted", color = "darkgrey") +
  scale_y_discrete(limits=rev, position = "right") +
  theme_ipsum(axis_text_size = 14, 
              axis_title_size = 14,
              strip_text_size = 14,
              grid = "Y",
              axis_title_just = "mm") +
  labs(y = "", x = "Difference in percentage occupancy change")  -> figsupp1b)



# multiplot ---------------------------------------------------------------


figsupp1a <- figsupp1a + theme(plot.margin = margin(0, 0, 0, 0), 
                       plot.tag = element_text(face = "bold", family = "Arial Narrow"))
figsupp1b <- figsupp1b + theme(plot.margin = margin(0, 0, 0, 0), 
                       plot.tag = element_text(face = "bold", family = "Arial Narrow"))

(figsupp1a) / (figsupp1b) +
  plot_annotation(tag_levels = "a",
                  theme = theme(plot.title = element_text(size = 14, 
                                                          family = "Arial Narrow",
                                                          face = "bold")))


# save plot
showtext_opts(dpi=600)
ggsave(width = 7.8, height = 7, bg = "white",
       file = "Figures/figsupp.png",
       dpi = 600)
showtext_opts(dpi=96)

