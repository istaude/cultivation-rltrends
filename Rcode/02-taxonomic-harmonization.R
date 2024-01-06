source("Rcode/00-preamble.R")

# load german red list and select relevant cols ---------------------------

rl <-
  read_excel(
    "Data/02_Datentabelle_RL_Farn-_und_Bluetenpflanzen_2018_Deutschland_20210317-1607.xlsx"
  )

rl <- rl %>% select(
  species = Name,
  type = Arten,
  native = Status,
  range = `aktuelle Bestandssituation`,
  trend = `langfristiger Bestandstrend`,
  threat_2018 = `RL Kat.`,
  threat_1998 = `alte RL- Kat.`
)

# some cleaning -----------------------------------------------------------

# only species
unique(rl$type)
rl %>% count(type)
rl <- rl %>% filter(!is.na(type))

# remove author name, using package flora
species_without <- NULL
for (i in 1:nrow(rl)) {
  species_without[i] <- flora::remove.authors(rl$species[i])
}

rl$species_without <- species_without

# change native cats
unique(rl$native)
rl %>% filter(native == "F") %>% View

rl <- rl %>% mutate(native = ifelse(
  native == "I",
  "native",
  ifelse(
    native == "N",
    "established non-native",
    ifelse(native == "U", "transient", "doubtful")
  )
))

rl <- rl %>% select(-type)


# load rothmaler data -----------------------------------------------------

rd <- read_delim("Data/rothmaler_complete_corrected.csv", 
                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
# sort columns
rd <- rd %>% select(genus_scientific,
              genus_vernacular,
              species_scientific,
              species_vernacular,
              frequency,
              primary_use,
              secondary_use,
              wild)

# modify frequency column
rd %>% filter(is.na(frequency)) %>% View

# only keep lower case letters
rd$frequency <- gsub("[^a-z\\s]", "", rd$frequency)
rd$frequency <- trimws(rd$frequency)

# change primary use column to yes Z, so that the second column is N only
rd <- rd %>%
  mutate(primary_use = ifelse(
    primary_use == "Z",
    "ornamental",
    ifelse(is.na(primary_use) == TRUE, NA, "crop")
  )) %>%
  mutate(secondary_use = ifelse(
    secondary_use == "Z",
    "ornamental",
    ifelse(is.na(secondary_use) == TRUE, NA, "crop")
  )) %>%
  mutate(
    ornamental = ifelse(primary_use == "ornamental" | 
                          secondary_use == "ornamental",
      "ornamental",
      NA
    ),
    crop = ifelse(primary_use == "crop" | 
                    secondary_use == "crop",
                  "crop", NA
    )
  ) %>% select(-primary_use, - secondary_use)


# now paste the species name
rd$species_scientific <- sub("^[^.]+\\.", "", rd$species_scientific)
rd$species_scientific <- trimws(rd$species_scientific)
rd$species_scientific <- paste(rd$genus_scientific, rd$species_scientific)

# remove f.
rd$species_scientific <- gsub("\\s*f\\.$", "", rd$species_scientific)


# harmonize red list with rothmaler ---------------------------------------

# perform other check with rwcvp for rl
wcvp_match_rl <- wcvp_match_names(rl, 
                                  name_col = "species_without",
                                  fuzzy = T)

wcvp_acc_rl <-
  wcvp_match_rl %>% select(species_without, wcvp_status, wcvp_accepted_id)

# give preference to accepted (and then synonyms) names, 
# if there are multiple rows for one and 
# the same species
wcvp_acc_rl <- wcvp_acc_rl %>% 
  arrange(desc(wcvp_status %in% c("Accepted", "Synonym")), 
          desc(wcvp_status == "Accepted")) %>%
  distinct(species_without, .keep_all = TRUE)

names_rl <- wcvp_names %>% select(wcvp_accepted_id = plant_name_id, taxon_name)
wcvp_acc_rl <- left_join(wcvp_acc_rl, names_rl) 
nrow(rl)
nrow(wcvp_acc_rl)

rl_wcvp <- left_join(rl, wcvp_acc_rl)


# now for rothmaler
wcvp_match_rd <- wcvp_match_names(rd, 
                                  name_col = "species_scientific",
                                  fuzzy = TRUE)

wcvp_acc_rd <- wcvp_match_rd %>% select(species_scientific, wcvp_status, wcvp_accepted_id)

# give preference to accepted (and then synonyms) names, 
# if there are multiple rows for one and 
# the same species
wcvp_acc_rd <- wcvp_acc_rd %>% 
  arrange(desc(wcvp_status %in% c("Accepted", "Synonym")), 
          desc(wcvp_status == "Accepted")) %>%
  distinct(species_scientific, .keep_all = TRUE)

names_rd <- wcvp_names %>% select(wcvp_accepted_id = plant_name_id, taxon_name)
wcvp_acc_rd <- left_join(wcvp_acc_rd, names_rd) 
nrow(rd)
nrow(wcvp_acc_rd)

rd_wcvp <- left_join(rd, wcvp_acc_rd)
rd_wcvp <- rd_wcvp %>% mutate(cultivated = "yes")

# join them together, but first remove unplaced names
rd_wcvp %>% filter(wcvp_status == "Unplaced") %>% nrow
rd_wcvp <- rd_wcvp %>% filter(wcvp_status != "Unplaced")

rl_wcvp %>% filter(wcvp_status == "Unplaced") %>% nrow
rl_wcvp <- rl_wcvp %>% filter(wcvp_status != "Unplaced")

# now there are again some duplicates, this time from true synonyms in the rl
rl_wcvp %>%
  group_by(taxon_name) %>% 
  count() %>% 
  arrange(desc(n))

rl_wcvp <- rl_wcvp %>% 
  arrange(desc(wcvp_status %in% c("Accepted", "Synonym")), 
          desc(wcvp_status == "Accepted")) %>%
  distinct(taxon_name, .keep_all = TRUE)

# same for rothmaler
rd_wcvp %>% distinct %>% 
  group_by(taxon_name) %>% 
  count() %>% 
  arrange(desc(n))

rd_wcvp <- rd_wcvp %>% distinct %>% 
  arrange(desc(wcvp_status %in% c("Accepted", "Synonym")), 
          desc(wcvp_status == "Accepted")) %>%
  distinct(taxon_name, .keep_all = TRUE)


d <- full_join(rd_wcvp %>% select(taxon_name, frequency, ornamental, crop, wild) %>% 
                 mutate(cultivated = "yes"), 
          rl_wcvp %>% select(taxon_name, native, range, trend, 
                             threat_2018, threat_1998))

# remove all non-herbaceous plants ----------------------------------------

# Rothmaler data only for herbaceous seed plants, thus any RL
# species that is a fern, tree or a shrub is listed as non-cultivated in this analysis
tree <- read_excel("Data/TRY_Categorical_Traits_Lookup_Table_2012_03_17_TestRelease.xlsx")
tree <- tree %>% select(AccSpeciesName, PlantGrowthForm) 

# match with wcvp
tree_wcvp <- wcvp_match_names(tree, 
                 name_col = "AccSpeciesName",
                 fuzzy = F)
# select
tree_wcvp <- tree_wcvp %>% select(AccSpeciesName, wcvp_status, wcvp_accepted_id) %>% 
  arrange(desc(wcvp_status %in% c("Accepted", "Synonym")), 
          desc(wcvp_status == "Accepted")) %>%
  distinct(AccSpeciesName, .keep_all = TRUE)

# find right species name and join back
names <- wcvp_names %>% select(wcvp_accepted_id = plant_name_id, taxon_name)
tree_wcvp <- left_join(tree_wcvp, names) 
tree_wcvp <- left_join(tree, tree_wcvp)

tree_wcvp <- tree_wcvp %>% select(taxon_name, PlantGrowthForm) %>% distinct
tree_wcvp %>% group_by(taxon_name) %>% count() %>% arrange(desc(n))
# clean
tree_wcvp <- tree_wcvp %>% na.omit() %>% group_by(taxon_name) %>% 
  summarise(PlantGrowthForm = first(PlantGrowthForm))

# merge with d
d <- left_join(d, tree_wcvp)
d %>% group_by(cultivated, PlantGrowthForm) %>% count()

# overlap with rl? 
# native
(overlap <- left_join(rl_wcvp, tree_wcvp) %>% filter(native == "native") %>% 
  group_by(PlantGrowthForm) %>% count)
#all
(overlap <- left_join(rl_wcvp, tree_wcvp) %>% 
    group_by(PlantGrowthForm) %>% count)

1 - 1298/sum(overlap$n)

# it seems fair to exclude the category tree and shrub/tree for all
d <- d %>% 
  filter((PlantGrowthForm != "tree") %>% replace_na(TRUE)) %>% 
  filter((PlantGrowthForm != "shrub/tree") %>% replace_na(TRUE))
nrow(d)

# save
write_excel_csv2(d, "Data/redlist_rothmaler_t.csv")

