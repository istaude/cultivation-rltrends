source("Rcode/00-preamble.R")

# read html ---------------------------------------------------------------
# pdf was converted to html file using pdf24
# read html
html <- read_html("Data/978-3-662-50420-8_4.html")

# to text
text <- html_text(html)

# extract vernacular bold name --------------------------------------------

# extract the bold text (vernacular names)
bold_text <- html %>%
  html_nodes("b") %>%
  html_text()

# remove numeric
bold_text <- bold_text[!grepl("\\d", bold_text)]

# trim all white space
bold_text <- trimws(bold_text)

# make table
vector <- bold_text
species <- NULL
column1 <- NULL
column2 <- NULL
column3 <- NULL
column4 <- NULL
# loop through the vector
for (i in 2:length(vector)) {
  species[i] <- vector[i]
  column2[i] <- ifelse(vector[i-1] == "Z" | vector[i-1] == "N", vector[i-1], NA)
  # some where also Nutzfplanzen historically
  column3[i] <- ifelse(vector[i-2] == "Z" | vector[i-2] == "N", vector[i-2], NA)
  # indicate whether Wildpflanze
  column4[i] <- ifelse(vector[i-1] == "W" | vector[i-2] == "W", "W", NA)
}

df <- data.frame(species, column2, column3, column4)
df <- df %>% filter(!is.na(column2))
# remove rows that are not species. 
df <- df %>% filter(nchar(species) > 3)
# NA in column3 if same as column2
df <- df %>% mutate(column3 =ifelse(column3 == column2, NA, column3))
# remove the minus sign
df$species <- str_replace(df$species, "\\s+–$", "")



# extract scientific names ------------------------------------------------

# vernacular name vector
species_vector <- df$species

# initialize an empty data frame to store the extracted information
matched_content <- NULL
# loop through the species vector
for (k in species_vector) {
  # define the regular expression pattern to match the entire line
  # containing the species name in the HTML content
  pattern <- paste0("(?i)\\Q", k, "\\E\\s*(.{1,40})")
  # (.{1,40}) captures the next 1 to 40 characters after the minus sign
  
  # str_match to find the first occurrence of the search item and 
  # the next 40 characters
  matched_content[k] <- str_match(text, pattern)[, 2]
 
}

science_names <- data.frame(vernacular_names = names(matched_content), 
                 species = unname(matched_content))

# cleaning
# keep before the "-", paste to vernacular name.
science_names$vernacular_names2 <-
  paste0(science_names$vernacular_names,
         gsub("\\s*–.*", "", science_names$species))

# remove space and text after "–" in (species)
science_names$species2 <- gsub("^[^–]*–\\s*", "", science_names$species)

# define regex
pattern <- "–\\s*(?:([A-Za-z]+\\s+×\\s+[A-Za-z]+)|(\\S+\\s+\\S+))"

# extract desired word
science_names$s3 <- gsub(pattern, "\\1\\2", science_names$species2)

# regex to match the undesired content
pattern <- "(.*?)(\\d|:|Ä).*"

# str_replace to remove the undesired content and keep 
# everything before the first numeric, colon, or "Ä"
science_names$s4 <- str_replace(science_names$s3, pattern, "\\1")

# define a regular expression pattern to match everything before the 
# first capital letter
pattern <- ".*?(?=\\p{Lu})"

# remove everything before the first capital letter
science_names$s5 <- str_replace(science_names$s4, pattern, "")

# remove genera (indicated if second word is S.)
science_names <- science_names %>% filter(!grepl("^\\S+\\s+S\\.", s5)) 
science_names <- science_names %>% filter(nchar(str_extract(s5, "^\\p{L}+")) < 4)

# filter rows with var. convar.
science_names <- science_names %>% 
  filter(!grepl("^var\\.\\s+", s3)) %>% 
  filter(!grepl("^convar\\.\\s+", s3)) %>% 
  filter(vernacular_names != "Zier-K.")

# use flora to remove author names
science_names$s6 <- NULL
for (i in 1:nrow(science_names)){
  science_names$s6[i] <- flora::remove.authors(science_names$s5[i])
}

# manual cleaning
write_excel_csv2(science_names, "Data/rothmaler_extraction_species.csv")


# load corrected file -----------------------------------------------------

science_names_corrected <-
  read_delim(
    "Data/rothmaler_extraction_species_correctedJULY2023.csv",
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )


# join cultivation use with vernacular name and corrected species  --------
df_scientific <- left_join(science_names_corrected %>% 
                             select(vernacular_names, vernacular_names2, s6),
                           df, by = c("vernacular_names" = "species"))

# there is the problem that there may be several times the same
# German vernacular name, such as: Gewöhnlicher W.  the code extracts only
# the scientific name for the first occurrence. It would be possible for all
# those species that occur more than once, to extract all their scientific 
# names, or do this also manually. but it may also be that some scientific names
# are double because they have a different genus, and this may not be obvious, as
# genera are abbreviated. So I do this manually.
df_scientific %>% count(s6) %>% filter(n > 1)


# assign genus name -------------------------------------------------------

# use the bold text vector to sort species vernacular names by their
# German genus name.

# remove one letter words
genus_vector <- bold_text[nchar(bold_text) >= 4]
genus_vector <- data.frame(species = genus_vector)

# manual cleaning
write_excel_csv2(genus_vector, "Data/genus_vector.csv")

# based on the bold text extraction, I made this table manually.
genus_vector <- read_delim("genus_vector.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

genus_vector <- genus_vector %>% 
  select(Vernacular_name = genus, species) %>% 
  na.omit()

genus_vector$Vernacular_name <- trimws(gsub(" –", "", genus_vector$Vernacular_name))


# get genus scientific name -----------------------------------------------

# define regex
pattern <- "(.*?) (\\d+) Arten"

# extract the lines that match the pattern
matched_lines <- str_extract_all(text, pattern) %>%
  unlist() 

# extract genus names
genus_sc_names <- tibble(
  Vernacular_name = str_remove(matched_lines, " – .*"),
  Column2 = str_remove(matched_lines, ".* – ")
) %>% 
  separate(Column2, into = c("Genus", "Author"), 
           sep = " ", extra = "drop", fill = "right")

# remove families
genus_sc_names <- genus_sc_names %>%
  filter(!str_detect(Genus, "aceae"))


# join genera -------------------------------------------------------------

genus <- full_join(genus_vector %>% select(Vernacular_name, species),
          genus_sc_names)

# write and then manually correct
write_excel_csv2(genus, "Data/genus_vector_scientific.csv")



# read corrected file -----------------------------------------------------

genus_vector_scientific_corrected <-
  read_delim(
    "Data/genus_vector_scientific_corrected.csv",
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )

genus_vector_scientific_corrected$Species <-
  trimws(gsub(" –", "", genus_vector_scientific_corrected$Species))

# join with species dataframe ---------------------------------------------

genus_species_sientific <- full_join(genus_vector_scientific_corrected,
                                     df_scientific, 
          by = c( "Species" = "vernacular_names" ))

# write and then manually correct
write_excel_csv2(genus_species_sientific, "Data/genus_species_sientific.csv")




# read corrected file -----------------------------------------------------

genus_species_sientific_corrected <-
  read_delim(
    "Data/genus_species_sientific_corrected.csv",
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )


# text mine frequency info -----------------------------------------------

# remove all newline characters (\n)
clean_text <- gsub("\n", "", text)

science_names$lookup <- paste(science_names$vernacular_names, science_names$species)

k <- NULL
for (i in 1:nrow(science_names)){
  input <- qdapRegex::ex_between(clean_text, right = science_names$lookup[i+1], 
                                 left = science_names$lookup[i])[[1]]
  match_pattern <- str_extract(input, "(Z|N) [zsv]")
  k[[i]] <- data.frame(species = science_names$species[i+1],
                       lookup = science_names$lookup[i+1],
                       match = match_pattern)
  print(k[[i]])
}
k
length(k)
frequency <- bind_rows(k)

# manually fill and correct
write_excel_csv2(frequency, "Data/frequency.csv")


# load corrected version
frequency <- read_delim("Data/frequency.csv", 
                             delim = ";", 
                             escape_double = FALSE, 
                             trim_ws = TRUE)



# bring together gardening frequency data with the cleaned data -----------

# so that you can join it to "genus_species_sientific_corrected"
d <- left_join(
  genus_species_sientific_corrected,
  left_join(frequency, science_names_corrected) %>% select(
    species_scientific = s6,
    species_vernacular = vernacular_names2,
    match,
    lookup
  )
) %>% arrange(genus_scientific)

# tough ship, manually correct again.
write_excel_csv2(d, "Data/rothmaler_complete.csv")
