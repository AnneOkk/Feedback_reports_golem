## code to prepare `df` dataset 

library(haven)
library(rlang)
library(tidyverse)
library(foreign)
library(dplyr)
library(naniar)
library(stringr)
library(sjlabelled)
library(zoo)
library(lubridate)
library(sf)
library(labelled)


# Read in data ------------------------------------------------------------
read_data <- function(rel_directory, pattern) {
  files <- dir(paste0(dirname(getwd()),"/", rel_directory), pattern = pattern, full.names = FALSE) 
  df_list <- vector("list", length(files))
  for (fname in files) {
      df_list[[fname]] <- haven::read_sav(paste0(dirname(getwd()),"/", rel_directory ,fname))
    }
    names(df_list) <- paste0("", gsub(pattern,"",names(df_list)))
    return(df_list)
}


# Get full df -------------------------------------------------------------
transform_to_one_df <- function(df_list) {
  Events_T1 <- df_list[grep("EventT1", names(df_list))] %>% bind_rows() %>% as.data.frame(.) %>% 
    transform(., newcol=paste(t1code_1, t1code_2, t1code_3, sep="_"), stringsAsFactors = FALSE) %>% 
    replace_with_na(replace = list(newcol = "__")) %>% 
    mutate(., newcol = tolower(newcol))
  Events_T2 <- df_list[grep("EventT2", names(df_list))] %>% bind_rows() %>% as.data.frame(.) %>% 
    transform(., newcol=paste(t2code_1, t2code_2, t2code_3, sep="_"), stringsAsFactors = FALSE) %>% 
    replace_with_na(replace = list(newcol = "__")) %>% 
    mutate(., newcol = tolower(newcol))
  Events_T3 <- df_list[grep("EventT3", names(df_list))] %>% bind_rows() %>% as.data.frame(.) %>% 
    transform(., newcol=paste(t3code_1, t3code_2, t3code_3, sep="_"), stringsAsFactors = FALSE) %>% 
    replace_with_na(replace = list(newcol = "__")) %>% 
    mutate(., newcol = tolower(newcol))
  
  T12 = left_join(Events_T1, Events_T2, by = "newcol")
  T123= left_join(T12, Events_T3, by = "newcol") %>%
    # remove all rows that have no event reported 
    subset(., t1event == 1) %>%
    # remove all rows that have duplicated newcol
    .[!duplicated(.$newcol), ] %>%
    # remove all rows that have no novelty, disruptiveness, performance impact reported for event 
     filter_at(vars(matches("novel|disrup|perfo")) ,any_vars(!is.na(.))) %>% 
    # remove if no event happened based on texts
    filter_at(., vars(starts_with("t1evdes")), all_vars(!(. %in% c('hello','nothing','b', 'jhkjhkjhkjh', 'fbzfDB', 'ffff', 'TESTOMAT', '-'))))
  return(T123)
}
  
# Rename columns -------------------------------------------------------------
rename_cols <- function(df){
  df$T1Severity_1 <- df %>% dplyr::select(t1novel_1, t1disrup_1, t1perfo_1) %>% rowMeans(.)
  df$T1Severity_2 <- df %>% dplyr::select(t1novel_2, t1disrup_2, t1perfo_2) %>% rowMeans(.)
  df$T1Severity_3 <- df %>% dplyr::select(t1novel_3, t1disrup_3, t1perfo_3) %>% rowMeans(.)
  df$T1Severity_4 <- df %>% dplyr::select(t1novel_4, t1disrup_4, t1perfo_4) %>% rowMeans(.)
  df$T1Severity_5 <- df %>% dplyr::select(t1novel_5, t1disrup_5, t1perfo_5) %>% rowMeans(.)
  df$T1Severity_6 <- df %>% dplyr::select(t1novel_6, t1disrup_6, t1perfo_6) %>% rowMeans(.)
  df$T1Severity_7 <- df %>% dplyr::select(t1novel_7, t1disrup_7, t1perfo_7) %>% rowMeans(.)
  df$T1Severity_8 <- df %>% dplyr::select(t1novel_8, t1disrup_8, t1perfo_8) %>% rowMeans(.)
  # remove if NA on all severity columns
  df <- df[rowSums(is.na(df[, grep("Severity", names(df))]))  < 8, ]
  # get event with maximum severity 
  maxsev <- df %>% dplyr::select(matches("Severity")) %>% data.frame(.) %>% apply(.,1,which.max) 
  df <- cbind(df, maxsev)
  # select data events 
  T123_event_stuff <- df %>% dplyr::select(matches("email|maxsev|event|evcat|evdes|novel|disrup|perfo|cope|threat|emotionslast|threatlast|probsolv"))
  T123_event_stuff_no <- df %>% dplyr::select(!matches("maxsev|event|evcat|evdes|novel|disrup|perfo|cope|threat|emotionslast|threatlast|probsolv"))
  # change column names: threatlast, redundant '_'
  names(T123_event_stuff)  <- gsub("(^.*)(threatlast)([a-z]{3})(_[1-9]{1})(_1)", paste0("\\1", "\\2", "\\3", "\\4"), names(T123_event_stuff)) %>%
    gsub("_1_", "_", fixed = T, perl=F, .) %>% 
    gsub("_1_", "1_", fixed = T, perl=F, .) %>%
    gsub("_2_", "2_", fixed = T, perl=F, .) %>%
    
    # change event type names 
    gsub("(^.*)fin(_[0-9]+$)", paste0("\\1","\\2", "_1"), .) %>% 
    gsub("(^.*)cli(_[0-9]+$)", paste0("\\1","\\2", "_2"), .) %>%
    gsub("(^.*)bet(_[0-9]+$)", paste0("\\1","\\2", "_3"), .) %>% 
    gsub("(^.*)leg(_[0-9]+$)", paste0("\\1","\\2", "_4"), .) %>% 
    gsub("(^.*)abs(_[0-9]+$)", paste0("\\1","\\2", "_5"), .) %>% 
    gsub("(^.*)mat(_[0-9]+$)", paste0("\\1","\\2", "_6"), .) %>%
    gsub("(^.*)mis(_[0-9]+$)", paste0("\\1","\\2", "_7"), .) %>%
    gsub("(^.*)oth(_[0-9]+$)", paste0("\\1","\\2", "_8"), .) %>%
    
    gsub("(t[1-3]{1})(emotionslast)_([1-9]+)(_[0-9]{1})", paste0("\\1", "\\2", "\\3", "\\4"), fixed = F, perl=F, .) %>%
    gsub("(t[1-3]{1})(threatlast)_([1-9]+)(_[0-9]{1})", paste0("\\1", "\\2", "\\3", "\\4"), fixed = F, perl=F, .) %>%
    gsub("(t[1-3]{1})(threat)_([1-9]+)(_[0-9]{1})", paste0("\\1", "\\2", "\\3", "\\4"), fixed = F, perl=F, .)
  
  T123_event_stuff <- T123_event_stuff %>%
    pivot_longer(cols = -maxsev, names_to = c(".value", "index"), names_pattern = "(t[1-9]{1}[a-z]+._)(\\d$)") %>%
    filter(str_extract(as.character(maxsev), "\\d$") == index)
  
  T123 <- cbind(T123_event_stuff_no, T123_event_stuff)
  return(T123)
}

# Country labels --------------------------------------------------------------
country_label_dat <- function(df){
  Germany <- c("Germany", "Berlin")
  Canada <- c("Canada")
  USA <- c("usa", "United States")
  UK <- c("England", "Headquarters UK.", "UK")
  
  df$t1location <- ifelse(df$t1location_3_TEXT %in% Germany, 4, 
                          ifelse(df$t1location_3_TEXT %in% Canada, 5, 
                                 ifelse(df$t1location_3_TEXT %in% USA, 6,
                                        ifelse(df$t1location_3_TEXT %in% UK, 7, df$t1location)))) %>%
    set_labels(., labels = c("Netherlands", "India", "Other", "Germany", "Canada", "USA", "UK"))
  return(df)
}



# Sector labels -----------------------------------------------------------
sector_label_dat <- function(df){
  Information <- c("social media platform", "food tech", "Agri-Fintech", "Industrial + Communication Design", 
                   "Biotechnology, Medical Technology", "E-commerce", "Biotechnology", "Hardware")
  Finance <- c("Research and Development", "Coaching, Consulting", "Marketing Consulting", "Organisational and management advisor",
               "Hospitality Consultancy", "Consulting")
  Health <- c("Tourism and travel", "Fitness", "Entertainment, video production", "Education", "Charity",  "Gastronomy",
              "Design, Editorial, or Publishing", "Entertainment and Stage performance", "gastronomy", "Art/Music", "Music",
              "Film and theatre", "Creative Industry", "architecture", "Creative Industries", "filmmaking")
  Wholesale <- c("Communications and Wholesale")
  Manufacturing <- c("Post Harvest and food industry", "Nutraceutical & Secondary Agriculture", "Utility, drinking water",
                     "FastFood", "Textile industry", "Food service", "Bakery", "Cosmetic", "Consumer Goods", "Coffee industry")
  Agriculture <- c("Energy")

  df$t1sector <- ifelse(df$t1sector_7_TEXT %in% Information, 1,
                        ifelse(df$t1sector_7_TEXT %in% Finance, 2,
                               ifelse(df$t1sector_7_TEXT %in% Health, 3,
                                      ifelse(df$t1sector_7_TEXT %in% Wholesale, 4,
                                             ifelse(df$t1sector_7_TEXT %in% Manufacturing, 5,
                                                    ifelse(df$t1sector_7_TEXT %in% Agriculture, 6, df$t1sector)))))) %>%
    set_labels(., labels = c(
      "Information, Communications, \nor Technology", 
      "Finance, Real Estate,\nor Business Services", 
      "Health, Education, Government, \nor Social and Consumer Services",
      "Wholesale, Retail", 
      "Manufacturing, Logistics", 
      "Agriculture, Extractive, \nor Construction", 
      "Other"
    ))
  
  return(df)
}

  

# Time since business foundation ------------------------------------------
business_found <- function(df){
  year <- as.numeric(names(attr(df$t1date_1,"labels")[match(df$t1date_1,attr(df$t1date_1,"labels"))]))
  month <- as.numeric(df$t1date_2_1 %>% remove_all_labels(.))
  t1timebuiss <- as.yearmon(paste(year, month), "%Y %m") %>% as_date(.)
  RecordedDate <- as.yearmon(df$RecordedDate.x, "%Y %m") %>% as_date(.)
  df$t1timebuiss <- difftime(RecordedDate, t1timebuiss, UTC, units = c("days")) %>% as.numeric(., units = "days")
  return(df)
}


# Create mean event severity score ----------------------------------------

get_mean_severity <- function(df){
  df <- df %>%
    mutate(t1meansev = select(., starts_with("T1Severity")) %>%
             rowMeans(na.rm = TRUE)) %>% 
    select(-contains("T1Severity"))
  return(df)
}


# Renaming -----------------------------------------------------------------

# Perform all cleaning operations defined in functions above
T123 <- read_data(rel_directory = "Data/", pattern = "\\.sav$")  %>%
  transform_to_one_df(.) %>% 
  rename_cols(.) %>% 
  country_label_dat(.) %>% 
  sector_label_dat(.) %>% 
  business_found(.) %>%
  get_mean_severity(.)


names(T123) <- gsub("(threat)([1-9])", "\\1_\\2", names(T123)) %>%
  gsub("(last)([1-9])", "\\1_\\2", .) %>%
  gsub("(.*)_$", "\\1", .) %>%
  gsub("jobsa_2_1", "jobsa_2", .) %>%
  gsub("jobsa_2_3", "jobsa_3", .) %>%
  gsub('^(t.)(cope)(.*)(\\d{1})$', '\\1\\2\\3_\\4', .)

# use age labels for age variable
T123$t1age <- as.numeric(names(attr(T123$t1age_1,"labels")[match(T123$t1age_1,attr(T123$t1age_1,"labels"))]))
  

# make one overall satisfaction with enrepreneurial job column 
T123$overallsat <- T123$t1jobsa_1
# Remove irrelevant/ not anonymous variables
T123 <- T123 %>% 
  # coalesce location variables from different time points
  dplyr::mutate(LocationLat = dplyr::coalesce(LocationLatitude.x, LocationLatitude.y, LocationLatitude)) %>% 
  mutate(LocationLon = coalesce(LocationLongitude.x, LocationLongitude.y, LocationLongitude)) %>%
  # Z score for jobsa and recode jobsa_3 (not measured on same scale)
  mutate_at(vars(contains("jobsa")), list( ~as.vector(scale(.)))) %>% 
  mutate_at(vars(contains("jobsa_3")), list( ~(-1*(.)))) %>%
  dplyr::select(-matches("Status|IPAddress|Progress|Duration|Finished|Response|Name|Email|Reference|Distribution|Language|consent|email|code|t1date_1|t1date_2_1|_TEXT|t1age_1|t1product|jobsa_2_2|t1evdes_|goodbye|Date|.x$|.y$|LocationLatitude|LocationLongitude|t1evcat")) 
  



# Reliabilities  -----------------------------------------------------
get_reliabilities <- function(df) {
  T123_alph <- df %>% select(-matches("overallsat|evdes|newcol|t1occ|found|own|sector|gender|lang|edu|newcol|preocc|evcat|timebuiss|age|novel|disrup|perfo|probsolv|cope|goodbye|maxsev|LocationLat|LocationLon|location|t1meansev")) %>%
  ## Remove columns with more than 60% NA
  .[, which(colMeans(!is.na(.)) > 0.4)]
  alph_split <- T123_alph %>% remove_all_labels(.) %>%
    split.default(sub("_.*", "", names(T123_alph))) 
  alph <- purrr::map(alph_split, ~ psych::alpha(.x), data = .x) %>%
    purrr::map(~ .x$total)
  alph_df <- do.call("rbind", alph) %>% round(., 2)
  return(alph_df)
}


# Create composite data frame -----------------------------------------------------------------
make_comp_df <- function(df) {
  T123_comp <- df %>% select(-matches("evdes|newcol|LocationLat|LocationLon")) 
  comp_split <- T123_comp %>% remove_all_labels(.) %>% split.default(sub("_.*", "", names(T123_comp))) 
  comp <- purrr::map(comp_split, ~ multicon::composite(.x, nomiss = 0.8), data = .x)
  comp_df <- do.call("cbind", comp) %>% as.data.frame(.)
  return(comp_df)
}

df_orig <- make_comp_df(T123)
df_orig$overallsat <- T123$overallsat

df_orig <- T123 %>% select(t1evdes, LocationLon, LocationLat) %>% cbind(., df_orig ) %>%  
  mutate_at(vars(matches("LocationL")),
            ~(as.numeric(.))) %>%
  # Add anonymous ID
  mutate(ID_code = seq(1, nrow(df_orig)) + 1000) %>%
  # Add descriptive labels
  set_value_labels(maxsev = c("Financial difficulties" = 1, 
                              "Conflicts with clients, stakeholders, or colleagues" = 2,
                              "Conflicts between clients, stakeholders, or colleagues" = 3,
                              "Legal issues" = 4,
                              "Absence or lack of personnel" = 5,
                              "Problems with material or supply" = 6,
                              "Mistakes" = 8,
                              "Other" = 9),
                   t1edu = c("Primary school" = 1,
                             "Secondary school" = 2,
                             "Technical school diploma" = 3,
                             "University degree" = 4,
                             "Doctorate degree" = 5,
                             "Other" = 6),
                   t1own = c("Single owner" = 1,
                             "Co-owned venture" = 2,
                             "Other" = 3),
                   t1sector = c("Information, Communications, \nor Technology" = 1, 
                                "Finance, Real Estate, or \nBusiness Services" = 2, 
                                "Health, Education, Government, \nor Social and Consumer \nServices" = 3,
                                "Wholesale, Retail" = 4, 
                                "Manufacturing, Logistics" = 5, 
                                "Agriculture, Extractive, or \nConstruction" = 6, 
                                "Other" = 7)) 
  # rename columns for input selection in app 
df <- df_orig %>%
rename(
    "Event description" = t1evdes,
    "Severest event" = maxsev,
    "Age" = t1age,
    "Event disruptiveness" = t1disrup,
    "Event novelty" = t1novel,
    "Event performance impact" = t1perfo,
    "Problem solved" = t1probsolv,
    "Business age" = t1timebuiss,
    "Job satisfaction" = overallsat,
    "Education level" = t1edu,
    "Co-ownership" = t1own,
    "Industry" = t1sector
  ) %>% 
  mutate(`Average event severity` = round(t1meansev,0)) %>%
  mutate(`Job strain` = round(t1jobstr,0)) %>%
  drop_na(Age, `Job satisfaction`, `Job strain`, `Average event severity`) %>%
  set_value_labels(`Job satisfaction` = c("extremely dissatisfied" = 1,
                                          "somewhat dissatisfied" = 2,
                                          "neither satisfied \nnor dissatisfied" = 3,
                                          "somewhat satisfied" = 4,
                                          "extremely satisfied" = 5),
                   `Job strain` = c("never feeling strained" = 1, 
                                    "sometimes feeling strained" = 2,
                                    "about half of the \ntime feeling strained" = 3, 
                                    "most of the time \nfeeling strained" = 4, 
                                    "always feeling strained" = 5))

# Write data   ---------------------------------------------------------
rels <- get_reliabilities(T123) # reliabilities
write.csv(rels, "reliabilities.csv") 
write_sav(T123, "df_full_raw.sav") # full data set 
write_sav(df_orig, "comp_df.sav") # composite data


usethis::use_data(df, overwrite = TRUE, internal = TRUE) # composite data for internal use in App 

world_df <- map_data("world")

usethis::use_data(world_df, overwrite = TRUE, internal = FALSE) # world data for world map  


