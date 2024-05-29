library(tidyverse)
library(textcat)

read_wos_fast5000 <- function(folder_path){
  
  files <- list.files(path = folder_path, 
                      pattern = "\\.txt$", 
                      full.names = TRUE
  )
  data_list <- list()
  
  for (file in files) {
    data <- read_tsv(file)%>%
      select(c(AU, PY, TI, AB, SO)) %>% 
      rename(author = AU, 
             year = PY, 
             title = TI, 
             abstract = AB, 
             source = SO
      )
    
    data_list[[length(data_list) + 1]] <- data
  }
  
  combined_data <- bind_rows(data_list)
  
  return(combined_data)
}


# Load Data ----
data_1970_1985 <- read_wos_fast5000("data/1970-1985")
data_1986_2000 <- read_wos_fast5000("data/1986-2000")
data_2001_2014 <- read_wos_fast5000("data/2001-2014")
data_2015_2023 <- read_wos_fast5000("data/2015-2023")

data <- rbind(data_1970_1985, data_1986_2000, data_2001_2014, data_2015_2023) %>% 
  mutate(source = factor(source, levels = sort(unique(source)))) 

rm(data_1970_1985, data_1986_2000, data_2001_2014, data_2015_2023)

# get article language ----
data <- data %>%
  mutate(language = textcat(abstract))


# Clean Data ----

data <- data %>% 
  arrange(year) %>% # sort by year
  mutate(ID = 1:nrow(data)) %>% select(ID, everything()) %>%  # create ID
  mutate(source = fct_collapse(
    source,
    `ANNUAL REVIEW OF LAW AND SOCIAL SCIENCE` = c(
      "ANNUAL REVIEW OF LAW AND SOCIAL SCIENCE", 
      "Annual Review of Law and Social Science"),
    `ANNUAL REVIEW OF SOCIOLOGY` = c(
      "ANNUAL REVIEW OF SOCIOLOGY",
      "Annual Review of Sociology"),
    `CANADIAN REVIEW OF SOCIOLOGY AND ANTHROPOLOGY` = c(
      "CANADIAN REVIEW OF SOCIOLOGY AND ANTHROPOLOGY",
      "CANADIAN REVIEW OF SOCIOLOGY AND ANTHROPOLOGY-REVUE CANADIENNE DE SOCIOLOGIE ET D ANTHROPOLOGIE"
    ),
    `CANADIAN REVIEW OF SOCIOLOGY-REVUE CANADIENNE DE SOCIOLOGIE` = c(
      "CANADIAN REVIEW OF SOCIOLOGY-REVUE CANADIENNE DE SOCIOLOGIE",
      "Canadian Review of Sociology-Revue Canadienne de Sociologie"
    ),
    `CURRENT PERSPECTIVES IN SOCIAL THEORY` = c(
      "CURRENT PERSPECTIVES IN SOCIAL THEORY",
      "Current Perspectives in Social Theory"
    ),
    `CURRENT SOCIOLOGY` = c(
      "CURRENT SOCIOLOGY",
      "CURRENT SOCIOLOGY-1976",
      "CURRENT SOCIOLOGY-SOCIOLOGIE CONTEMPORAINE"
    ),
    `DEVIANCE ET SOCIETE` = c(
      "DEVIANCE ET SOCIETE",
      "Deviance et Societe"
    ),
    `DEVIANT BEHAVIOR` = c(
      "DEVIANT BEHAVIOR",
      "DEVIANT BEHAVIOR; DEVIANT BEHAVIOR"
    ),
    `ETHNOGRAPHY` = c(
      "ETHNOGRAPHY",
      "ETHNOGRAPHY; ETHNOGRAPHY"
    ),
    `EUROPEAN JOURNAL OF SOCIAL THEORY` = c(
      "EUROPEAN JOURNAL OF SOCIAL THEORY",
      "EUROPEAN JOURNAL OF SOCIAL THEORY; EUROPEAN JOURNAL OF SOCIAL THEORY"
    ),
    `FILOSOFIJA-SOCIOLOGIJA` = c(
      "FILOSOFIJA-SOCIOLOGIJA",
      "Filosofija-Sociologija"
    ),
    `HEALTH SOCIOLOGY REVIEW` = c(
      "HEALTH SOCIOLOGY REVIEW",
      "Health Sociology Review"
    ),
    `INTERNATIONAL JOURNAL OF INTERCULTURAL RELATIONS` = c(
      "INTERNATIONAL JOURNAL OF INTERCULTURAL RELATIONS",
      "INTERNATIONAL JOURNAL OF INTERCULTURAL RELATIONS; INTERNATIONAL JOURNAL OF INTERCULTURAL RELATIONS"
    ),
    `INTERNATIONAL REVIEW OF SOCIOLOGY-REVUE INTERNATIONALE DE SOCIOLOGIE` = c(
      "INTERNATIONAL REVIEW OF SOCIOLOGY-REVUE INTERNATIONALE DE SOCIOLOGIE",
      "INTERNATIONAL REVIEW OF SOCIOLOGY-REVUE INTERNATIONALE DE SOCIOLOGIE; INTERNATIONAL REVIEW OF SOCIOLOGY-REVUE INTERNATIONALE DE SOCIOLOGIE"
    ),
    `JOURNAL OF CULTURAL ECONOMY` = c(
      "JOURNAL OF CULTURAL ECONOMY",
      "JOURNAL OF CULTURAL ECONOMY; JOURNAL OF CULTURAL ECONOMY"
    ),
    `JOURNAL OF MARRIAGE AND FAMILY` = c(
      "JOURNAL OF MARRIAGE AND FAMILY",
      "JOURNAL OF MARRIAGE AND THE FAMILY",
      "JOURNAL OF MARRIAGE AND FAMILY; JOURNAL OF MARRIAGE AND FAMILY"
    ),
    `KOLNER ZEITSCHRIFT FUR SOZIOLOGIE UND SOZIALPSYCHOLOGIE` = c(
      "KOLNER ZEITSCHRIFT FUR SOZIOLOGIE UND SOZIALPSYCHOLOGIE",
      "ZEITSCHRIFT FUR SOZIOLOGIE"
    ),
    `LAW & SOCIETY REVIEW` = c(
      "LAW & SOCIETY REVIEW",
      "LAW & SOCIETY REVIEW; LAW & SOCIETY REVIEW"
    ),
    `NATIONAL COUNCIL FOR THE SOCIAL STUDIES` = c(
      "NATIONAL COUNCIL FOR THE SOCIAL STUDIES-BULLETIN",
      "NATIONAL COUNCIL FOR THE SOCIAL STUDIES-YEARBOOK"
    ),
    `POPULATION AND DEVELOPMENT REVIEW` = c(
      "POPULATION AND DEVELOPMENT REVIEW",
      "POPULATION AND DEVELOPMENT REVIEW; POPULATION AND DEVELOPMENT REVIEW"
    ),
    `RESEARCH IN SOCIAL STRATIFICATION AND MOBILITY` = c(
      "RESEARCH IN SOCIAL STRATIFICATION AND MOBILITY",
      "Research in Social Stratification and Mobility"
    ),
    `SCANDINAVIAN JOURNAL OF HOSPITALITY AND TOURISM` = c(
      "SCANDINAVIAN JOURNAL OF HOSPITALITY AND TOURISM",
      "Scandinavian Journal of Hospitality and Tourism"
    ),
    `SEXUALITIES` = c(
      "SEXUALITIES",
      "SEXUALITIES; SEXUALITIES"
    ),
    `SOCIAL INDICATORS RESEARCH` = c(
      "SOCIAL INDICATORS RESEARCH",
      "SOCIAL INDICATORS RESEARCH; SOCIAL INDICATORS RESEARCH"
    ),
    `SOCIAL PROBLEMS` = c(
      "SOCIAL PROBLEMS",
      "SOCIAL PROBLEMS; SOCIAL PROBLEMS"
    ),
    `SOCIAL SCIENCE QUARTERLY` = c(
      "SOCIAL SCIENCE QUARTERLY",
      "SOCIAL SCIENCE QUARTERLY; SOCIAL SCIENCE QUARTERLY"
    ),
    `SOCIETIES` = c(
      "SOCIETIES",
      "SOCIETIES; SOCIETIES"
    ),
    `SOCIOLOGICAL METHODOLOGY` = c(
      "SOCIOLOGICAL METHODOLOGY",
      "Sociological Methodology"
    ),
    `SOCIOLOGICKY CASOPIS` = c(
      "SOCIOLOGICKY CASOPIS",
      "SOCIOLOGICKY CASOPIS-CZECH SOCIOLOGICAL REVIEW"
    ),
    `SPORT IN SOCIETY` = c(
      "SPORT IN SOCIETY",
      "SPORT IN SOCIETY; SPORT IN SOCIETY"
    ),
    `STUDIES IN SYMBOLIC INTERACTION` = c(
      "Studies in Symbolic Interaction",
      "STUDIES IN SYMBOLIC INTERACTION",
      "STUDIES IN SYMBOLIC INTERACTION - US"
    ),
    `YOUNG` = c(
      "YOUNG",
      "Young"
    )
  ))



# save data
write.csv(data, "data/data.csv")
