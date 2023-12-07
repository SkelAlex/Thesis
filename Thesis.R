#!radian
#devtools::install_github("clessn/clessnverse", force = T)
#extrafont::font_import()
#extrafont::font_install('fontcm')
#extrafont::loadfonts()
#webshot::install_phantomjs() for saving modelsummary to png or jpg
library(tidyverse)
library(extrafont)

##### 1. Data cleaning #####
#### 1.1 CCPIS ####
CCPISEN <- read.csv("_data/CCPIS/StudentsEN.csv")
CCPISFR1 <- read.csv("_data/CCPIS/StudentsFR1.csv")
CCPISFR3 <- read.csv("_data/CCPIS/StudentsFR3.csv")
CCPISBad <- bind_rows(CCPISEN, CCPISFR1, CCPISFR3) |>
  filter(str_detect(StartDate, "^20") & # remove column descriptions
           (Consent == "I do not accept to participate in this survey" |
              Consentement ==
              "Je n'accepte pas de participer à cette recherche ") |
           Q5_1 == "" | is.na(Q5_1))
CCPIS <- bind_rows(CCPISEN, CCPISFR1, CCPISFR3) |>
  filter(str_detect(StartDate, "^20") & # remove column descriptions
           (Consent == "I accept to participate in this survey" |
              Consentement == "J'accepte de participer à cette recherche") &
           Q5_1 != "" & !is.na(Q5_1))
CCPIS$Class <- CCPIS$Classe
CCPIS$Class[is.na(CCPIS$Class)] <- CCPIS$Class.ID[
  is.na(CCPIS$Class)]
CCPIS$Class <- str_remove_all(CCPIS$Class, "\\s+")
CCPIS$number <- 1:length(CCPIS$StartDate)
Codes <- openxlsx::read.xlsx("_data/CCPIS/8digitCodes.xlsx")
CCPISManageManually <- CCPIS |>
  filter(!(Class %in% Codes$Code.à.8.chiffres)) |>
  select(StartDate:EndDate, LocationLatitude:LocationLongitude, Class:Élève,
         number)
# manual check of classrooms numbers which don't match the list to see if I
# can match them with other classes through common latitude/longitude &
# similar star time and end time
CCPIS$Class[!(CCPIS$Class %in% Codes$Code.à.8.chiffres)] <- c(
  rep("", 9), rep("16554548", 2), "79362677", "41083499", "04120709",
  rep("61150875", 2), rep("40965312", 3), rep("43731111", 2),
  rep("27787836", 4), rep("35043304", 17), rep("56697824", 5), "07892125",
  "26319798", "", rep("46537164", 2), rep("65988237", 3), "53394381",
  "84691593", "", "72249157", "36611780", rep("", 2), "14748972")
CCPIS$school <- qdap::mgsub(text.var = CCPIS$Class,
                               pattern = Codes$Code.à.8.chiffres,
                               replacement = Codes$École)
CCPIS$school[CCPIS$school == paste0(
  "Student Senate (https://www.rcdsb.on.ca/en/parents-and-students/",
  "student-senate.aspx)")] <- "Renfrew County DSB Student Senate"
CCPIS <- filter(CCPIS, school != "")
CCPIS$teacher <- qdap::mgsub(text.var = CCPIS$Class,
                                pattern = Codes$Code.à.8.chiffres,
                                replacement = Codes$`Nom.de.l'enseignant`)
table(CCPIS$school, useNA = "always")
table(CCPIS$Class, useNA = "always") # 38 classes, but 6 with 1-2 students
# so 32 classes with at least 9 students (or 30 with at least 12)
table(CCPIS$teacher, useNA = "always")
CCPIS$province <- NA
CCPIS$province[CCPIS$school %in% unique(CCPIS$school)[4:8]] <-
  "Quebec"
CCPIS$province[CCPIS$school %in% unique(CCPIS$school)[1:3]] <-
  "Ontario"
CCPIS$school_lang <- NA
CCPIS$school_lang[CCPIS$school %in% unique(CCPIS$school)[4:8]] <-
  "French"
CCPIS$school_lang[CCPIS$school %in% unique(CCPIS$school)[1:3]] <-
  "English"
CCPIS$private <- 1
CCPIS$private[CCPIS$school %in% c(
  "École Jean-de-Brébeuf", # CSS de la Capitale
  "École de la Rose-des-Vents", # CSS des Draveurs
  "Renfrew County DSB Student Senate")] <- 0 # public
table(CCPIS$province, useNA = "always")
table(CCPIS$school_lang, useNA = "always")
table(CCPIS$private, useNA = "always")
CCPIS$city <- case_when(
  CCPIS$school == "Collège Citoyen" ~ "Laval",
  CCPIS$school == "Collège Boisbriand 2016" ~ "Boisbriand",
  CCPIS$school == "École Jean-de-Brébeuf" ~ "Québec",
  CCPIS$school == "École de la Rose-des-Vents" ~ "Cantley",
  CCPIS$school == "Renfrew County DSB Student Senate" ~ "Pembroke",
  CCPIS$school == "Collège mariste de Québec" ~ "Québec",
  CCPIS$school == "Urban Village Academy" ~ "Barrie",
  CCPIS$school == "Jaya International High School" ~ "Mississauga")
table(CCPIS$city, useNA = "always")
names(CCPIS)[20:77] <- c(
  "pol_meaning", "interest", "lockdown_political", "nurses_political",
  "china_political", "ukraine_political", "police_political",
  "crime_political", "tuition_political", "privateschool_political",
  "elections_political", "parties_political", "interest_health",
  "interest_foreign", "interest_law", "interest_education",
  "interest_partisan", "fam_situation", "parent_discuss", "parent_health",
  "parent_foreign", "parent_law", "parent_education", "parent_partisan",
  "mother_discuss", "father_discuss", "friends_gender", "malefriends_discuss",
  "femalefriends_discuss", "teacher_discuss", "teacher_gender",
  "influencer_discuss", "influencer_gender", "gender", "yob", "language",
  "lang_aboriginal", "lang_other", "race", "race_other", "born_canada",
  "sexrole_independent", "sexrole_passive", "sexrole_competitive",
  "sexrole_easydecisions", "sexrole_giveup", "sexrole_selfconfident",
  "sexrole_inferior", "sexrole_underpressure", "sexrole_emotional",
  "sexrole_devote", "sexrole_gentle", "sexrole_helpful", "sexrole_kind",
  "sexrole_awarefeelings", "sexrole_understanding", "sexrole_warm", "email")
CCPIS$female <- NA
CCPIS$female[CCPIS$gender %in% c("A boy", "Un garçon")] <- 0
CCPIS$female[CCPIS$gender %in% c("A girl", "Une fille")] <- 1
CCPIS$female <- as.factor(CCPIS$female)
table(CCPIS$female, useNA = "always") # 17 "other", 33 did not answer
CCPIS$female_alt <- NA
CCPIS$female_alt[CCPIS$gender %in% c("A boy", "Un garçon")] <- "Boy"
CCPIS$female_alt[CCPIS$gender %in% c("A girl", "Une fille")] <- "Girl"
CCPIS$female_alt[
  CCPIS$gender ==
    "Autre (ex.: trans, non-binaire, bispirituel, gender-queer)"] <- "Other"
CCPIS$female_alt2 <- CCPIS$female_alt
CCPIS$female_alt2[CCPIS$female_alt == "Girl"] <- "Girls"
CCPIS$female_alt2[CCPIS$female_alt == "Boy"] <- "Boys"
CCPIS$female_alt <- as.factor(CCPIS$female_alt)
CCPIS$female_alt2 <- as.factor(CCPIS$female_alt2)
CCPIS$age <- 2022 - as.numeric(CCPIS$yob)
CCPIS$age[CCPIS$age > 18 | CCPIS$age < 10] <- NA
table(CCPIS$age, useNA = "always")
CCPIS$age_squared <- CCPIS$age ^ 2
CCPIS$lang <- "Allophone"
CCPIS$lang[CCPIS$language == "Français"] <- "Francophone"
CCPIS$lang[CCPIS$language %in% c("English", "Anglais")] <- "Anglophone"
CCPIS$lang[CCPIS$language %in% c(
  "Don’t know/Prefer not to answer", "Je ne sais pas/Préfère ne pas répondre",
  "")] <- NA
table(CCPIS$lang, useNA = "always")
CCPIS$immig <- NA
CCPIS$immig[CCPIS$born_canada %in% c("Yes", "Oui")] <- 0
CCPIS$immig[CCPIS$born_canada %in% c("No", "Non")] <- 1
CCPIS$ethnicity[CCPIS$race %in% c("Blanc(he)", "White")] <- "White"
CCPIS$ethnicity[CCPIS$race == "Noir(e)"] <- "Black"
CCPIS$ethnicity[CCPIS$race == paste(
  "Asiatique occidental(e) (e.g., Iranien(ne), Afghan(e), etc.)")] <-
  "West Asian"
CCPIS$ethnicity[CCPIS$race == paste(
  "Asiatique du Sud-Est (ex.: Vietnamien(ne), Cambodgien(ne), Laotien(ne),",
  "Thaïlandais(e), etc.)")] <- "Southeast Asian"
CCPIS$ethnicity[CCPIS$race == "Arabe"] <- "Arabic"
CCPIS$ethnicity[CCPIS$race == paste(
  "Sud-Asiatique (ex.: Indien(ne) de l’Inde, Pakistanais(e),",
  "Sri-Lankais(e), etc.)")] <- "South Asian"
CCPIS$ethnicity[CCPIS$race == "Latino-Américain(e)"] <- "Hispanic"
CCPIS$ethnicity[CCPIS$race == paste(
  "Première Nation (Indien(ne) de l’Amérique du Nord), Métis(se) ou",
  "Inuk (Inuit)")] <- "Indigenous"
CCPIS$ethnicity[CCPIS$race %in% c("Chinese", "Chinois(e)")] <- "Chinese"
CCPIS$ethnicity[CCPIS$race == "Philippin(e)"] <- "Filipino"
CCPIS$ethnicity[CCPIS$race == "Coréen(ne)"] <- "Korean"
CCPIS$ethnicity[CCPIS$race == "Japonais(e)"] <- "Japanese"
CCPIS$ethnicity[CCPIS$race %in% c("Autre (veuillez spécifier)",
                                        "Other (please specify)")] <- "Other"
table(CCPIS$ethnicity, useNA = "always")
CCPIS$white <- 0
CCPIS$white[CCPIS$ethnicity == "White"] <- 1
CCPIS$white[is.na(CCPIS$ethnicity)] <- NA
table(CCPIS$white, useNA = "always")
CCPIS$interest <- as.numeric(CCPIS$interest)
CCPIS$interest_health <- as.numeric(CCPIS$interest_health)
CCPIS$interest_foreign <- as.numeric(CCPIS$interest_foreign)
CCPIS$interest_law <- as.numeric(CCPIS$interest_law)
CCPIS$interest_education <- as.numeric(CCPIS$interest_education)
CCPIS$interest_partisan <- as.numeric(CCPIS$interest_partisan)
CCPIS$gender_parent_health <- NA
CCPIS$gender_parent_health[
  CCPIS$parent_health %in% c("Mon père", "Father")] <- 0
CCPIS$gender_parent_health[
  CCPIS$parent_health %in% c("Ma mère", "Mother")] <- 1
table(CCPIS$gender_parent_health, useNA = "always")
CCPIS$gender_parent_education <- NA
CCPIS$gender_parent_education[
  CCPIS$parent_education %in% c("Mon père", "Father")] <- 0
CCPIS$gender_parent_education[
  CCPIS$parent_education %in% c("Ma mère", "Mother")] <- 1
table(CCPIS$gender_parent_education, useNA = "always")
CCPIS$gender_parent_law <- NA
CCPIS$gender_parent_law[
  CCPIS$parent_law %in% c("Mon père", "Father")] <- 0
CCPIS$gender_parent_law[
  CCPIS$parent_law %in% c("Ma mère", "Mother")] <- 1
table(CCPIS$gender_parent_law, useNA = "always")
CCPIS$gender_parent_foreign <- NA
CCPIS$gender_parent_foreign[
  CCPIS$parent_foreign %in% c("Mon père", "Father")] <- 0
CCPIS$gender_parent_foreign[
  CCPIS$parent_foreign %in% c("Ma mère", "Mother")] <- 1
table(CCPIS$gender_parent_foreign, useNA = "always")
CCPIS$gender_parent_partisan <- NA
CCPIS$gender_parent_partisan[
  CCPIS$parent_partisan %in% c("Mon père", "Father")] <- 0
CCPIS$gender_parent_partisan[
  CCPIS$parent_partisan %in% c("Ma mère", "Mother")] <- 1
table(CCPIS$gender_parent_partisan, useNA = "always")
CCPIS$mother_discuss_clean <- CCPIS$mother_discuss
CCPIS$mother_discuss_clean[CCPIS$mother_discuss %in% c(
  "Je ne sais pas/Préfère ne pas répondre",
  "Don't know/Prefer not to answer", "")] <- NA
CCPIS$mother_discuss_clean[CCPIS$mother_discuss == "Santé"] <- "Health care"
CCPIS$mother_discuss_clean[
  CCPIS$mother_discuss == "Affaires internationales"] <-
  "International affairs"
CCPIS$mother_discuss_clean[CCPIS$mother_discuss == "Loi et crime"] <-
  "Law and crime"
CCPIS$mother_discuss_clean[CCPIS$mother_discuss == "Éducation"] <- "Education"
CCPIS$mother_discuss_clean[CCPIS$mother_discuss == "Politique partisane"] <-
  "Partisan politics"
table(CCPIS$mother_discuss_clean, useNA = "always")
CCPIS$mother_discuss_health <- 0
CCPIS$mother_discuss_health[CCPIS$mother_discuss_clean == "Health care"] <- 1
CCPIS$mother_discuss_health[is.na(CCPIS$mother_discuss_clean)] <- NA
table(CCPIS$mother_discuss_health, useNA = "always")
CCPIS$mother_discuss_foreign <- 0
CCPIS$mother_discuss_foreign[CCPIS$mother_discuss_clean ==
                               "International affairs"] <- 1
CCPIS$mother_discuss_foreign[is.na(CCPIS$mother_discuss_clean)] <- NA
table(CCPIS$mother_discuss_foreign, useNA = "always")
CCPIS$mother_discuss_law <- 0
CCPIS$mother_discuss_law[CCPIS$mother_discuss_clean == "Law and crime"] <- 1
CCPIS$mother_discuss_law[is.na(CCPIS$mother_discuss_clean)] <- NA
table(CCPIS$mother_discuss_law, useNA = "always")
CCPIS$mother_discuss_education <- 0
CCPIS$mother_discuss_education[CCPIS$mother_discuss_clean == "Education"] <- 1
CCPIS$mother_discuss_education[is.na(CCPIS$mother_discuss_clean)] <- NA
table(CCPIS$mother_discuss_education, useNA = "always")
CCPIS$mother_discuss_partisan <- 0
CCPIS$mother_discuss_partisan[CCPIS$mother_discuss_clean ==
                                "Partisan politics"] <- 1
CCPIS$mother_discuss_partisan[is.na(CCPIS$mother_discuss_clean)] <- NA
table(CCPIS$mother_discuss_partisan, useNA = "always")
CCPIS$father_discuss_clean <- CCPIS$father_discuss
CCPIS$father_discuss_clean[CCPIS$father_discuss %in% c(
  "Je ne sais pas/Préfère ne pas répondre",
  "Don't know/Prefer not to answer", "")] <- NA
CCPIS$father_discuss_clean[CCPIS$father_discuss == "Santé"] <- "Health care"
CCPIS$father_discuss_clean[
  CCPIS$father_discuss == "Affaires internationales"] <-
  "International affairs"
CCPIS$father_discuss_clean[CCPIS$father_discuss == "Loi et crime"] <-
  "Law and crime"
CCPIS$father_discuss_clean[CCPIS$father_discuss == "Éducation"] <- "Education"
CCPIS$father_discuss_clean[CCPIS$father_discuss == "Politique partisane"] <-
  "Partisan politics"
table(CCPIS$father_discuss_clean, useNA = "always")
CCPIS$father_discuss_health <- 0
CCPIS$father_discuss_health[CCPIS$father_discuss_clean == "Health care"] <- 1
CCPIS$father_discuss_health[is.na(CCPIS$father_discuss_clean)] <- NA
table(CCPIS$father_discuss_health, useNA = "always")
CCPIS$father_discuss_foreign <- 0
CCPIS$father_discuss_foreign[CCPIS$father_discuss_clean ==
                               "International affairs"] <- 1
CCPIS$father_discuss_foreign[is.na(CCPIS$father_discuss_clean)] <- NA
table(CCPIS$father_discuss_foreign, useNA = "always")
CCPIS$father_discuss_law <- 0
CCPIS$father_discuss_law[CCPIS$father_discuss_clean == "Law and crime"] <- 1
CCPIS$father_discuss_law[is.na(CCPIS$father_discuss_clean)] <- NA
table(CCPIS$father_discuss_law, useNA = "always")
CCPIS$father_discuss_education <- 0
CCPIS$father_discuss_education[CCPIS$father_discuss_clean == "Education"] <- 1
CCPIS$father_discuss_education[is.na(CCPIS$father_discuss_clean)] <- NA
table(CCPIS$father_discuss_education, useNA = "always")
CCPIS$father_discuss_partisan <- 0
CCPIS$father_discuss_partisan[CCPIS$father_discuss_clean ==
                                "Partisan politics"] <- 1
CCPIS$father_discuss_partisan[is.na(CCPIS$father_discuss_clean)] <- NA
table(CCPIS$father_discuss_partisan, useNA = "always")
CCPIS$peers_female <- NA
CCPIS$peers_female[CCPIS$friends_gender %in% c("Garçons", "Boys")] <- 0
CCPIS$peers_female[CCPIS$friends_gender %in% c("Filles", "Girls")] <- 1
table(CCPIS$peers_female, useNA = "always")
CCPIS$malefriends_discuss_clean <- CCPIS$malefriends_discuss
CCPIS$malefriends_discuss_clean[CCPIS$malefriends_discuss %in% c(
  "Je ne sais pas/Préfère ne pas répondre",
  "Don't know/Prefer not to answer", "")] <- NA
CCPIS$malefriends_discuss_clean[CCPIS$malefriends_discuss ==
                                  "Santé"] <- "Health care"
CCPIS$malefriends_discuss_clean[
  CCPIS$malefriends_discuss == "Affaires internationales"] <-
  "International affairs"
CCPIS$malefriends_discuss_clean[CCPIS$malefriends_discuss ==
                                  "Loi et crime"] <-
  "Law and crime"
CCPIS$malefriends_discuss_clean[CCPIS$malefriends_discuss ==
                                  "Éducation"] <- "Education"
CCPIS$malefriends_discuss_clean[CCPIS$malefriends_discuss ==
                                  "Politique partisane"] <-
  "Partisan politics"
table(CCPIS$malefriends_discuss_clean, useNA = "always")
CCPIS$malefriends_discuss_health <- 0
CCPIS$malefriends_discuss_health[CCPIS$malefriends_discuss_clean %in% c(
  "Santé", "Health care")] <- 1
CCPIS$malefriends_discuss_health[
  is.na(CCPIS$malefriends_discuss_clean)] <- NA
table(CCPIS$malefriends_discuss_health, useNA = "always")
CCPIS$malefriends_discuss_foreign <- 0
CCPIS$malefriends_discuss_foreign[
  CCPIS$malefriends_discuss_clean %in% c(
    "Affaires internationales", "International affairs")] <- 1
CCPIS$malefriends_discuss_foreign[
  is.na(CCPIS$malefriends_discuss_clean)] <- NA
table(CCPIS$malefriends_discuss_foreign, useNA = "always")
CCPIS$malefriends_discuss_law <- 0
CCPIS$malefriends_discuss_law[CCPIS$malefriends_discuss_clean %in% c(
  "Loi et crime", "Law and crime")] <- 1
CCPIS$malefriends_discuss_law[
  is.na(CCPIS$malefriends_discuss_clean)] <- NA
table(CCPIS$malefriends_discuss_law, useNA = "always")
CCPIS$malefriends_discuss_education <- 0
CCPIS$malefriends_discuss_education[
  CCPIS$malefriends_discuss_clean %in% c("Éducation", "Education")] <- 1
CCPIS$malefriends_discuss_education[
  is.na(CCPIS$malefriends_discuss_clean)] <- NA
table(CCPIS$malefriends_discuss_education, useNA = "always")
CCPIS$malefriends_discuss_partisan <- 0
CCPIS$malefriends_discuss_partisan[
  CCPIS$malefriends_discuss_clean %in% c(
    "Politique partisane", "Partisan politics")] <- 1
CCPIS$malefriends_discuss_partisan[
  is.na(CCPIS$malefriends_discuss_clean)] <- NA
table(CCPIS$malefriends_discuss_partisan, useNA = "always")
CCPIS$femalefriends_discuss_clean <- CCPIS$femalefriends_discuss
CCPIS$femalefriends_discuss_clean[CCPIS$femalefriends_discuss %in% c(
  "Je ne sais pas/Préfère ne pas répondre",
  "Don't know/Prefer not to answer", "")] <- NA
CCPIS$femalefriends_discuss_clean[CCPIS$femalefriends_discuss ==
                                    "Santé"] <- "Health care"
CCPIS$femalefriends_discuss_clean[
  CCPIS$femalefriends_discuss == "Affaires internationales"] <-
  "International affairs"
CCPIS$femalefriends_discuss_clean[CCPIS$femalefriends_discuss ==
                                    "Loi et crime"] <-
  "Law and crime"
CCPIS$femalefriends_discuss_clean[CCPIS$femalefriends_discuss ==
                                    "Éducation"] <- "Education"
CCPIS$femalefriends_discuss_clean[CCPIS$femalefriends_discuss ==
                                    "Politique partisane"] <-
  "Partisan politics"
table(CCPIS$femalefriends_discuss_clean, useNA = "always")
CCPIS$femalefriends_discuss_health <- 0
CCPIS$femalefriends_discuss_health[
  CCPIS$femalefriends_discuss_clean %in% c("Santé", "Health care")] <- 1
CCPIS$femalefriends_discuss_health[
  is.na(CCPIS$femalefriends_discuss_clean)] <- NA
table(CCPIS$femalefriends_discuss_health, useNA = "always")
CCPIS$femalefriends_discuss_foreign <- 0
CCPIS$femalefriends_discuss_foreign[
  CCPIS$femalefriends_discuss_clean %in% c(
    "Affaires internationales", "International affairs")] <- 1
CCPIS$femalefriends_discuss_foreign[
  is.na(CCPIS$femalefriends_discuss_clean)] <- NA
table(CCPIS$femalefriends_discuss_foreign, useNA = "always")
CCPIS$femalefriends_discuss_law <- 0
CCPIS$femalefriends_discuss_law[
  CCPIS$femalefriends_discuss_clean %in% c(
    "Loi et crime", "Law and crime")] <- 1
CCPIS$femalefriends_discuss_law[
  is.na(CCPIS$femalefriends_discuss_clean)] <- NA
table(CCPIS$femalefriends_discuss_law, useNA = "always")
CCPIS$femalefriends_discuss_education <- 0
CCPIS$femalefriends_discuss_education[
  CCPIS$femalefriends_discuss_clean %in% c("Éducation", "Education")] <- 1
CCPIS$femalefriends_discuss_education[
  is.na(CCPIS$femalefriends_discuss_clean)] <- NA
table(CCPIS$femalefriends_discuss_education, useNA = "always")
CCPIS$femalefriends_discuss_partisan <- 0
CCPIS$femalefriends_discuss_partisan[
  CCPIS$femalefriends_discuss_clean %in% c(
    "Politique partisane", "Partisan politics")] <- 1
CCPIS$femalefriends_discuss_partisan[is.na(
  CCPIS$femalefriends_discuss_clean)] <- NA
table(CCPIS$femalefriends_discuss_partisan, useNA = "always")
CCPIS[, 61:76] <- map(CCPIS[, 61:76], as.numeric)
CCPIS[, 61:76] <- apply(CCPIS[, 61:76], 2, function(x) {(x - 1) / 4})
CCPIS$sexrole_easydecisions_rev <- 1 - CCPIS$sexrole_easydecisions
CCPIS$fam_situation_alt <- CCPIS$fam_situation
CCPIS$fam_situation_alt[CCPIS$fam_situation == "Une mère uniquement"] <-
  "One mother only"
CCPIS$fam_situation_alt[CCPIS$fam_situation == "Un père uniquement"] <-
  "One father only"
CCPIS$fam_situation_alt[CCPIS$fam_situation == "Deux mères"] <-
  "Two mothers"
CCPIS$fam_situation_alt[CCPIS$fam_situation == "Deux pères"] <-
  "Two fathers"
CCPIS$fam_situation_alt[CCPIS$fam_situation == "Autre"] <- "Other"
CCPIS$fam_situation_alt[CCPIS$fam_situation ==
                             "Une mère, un père et au moins un beau-parent"] <-
  "One mother, one\nfather and at least\none stepparent"
CCPIS$fam_situation_alt[CCPIS$fam_situation == paste(
  "One mother, one father and at least one stepparent")] <-
  "One mother, one\nfather and at least\none stepparent"
CCPIS$fam_situation_alt[CCPIS$fam_situation ==
                             "Une mère, un père et aucun beau-parent"] <-
  "One mother, one \nfather and no\nstepparents"
CCPIS$fam_situation_alt[CCPIS$fam_situation ==
                             "One mother, one father and no stepparents"] <-
  "One mother, one \nfather and no\nstepparents"
table(CCPIS$fam_situation_alt, useNA = "always")
CCPIS$friends_gender_alt <- CCPIS$friends_gender
CCPIS$friends_gender_alt[CCPIS$friends_gender == "Filles"] <- "Girls"
CCPIS$friends_gender_alt[CCPIS$friends_gender == "Garçons"] <- "Boys"
CCPIS$friends_gender_alt[CCPIS$friends_gender ==
                              "Environ autant des deux genres"] <-
  "About the same\nfor both genders"
CCPIS$friends_gender_alt[CCPIS$friends_gender ==
                              "About the same for both genders"] <-
  "About the same\nfor both genders"
CCPIS$friends_gender_alt[CCPIS$friends_gender %in% c(
  "Don't know/Prefer not to answer", "",
  "Je ne sais pas/Préfère ne pas répondre")] <- NA
table(CCPIS$friends_gender_alt, useNA = "always")
CCPIS$parent_discuss_alt <- CCPIS$parent_discuss
CCPIS$parent_discuss_alt[CCPIS$parent_discuss == "Ma mère"] <- "Mother"
CCPIS$parent_discuss_alt[CCPIS$parent_discuss == "Mon père"] <- "Father"
CCPIS$parent_discuss_alt[CCPIS$parent_discuss == "Les deux autant"] <-
  "Both equally"
CCPIS$parent_discuss_alt[CCPIS$parent_discuss %in% c(
  "Don't know/Prefer not to answer", "",
  "Je ne sais pas/Préfère ne pas répondre")] <- NA
table(CCPIS$parent_discuss_alt, useNA = "always")
CCPIS$teacher_gender_alt <- CCPIS$teacher_gender
CCPIS$teacher_gender_alt[CCPIS$teacher_gender %in% c(
  "A boy", "Un homme")] <- "A man"
CCPIS$teacher_gender_alt[CCPIS$teacher_gender %in% c(
  "A girl", "Une femme")] <- "A woman"
CCPIS$teacher_gender_alt[
  CCPIS$teacher_gender ==
    "Autre (ex.: trans, non-binaire, bispirituel, gender-queer)"] <-
  "Other (e.g. Trans,\nnon-binary, two-\nspirit, gender-queer)"
CCPIS$teacher_gender_alt[
  CCPIS$teacher_gender ==
    "Other (e.g. Trans, non-binary, two-spirit, gender-queer)"] <-
  "Other (e.g. Trans,\nnon-binary, two-\nspirit, gender-queer)"
CCPIS$teacher_gender_alt[CCPIS$teacher_gender == ""] <- NA
table(CCPIS$teacher_gender_alt, useNA = "always")
CCPIS$influencer_gender_alt <- CCPIS$influencer_gender
CCPIS$influencer_gender_alt[CCPIS$influencer_gender %in% c(
  "A boy", "Un homme")] <- "A man"
CCPIS$influencer_gender_alt[CCPIS$influencer_gender %in% c(
  "A girl", "Une femme")] <- "A woman"
CCPIS$influencer_gender_alt[
  CCPIS$influencer_gender ==
    "Autre (ex.: trans, non-binaire, bispirituel, gender-queer)"] <-
  "Other (e.g. Trans,\nnon-binary, two-\nspirit, gender-queer)"
CCPIS$influencer_gender_alt[
  CCPIS$influencer_gender ==
    "Other (e.g. Trans, non-binary, two-spirit, gender-queer)"] <-
  "Other (e.g. Trans,\nnon-binary, two-\nspirit, gender-queer)"
CCPIS$influencer_gender_alt[CCPIS$influencer_gender == ""] <- NA
table(CCPIS$influencer_gender_alt, useNA = "always")
political <- function(new, old) {
  new <- NA
  new[old %in% c("Not political", "Pas politique")] <- 0
  new[old %in% c("Political", "Politique")] <- 1
  return(new)
}
CCPIS$lockdown_political_alt <- political(
  old = CCPIS$lockdown_political, new = CCPIS$lockdown_political_alt)
CCPIS$nurses_political_alt <- political(
  old = CCPIS$nurses_political, new = CCPIS$nurses_political_alt)
CCPIS$china_political_alt <- political(
  old = CCPIS$china_political, new = CCPIS$china_political_alt)
CCPIS$ukraine_political_alt <- political(
  old = CCPIS$ukraine_political, new = CCPIS$ukraine_political_alt)
CCPIS$police_political_alt <- political(
  old = CCPIS$police_political, new = CCPIS$police_political_alt)
CCPIS$crime_political_alt <- political(
  old = CCPIS$crime_political, new = CCPIS$crime_political_alt)
CCPIS$tuition_political_alt <- political(
  old = CCPIS$tuition_political, new = CCPIS$tuition_political_alt)
CCPIS$privateschool_political_alt <- political(
  old = CCPIS$privateschool_political, new = CCPIS$privateschool_political_alt)
CCPIS$elections_political_alt <- political(
  old = CCPIS$elections_political, new = CCPIS$elections_political_alt)
CCPIS$parties_political_alt <- political(
  old = CCPIS$parties_political, new = CCPIS$parties_political_alt)

### 1.1.1 Factor analysis #####
AgencyScale <- na.omit(CCPIS[, c(
  "sexrole_independent", "sexrole_passive", "sexrole_competitive",
  "sexrole_easydecisions_rev", "sexrole_giveup", "sexrole_selfconfident",
  "sexrole_inferior", "sexrole_underpressure")])
AgencyCronbach <- round(as.numeric(psy::cronbach(AgencyScale)[3]), digits = 2)
AgencyFactorAnalysis <- factanal(AgencyScale, factors = 1)
AgencyVariableNames <- c(
  "Very independent", "Very active", "Very competitive",
  "Have difficulty making\ndecisions (reversed)", "Never give up easily",
  "Very self confident", "Feel very superior", "Stand up well\nunder pressure")
AgencyFactorLoadings <- AgencyFactorAnalysis$loadings[, 1]
AgencyFirstEigenvalue <- round(eigen(cor(AgencyScale))$values[1], digits = 2)
ggplot(data.frame(AgencyVariableNames, AgencyFactorLoadings),
       aes(x = AgencyVariableNames, y = AgencyFactorLoadings)) +
  coord_flip() +
  geom_bar(stat = "identity", colour = "black", fill = "black", linewidth = 1,
           width = 0.4) +
  geom_text(aes(label = as.character(round(AgencyFactorLoadings, digits = 2))),
            vjust = 0.35, hjust = -0.3, family = "CM Roman") +
  geom_hline(yintercept = 0.3, colour = "gray", linetype = "longdash") +
  annotate("text", label = paste("Cronbach's alpha =", as.character(
    AgencyCronbach)), x = 1.2, y = 0.85, size = 3.8, family = "CM Roman") +
  annotate("text", label = paste("First eigenvalue =", as.character(
    AgencyFirstEigenvalue)), x = 0.8, y = 0.85, size = 3.8,
    family = "CM Roman") +
  scale_y_continuous(name = "Factor loadings", limits = c(-0.1, 1),
                     breaks = seq(-0.1, 1, by = 0.1)) +
  xlab("") +
  theme_linedraw() +
  theme(axis.text.y = element_text(size = 14),
        axis.title.x = element_text(hjust = 0.3, vjust = -0.17, size = 14),
        panel.grid = element_blank(),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/AgencyScale.pdf", width = 11, height = 4.25)
CCPIS$agentic <- (
  CCPIS$sexrole_independent * AgencyFactorLoadings[1] +
    CCPIS$sexrole_passive * AgencyFactorLoadings[2] +
    CCPIS$sexrole_competitive * AgencyFactorLoadings[3] +
    CCPIS$sexrole_easydecisions_rev * AgencyFactorLoadings[4] +
    CCPIS$sexrole_giveup * AgencyFactorLoadings[5] +
    CCPIS$sexrole_selfconfident * AgencyFactorLoadings[6] +
    CCPIS$sexrole_inferior * AgencyFactorLoadings[7] +
    CCPIS$sexrole_underpressure * AgencyFactorLoadings[8]) /
  sum(AgencyFactorLoadings) # 0 = not agentic, 1 = agentic
length(na.omit(CCPIS$agentic)) / nrow(CCPIS) * 100 # 72% available data
CommunalityScale <- na.omit(CCPIS[, c(
  "sexrole_emotional", "sexrole_devote", "sexrole_gentle", "sexrole_helpful",
  "sexrole_kind", "sexrole_awarefeelings", "sexrole_understanding",
  "sexrole_warm")])
CommunalityCronbach <- round(as.numeric(psy::cronbach(CommunalityScale)[3]),
                             digits = 2)
CommunalityFactorAnalysis <- factanal(CommunalityScale, factors = 1)
CommunalityVariableNames <- c(
  "Very emotional", "Able to devote self\ncompletely to others",
  "Very gentle", "Very helpful to others", "Very kind",
  "Very aware of\nfeelings of others", "Very understanding\nof others",
  "Very warm in\nrelations with others")
CommunalityFactorLoadings <- CommunalityFactorAnalysis$loadings[, 1]
CommunalityFirstEigenvalue <- round(eigen(cor(CommunalityScale))$values[1],
                                    digits = 2)
ggplot(data.frame(CommunalityVariableNames, CommunalityFactorLoadings),
       aes(x = CommunalityVariableNames, y = CommunalityFactorLoadings)) +
  coord_flip() +
  geom_bar(stat = "identity", colour = "black", fill = "black", linewidth = 1,
           width = 0.4) +
  geom_text(aes(label = as.character(round(
    CommunalityFactorLoadings, digits = 2))), vjust = 0.35, hjust = -0.3,
    family = "CM Roman") +
  geom_hline(yintercept = 0.3, colour = "gray", linetype = "longdash") +
  annotate("text", label = paste("Cronbach's alpha =", as.character(
    CommunalityCronbach)), x = 1.2, y = 0.85, size = 3.8,
    family = "CM Roman") +
  annotate("text", label = paste("First eigenvalue =", as.character(
    CommunalityFirstEigenvalue)), x = 0.8, y = 0.85, size = 3.8,
    family = "CM Roman") +
  scale_y_continuous(name = "Factor loadings", limits = c(-0.1, 1),
                     breaks = seq(-0.1, 1, by = 0.1)) +
  xlab("") +
  theme_linedraw() +
  theme(axis.text.y = element_text(size = 14),
        axis.title.x = element_text(hjust = 0.3, vjust = -0.17, size = 14),
        panel.grid = element_blank(),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/CommunalityScale.pdf", width = 11, height = 4.25)
CCPIS$communal <- (
  CCPIS$sexrole_emotional * CommunalityFactorLoadings[1] +
    CCPIS$sexrole_devote * CommunalityFactorLoadings[2] +
    CCPIS$sexrole_gentle * CommunalityFactorLoadings[3] +
    CCPIS$sexrole_helpful * CommunalityFactorLoadings[4] +
    CCPIS$sexrole_kind * CommunalityFactorLoadings[5] +
    CCPIS$sexrole_awarefeelings * CommunalityFactorLoadings[6] +
    CCPIS$sexrole_understanding * CommunalityFactorLoadings[7] +
    CCPIS$sexrole_warm * CommunalityFactorLoadings[8]) /
  sum(CommunalityFactorLoadings) # 0 = not communal, 1 = communal
length(na.omit(CCPIS$communal)) / nrow(CCPIS) * 100 # 73% available data
CCPISBoys <- filter(CCPIS, female == 0)
CCPISGirls <- filter(CCPIS, female == 1)
mean(CCPIS$agentic, na.rm= T)
mean(CCPIS$communal, na.rm= T)

### 1.2.0 Census ####
#Census16 <- readstata13::read.dta13( # Census microdata for raking
#  "_data/Census16/pumf-98M0001-E-2016-individuals_F1.dta")
#Censusdta <- readstata13::read.dta13( # Census microdata for raking
#  "_data/Census21/Data/Census_2021_Individual_pumf.dta")
# dta too long to run (116 seconds)
#Censusspss <- haven::read_spss(
# spss slightly longer to run than csv (49 vs. 43 sec)
#  "_data/Census21/Data/Census_2021_Individual_pumf.sav")
Census21 <- read.csv("_data/Census21/Data/data_donnees_2021_ind.csv")
Census21$ses_female <- 3 - Census21$Gender
Census21$age <- Census21$AGEGRP
Census21$age[Census21$age == 88] <- NA
Census21$ses_age <- NA
Census21$ses_age[Census21$age %in% seq(1, 10)] <- 1
Census21$ses_age[Census21$age %in% seq(11, 14)] <- 2
Census21$ses_age[Census21$age %in% seq(15, 21)] <- 3
Census21$adult <- 0
Census21$adult[Census21$age > 6] <- 1
Census21$lang <- NA
Census21$lang[Census21$HLMOSTEN == 1 & Census21$HLMOSTFR == 0] <- "Anglophone"
Census21$lang[Census21$HLMOSTEN == 0 & Census21$HLMOSTFR == 1] <- "Francophone"
Census21$lang[Census21$HLMOSTNO > 1 & Census21$HLMOSTNO < 88] <- "Allophone"
Census21$ses_franco <- NA
Census21$ses_franco[Census21$FOL %in% c(1, 3, 4)] <- 1
Census21$ses_franco[Census21$FOL == 2] <- 2
Census21$immig <- Census21$IMMSTAT
Census21$immig[Census21$immig == 88] <- NA
Census21$immig[Census21$immig == 3] <- 2
Census21$ethnicity <- Census21$DPGRSUM
Census21$ethnicity[Census21$ethnicity == 88] <- NA
Census21$white[Census21$ethnicity == 1] <- 2
Census21$white[Census21$ethnicity > 1] <- 1
Census21$education <- Census21$HDGREE
Census21$education[Census21$education == 88] <- NA
Census21$ses_education <- NA
Census21$ses_education[Census21$education %in% seq(1, 2)] <- 1
Census21$ses_education[Census21$education %in% seq(3, 7)] <- 2
Census21$ses_education[Census21$education %in% seq(8, 13)] <- 3
Census21$income <- Census21$CFInc
Census21$income[Census21$income == 88] <- NA
Census21$ses_income <- NA
Census21$ses_income[Census21$income %in% seq(1, 16)] <- 1 # 0-60000
Census21$ses_income[Census21$income %in% seq(17, 25)] <- 2 # 60000-110000
Census21$ses_income[Census21$income %in% seq(26, 33)] <- 3 # 110000-...

# Adult data
CensusAdult <- filter(Census21, adult == 1) # 782,545 adults or 79.8% of sample
prop.table(table(CensusAdult$ses_female)) # 51.1% of women
cumsum(prop.table(table(CensusAdult$age))) # median age: 45-49
prop.table(table(CensusAdult$lang, useNA = "always")) # 63.4% anglo,
# 19.2% franco, 16.5% allo (first language spoken at home)
prop.table(table(CensusAdult$immig)) # 70.5% born in Canada
prop.table(table(CensusAdult$ethnicity)) # 70.2% white
prop.table(table(CensusAdult$PR)) # 22.9% Quebec, 38.8% Ontario
cumsum(prop.table(table(CensusAdult$education))) # 30.7% university degree
cumsum(prop.table(table(CensusAdult$income))) # yearly median between
# $85000 and $89999 (before tax)

# Adult Quebec data
CensusQcAdult <- filter(CensusAdult, PR == 24) # 179,178 adults or 18.3%
prop.table(table(CensusQcAdult$ses_female)) # 50.7% of women
cumsum(prop.table(table(CensusQcAdult$age))) # median age: 50-54
prop.table(table(CensusQcAdult$lang, useNA = "always")) # 10.5% anglo,
# 77.6% franco, 10.2% allo (first language spoken at home)
prop.table(table(CensusQcAdult$immig)) # 80.7% born in Canada
prop.table(table(CensusQcAdult$ethnicity)) # 82.1% white
cumsum(prop.table(table(CensusQcAdult$education))) # 28.2% university degree
cumsum(prop.table(table(CensusQcAdult$income))) # yearly median between
# $80000 and $84999 (before tax)
cumsum(prop.table(table(CensusQcAdult$ses_income)))
cumsum(prop.table(table(CensusQcAdult$ses_education)))
cumsum(prop.table(table(CensusQcAdult$ses_age)))
prop.table(table(CensusQcAdult$ses_franco))

# Teen data
CensusTeen <- filter(Census21, AGEGRP %in% c(5, 6))
# 66,774 teenagers or 6.8% of sample
prop.table(table(CensusTeen$ses_female)) # 51.5% of men
prop.table(table(CensusTeen$lang, useNA = "always")) # 67.6% anglo,
# 18.3% franco, 13.2% allo (first language spoken at home)
prop.table(table(CensusTeen$immig)) # 86.8% born in Canada
prop.table(table(CensusTeen$ethnicity)) # 59.8% white

# Full data
prop.table(table(Census21$ses_female))
18226240/36991980 # female
prop.table(table(Census21$age, useNA = "always"))
1831195/36991980 # age group 0-4
prop.table(table(Census21$ethnicity))
26689275/36328480 # not visible minority... doesn't match with white
1547870/36328480 # quite close to #4 (Black)
2571400/36328480 # quite close to #2 (South Asian)

#### 1.2 Datagotchi PES ####
DGFR <- read.csv("_data/DatagotchiPES/PES_prov2022_April+6,+2023_22.00.csv")
DGEN <- read.csv("_data/DatagotchiPES/EN_PES_prov2022_April+6,+2023_22.00.csv")
DG <- bind_rows(DGFR, DGEN) |>
  filter(str_detect(StartDate, "^20")) # remove column descriptions
DG$female <- NA
DG$female[DG$ses_gender %in% c(
  "Male", "Masculin", "Masculin (homme trans)")] <- 0
DG$female[DG$ses_gender %in% c(
  "Female", "Female (trans female)", "Féminin", "Féminin (femme trans)")] <- 1
DG$female <- as.factor(DG$female)
DG$female_alt <- NA
DG$female_alt[DG$ses_gender %in% c(
  "Male", "Masculin", "Masculin (homme trans)")] <- "Men"
DG$female_alt[DG$ses_gender %in% c(
  "Female", "Female (trans female)", "Féminin", "Féminin (femme trans)")] <-
  "Women"
DG$female_alt[DG$ses_gender %in% c(
  "Queer", "Non-binaire", "Non-binary", "Agender", "Agenre")] <- "Other"
DG$female_alt <- factor(DG$female_alt, levels = c("Men", "Women", "Other"))
DG$ses_female <- as.numeric(DG$female)
DG$age <- as.numeric(DG$ses_age)
table(DG$age)
DG$age_squared <- DG$age ^ 2
DG$age_low <- NA
DG$age_low[DG$age >= 35] <- 0
DG$age_low[DG$age < 35] <- 1
DG$age_mid <- NA
DG$age_mid[DG$age < 35 | DG$age > 54] <- 0
DG$age_mid[DG$age >= 35 & DG$age <= 54] <- 1
DG$age_high <- NA
DG$age_high[DG$age <= 54] <- 0
DG$age_high[DG$age > 54] <- 1
DG$ses_age <- NA
DG$ses_age[DG$age < 35] <- 1
DG$ses_age[DG$age >= 35 & DG$age <= 54] <- 2
DG$ses_age[DG$age > 54] <- 3
DG$lang <- "English"
DG$lang[DG$QlangueSplit == "Français"] <- "French"
DG$ses_franco <- NA
DG$ses_franco[DG$lang == "English"] <- 1
DG$ses_franco[DG$lang == "French"] <- 2
DG$immig <- 2
DG$immig[DG$ses_birth_country == "Canada"] <- 1
DG$immig[is.na(DG$ses_birth_country) | DG$ses_birth_country == ""] <- NA
DG$ethnicity <- NA
DG$ethnicity[DG$ses_ethnicity %in% c("Blanc", "White")] <- "White"
DG$ethnicity[DG$ses_ethnicity == "Noir"] <- "Black"
DG$ethnicity[DG$ses_ethnicity %in% c("Asian", "Asiatique")] <- "Asian"
DG$ethnicity[DG$ses_ethnicity %in% c("Arab", "Arabe")] <- "Arabic"
DG$ethnicity[DG$ses_ethnicity %in% c("Hispanic", "Hispanique")] <- "Hispanic"
DG$ethnicity[DG$ses_ethnicity == "Autochtone"] <- "Indigenous"
DG$ethnicity[DG$ses_ethnicity %in% c("Autre", "Other")] <- "Other"
table(DG$ethnicity, useNA = "always")
DG$white <- 0
DG$white[DG$ethnicity == "White"] <- 1
DG$white[is.na(DG$ethnicity)] <- NA
table(DG$white, useNA = "always")
DG$white <- 0
DG$white[DG$ses_ethnicity %in% c("Blanc", "White")] <- 1
DG$white[is.na(DG$ses_ethnicity) | DG$ses_ethnicity == ""] <- NA
DG$education <- DG$ses_education
DG$educ_low <- NA
DG$educ_low[DG$education %in% c(
  "Baccalauréat", "Bachelor's degree", "Collège, CÉGEP ou Collège classique",
  "Doctorat", "Doctorate", "Maîtrise", "Master's degree",
  "Technical, community college, CEGEP or College classique")] <- 0
DG$educ_low[DG$education %in% c(
  "Aucune scolarité", "École primaire", "École secondaire",
  "High school")] <- 1
DG$educ_mid <- NA
DG$educ_mid[DG$education %in% c(
  "Aucune scolarité", "École primaire", "École secondaire", "High school",
  "Baccalauréat", "Bachelor's degree", "Doctorat", "Doctorate", "Maîtrise",
  "Master's degree")] <- 0
DG$educ_mid[DG$education %in% c(
  "Collège, CÉGEP ou Collège classique",
  "Technical, community college, CEGEP or College classique")] <- 1
DG$educ_high <- NA
DG$educ_high[DG$education %in% c(
  "Aucune scolarité", "École primaire", "École secondaire", "High school",
  "Collège, CÉGEP ou Collège classique",
  "Technical, community college, CEGEP or College classique")] <- 0
DG$educ_high[DG$education %in% c(
  "Baccalauréat", "Bachelor's degree", "Doctorat", "Doctorate", "Maîtrise",
  "Master's degree")] <- 1
DG$education[DG$education == "Baccalauréat"] <- "Bachelor's degree"
DG$education[DG$education == "Collège, CÉGEP ou Collège classique"] <-
  "Technical, community\ncollege, CEGEP or\nCollege classique"
DG$education[DG$education ==
               "Technical, community college, CEGEP or College classique"] <-
  "Technical, community\ncollege, CEGEP or\nCollege classique"
DG$education[DG$education == "Doctorat"] <- "Doctorate"
DG$education[DG$education == "Maîtrise"] <- "Master's degree"
DG$education[DG$education == "Aucune scolarité"] <- "No schooling"
DG$education[DG$education == "École primaire"] <- "Elementary school"
DG$education[DG$education == "École secondaire"] <- "High school"
DG$education[DG$education == ""] <- NA
DG$education <- factor(DG$education, levels = c(
  "No schooling", "Elementary school", "High school",
  "Technical, community\ncollege, CEGEP or\nCollege classique",
  "Bachelor's degree", "Master's degree", "Doctorate"))
table(DG$education, useNA = "always")
DG$ses_education <- NA
DG$ses_education[DG$education %in% c(
  "No schooling", "Elementary school", "High school")] <- 1
DG$ses_education[DG$education == paste0("Technical, community college,\n",
                                        "CEGEP or College classique")] <- 2
DG$ses_education[DG$education %in% c(
  "Bachelor's degree", "Doctorate", "Master's degree")] <- 3
DG$income <- DG$ses_income
DG$income[DG$income == "Aucun revenu"] <- "No income"
DG$income[DG$income == "1$ à 30 000$"] <- "$1 to $30 000"
DG$income[DG$income == "30 001$ à 60 000$"] <- "$30 001 to $60 000"
DG$income[DG$income == "60 001$ à 90 000$"] <- "$60 001 to $90 000"
DG$income[DG$income == "90 001 à 110 000$"] <- "$90 001 to $110 000"
DG$income[DG$income == "110 001$ à 150 000$"] <- "$110 001 to $150 000"
DG$income[DG$income == "150 001$ à 200 000$"] <- "$150 001 to $200 000"
DG$income[DG$income == "Plus de 200 000$"] <- "More than $200 000"
DG$income[DG$income == ""] <- NA
DG$income <- factor(DG$income, levels = c(
  "No income", "$1 to $30 000", "$30 001 to $60 000", "$60 001 to $90 000",
  "$90 001 to $110 000", "$110 001 to $150 000", "$150 001 to $200 000",
  "More than $200 000"))
table(DG$income, useNA = "always")
DG$income_low <- NA
DG$income_low[DG$income %in% c(
  "$110 001 to $150 000", "$150 001 to $200 000", "$60 001 to $90 000",
  "$90 001 to $110 000", "110 001$ à 150 000$", "150 001$ à 200 000$",
  "60 001$ à 90 000$", "90 001 à 110 000$", "More than $200 000",
  "Plus de 200 000$")] <- 0
DG$income_low[DG$income %in% c(
  "$1 to $30 000", "$30 001 to $60 000", "1$ à 30 000$", "30 001$ à 60 000$",
  "Aucun revenu", "No income")] <- 1
DG$income_mid <- NA
DG$income_mid[DG$income %in% c(
  "$1 to $30 000", "$30 001 to $60 000", "1$ à 30 000$", "30 001$ à 60 000$",
  "Aucun revenu", "No income", "$150 001 to $200 000", "150 001$ à 200 000$",
  "More than $200 000", "Plus de 200 000$")] <- 0
DG$income_mid[DG$income %in% c(
  "$110 001 to $150 000", "$60 001 to $90 000", "$90 001 to $110 000",
  "110 001$ à 150 000$", "60 001$ à 90 000$", "90 001 à 110 000$")] <- 1
DG$income_high <- NA
DG$income_high[DG$income %in% c(
  "$1 to $30 000", "$30 001 to $60 000", "1$ à 30 000$", "30 001$ à 60 000$",
  "Aucun revenu", "No income", "$110 001 to $150 000", "$60 001 to $90 000",
  "$90 001 to $110 000", "110 001$ à 150 000$", "60 001$ à 90 000$",
  "90 001 à 110 000$")] <- 0
DG$income_high[DG$income %in% c(
  "$150 001 to $200 000", "150 001$ à 200 000$", "More than $200 000",
  "Plus de 200 000$")] <- 1
DG$ses_income <- NA
DG$ses_income[DG$income %in% c(
  "$1 to $30 000", "$30 001 to $60 000", "No income")] <- 1
DG$ses_income[DG$income %in% c(
  "$110 001 to $150 000", "$60 001 to $90 000", "$90 001 to $110 000")] <- 2
DG$ses_income[DG$income %in% c(
  "$150 001 to $200 000", "More than $200 000")] <- 3
DG$interest <- as.numeric(DG$pol_interest_1)
DG$interest_health <- as.numeric(DG$issues_interest_1)
DG$interest_foreign <- as.numeric(DG$issues_interest_2)
DG$interest_law <- as.numeric(DG$issues_interest_3)
DG$interest_education <- as.numeric(DG$issues_interest_4)
DG$interest_partisan <- as.numeric(DG$issues_interest_5)

### 1.2.1 Weighting ####
calculate_unweighted_props <- function(data, variable) {
  data |> # calculate proportions for one variable
    select({{variable}}) |>
    group_by({{variable}}) |>
    summarise(n = n()) |>
    na.omit() |>
    mutate(prop = n / sum(n))
}
calculate_5_unweighted_props <- function(
    data, variable1, variable2, variable3, variable4, variable5) {
  Prop1 <- calculate_unweighted_props(data, # calculate proportions for
                                      variable = {{variable1}}) # variable 1
  Prop2 <- calculate_unweighted_props(data, variable = {{variable2}})
  Prop3 <- calculate_unweighted_props(data, variable = {{variable3}})
  Prop4 <- calculate_unweighted_props(data, variable = {{variable4}})
  Prop5 <- calculate_unweighted_props(data, variable = {{variable5}})
  DataProp <- bind_rows(Prop1, Prop2, Prop3, Prop4, Prop5) |>
    pivot_longer(!c(n, prop)) |>
    na.omit() |>
    select(name, value, n, prop)
  return(DataProp) # calculate proportions for multiple variables
}
add_raking_weights_column_5_var <- function(
    popData, sampleData, variable1, variable2, variable3, variable4,
    variable5) {
  sampleData$mergeId <- 1:nrow(sampleData) # add a variable for row number
  subsetRaking <- sampleData |> # keep only relevant variables
    select(mergeId, {{variable1}}, {{variable2}}, {{variable3}},
           {{variable4}}, {{variable5}}) |>
    as.data.frame() # transform into data.frame
  popProps <- calculate_5_unweighted_props(data = popData, # population data
                                           variable1 = {{variable1}},
                                           variable2 = {{variable2}},
                                           variable3 = {{variable3}},
                                           variable4 = {{variable4}},
                                           variable5 = {{variable5}})
  targets <- unstack(popProps, form = prop ~ name)
  # transform data.frame into list (needed for anesrake)
  raking <- anesrake::anesrake(
    inputter = targets, # target proportions from the population
    dataframe = subsetRaking, # sample data
    caseid = subsetRaking$mergeId,
    cap = 5, # maximum value the weight variable is allowed to take
    type = "pctlim", # among the 5 SES variables, only those whose
    # proportions deviate  enough from the population proportion are included
    pctlim = 5, # the "enough" on the previous line is set to 5 percentage
    # points
    choosemethod = "total") # this 5 points applies to all variable values
  # added together
  sampleData$weightRaking <- raking$weightvec # add raking weights column
  # to sample
  return(sampleData)
}
DG2 <- DG[complete.cases(DG$ses_income),]
DG <- add_raking_weights_column_5_var(
  popData = Census21,
  sampleData = DG2,
  variable1 = ses_female,
  variable2 = ses_education,
  variable3 = ses_franco,
  #variable4 = ses_age,
  variable5 = ses_income)
summary(DG$weightRaking)
prop.table(table(CensusQcAdult$ses_female))
prop.table(table(DG$ses_female))
prop.table(questionr::wtd.table(DG$ses_female, weights = DG$weightRaking))
prop.table(table(CensusQcAdult$ses_education))
prop.table(table(DG$ses_education))
prop.table(questionr::wtd.table(DG$ses_education, weights = DG$weightRaking))
prop.table(table(CensusQcAdult$ses_franco))
prop.table(table(DG$ses_franco))
prop.table(questionr::wtd.table(DG$ses_franco, weights = DG$weightRaking))
prop.table(table(CensusQcAdult$ses_age))
prop.table(table(DG$ses_age))
prop.table(questionr::wtd.table(DG$ses_age, weights = DG$weightRaking))
prop.table(table(CensusQcAdult$ses_income))
prop.table(table(DG$ses_income))
prop.table(questionr::wtd.table(DG$ses_income, weights = DG$weightRaking))

#### 1.3 CES ####
CES97 <- readstata13::read.dta13("_data/CES/CES97/CES97.dta")
CES97$female <- NA
CES97$female[CES97$cpsrgen == "male"] <- 0
CES97$female[CES97$cpsrgen == "female"] <- 1
CES97$female <- as.factor(CES97$female)
CES97$interest <- NA
CES97$interest <- CES97$cpsb5 * 10
# 0 not interested in politics, 100 interested in politics
CES97$interest[CES97$interest > 100] <- NA
CES97$interest <- as.integer(CES97$interest)
CES97$age <- CES97$cpsage
CES97$age[CES97$cpsage > 1979] <- NA
CES97$age <- 1997 - CES97$age
CES97men <- filter(CES97, female == 0)
CES97women <- filter(CES97, female == 1)

CES00 <- readstata13::read.dta13("_data/CES/CES00/CES00.dta")
CES00$female <- NA
CES00$female[CES00$cpsrgen == "male"] <- 0
CES00$female[CES00$cpsrgen == "female"] <- 1
CES00$female <- as.factor(CES00$female)
CES00$interest <- NA
CES00$interest <- CES00$cpsb5 * 10
# 0 not interested in politics, 100 interested in politics
CES00$interest[CES00$interest > 100] <- NA
CES00$interest <- as.integer(CES00$interest)
CES00$age <- CES00$cpsage
CES00$age[CES00$cpsage > 1982] <- NA
CES00$age <- 2000 - CES00$age
CES00men <- filter(CES00, female == 0)
CES00women <- filter(CES00, female == 1)

CES0411 <- readstata13::read.dta13("_data/CES/CES04060811.dta")
CES04 <- filter(CES0411, str_detect(CES0411$Survey_Type04060811,
                                    "CPS04"))
CES04$female <- NA
CES04$female[CES04$gender == "Male"] <- 0
CES04$female[CES04$gender == "Female"] <- 1
CES04$female <- as.factor(CES04$female)
CES04$interest <- NA
CES04$interest <- CES04$ces04_CPS_A6 * 10
# 0 not interested in politics, 100 interested in politics
CES04$interest[CES04$interest > 100] <- NA
CES04$interest <- as.integer(CES04$interest)
CES04$age <- CES04$YEARofBIRTH
CES04$age[CES04$YEARofBIRTH > 1986] <- NA
CES04$age <- 2004 - CES04$age
CES04men <- filter(CES04, female == 0)
CES04women <- filter(CES04, female == 1)
CES06 <- filter(CES0411, str_detect(CES0411$Survey_Type04060811,
                                    "CPS06"))
CES06$female <- NA
CES06$female[CES06$gender == "Male"] <- 0
CES06$female[CES06$gender == "Female"] <- 1
CES06$female <- as.factor(CES06$female)
CES06$interest <- NA
CES06$interest <- CES06$ces06_CPS_A8 * 10
# 0 not interested in politics, 100 interested in politics
CES06$interest[CES06$interest > 100] <- NA
CES06$interest <- as.integer(CES06$interest)
CES06$age <- CES06$YEARofBIRTH
CES06$age[CES06$YEARofBIRTH > 1988] <- NA
CES06$age <- 2006 - CES06$age
CES06men <- filter(CES06, female == 0)
CES06women <- filter(CES06, female == 1)
CES08 <- filter(CES0411, str_detect(CES0411$Survey_Type04060811,
                                    "CPS08"))
CES08$female <- NA
CES08$female[CES08$gender == "Male"] <- 0
CES08$female[CES08$gender == "Female"] <- 1
CES08$female <- as.factor(CES08$female)
CES08$interest <- NA
CES08$interest <- CES08$ces08_CPS_A4 * 10
# 0 not interested in politics, 100 interested in politics
CES08$interest[CES08$interest > 100] <- NA
CES08$interest <- as.integer(CES08$interest)
CES08$age <- CES08$YEARofBIRTH
CES08$age[CES08$YEARofBIRTH > 1990] <- NA
CES08$age <- 2008 - CES08$age
CES08men <- filter(CES08, female == 0)
CES08women <- filter(CES08, female == 1)
CES11 <- filter(CES0411, str_detect(CES0411$Survey_Type04060811,
                                    "CPS11"))
CES11$female <- NA
CES11$female[CES11$gender == "Male"] <- 0
CES11$female[CES11$gender == "Female"] <- 1
CES11$female <- as.factor(CES11$female)
CES11$interest <- NA
CES11$interest <- CES11$PES11_60 * 10
# 0 not interested in politics, 100 interested in politics
CES11$interest[CES11$interest > 100] <- NA
CES11$interest <- as.integer(CES11$interest)
CES11$age <- CES11$YEARofBIRTH
CES11$age[CES11$YEARofBIRTH > 1993] <- NA
CES11$age <- 2011 - CES11$age
CES11men <- filter(CES11, female == 0)
CES11women <- filter(CES11, female == 1)

CES15 <- readstata13::read.dta13(
  "_data/CES/CES15/CES15-online+phone.dta")
# CES15online <- readstata13::read.dta13(
#   "_data/CES/CES15/CES15-online.dta") # includes more respondents
# CES15phone <- readstata13::read.dta13(
#   "_data/CES/CES15/CES15-phone.dta")
CES15$female <- NA
CES15$female[CES15$sex_r == "Male"] <- 0
CES15$female[CES15$sex_r == "Female"] <- 1
CES15$female <- as.factor(CES15$female)
CES15$interest <- NA
CES15$interest <- CES15$p_intpol * 10
# 0 not interested in politics, 100 interested in politics
CES15$interest[CES15$interest > 100] <- NA
CES15$interest <- as.integer(CES15$interest)
CES15$age[CES15$age > 1997 | CES15$age < 1900] <- NA
CES15$age <- 2015 - CES15$age
CES15men <- filter(CES15, female == 0)
CES15women <- filter(CES15, female == 1)

CES19online <- haven::read_dta(
  "_data/CES/CES19/CES19-online.dta")
CES19online$female <- NA
CES19online$female[CES19online$cps19_gender == 1] <- 0
CES19online$female[CES19online$cps19_gender == 2] <- 1
CES19online$female <- as.factor(CES19online$female)
CES19online$interest <- NA
CES19online$interest <- CES19online$cps19_interest_gen_1 * 10
# 0 uninterested in politics, 100 interested
CES19online$interest <- as.integer(CES19online$interest)
CES19online$age <- CES19online$cps19_age
CES19phone <- readstata13::read.dta13("_data/CES/CES19/CES19-phone.dta")
CES19phone$female <- NA
CES19phone$female[CES19phone$q3 == "(1) Male"] <- 0
CES19phone$female[CES19phone$q3 == "(2) Female"] <- 1
CES19phone$female <- as.factor(CES19phone$female)
CES19phone$interest <- NA
CES19phone$interest[CES19phone$p27 ==
                      "(0) 0 - No interest at all"] <- 0
CES19phone$interest[CES19phone$p27 == "(1) 1"] <- 10
CES19phone$interest[CES19phone$p27 == "(2) 2"] <- 20
CES19phone$interest[CES19phone$p27 == "(3) 3"] <- 30
CES19phone$interest[CES19phone$p27 == "(4) 4"] <- 40
CES19phone$interest[CES19phone$p27 == "(5) 5"] <- 50
CES19phone$interest[CES19phone$p27 == "(6) 6"] <- 60
CES19phone$interest[CES19phone$p27 == "(7) 7"] <- 70
CES19phone$interest[CES19phone$p27 == "(8) 8"] <- 80
CES19phone$interest[CES19phone$p27 == "(9) 9"] <- 90
CES19phone$interest[CES19phone$p27 ==
                      "(10) 10 - A great deal of interest"] <- 100
CES19phone$interest <- as.integer(CES19phone$interest)
CES19phone$age <- 2019 - CES19phone$q2
CES19 <- data.frame(
  id = c(CES19online$cps19_ResponseId, CES19phone$sample_id),
  # merge online + phone data
  mode = c(rep("online", length(CES19online$cps19_ResponseId)),
           rep("phone", length(CES19phone$sample_id))),
  female = as.factor(c(CES19online$female, CES19phone$female)),
  interest = as.integer(c(CES19online$interest, CES19phone$interest)),
  age = c(CES19online$age, CES19phone$age),
  weight = c(CES19online$cps19_weight_general_all,
             CES19phone$weight_CES))
CES19men <- filter(CES19, female == 0)
CES19women <- filter(CES19, female == 1)

CES21 <- readstata13::read.dta13("_data/CES/CES21/CES21.dta")
CES21$ethnicity <- NA
CES21$ethnicity[
  CES21$cps21_vismin_1 == -99 & CES21$cps21_vismin_2 == -99 &
    CES21$cps21_vismin_3 == -99 & CES21$cps21_vismin_4 == -99 &
    CES21$cps21_vismin_5 == -99 & CES21$cps21_vismin_6 == -99 &
    CES21$cps21_vismin_7 == -99 & CES21$cps21_vismin_8 == -99 &
    CES21$cps21_vismin_9 == 1 & CES21$cps21_vismin_10 == -99 &
    CES21$cps21_vismin_1 == -99] <- "White"
CES21$ethnicity[
  CES21$cps21_vismin_1 == -99 & CES21$cps21_vismin_2 == -99 &
    CES21$cps21_vismin_3 == 1 & CES21$cps21_vismin_4 == -99 &
    CES21$cps21_vismin_5 == -99 & CES21$cps21_vismin_6 == -99 &
    CES21$cps21_vismin_7 == -99 & CES21$cps21_vismin_8 == -99 &
    CES21$cps21_vismin_9 == -99 & CES21$cps21_vismin_10 == -99 &
    CES21$cps21_vismin_1 == -99] <- "Black"
CES21$ethnicity[
  CES21$cps21_vismin_1 == -99 & CES21$cps21_vismin_2 == -99 &
    CES21$cps21_vismin_3 == -99 & CES21$cps21_vismin_4 == -99 &
    CES21$cps21_vismin_5 == -99 & CES21$cps21_vismin_6 == -99 &
    CES21$cps21_vismin_7 == -99 & CES21$cps21_vismin_8 == 1 &
    CES21$cps21_vismin_9 == -99 & CES21$cps21_vismin_10 == -99 &
    CES21$cps21_vismin_1 == -99] <- "West Asian"
CES21$ethnicity[
  CES21$cps21_vismin_1 == -99 & CES21$cps21_vismin_2 == -99 &
    CES21$cps21_vismin_3 == -99 & CES21$cps21_vismin_4 == -99 &
    CES21$cps21_vismin_5 == -99 & CES21$cps21_vismin_6 == -99 &
    CES21$cps21_vismin_7 == 1 & CES21$cps21_vismin_8 == -99 &
    CES21$cps21_vismin_9 == -99 & CES21$cps21_vismin_10 == -99 &
    CES21$cps21_vismin_1 == -99] <- "Southeast Asian"
CES21$ethnicity[
  CES21$cps21_vismin_1 == 1 & CES21$cps21_vismin_2 == -99 &
    CES21$cps21_vismin_3 == -99 & CES21$cps21_vismin_4 == -99 &
    CES21$cps21_vismin_5 == -99 & CES21$cps21_vismin_6 == -99 &
    CES21$cps21_vismin_7 == -99 & CES21$cps21_vismin_8 == -99 &
    CES21$cps21_vismin_9 == -99 & CES21$cps21_vismin_10 == -99 &
    CES21$cps21_vismin_1 == -99] <- "Arabic"
CES21$ethnicity[
  CES21$cps21_vismin_1 == -99 & CES21$cps21_vismin_2 == -99 &
    CES21$cps21_vismin_3 == -99 & CES21$cps21_vismin_4 == -99 &
    CES21$cps21_vismin_5 == -99 & CES21$cps21_vismin_6 == 1 &
    CES21$cps21_vismin_7 == -99 & CES21$cps21_vismin_8 == -99 &
    CES21$cps21_vismin_9 == -99 & CES21$cps21_vismin_10 == -99 &
    CES21$cps21_vismin_1 == -99] <- "South Asian"
CES21$ethnicity[
  CES21$cps21_vismin_1 == -99 & CES21$cps21_vismin_2 == -99 &
    CES21$cps21_vismin_3 == -99 & CES21$cps21_vismin_4 == -99 &
    CES21$cps21_vismin_5 == 1 & CES21$cps21_vismin_6 == -99 &
    CES21$cps21_vismin_7 == -99 & CES21$cps21_vismin_8 == -99 &
    CES21$cps21_vismin_9 == -99 & CES21$cps21_vismin_10 == -99 &
    CES21$cps21_vismin_1 == -99] <- "Hispanic"
CES21$ethnicity[
  CES21$cps21_vismin_1 == -99 & CES21$cps21_vismin_2 == -99 &
    CES21$cps21_vismin_3 == -99 & CES21$cps21_vismin_4 == 1 &
    CES21$cps21_vismin_5 == -99 & CES21$cps21_vismin_6 == -99 &
    CES21$cps21_vismin_7 == -99 & CES21$cps21_vismin_8 == -99 &
    CES21$cps21_vismin_9 == -99 & CES21$cps21_vismin_10 == -99 &
    CES21$cps21_vismin_1 == -99] <- "Indigenous"
CES21$ethnicity[
  CES21$cps21_vismin_1 == -99 & CES21$cps21_vismin_2 == -99 &
    CES21$cps21_vismin_3 == -99 & CES21$cps21_vismin_4 == -99 &
    CES21$cps21_vismin_5 == -99 & CES21$cps21_vismin_6 == -99 &
    CES21$cps21_vismin_7 == -99 & CES21$cps21_vismin_8 == -99 &
    CES21$cps21_vismin_9 == -99 & CES21$cps21_vismin_10 == 1 &
    CES21$cps21_vismin_1 == -99] <- "Other"
CES21$female <- NA
CES21$female[CES21$cps21_genderid == "A man"] <- 0
CES21$female[CES21$cps21_genderid == "A woman"] <- 1
CES21$female <- as.factor(CES21$female)
CES21$female_alt <- NA
CES21$female_alt[CES21$cps21_genderid == "A man"] <- "Men"
CES21$female_alt[CES21$cps21_genderid == "A woman"] <- "Women"
CES21$female_alt[CES21$cps21_genderid %in% c(
  "Non-binary", "Another gender, please specify:")] <- "Other"
CES21$female_alt <- factor(CES21$female_alt, levels = c(
  "Men", "Women", "Other"))
CES21$lang <- "Allophone"
CES21$lang[CES21$pes21_lang == "French"] <- "Francophone"
CES21$lang[CES21$pes21_lang == "English"] <- "Anglophone"
CES21$lang[CES21$lang %in% c("Don't know/ Prefer not to answer")] <- NA
CES21$interest <- NA
CES21$interest <- CES21$cps21_interest_gen * 10
CES21$interest[CES21$interest == -990] <- NA
# 0 uninterested in politics, 100 interested
CES21$interest <- as.integer(CES21$interest)
CES21$age <- CES21$cps21_age
CES21$immig <- ifelse(CES21$cps21_bornin_canada == "Yes", 1,
                      ifelse(CES21$cps21_bornin_canada == "No", 2, NA))
CES21$weight <- CES21$cps21_weight_general_all
CES21men <- filter(CES21, female == 0)
CES21women <- filter(CES21, female == 1)
CES21white <- filter(CES21, ethnicity == "White")
CES21black <- filter(CES21, ethnicity == "Black")
CES21westasian <- filter(CES21, ethnicity == "West Asian")
CES21southeastasian <- filter(CES21, ethnicity == "Southeast Asian")
CES21arabic <- filter(CES21, ethnicity == "Arabic")
CES21southasian <- filter(CES21, ethnicity == "South Asian")
CES21hispanic <- filter(CES21, ethnicity == "Hispanic")
CES21indigenous <- filter(CES21, ethnicity == "Indigenous")
CES21other <- filter(CES21, ethnicity == "Other")
CES21whitemen <- filter(CES21, female == 0 & ethnicity == "White")
CES21whitewomen <- filter(CES21, female == 1 & ethnicity == "White")
CES21nonwhitemen <- filter(CES21, female == 0 & ethnicity != "White")
CES21nonwhitewomen <- filter(CES21, female == 1 & ethnicity != "White")
CES21immigrantmen <- filter(CES21, female == 0 & immig == 2)
CES21immigrantwomen <- filter(CES21, female == 1 & immig == 2)
CES21nonimmigrantmen <- filter(CES21, female == 0 & immig == 1)
CES21nonimmigrantwomen <- filter(CES21, female == 1 & immig == 1)
CES21$education <- as.character(CES21$cps21_education)
CES21$education[
  CES21$education == "Don't know/ Prefer not to answer"] <- NA
CES21$education[CES21$education == paste0(
  "Some technical, community college, CEGEP, College Classique")] <-
  "Some technical,\ncommunity college,\nCEGEP, College Classique"
CES21$education[CES21$education == paste0(
  "Completed technical, community college, CEGEP, College Classique")] <-
  "Completed technical,\ncommunity college,\nCEGEP, College Classique"
CES21$education[CES21$education == paste0(
  "Some secondary/ high school")] <-
  "Some secondary/\nhigh school"
CES21$education[CES21$education == paste0(
  "Some elementary school")] <-
  "Some elementary\nschool"
CES21$education[CES21$education == paste0(
  "Completed secondary/ high school")] <-
  "Completed secondary/\nhigh school"
CES21$education[CES21$education == paste0(
  "Completed elementary school")] <-
  "Completed\nelementary school"
CES21$education[CES21$education == paste0(
  "Professional degree or doctorate")] <-
  "Professional degree\nor doctorate"
CES21$education <- as.factor(CES21$education)
CES21$income <- NA
CES21$income[CES21$cps21_income_number < 1] <- "No income"
CES21$income[CES21$cps21_income_number >= 1 &
               CES21$cps21_income_number < 30000] <- "$1 to $30 000"
CES21$income[CES21$cps21_income_number >= 30000 &
               CES21$cps21_income_number < 60000] <- "$30 001 to $60 000"
CES21$income[CES21$cps21_income_number >= 60000 &
               CES21$cps21_income_number < 90000] <- "$60 001 to $90 000"
CES21$income[CES21$cps21_income_number >= 90000 &
               CES21$cps21_income_number < 110000] <- "$90 001 to $110 000"
CES21$income[CES21$cps21_income_number >= 110000 &
               CES21$cps21_income_number < 150000] <- "$110 001 to $150 000"
CES21$income[CES21$cps21_income_number >= 150000 &
               CES21$cps21_income_number < 200000] <- "$150 001 to $200 000"
CES21$income[CES21$cps21_income_number >= 200000] <- "More than $200 000"
CES21$income <- factor(CES21$income, levels = c(
  "No income", "$1 to $30 000", "$30 001 to $60 000", "$60 001 to $90 000",
  "$90 001 to $110 000", "$110 001 to $150 000", "$150 001 to $200 000",
  "More than $200 000"))
CES21$province <- CES21$pes21_province # no data about territories
CES21$province[CES21$province == "Newfoundland and Labrador"] <-
  "Newfoundland\nand Labrador"
CES21$province[CES21$province == "Northwest Territories"] <-
  "Northwest\nTerritories"
CES21$province[CES21$province == "Prince Edward Island"] <-
  "Prince Edward\nIsland"
prop.table(table(CES21$female))
prop.table(table(CES21$lang))
prop.table(table(CES21$education))
prop.table(table(CES21$immig))
prop.table(table(CES21$ethnicity))
cumsum(prop.table(table(CES21$income)))
cumsum(prop.table(table(CES21$age)))
prop.table(table(CES21$province))
summary(CES21$weight)

#### 1.4 WVS ####
WVS <- readRDS("_data/WVS/WVS_TimeSeries_1981_2022_Rds_v3_0.rds")
WVS$ethnicity <- NA
WVS$ethnicity[WVS$X051 == 124001] <- "White"
WVS$ethnicity[WVS$X051 == 124002] <- "Black"
WVS$ethnicity[WVS$X051 == 124003] <- "West Asian"
WVS$ethnicity[WVS$X051 == 124004] <- "Southeast Asian"
WVS$ethnicity[WVS$X051 == 124005] <- "Arabic"
WVS$ethnicity[WVS$X051 == 124006] <- "South Asian"
WVS$ethnicity[WVS$X051 == 124007] <- "Hispanic"
WVS$ethnicity[WVS$X051 == 124008] <- "Indigenous"
WVS$ethnicity[WVS$X051 == 124009] <- "Chinese"
WVS$ethnicity[WVS$X051 == 124010] <- "Filipino"
WVS$ethnicity[WVS$X051 == 124011] <- "Korean"
WVS$ethnicity[WVS$X051 == 124012] <- "Japanese"
WVS$ethnicity[WVS$X051 == 124999] <- "Other"
WVS$ethnicity <- as.factor(WVS$ethnicity)
WVS$female <- NA
WVS$female[WVS$X001 == 1] <- 0
WVS$female[WVS$X001 == 2] <- 1
WVS$female <- as.factor(WVS$female)
WVS$female_alt <- NA
WVS$female_alt[WVS$female == 0] <- "Men"
WVS$female_alt[WVS$female == 1] <- "Women"
WVS$female_alt <- factor(WVS$female_alt, levels = c("Men", "Women"))
WVS$interest <- NA
WVS$interest[WVS$E023 == 4] <- 0
WVS$interest[WVS$E023 == 3] <- (100 / 3)
WVS$interest[WVS$E023 == 2] <- (100 / 1.5)
WVS$interest[WVS$E023 == 1] <- 100
WVS$age <- WVS$X003
WVS$age[WVS$X003 < 0] <- NA
WVS$age <- as.numeric(WVS$age)
WVS$immig <- WVS$G027A
WVS$immig[WVS$G027A < 0] <- NA
WVS$immig <- as.factor(WVS$immig)
WVS$lang <- "Allophone"
WVS$lang[WVS$G016 == 1400] <- "Francophone"
WVS$lang[WVS$G016 == 1240] <- "Anglophone"
WVS$lang[WVS$G016 < 0] <- NA
WVS$lang <- as.factor(WVS$lang)
WVS$education <- as.numeric(WVS$X025A_01)
WVS$education[WVS$education < 0] <- NA
WVS$education[WVS$education == 0] <-
  "Early childhood\neducation/\nno education"
WVS$education[WVS$education == 1] <- "Primary education"
WVS$education[WVS$education == 2] <- "Lower secondary\neducation"
WVS$education[WVS$education == 3] <- "Upper secondary\neducation"
WVS$education[WVS$education == 4] <- "Post-secondary\nnon-tertiary\neducation"
WVS$education[WVS$education == 5] <- "Short-cycle\ntertiary education"
WVS$education[WVS$education == 6] <- "Bachelor or\nequivalent"
WVS$education[WVS$education == 7] <- "Master or\nequivalent"
WVS$education[WVS$education == 8] <- "Doctoral or\nequivalent"
WVS$education <- as.factor(WVS$education)
WVS$province <- NA
WVS$province[as.numeric(WVS$X048ISO) == 124001] <- "Alberta"
WVS$province[as.numeric(WVS$X048ISO) == 124002] <- "British Columbia"
WVS$province[as.numeric(WVS$X048ISO) == 124003] <- "Manitoba"
WVS$province[as.numeric(WVS$X048ISO) == 124004] <- "New Brunswick"
WVS$province[as.numeric(WVS$X048ISO) == 124005] <- "Newfoundland\nand Labrador"
WVS$province[as.numeric(WVS$X048ISO) == 124006] <- "Nova Scotia"
WVS$province[as.numeric(WVS$X048ISO) == 124007] <- "Ontario"
WVS$province[as.numeric(WVS$X048ISO) == 124008] <- "Prince Edward\nIsland"
WVS$province[as.numeric(WVS$X048ISO) == 124009] <- "Quebec"
WVS$province[as.numeric(WVS$X048ISO) == 124010] <- "Saskatchewan"
WVS$province[as.numeric(WVS$X048ISO) == 124011] <- "Northwest\nTerritories"
WVS$province[as.numeric(WVS$X048ISO) == 124012] <- "Nunavut"
WVS$province[as.numeric(WVS$X048ISO) == 124013] <- "Yukon"
WVS$province <- as.factor(WVS$province)
WVS$weight <- as.numeric(WVS$S017)
WVSWave7 <- filter(WVS, S020 %in% seq(2017, 2022))
length(table(WVSWave7$COUNTRY_ALPHA))
WVSWave7$canada <- 0
WVSWave7$canada[WVSWave7$COUNTRY_ALPHA == "CAN"] <- 1
WVSCA <- filter(WVS, COUNTRY_ALPHA == "CAN")
WVSCA90 <- filter(WVSCA, S020 == 1990) # no ethnicity variable
WVSCA90men <- filter(WVSCA90, female == 0)
WVSCA90women <- filter(WVSCA90, female == 1)
WVSCA00 <- filter(WVSCA, S020 == 2000)
WVSCA00men <- filter(WVSCA00, female == 0)
WVSCA00women <- filter(WVSCA00, female == 1)
WVSCA00whitemen <- filter(WVSCA00, female == 0 & ethnicity == "white")
WVSCA00whitewomen <- filter(WVSCA00, female == 1 &
                              ethnicity == "white")
WVSCA00nonwhitemen <- filter(WVSCA00, female == 0 &
                               ethnicity != "white")
WVSCA00nonwhitewomen <- filter(WVSCA00, female == 1 &
                                 ethnicity != "white")
WVSCA06 <- filter(WVSCA, S020 == 2006)
WVSCA06men <- filter(WVSCA06, female == 0)
WVSCA06women <- filter(WVSCA06, female == 1)
WVSCA06whitemen <- filter(WVSCA06, female == 0 & ethnicity == "white")
WVSCA06whitewomen <- filter(WVSCA06, female == 1 &
                              ethnicity == "white")
WVSCA06nonwhitemen <- filter(WVSCA06, female == 0 &
                               ethnicity != "white")
WVSCA06nonwhitewomen <- filter(WVSCA06, female == 1 &
                                 ethnicity != "white")
WVSCA20 <- filter(WVSCA, S020 == 2020)
WVSCA20men <- filter(WVSCA20, female == 0)
WVSCA20women <- filter(WVSCA20, female == 1)
WVSCA20white <- filter(WVSCA20, ethnicity == "white")
WVSCA20black <- filter(WVSCA20, ethnicity == "black")
WVSCA20westasian <- filter(WVSCA20, ethnicity == "westasian")
WVSCA20southeastasian <- filter(WVSCA20, ethnicity == "southeastasian")
WVSCA20arabic <- filter(WVSCA20, ethnicity == "arabic")
WVSCA20southasian <- filter(WVSCA20, ethnicity == "southasian")
WVSCA20hispanic <- filter(WVSCA20, ethnicity == "hispanic")
WVSCA20indigenous <- filter(WVSCA20, ethnicity == "indigenous")
WVSCA20chinese <- filter(WVSCA20, ethnicity == "chinese")
WVSCA20filipino <- filter(WVSCA20, ethnicity == "filipino")
WVSCA20korean <- filter(WVSCA20, ethnicity == "korean")
WVSCA20japanese <- filter(WVSCA20, ethnicity == "japanese")
WVSCA20other <- filter(WVSCA20, ethnicity == "other")
WVSCA20whitemen <- filter(WVSCA20, female == 0 & ethnicity == "white")
WVSCA20whitewomen <- filter(WVSCA20, female == 1 &
                              ethnicity == "white")
WVSCA20nonwhitemen <- filter(WVSCA20, female == 0 &
                               ethnicity != "white")
WVSCA20nonwhitewomen <- filter(WVSCA20, female == 1 &
                                 ethnicity != "white")
WVSCA20immigrantmen <- filter(WVSCA20, female == 0 & immig == 2)
WVSCA20immigrantwomen <- filter(WVSCA20, female == 1 & immig == 2)
WVSCA20nonimmigrantmen <- filter(WVSCA20, female == 0 & immig == 1)
WVSCA20nonimmigrantwomen <- filter(WVSCA20, female == 1 & immig == 1)
prop.table(table(WVSCA20$female))
prop.table(table(WVSCA20$lang))
prop.table(table(WVSCA20$education))
prop.table(table(WVSCA20$immig))
prop.table(table(WVSCA20$ethnicity))
cumsum(prop.table(table(WVSCA20$age)))
prop.table(table(WVSCA20$province))
summary(WVSCA20$weight)

#### 1.5 GSS ####
GSS13 <- readstata13::read.dta13("_data/GSS2013/gss-89M0032x-E-2013-c27_F1.dta")
GSS13$female <- NA
GSS13$female[GSS13$SEX == "Male"] <- 0
GSS13$female[GSS13$SEX == "Female"] <- 1
GSS13$female <- as.factor(GSS13$female)
GSS13$female_alt <- NA
GSS13$female_alt[GSS13$SEX == "Male"] <- "Men"
GSS13$female_alt[GSS13$SEX == "Female"] <- "Women"
GSS13$female_alt <- as.factor(GSS13$female_alt)
GSS13$age <- as.character(GSS13$AGEGR10)
GSS13$age[GSS13$age == "15 to 24 years"] <- "15-24"
GSS13$age[GSS13$age == "25 to 34 years"] <- "25-34"
GSS13$age[GSS13$age == "35 to 44 years"] <- "35-44"
GSS13$age[GSS13$age == "45 to 54 years"] <- "45-54"
GSS13$age[GSS13$age == "55 to 64 years"] <- "55-64"
GSS13$age[GSS13$age == "65 to 74 years"] <- "65-74"
GSS13$age[GSS13$age == "75 years and over"] <- "75+"
GSS13$age <- as.factor(GSS13$age)
GSS13$age_low <- 0
GSS13$age_low[GSS13$AGEGR10 %in% c("15 to 24 years", "25 to 34 years")] <- 1
GSS13$age_mid <- 0
GSS13$age_mid[GSS13$AGEGR10 %in% c("35 to 44 years", "45 to 54 years")] <- 1
GSS13$age_high <- 0
GSS13$age_high[GSS13$AGEGR10 %in% c("55 to 64 years", "65 to 74 years",
                                "75 years and over")] <- 1
GSS13$lang <- NA
GSS13$lang[GSS13$LANCH %in% c("French only", "French and other equally")] <-
  "Francophone"
GSS13$lang[GSS13$LANCH %in% c("English only", "English and other equally")] <-
  "Anglophone"
GSS13$lang[GSS13$LANCH == "Other language only"] <- "Allophone"
GSS13$lang[GSS13$LANCH %in% c("English and French equally",
                          "English, French and other equally")] <- "Bilingual"
table(GSS13$LANCH, useNA = "always")
GSS13$immig <- NA
GSS13$immig[GSS13$BRTHCAN == "Born in Canada"] <- 0
GSS13$immig[GSS13$BRTHCAN == "Born outside Canada"] <- 1
GSS13$education <- as.character(GSS13$EHG_ALL)
GSS13$education[
  GSS13$education == "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)"] <-
  "Bachelor's degree\n(e.g. B.A., B.Sc., LL.B.)"
GSS13$education[
  GSS13$education ==
    "College/CEGEP/other non-university certificate or diploma"] <-
  "College/CEGEP/other non-university\ncertificate or diploma"
GSS13$education[
  GSS13$education ==
    "University certificate, diploma, degree above the BA level"] <-
  "University certificate, diploma,\ndegree above the BA level"
GSS13$education[
  GSS13$education ==
    "University certificate or diploma below the bachelor's level"] <-
  "University certificate or diploma\nbelow the bachelor's level"
GSS13$education[
  GSS13$education ==
    "Less than high school diploma or its equivalent"] <-
  "Less than high school\ndiploma or its equivalent"
GSS13$education[
  GSS13$education ==
    "High school diploma or a high school equivalency certificate"] <-
  "High school diploma or a high\nschool equivalency certificate"
GSS13$education <- as.factor(GSS13$education)
table(GSS13$education, useNA = "always")
GSS13$educ_low <- NA
GSS13$educ_low[GSS13$DH1GED %in% c(
  "Post-secondary diploma", "University degree")] <- 0
GSS13$educ_low[GSS13$DH1GED %in% c(
  "Less than High School", "Graduated from High School")] <- 1
GSS13$educ_mid <- NA
GSS13$educ_mid[GSS13$DH1GED %in% c(
  "Less than High School", "Graduated from High School",
  "University degree")] <- 0
GSS13$educ_mid[GSS13$DH1GED == "Post-secondary diploma"] <- 1
GSS13$educ_high <- NA
GSS13$educ_high[GSS13$DH1GED %in% c(
  "Less than High School", "Graduated from High School",
  "Post-secondary diploma")] <- 0
GSS13$educ_high[GSS13$DH1GED == "University degree"] <- 1
GSS13$income <- GSS13$INCMHSD
GSS13$income_low <- 0
GSS13$income_low[GSS13$INCMHSD %in% c(
  "No income or loss", "Less than $ 5,000", "$ 5,000 to $ 9,999",
  "$ 10,000 to $ 14,999", "$ 15,000 to $ 19,999", "$ 20,000 to $ 29,999",
  "$ 30,000 to $ 39,999", "$ 40,000 to $ 49,999", "$ 50,000 to $ 59,999")] <- 1
GSS13$income_low[is.na(GSS13$INCMHSD)] <- NA
GSS13$income_mid <- 0
GSS13$income_mid[GSS13$INCMHSD %in% c(
  "$ 60,000 to $ 79,999", "$ 80,000 to $ 99,999", "$ 100,00 to $ 149,999")] <- 1
GSS13$income_mid[is.na(GSS13$INCMHSD)] <- NA
GSS13$income_high <- 0
GSS13$income_high[GSS13$INCMHSD == "$ 150,000 or more"] <- 1
GSS13$income_high[is.na(GSS13$INCMHSD)] <- NA
GSS13$interest <- NA
GSS13$interest[GSS13$REP_05 == "Very interested"] <- 100
GSS13$interest[GSS13$REP_05 == "Somewhat interested"] <- (2 / 3) * 100
GSS13$interest[GSS13$REP_05 == "Not very interested"] <- (1 / 3) * 100
GSS13$interest[GSS13$REP_05 == "Not at all interested"] <- 0
GSS13$province <- GSS13$PRCODE # no data about territories
GSS13$weight <- GSS13$WGHT_PER

GSS20 <- read.csv(
  "_data/GSS2020/dataverse_files/CSV/gss-89M0032x-E-2020-c35_F1.csv")
GSS20$female <- NA
GSS20$female[GSS20$GENDER2P == 1] <- 0
GSS20$female[GSS20$GENDER2P == 2] <- 1
GSS20$female <- as.factor(GSS20$female)
GSS20$female_alt <- NA
GSS20$female_alt[GSS20$GENDER2P == 1] <- "Men"
GSS20$female_alt[GSS20$GENDER2P == 2] <- "Women"
GSS20$female_alt <- as.factor(GSS20$female_alt)
GSS20$age <- NA
GSS20$age[GSS20$AGEGR10 == 1] <- "15-24"
GSS20$age[GSS20$AGEGR10 == 2] <- "25-34"
GSS20$age[GSS20$AGEGR10 == 3] <- "35-44"
GSS20$age[GSS20$AGEGR10 == 4] <- "45-54"
GSS20$age[GSS20$AGEGR10 == 5] <- "55-64"
GSS20$age[GSS20$AGEGR10 == 6] <- "65-74"
GSS20$age[GSS20$AGEGR10 == 7] <- "75+"
GSS20$age_low <- 0
GSS20$age_low[GSS20$AGEGR10 %in% c(1, 2)] <- 1
GSS20$age_mid <- 0
GSS20$age_mid[GSS20$AGEGR10 %in% c(3, 4)] <- 1
GSS20$age_high <- 0
GSS20$age_high[GSS20$AGEGR10 %in% c(5, 6, 7)] <- 1
GSS20$lang <- NA
GSS20$lang[GSS20$LANHSD_C == 2] <- "Francophone"
GSS20$lang[GSS20$LANHSD_C == 1] <- "Anglophone"
GSS20$lang[GSS20$LANHSD_C == 3] <- "Allophone"
GSS20$lang[GSS20$LANHSD_C == 4] <- "Bilingual"
GSS20$lang[GSS20$LANHSD_C == 99] <- NA
table(GSS20$lang, useNA = "always")
GSS20$immig <- NA
GSS20$immig[GSS20$IM_05A1 == 1] <- 0
GSS20$immig[GSS20$IM_05A1 == 2] <- 1
GSS20$education <- GSS20$ED_05
GSS20$education[GSS20$education == 1] <-
  "Less than high\nschool diploma\nor its equivalent"
GSS20$education[GSS20$education == 2] <-
  "High school diploma\nor a high school\nequivalency certificate"
GSS20$education[GSS20$education %in% c(3, 4)] <-
  "College/CEGEP/other\nnon-university\ncertificate or diploma"
GSS20$education[GSS20$education == 5] <-
  "University certificate\nor diploma below\nthe bachelor's level"
GSS20$education[GSS20$education == 6] <-
  "Bachelor's degree\n(e.g. B.A., B.Sc., LL.B.)"
GSS20$education[GSS20$education == 7] <-
  "University certificate,\ndiploma, degree above\nthe BA level"
GSS20$education[GSS20$education == 99] <- NA
GSS20$education <- as.factor(GSS20$education)
GSS20$educ_low <- NA
GSS20$educ_low[GSS20$ED_05 %in% c(3, 4, 5, 6, 7)] <- 0
GSS20$educ_low[GSS20$ED_05 %in% c(1, 2)] <- 1
GSS20$educ_mid <- NA
GSS20$educ_mid[GSS20$ED_05 %in% c(1, 2, 5, 6, 7)] <- 0
GSS20$educ_mid[GSS20$ED_05 %in% c(3, 4)] <- 1
GSS20$educ_high <- NA
GSS20$educ_high[GSS20$ED_05 %in% c(1, 2, 3, 4)] <- 0
GSS20$educ_high[GSS20$ED_05 %in% c(5, 6, 7)] <- 1
GSS20$income <- GSS20$FAMINC_C
GSS20$income[GSS20$income == 1] <- "Less than $24,999"
GSS20$income[GSS20$income == 2] <- "$25,000 to $49,999"
GSS20$income[GSS20$income == 3] <- "$50,000 to $74,999"
GSS20$income[GSS20$income == 4] <- "$75,000 to $99,999"
GSS20$income[GSS20$income == 5] <- "$100,000 and over"
GSS20$income <- factor(GSS20$income, levels = c(
  "Less than $24,999", "$25,000 to $49,999", "$50,000 to $74,999",
  "$75,000 to $99,999", "$100,000 and over"))
GSS20$income_low <- 0
GSS20$income_low[GSS20$FAMINC_C %in% c(1, 2)] <- 1
GSS20$income_low[is.na(GSS20$FAMINC_C)] <- NA
GSS20$income_mid <- 0
GSS20$income_mid[GSS20$FAMINC_C %in% c(3, 4)] <- 1
GSS20$income_mid[is.na(GSS20$FAMINC_C)] <- NA
GSS20$income_high <- 0
GSS20$income_high[GSS20$FAMINC_C == 5] <- 1
GSS20$income_high[is.na(GSS20$FAMINCMHSD)] <- NA
GSS20$interest <- NA
GSS20$interest[GSS20$REP_05 == 1] <- 100
GSS20$interest[GSS20$REP_05 == 2] <- (2 / 3) * 100
GSS20$interest[GSS20$REP_05 == 3] <- (1 / 3) * 100
GSS20$interest[GSS20$REP_05 == 4] <- 0
GSS20$province <- GSS20$PRV # no data about territories
GSS20$province[GSS20$province == 10] <- "Newfoundland\nand Labrador"
GSS20$province[GSS20$province == 11] <- "Prince Edward\nIsland"
GSS20$province[GSS20$province == 12] <- "Nova Scotia"
GSS20$province[GSS20$province == 13] <- "New Brunswick"
GSS20$province[GSS20$province == 24] <- "Quebec"
GSS20$province[GSS20$province == 35] <- "Ontario"
GSS20$province[GSS20$province == 46] <- "Manitoba"
GSS20$province[GSS20$province == 47] <- "Saskatchewan"
GSS20$province[GSS20$province == 48] <- "Alberta"
GSS20$province[GSS20$province == 59] <- "British Columbia"
GSS20$ethnicity <- NA
GSS20$ethnicity[GSS20$VISMIN_C == 3] <- "Black"
GSS20$ethnicity[GSS20$VISMIN_C == 8] <- "West Asian"
GSS20$ethnicity[GSS20$VISMIN_C == 7] <- "Southeast Asian"
GSS20$ethnicity[GSS20$VISMIN_C == 5] <- "Arabic"
GSS20$ethnicity[GSS20$VISMIN_C == 1] <- "South Asian"
GSS20$ethnicity[GSS20$VISMIN_C == 6] <- "Hispanic"
GSS20$ethnicity[GSS20$VISMIN_C == 2] <- "Chinese"
GSS20$ethnicity[GSS20$VISMIN_C == 4] <- "Filipino"
GSS20$ethnicity[GSS20$VISMIN_C == 9] <- "Other"
GSS20$ethnicity[GSS20$VISMIN_C == 10] <- "White"
GSS20$ethnicity[GSS20$ABM_01A == 2] <- "Indigenous"
GSS20$ethnicity <- as.factor(GSS20$ethnicity)
GSS20$weight <- GSS20$WGHT_PER # 10.0000 - 32631.0308
prop.table(table(GSS20$female))
prop.table(table(GSS20$lang))
prop.table(table(GSS20$education))
prop.table(table(GSS20$immig))
cumsum(prop.table(table(GSS20$income)))
cumsum(prop.table(table(GSS20$age)))
prop.table(table(GSS20$province))
prop.table(table(GSS20$ethnicity))
summary(GSS20$weight)

##### 2. Descriptive statistics #####
#### 2.1 CCPIS ####
PlotGender <- CCPIS |>
  filter(!is.na(female_alt)) |>
  ggplot(aes(x = female_alt)) +
  geom_bar() +
  labs(x = "Gender", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotAge <- ggplot(CCPIS, aes(x = age)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Age", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotRace <- CCPIS |>
  filter(!is.na(ethnicity)) |>
  ggplot(aes(x = ethnicity)) +
  geom_bar() +
  labs(x = "Race", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotLanguage <- CCPIS |>
  filter(!is.na(lang)) |>
  ggplot(aes(x = lang)) +
  geom_bar() +
  labs(x = "Language spoken at home", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotImmigrant <- CCPIS |>
  filter(!is.na(immig)) |>
  ggplot(aes(x = as.factor(immig))) +
  geom_bar() +
  labs(x = "Born in Canada?", y = "Frequency") +
  scale_x_discrete(labels = c("Yes", "No")) +
  theme(text = element_text(family = "CM Roman"))
PlotAgentic <- ggplot(CCPIS, aes(x = agentic)) +
  geom_histogram(breaks = seq(0, 1, 0.1)) +
  labs(x = "Agency scale score", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotCommunal <- ggplot(CCPIS, aes(x = communal)) +
  geom_histogram(breaks = seq(0, 1, 0.1)) +
  labs(x = "Communality scale score", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotFamSituation <- CCPIS |>
  ggplot(aes(x = as.factor(fam_situation_alt))) +
  geom_bar() +
  labs(x = "Family situation", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotParentDiscuss <- filter(CCPIS, !is.na(parent_discuss_alt) &
                              !is.na(female)) |>
  ggplot(aes(x = as.factor(parent_discuss_alt))) +
  geom_bar() +
  facet_grid(~female_alt2) +
  labs(x = "Gender of parent who has the most discussions", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotFriendsGender <- filter(CCPIS, !is.na(friends_gender_alt) &
                              !is.na(female)) |>
  ggplot(aes(x = as.factor(friends_gender_alt))) +
  geom_bar() +
  facet_grid(~female_alt2) +
  labs(x = "Gender of most of friends", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotTeacherGender <- filter(CCPIS, !is.na(teacher_gender_alt) &
                              !is.na(female)) |>
  ggplot(aes(x = as.factor(teacher_gender_alt))) +
  geom_bar() +
  facet_grid(~female_alt2) +
  labs(x = "Gender of liked teacher", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotInfluencerGender <- filter(CCPIS, !is.na(influencer_gender_alt) &
                              !is.na(female)) |>
  ggplot(aes(x = as.factor(influencer_gender_alt))) +
  geom_bar() +
  facet_grid(~female_alt2) +
  labs(x = "Gender of follower influencer", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
ggsave(plot = ggpubr::ggarrange(
  PlotGender, PlotAge, PlotRace, PlotLanguage, PlotImmigrant, PlotAgentic,
  PlotCommunal, PlotFamSituation, nrow = 3, ncol = 3),
  "_graphs/CCPISDescriptive1.pdf", height = 8.5, width = 11)

PlotInterest <- ggplot(CCPIS, aes(x = interest)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 120)) +
  labs(x = "General political interest", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotHealth <- ggplot(CCPIS, aes(x = interest_health)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 120)) +
  labs(x = "Interest in health care", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotForeign <- ggplot(CCPIS, aes(x = interest_foreign)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 120)) +
  labs(x = "Interest in international relations", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotLaw <- ggplot(CCPIS, aes(x = interest_law)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 120)) +
  labs(x = "Interest in law and crime", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotEducation <- ggplot(CCPIS, aes(x = interest_education)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 120)) +
  labs(x = "Interest in education", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotPartisan <- ggplot(CCPIS, aes(x = interest_partisan)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 120)) +
  labs(x = "Interest in partisan politics", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
ggsave(plot = ggpubr::ggarrange(
  PlotInterest, PlotHealth, PlotForeign, PlotLaw, PlotEducation, PlotPartisan,
  nrow = 3, ncol = 2), width = 5.5, height = 4.25,
  "_graphs/CCPISDescriptive2.pdf")

PoliticalGraphData <- pivot_longer(
  CCPIS, cols = lockdown_political_alt:parties_political_alt,
  names_to = "name", values_to = "value") |>
  group_by(name) |>
  summarise(value = mean(value, na.rm = T))
PoliticalGraphData$name_full <- case_when(
  PoliticalGraphData$name == "lockdown_political_alt" ~ "Pandemic restrictions",
  PoliticalGraphData$name == "nurses_political_alt" ~
    "Working conditions of nurses",
  PoliticalGraphData$name == "china_political_alt" ~
    "Diplomatic disputes\nbetween Canada and China",
  PoliticalGraphData$name == "ukraine_political_alt" ~ "Ukrainian war",
  PoliticalGraphData$name == "police_political_alt" ~ "Police funding",
  PoliticalGraphData$name == "crime_political_alt" ~ "Sentences for violent crimes",
  PoliticalGraphData$name == "tuition_political_alt" ~ "University tuition",
  PoliticalGraphData$name == "privateschool_political_alt" ~
    "Funding of public\nand private schools",
  PoliticalGraphData$name == "elections_political_alt" ~ "Federal elections",
  PoliticalGraphData$name == "parties_political_alt" ~ "Political parties")
ggplot(PoliticalGraphData, aes(x = name_full, y = value)) +
  geom_point() +
  scale_y_continuous("Average view of students", limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("Non-political", "", "", "", "Political")) +
  scale_x_discrete("Issue") +
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/CCPISPolitical.pdf", width = 5.5, height = 4.25)

#### 2.2 Datagotchi PES, CES, WVS and GSS ####
PlotAgeDG <- ggplot(DG, aes(x = age)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Age", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotGenderDG <- DG |>
  filter(!is.na(female_alt)) |>
  ggplot(aes(x = female_alt)) +
  geom_bar() +
  labs(x = "Gender", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotLanguageDG <- DG |>
  filter(!is.na(lang)) |>
  ggplot(aes(x = lang)) +
  geom_bar() +
  labs(x = "Language of the survey", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotRaceDG <- DG |>
  filter(!is.na(ethnicity)) |>
  ggplot(aes(x = ethnicity)) +
  geom_bar() +
  labs(x = "Race", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotImmigrantDG <- DG |>
  filter(!is.na(immig)) |>
  ggplot(aes(x = as.factor(immig))) +
  geom_bar() +
  labs(x = "Born in Canada?", y = "Frequency") +
  scale_x_discrete(labels = c("Yes", "No")) +
  theme(text = element_text(family = "CM Roman"))
PlotIncomeDG <- DG |>
  filter(!is.na(income)) |>
  ggplot(aes(x = as.factor(income))) +
  geom_bar() +
  labs(x = "Household yearly income", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotEducationDG <- DG |>
  filter(!is.na(education)) |>
  ggplot(aes(x = as.factor(education))) +
  geom_bar() +
  labs(x = "Level of education", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
ggsave(plot = ggpubr::ggarrange(
  PlotGenderDG, PlotAgeDG, PlotRaceDG, PlotLanguageDG, PlotImmigrantDG,
  PlotIncomeDG, PlotEducationDG, nrow = 3, ncol = 3),
  "_graphs/DGDescriptive1.pdf", height = 8.5, width = 11)

PlotAgeCES <- CES21 |>
  filter(!is.na(age)) |>
  ggplot(aes(x = age)) +
  geom_bar() +
  labs(x = "Age", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotGenderCES <- CES21 |>
  filter(!is.na(female_alt)) |>
  ggplot(aes(x = female_alt)) +
  geom_bar() +
  labs(x = "Gender", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotLanguageCES <- CES21 |>
  filter(!is.na(lang)) |>
  ggplot(aes(x = lang)) +
  geom_bar() +
  labs(x = "Language spoken at home", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotImmigrantCES <- CES21 |>
  filter(!is.na(immig)) |>
  ggplot(aes(x = as.factor(immig))) +
  geom_bar() +
  labs(x = "Born in Canada?", y = "Frequency") +
  scale_x_discrete(labels = c("Yes", "No")) +
  theme(text = element_text(family = "CM Roman"))
PlotIncomeCES <- CES21 |>
  filter(!is.na(income)) |>
  ggplot(aes(x = as.factor(income))) +
  geom_bar() +
  labs(x = "Household yearly income", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotEducationCES <- CES21 |>
  filter(!is.na(education)) |>
  ggplot(aes(x = as.factor(education))) +
  geom_bar() +
  labs(x = "Level of education", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotProvinceCES <- CES21 |>
  filter(!is.na(province)) |>
  ggplot(aes(x = as.factor(province))) +
  geom_bar() +
  labs(x = "Province of residence", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotEthnicityCES <- CES21 |>
  filter(!is.na(ethnicity)) |>
  ggplot(aes(x = as.factor(ethnicity))) +
  geom_bar() +
  labs(x = "Ethnicity", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
ggsave(plot = ggpubr::ggarrange(
  PlotGenderCES, PlotAgeCES, PlotLanguageCES, PlotImmigrantCES,
  PlotIncomeCES, PlotEducationCES, PlotEthnicityCES, PlotProvinceCES,
  nrow = 3, ncol = 3),
  "_graphs/CESDescriptive1.pdf", height = 8.5, width = 11)

PlotAgeWVS <- WVSCA20 |>
  filter(!is.na(age)) |>
  ggplot(aes(x = age)) +
  geom_bar() +
  labs(x = "Age", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotGenderWVS <- WVSCA20 |>
  filter(!is.na(female_alt)) |>
  ggplot(aes(x = female_alt)) +
  geom_bar() +
  labs(x = "Gender", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotLanguageWVS <- WVSCA20 |>
  filter(!is.na(lang)) |>
  ggplot(aes(x = lang)) +
  geom_bar() +
  labs(x = "Language spoken at home", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotImmigrantWVS <- WVSCA20 |>
  filter(!is.na(immig)) |>
  ggplot(aes(x = as.factor(immig))) +
  geom_bar() +
  labs(x = "Born in Canada?", y = "Frequency") +
  scale_x_discrete(labels = c("Yes", "No")) +
  theme(text = element_text(family = "CM Roman"))
PlotEducationWVS <- WVSCA20 |>
  filter(!is.na(education)) |>
  ggplot(aes(x = as.factor(education))) +
  geom_bar() +
  labs(x = "Level of education", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotProvinceWVS <- WVSCA20 |>
  filter(!is.na(province)) |>
  ggplot(aes(x = as.factor(province))) +
  geom_bar() +
  labs(x = "Province of residence", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotEthnicityWVS <- WVSCA20 |>
  filter(!is.na(ethnicity)) |>
  ggplot(aes(x = as.factor(ethnicity))) +
  geom_bar() +
  labs(x = "Ethnicity", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
ggsave(plot = ggpubr::ggarrange(
  PlotGenderWVS, PlotAgeWVS, PlotLanguageWVS, PlotImmigrantWVS,
  PlotEducationWVS, PlotEthnicityWVS, PlotProvinceWVS, nrow = 3, ncol = 3),
  "_graphs/WVSDescriptive1.pdf", height = 8.5, width = 11)

PlotAgeGSS <- GSS20 |>
  filter(!is.na(age)) |>
  ggplot(aes(x = age)) +
  geom_bar() +
  labs(x = "Age", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotGenderGSS <- GSS20 |>
  filter(!is.na(female_alt)) |>
  ggplot(aes(x = female_alt)) +
  geom_bar() +
  labs(x = "Gender", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotLanguageGSS <- GSS20 |>
  filter(!is.na(lang)) |>
  ggplot(aes(x = lang)) +
  geom_bar() +
  labs(x = "Language spoken at home", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotImmigrantGSS <- GSS20 |>
  filter(!is.na(immig)) |>
  ggplot(aes(x = as.factor(immig))) +
  geom_bar() +
  labs(x = "Born in Canada?", y = "Frequency") +
  scale_x_discrete(labels = c("Yes", "No")) +
  theme(text = element_text(family = "CM Roman"))
PlotIncomeGSS <- GSS20 |>
  filter(!is.na(income)) |>
  ggplot(aes(x = as.factor(income))) +
  geom_bar() +
  labs(x = "Household yearly income", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotEducationGSS <- GSS20 |>
  filter(!is.na(education)) |>
  ggplot(aes(x = as.factor(education))) +
  geom_bar() +
  labs(x = "Level of education", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotProvinceGSS <- GSS20 |>
  filter(!is.na(province)) |>
  ggplot(aes(x = as.factor(province))) +
  geom_bar() +
  labs(x = "Province of residence", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotEthnicityGSS <- GSS20 |>
  filter(!is.na(ethnicity)) |>
  ggplot(aes(x = as.factor(ethnicity))) +
  geom_bar() +
  labs(x = "Ethnicity", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
ggsave(plot = ggpubr::ggarrange(
  PlotGenderGSS, PlotAgeGSS, PlotLanguageGSS, PlotImmigrantGSS,
  PlotIncomeGSS, PlotEducationGSS, PlotEthnicityGSS, PlotProvinceGSS,
  nrow = 3, ncol = 3),
  "_graphs/GSSDescriptive1.pdf", height = 8.5, width = 11)

PlotInterestDG <- ggplot(DG, aes(x = interest, weight = weightRaking)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 450)) +
  labs(x = "General political interest", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotHealthDG <- ggplot(DG, aes(x = interest_health, weight = weightRaking)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 450)) +
  labs(x = "Interest in health care", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotForeignDG <- ggplot(DG, aes(x = interest_foreign, weight = weightRaking)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 450)) +
  labs(x = "Interest in international affairs", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotLawDG <- ggplot(DG, aes(x = interest_law, weight = weightRaking)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 450)) +
  labs(x = "Interest in law and crime", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotEducationDG <- ggplot(DG, aes(x = interest_education,
                                  weight = weightRaking)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 450)) +
  labs(x = "Interest in education", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotPartisanDG <- ggplot(DG, aes(x = interest_partisan,
                                 weight = weightRaking)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 450)) +
  labs(x = "Interest in partisan politics", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
ggsave(plot = ggpubr::ggarrange(
  PlotInterestDG, PlotHealthDG, PlotForeignDG, PlotLawDG, PlotEducationDG,
  PlotPartisanDG, nrow = 3, ncol = 2), width = 5.5, height = 4.25,
  "_graphs/DGInterest.pdf")

PlotInterestCES <- ggplot(CES21, aes(x = interest, weight = weight)) +
  geom_histogram(binwidth = 10) +
  labs(x = "General political interest -\n2021 CES", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotInterestWVS <- ggplot(WVSCA20, aes(x = interest, weight = weight)) +
  geom_histogram(binwidth = (100/3)) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100)) +
  labs(x = "General political interest -\n2020 WVS - Canada",
       y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
PlotInterestGSS <- ggplot(GSS20, aes(x = interest, weight = weight)) +
  geom_histogram(binwidth = (100/3)) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100)) +
  labs(x = "General political interest -\n2020 GSS - Canada",
       y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
ggsave(plot = ggpubr::ggarrange(
  PlotInterestCES, PlotInterestWVS, PlotInterestGSS,
  nrow = 2, ncol = 2), width = 5.5, height = 4.25,
  "_graphs/CESWVSGSSInterest.pdf")
mean(CCPIS$interest, na.rm=T)
mean(CCPIS$interest_law, na.rm=T)
mean(CCPIS$interest_education, na.rm=T)
mean(CCPIS$interest_foreign, na.rm=T)
mean(CCPIS$interest_partisan, na.rm=T)
mean(CCPIS$interest_health, na.rm=T)
mean(DG$interest, na.rm=T)
weighted.mean(DG$interest, na.rm=T, w = DG$weight)
weighted.mean(DG$interest_law, na.rm=T, w = DG$weight)
weighted.mean(DG$interest_education, na.rm=T, w = DG$weight)
weighted.mean(DG$interest_health, na.rm=T, w = DG$weight)
weighted.mean(DG$interest_partisan, na.rm=T, w = DG$weight)
weighted.mean(DG$interest_foreign, na.rm=T, w = DG$weight)
mean(CES21$interest, na.rm=T)
CES21noNA <- filter(CES21, !is.na(interest) & !is.na(weight))
weighted.mean(CES21noNA$interest, w = CES21noNA$weight)
mean(WVSCA20$interest, na.rm=T)
weighted.mean(WVSCA20$interest, na.rm=T, w = WVSCA20$weight)
mean(GSS20$interest, na.rm=T)
weighted.mean(GSS20$interest, na.rm=T, w = GSS20$weight)

##### 3. Data analysis #####
#### 3.1 Chapter 3 ####
### 3.1.1 Political interest by age & gender (all) ####
Model0 <- nlme::lme(data = CCPIS, fixed = interest ~ 1, # empty model
                    random = ~ 1 | Class, na.action = na.omit)
Model0effects <- nlme::VarCorr(Model0)
100 * as.numeric(Model0effects[1]) / (as.numeric(Model0effects[1]) +
                                        as.numeric(Model0effects[2]))
# ~6.2% of variance in political interest is located at the classroom level
Model1 <- nlme::lme(data = CCPIS, fixed = interest ~ female,
                    random = ~ 1 | Class, na.action = na.omit)
summary(Model1)
# girls' political interest = 4.1/10; boys' political interest = 4.6/10; p<0.05
Model01 <- nlme::lme(data = CCPIS, fixed = interest ~ female * age +
                       age_squared + female * white + immig + lang + agentic +
                       communal + school, random = ~ 1 | Class,
                     na.action = na.omit)
summary(Model01)
ModelHealth <- nlme::lme(data = CCPIS, fixed = interest_health ~ 1,
                    random = ~ 1 | Class, na.action = na.omit) # empty model
ModelHealtheffects <- nlme::VarCorr(ModelHealth)
100 * as.numeric(ModelHealtheffects[1]) / (as.numeric(ModelHealtheffects[1]) +
                                             as.numeric(ModelHealtheffects[2]))
# ~4.6% of variance in interest in health care is located at the classroom level
Model2 <- nlme::lme(data = CCPIS, fixed = interest_health ~ female,
                    random = ~ 1 | Class, na.action = na.omit)
summary(Model2) # N.S.
Model02 <- nlme::lme(data = CCPIS, fixed = interest_health ~ female * age +
                       age_squared + female * white + immig + lang + agentic +
                       communal + school, random = ~ 1 | Class,
                     na.action = na.omit)
ModelForeign <- nlme::lme(data = CCPIS, fixed = interest_foreign ~ 1,
                         random = ~ 1 | Class, na.action = na.omit) # empty model
ModelForeigneffects <- nlme::VarCorr(ModelForeign)
100 * as.numeric(ModelForeigneffects[1]) / (
  as.numeric(ModelForeigneffects[1]) + as.numeric(ModelForeigneffects[2]))
# ~3.6% of variance in interest in international affairs is located at the
# classroom level
Model3 <- nlme::lme(data = CCPIS, fixed = interest_foreign ~ female,
                    random = ~ 1 | Class, na.action = na.omit)
summary(Model3) # p<0.001
Model03 <- nlme::lme(data = CCPIS, fixed = interest_foreign ~ female * age +
                       age_squared + female * white + immig + lang + agentic +
                       communal + school, random = ~ 1 | Class,
                     na.action = na.omit)
ModelLaw <- nlme::lme(data = CCPIS, fixed = interest_law ~ 1, # empty model
                          random = ~ 1 | Class, na.action = na.omit)
ModelLaweffects <- nlme::VarCorr(ModelLaw)
100 * as.numeric(ModelLaweffects[1]) / (as.numeric(ModelLaweffects[1]) +
                                              as.numeric(ModelLaweffects[2]))
# ~1.4% of variance in interest in law and crime is located at the classroom
# level
Model4 <- nlme::lme(data = CCPIS, fixed = interest_law ~ female,
                    random = ~ 1 | Class, na.action = na.omit)
summary(Model4) # p<0.05
Model04 <- nlme::lme(data = CCPIS, fixed = interest_law ~ female * age +
                       age_squared + female * white + immig + lang + agentic +
                       communal + school, random = ~ 1 | Class,
                     na.action = na.omit)
ModelEducation <- nlme::lme(data = CCPIS, fixed = interest_education ~ 1,
                          random = ~ 1 | Class, na.action = na.omit) # empty model
ModelEducationeffects <- nlme::VarCorr(ModelEducation)
100 * as.numeric(ModelEducationeffects[1]) / (as.numeric(ModelEducationeffects[1]) +
                                              as.numeric(ModelEducationeffects[2]))
# ~8.1% of variance in interest in education is located at the classroom level
Model5 <- nlme::lme(data = CCPIS, fixed = interest_education ~ female,
                    random = ~ 1 | Class, na.action = na.omit)
summary(Model5) # N.S.
Model05 <- nlme::lme(data = CCPIS, fixed = interest_education ~
                       female * age + age_squared + female * white + immig +
                       lang + agentic + communal + school, random = ~ 1 |
                       Class, na.action = na.omit)
ModelPartisan <- nlme::lme(data = CCPIS, fixed = interest_partisan ~ 1,
                          random = ~ 1 | Class, na.action = na.omit) # empty model
ModelPartisaneffects <- nlme::VarCorr(ModelPartisan)
100 * as.numeric(ModelPartisaneffects[1]) / (as.numeric(ModelPartisaneffects[1]) +
                                              as.numeric(ModelPartisaneffects[2]))
# ~2.6% of variance in interest in partisan politics is located at the
# classroom level
Model6 <- nlme::lme(data = CCPIS, fixed = interest_partisan ~ female,
                    random = ~ 1 | Class, na.action = na.omit)
summary(Model6) # p<0.001
Model06 <- nlme::lme(data = CCPIS, fixed = interest_partisan ~ female *
                       age + age_squared + female * white + immig + lang +
                       agentic + communal + school, random = ~ 1 | Class,
                     na.action = na.omit)
modelsummary::modelsummary(models = list(
  "Politics (general)" = Model1, "Health care" = Model2,
  "International affairs" = Model3, "Law and crime" = Model4,
  "Education" = Model5, "Partisan politics" = Model6),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  title = paste("Interest in topic by gender {#tbl-lmeInterestCCPIS}"),
  coef_rename = c("female1" = "Gender (1 = girl)"))
modelsummary::modelsummary(models = list(
  "Politics (general)" = Model01, "Health care" = Model02,
  "International affairs" = Model03, "Law and crime" = Model04,
  "Education" = Model05, "Partisan politics" = Model06),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  #coef_omit = "age|white|immig|lang|agentic|communal|#school",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            #"Controls: Socio-demographic, personality traits#, schools",
            "Reference Category for Language: Other languages spoken at home",
            "Reference Category for School: School #1"
  ),
  title = paste("Interest in topic by gender {#tbl-lmeInterestCCPISAlt}"),
  coef_rename = c(
    "female1" = "Gender (1 = girl)",
    "age" = "Age",
    "age_squared" = "Age squared",
    "white" = "Race (1 = white)",
    "immig" = "Immigrant",
    "langAnglophone" = "English spoken at home",
    "langFrancophone" = "French spoken at home",
    "agentic" = "Agency",
    "communal" = "Communality",
    "schoolCollège Citoyen" = "School #4",
    "schoolCollège mariste de Québec" = "School #3",
    "schoolÉcole de la Rose-des-Vents" = "School #6",
    "schoolÉcole Jean-de-Brébeuf" = "School #2",
    "schoolJaya International High School" = "School #5",
    "schoolRenfrew County DSB Student Senate" = "School #8",
    "schoolUrban Village Academy" = "School #7"))
CCPISYoung <- CCPIS |> filter(age > 9 & age <= 15)
Model0Young <- nlme::lme(data = CCPISYoung, fixed = interest ~ # empty model
                           1, random = ~ 1 | Class, na.action = na.omit)
Model0Youngeffects <- nlme::VarCorr(Model0Young)
100 * as.numeric(Model0Youngeffects[1]) / (as.numeric(Model0Youngeffects[1]) +
                                             as.numeric(Model0Youngeffects[2]))
# ~7% of variance in political interest is located at the classroom level among
# students aged 10-15
Model1Young <- nlme::lme(data = CCPISYoung, fixed = interest ~ female,
                         random = ~ 1 | Class, na.action = na.omit)
summary(Model1Young) # N.S.
Model01Young <- nlme::lme(data = CCPISYoung, fixed = interest ~ female *
                            age + age_squared + female * white + immig + lang +
                            agentic + communal + school, random = ~ 1 | Class,
                          na.action = na.omit)
summary(Model01Young)
Model2Young <- nlme::lme(data = CCPISYoung, fixed = interest_health ~
                           female, random = ~ 1 | Class, na.action = na.omit)
summary(Model2Young) # N.S.
Model02Young <- nlme::lme(data = CCPISYoung, fixed = interest_health ~
                            female * age + age_squared + female * white +
                            immig + lang + agentic + communal + school,
                          random = ~ 1 | Class, na.action = na.omit)
summary(Model02Young)
Model3Young <- nlme::lme(data = CCPISYoung, fixed = interest_foreign ~
                           female, random = ~ 1 | Class, na.action = na.omit)
summary(Model3Young) # p<0.05
Model03Young <- nlme::lme(data = CCPISYoung, fixed = interest_foreign ~
                            female * age + age_squared + female * white +
                            immig + lang + agentic + communal + school,
                          random = ~ 1 | Class, na.action = na.omit)
summary(Model03Young)
Model4Young <- nlme::lme(data = CCPISYoung, fixed = interest_law ~
                           female, random = ~ 1 | Class, na.action = na.omit)
summary(Model4Young) # p<0.1 girls higher!
Model04Young <- nlme::lme(data = CCPISYoung, fixed = interest_law ~
                            female * age + age_squared + female * white +
                            immig + lang + agentic + communal + school,
                          random = ~ 1 | Class, na.action = na.omit)
summary(Model04Young)
Model5Young <- nlme::lme(data = CCPISYoung, fixed = interest_education ~
                           female, random = ~ 1 | Class, na.action = na.omit)
summary(Model5Young) # N.S.
Model05Young <- nlme::lme(data = CCPISYoung, fixed = interest_education ~
                            female * age + age_squared + female * white +
                            immig + lang + agentic + communal + school,
                          random = ~ 1 | Class, na.action = na.omit)
summary(Model05Young)
Model6Young <- nlme::lme(data = CCPISYoung, fixed = interest_partisan ~
                           female, random = ~ 1 | Class, na.action = na.omit)
summary(Model6Young) # p<0.05
Model06Young <- nlme::lme(data = CCPISYoung, fixed = interest_partisan ~
                            female * age + age_squared + female * white +
                            immig + lang + agentic + communal + school,
                          random = ~ 1 | Class, na.action = na.omit)
summary(Model06Young)
modelsummary::modelsummary(models = list(
  "Politics (general)" = Model1Young, "Health care" = Model2Young,
  "International affairs" = Model3Young, "Law and crime" = Model4Young,
  "Education" = Model5Young, "Partisan politics" = Model6Young),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  title = paste("Interest in topic by gender (ages 10-15)",
                "{#tbl-lmeInterestYoungCCPIS}"),
  coef_rename = c("female1" = "Gender (1 = girl)"))
modelsummary::modelsummary(models = list(
  "Politics (general)" = Model01Young, "Health care" = Model02Young,
  "International affairs" = Model03Young, "Law and crime" = Model04Young,
  "Education" = Model05Young, "Partisan politics" = Model06Young),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  #coef_omit = "age|white|immig|lang|agentic|communal|#school",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            #"Controls: Socio-demographic, personality traits#, schools",
            "Reference Category for Language: Other languages spoken at home",
            "Reference Category for School: School #1"
  ),
  title = paste("Interest in topic by gender (ages 10-15)",
                "{#tbl-lmeInterestYoungCCPISAlt}"),
  coef_rename = c(
    "female1" = "Gender (1 = girl)",
    "age" = "Age",
    "age_squared" = "Age squared",
    "white" = "Race (1 = white)",
    "immig" = "Immigrant",
    "langAnglophone" = "English spoken at home",
    "langFrancophone" = "French spoken at home",
    "agentic" = "Agency",
    "communal" = "Communality",
    "schoolCollège Citoyen" = "School #4",
    "schoolCollège mariste de Québec" = "School #3",
    "schoolÉcole de la Rose-des-Vents" = "School #6",
    "schoolÉcole Jean-de-Brébeuf" = "School #2",
    "schoolJaya International High School" = "School #5",
    "schoolRenfrew County DSB Student Senate" = "School #8",
    "schoolUrban Village Academy" = "School #7"))
CCPISOld <- CCPIS |> filter(age >= 16 & age < 19)
Model0Old <- nlme::lme(data = CCPISOld, fixed = interest ~ # empty model
                         1, random = ~ 1 | Class, na.action = na.omit)
Model0Oldeffects <- nlme::VarCorr(Model0Old)
100 * as.numeric(Model0Oldeffects[1]) / (as.numeric(Model0Oldeffects[1]) +
                                           as.numeric(Model0Oldeffects[2]))
# ~1.2% of variance in political interest is located at the classroom level
# among students aged 16-18
Model1Old <- nlme::lme(data = CCPISOld, fixed = interest ~ female,
                       random = ~ 1 | Class, na.action = na.omit)
summary(Model1Old) # p<0.05
Model01Old <- nlme::lme(data = CCPISOld, fixed = interest ~ female * age +
                          age_squared + female * white + immig + lang +
                          agentic + communal + school, random = ~ 1 | Class,
                        na.action = na.omit)
summary(Model01Old)
Model2Old <- nlme::lme(data = CCPISOld, fixed = interest_health ~
                         female, random = ~ 1 | Class, na.action = na.omit)
summary(Model2Old) # N.S.
Model02Old <- nlme::lme(data = CCPISOld, fixed = interest_health ~ female *
                          age + age_squared + female * white + immig + lang +
                          agentic + communal + school, random = ~ 1 | Class,
                        na.action = na.omit)
summary(Model02Old)
Model3Old <- nlme::lme(data = CCPISOld, fixed = interest_foreign ~
                         female, random = ~ 1 | Class, na.action = na.omit)
summary(Model3Old) # p<0.01
Model03Old <- nlme::lme(data = CCPISOld, fixed = interest_foreign ~ female *
                          age + age_squared + female * white + immig + lang +
                          agentic + communal + school, random = ~ 1 | Class,
                        na.action = na.omit)
summary(Model03Old)
Model4Old <- nlme::lme(data = CCPISOld, fixed = interest_law ~
                         female, random = ~ 1 | Class, na.action = na.omit)
summary(Model4Old) # N.S.
Model04Old <- nlme::lme(data = CCPISOld, fixed = interest_law ~ female *
                          age + age_squared + female * white + immig + lang +
                          agentic + communal + school, random = ~ 1 | Class,
                        na.action = na.omit)
summary(Model04Old)
Model5Old <- nlme::lme(data = CCPISOld, fixed = interest_education ~
                         female, random = ~ 1 | Class, na.action = na.omit)
summary(Model5Old) # N.S.
Model05Old <- nlme::lme(data = CCPISOld, fixed = interest_education ~
                          female * age + age_squared + female * white + immig +
                          lang + agentic + communal + school, random = ~ 1 |
                          Class, na.action = na.omit)
summary(Model05Old)
Model6Old <- nlme::lme(data = CCPISOld, fixed = interest_partisan ~
                         female, random = ~ 1 | Class, na.action = na.omit)
summary(Model6Old) # p<0.01
Model06Old <- nlme::lme(data = CCPISOld, fixed = interest_partisan ~
                          female * age + age_squared + female * white + immig +
                          lang + agentic + communal + school, random = ~ 1 |
                          Class, na.action = na.omit)
summary(Model06Old)
modelsummary::modelsummary(models = list(
  "Politics (general)" = Model1Old, "Health care" = Model2Old,
  "International affairs" = Model3Old, "Law and crime" = Model4Old,
  "Education" = Model5Old, "Partisan politics" = Model6Old),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  title = paste("Interest in topic by gender (ages 16-18)",
                "{#tbl-lmeInterestOldCCPIS}"),
  coef_rename = c("female1" = "Gender (1 = girl)"))
modelsummary::modelsummary(models = list(
  "Politics (general)" = Model01Old, "Health care" = Model02Old,
  "International affairs" = Model03Old, "Law and crime" = Model04Old,
  "Education" = Model05Old, "Partisan politics" = Model06Old),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  #coef_omit = "age|white|immig|lang|agentic|communal|#school",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            #"Controls: Socio-demographic, personality traits#, schools",
            "Reference Category for Language: Other languages spoken at home",
            "Reference Category for School: School #1"
  ),
  title = paste("Interest in topic by gender (ages 16-18)",
                "{#tbl-lmeInterestOldCCPISAlt}"),
  coef_rename = c(
    "female1" = "Gender (1 = girl)",
    "age" = "Age",
    "age_squared" = "Age squared",
    "white" = "Race (1 = white)",
    "immig" = "Immigrant",
    "langAnglophone" = "English spoken at home",
    "langFrancophone" = "French spoken at home",
    "agentic" = "Agency",
    "communal" = "Communality",
    "schoolCollège Citoyen" = "School #4",
    "schoolCollège mariste de Québec" = "School #3",
    "schoolÉcole de la Rose-des-Vents" = "School #6",
    "schoolÉcole Jean-de-Brébeuf" = "School #2",
    "schoolJaya International High School" = "School #5",
    "schoolRenfrew County DSB Student Senate" = "School #8",
    "schoolUrban Village Academy" = "School #7"))

CCPISGrouped <- CCPIS |>
  group_by(age, female) |>
  summarise(interest = mean(interest, na.rm = TRUE))
CCPISGroupedCategory <- CCPIS |>
  group_by(age, female) |>
  summarise(interest_health = mean(interest_health, na.rm = TRUE),
            interest_foreign = mean(interest_foreign, na.rm = TRUE),
            interest_law = mean(interest_law, na.rm = TRUE),
            interest_education = mean(interest_education, na.rm = TRUE),
            interest_partisan = mean(interest_partisan, na.rm = TRUE))

Plot1 <- ggplot(filter(CCPIS, !is.na(female)),
       aes(x = age, y = interest, color = female)) +
  geom_point(data = filter(CCPISGrouped, !is.na(female)), size = 0.25,
             aes(x = age, y = interest, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "General political interest",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age") +
  scale_color_grey(name = "", end = 0.5, labels = c("Boys", "Girls")) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        text = element_text(family = "CM Roman"))
Plot2 <- ggplot(filter(CCPIS, !is.na(female)),
       aes(x = age, y = interest_health, color = female)) +
  geom_point(data = filter(CCPISGroupedCategory, !is.na(female)), size = 0.25,
             aes(x = age, y = interest_health, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "Interest in health care",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age") +
  scale_color_grey(name = "", end = 0.5, labels = c("Boys", "Girls")) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.position = "none",
        text = element_text(family = "CM Roman"))
Plot3 <- ggplot(filter(CCPIS, !is.na(female)),
                aes(x = age, y = interest_foreign, color = female)) +
  geom_point(data = filter(CCPISGroupedCategory, !is.na(female)), size = 0.25,
             aes(x = age, y = interest_foreign, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "Interest in international affairs",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age") +
  scale_color_grey(name = "", end = 0.5, labels = c("Boys", "Girls")) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.position = "none",
        text = element_text(family = "CM Roman"))
Plot4 <- ggplot(filter(CCPIS, !is.na(female)),
                aes(x = age, y = interest_law, color = female)) +
  geom_point(data = filter(CCPISGroupedCategory, !is.na(female)), size = 0.25,
             aes(x = age, y = interest_law, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "Interest in law and crime",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age") +
  scale_color_grey(name = "", end = 0.5, labels = c("Boys", "Girls")) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.position = "none",
        text = element_text(family = "CM Roman"))
Plot5 <- ggplot(filter(CCPIS, !is.na(female)),
                aes(x = age, y = interest_education, color = female)) +
  geom_point(data = filter(CCPISGroupedCategory, !is.na(female)),
             size = 0.25, aes(x = age, y = interest_education, color = female,
                              weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "Interest in education",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age") +
  scale_color_grey(name = "", end = 0.5, labels = c("Boys", "Girls")) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.position = "none",
        text = element_text(family = "CM Roman"))
Plot6 <- ggplot(filter(CCPIS, !is.na(female)),
                aes(x = age, y = interest_partisan, color = female)) +
  geom_point(data = filter(CCPISGroupedCategory, !is.na(female)),
             size = 0.25, aes(x = age, y = interest_partisan, color = female,
                              weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "Interest in partisan politics",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age") +
  scale_color_grey(name = "", end = 0.5, labels = c("Boys", "Girls")) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.position = "none",
        text = element_text(family = "CM Roman"))
ggsave(plot = ggpubr::ggarrange(Plot1, Plot2, Plot3, Plot4, Plot5, Plot6,
       nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom"),
       "_graphs/InterestAgeGenderCCPIS.pdf", width = 11, height = 8.5)

Model1DG <- lm(data = DG, formula = interest ~ female)
# women's political interest = 6.9/10; men's political interest = 7.7; p<0.001
Model01DG <- lm(data = DG, formula = interest ~ female * age + age_squared +
                  female * white + immig + lang + income_mid + income_high +
                  educ_mid + educ_high)
Model2DG <- lm(data = DG, formula = interest_health ~ female)
Model02DG <- lm(data = DG, formula = interest_health ~ female * age +
                  age_squared + female * white + immig + lang +
                  income_mid + income_high + educ_mid + educ_high)
Model3DG <- lm(data = DG, formula = interest_foreign ~ female)
Model03DG <- lm(data = DG, formula = interest_foreign ~ female * age +
                  age_squared + female * white + immig + lang + income_mid +
                  income_high + educ_mid + educ_high)
Model4DG <- lm(data = DG, formula = interest_law ~ female)
Model04DG <- lm(data = DG, formula = interest_law ~ female * age +
                  age_squared + female * white + immig + lang + income_mid +
                  income_high + educ_mid + educ_high)
Model5DG <- lm(data = DG, formula = interest_education ~ female)
Model05DG <- lm(data = DG, formula = interest_education ~ female * age +
                  age_squared + female * white + immig + lang + income_mid +
                  income_high + educ_mid + educ_high)
Model6DG <- lm(data = DG, formula = interest_partisan ~ female)
Model06DG <- lm(data = DG, formula = interest_partisan ~ female * age +
                  age_squared + female * white + immig + lang + income_mid +
                  income_high + educ_mid + educ_high)
modelsummary::modelsummary(models = list(
  "Simple" = list("Politics (general)" = Model1DG, "Health care" = Model2DG,
                  "International affairs" = Model3DG,
                  "Law and crime" = Model4DG, "Education" = Model5DG,
                  "Partisan politics" = Model6DG),
  "Multiple" = list("Politics (general)" = Model01DG,
                    "Health care" = Model02DG,
                    "International affairs" = Model03DG,
                    "Law and crime" = Model04DG, "Education" = Model05DG,
                    "Partisan politics" = Model06DG)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  notes = "Ordinary least squares (OLS) regression",
  title = "(\\#tab:olsInterestDg) Political interest by gender",
  coef_rename = c(
    "female1" = "Gender (1 = women)",
    "age" = "Age",
    "age_squared" = "Age squared",
    "white" = "Race (1 = white)",
    "immig" = "Immigrant",
    "langFrancophone" = "French spoken at home",
    "income_mid" = "Income between \\$60,000 and \\$150,000",
    "income_high" = "Income above \\$150,000",
    "educ_mid" = "Education: college",
    "educ_high" = "Education: university"))

DGgrouped <- DG |>
  group_by(age, female) |>
  summarise(interest = mean(interest, na.rm = TRUE))
GroupedDGCategory <- DG |>
  group_by(age, female) |>
  summarise(interest_health = mean(interest_health, na.rm = TRUE),
            interest_foreign = mean(interest_foreign, na.rm = TRUE),
            interest_law = mean(interest_law, na.rm = TRUE),
            interest_education = mean(interest_education, na.rm = TRUE),
            interest_partisan = mean(interest_partisan, na.rm = TRUE))

DGPlot1 <- ggplot(filter(DG, !is.na(female)),
                  aes(x = age, y = interest, color = female)) +
  geom_point(data = filter(DGgrouped, !is.na(female)), size = 0.25,
             aes(x = age, y = interest, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "General political interest",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        text = element_text(family = "CM Roman")) +
  ggtitle("Political interest")
DGPlot2 <- ggplot(filter(DG, !is.na(female)),
                  aes(x = age, y = interest_health, color = female)) +
  geom_point(data = filter(GroupedDGCategory, !is.na(female)), size = 0.25,
             aes(x = age, y = interest_health, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "Interest in health care",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.position = "none",
        text = element_text(family = "CM Roman")) +
  ggtitle("Health care")
DGPlot3 <- ggplot(filter(DG, !is.na(female)),
                  aes(x = age, y = interest_foreign, color = female)) +
  geom_point(data = filter(GroupedDGCategory, !is.na(female)), size = 0.25,
             aes(x = age, y = interest_foreign, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "Interest in international affairs",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.position = "none",
        text = element_text(family = "CM Roman")) +
  ggtitle("International affairs")
DGPlot4 <- ggplot(filter(DG, !is.na(female)),
                  aes(x = age, y = interest_law, color = female)) +
  geom_point(data = filter(GroupedDGCategory, !is.na(female)), size = 0.25,
             aes(x = age, y = interest_law, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "Interest in law and crime",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.position = "none",
        text = element_text(family = "CM Roman")) +
  ggtitle("Law and crime")
DGPlot5 <- ggplot(filter(DG, !is.na(female)),
                  aes(x = age, y = interest_education, color = female)) +
  geom_point(data = filter(GroupedDGCategory, !is.na(female)), size = 0.25,
             aes(x = age, y = interest_education, color = female,
                 weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "Interest in education",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.position = "none",
        text = element_text(family = "CM Roman")) +
  ggtitle("Education")
DGPlot6 <- ggplot(filter(DG, !is.na(female)),
                  aes(x = age, y = interest_partisan, color = female)) +
  geom_point(data = filter(GroupedDGCategory, !is.na(female)), size = 0.25,
             aes(x = age, y = interest_partisan, color = female,
                 weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "Interest in partisan politics",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        legend.position = "none",
        text = element_text(family = "CM Roman")) +
  ggtitle("Partisan politics")
ggsave(plot = ggpubr::ggarrange(
  DGPlot1, DGPlot2, DGPlot3, DGPlot4, DGPlot5, DGPlot6, nrow = 2, ncol = 3,
  common.legend = TRUE, legend = "bottom"),
  "_graphs/InterestAgeGenderDGPanel.pdf", width = 11, height = 8.5)

CES21grouped <- CES21 |>
  group_by(age, female) |>
  summarise(interest = weighted.mean(interest, w = weight, na.rm = TRUE))
PlotTimeCES <- ggplot(filter(CES21, !is.na(female)),
       aes(x = age, y = interest / 10, color = female, weight = weight)) +
  geom_point(data = filter(CES21grouped, !is.na(female)), size = 0.25,
             aes(x = age, y = interest / 10, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "General political\ninterest",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age, 2021 CES", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme(text = element_text(family = "CM Roman"))

summary(lm(data = filter(CES21, age <= 50), formula = interest / 10 ~ female,
           weights = weight))
summary(lm(data = CES21, formula = interest / 10 ~ female,
           weights = weight))
# women's political interest = 5.4/10; men's political interest = 6.8/10;
# p<0.001

WVSCA20grouped <- WVSCA20 |>
  group_by(age, female) |>
  summarise(interest = weighted.mean(interest, w = weight, na.rm = TRUE))
PlotTimeWVSCA <- ggplot(filter(WVSCA20, !is.na(female)),
       aes(x = age, y = interest / 10, color = female, weight = weight)) +
  geom_point(data = filter(WVSCA20grouped, !is.na(female)), size = 0.25,
             aes(x = age, y = interest / 10, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "General political\ninterest",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age, 2020 WVS, Canada", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme(text = element_text(family = "CM Roman"))

summary(lm(data = WVSCA20, formula = interest / 10 ~ female, weights = weight))
# women's political interest = 5/10; men's political interest = 6.2/10;
# p<0.001

WVSWave7grouped <- WVSWave7 |>
  group_by(age, female) |>
  summarise(interest = weighted.mean(interest, w = weight, na.rm = TRUE))
PlotTimeWVS <- ggplot(filter(WVSWave7, !is.na(female)),
                      aes(x = age, y = interest / 10, color = female,
                          weight = weight)) +
  geom_point(data = filter(WVSWave7grouped, !is.na(female)), size = 0.25,
             aes(x = age, y = interest / 10, color = female, weight = NULL)) +
  geom_smooth() +
  scale_y_continuous(name = "General political\ninterest",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age, 2017/22 WVS", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme(text = element_text(family = "CM Roman"))

summary(lm(data = WVSWave7, formula = interest / 10 ~ female,
           weights = weight))
# women's political interest = 4/10; men's political interest = 4.8/10;
# p<0.001
summary(lm(data = WVSWave7, formula = interest / 10 ~ female * canada,
           weights = weight))

GSSgrouped <- GSS20 |>
  group_by(age, female) |>
  summarise(interest = weighted.mean(interest, w = weight, na.rm = TRUE))
PlotTimeGSS <- ggplot(filter(GSS20, !is.na(female)),
       aes(x = age, y = interest / 10, color = female, weight = weight,
           group = as.factor(female))) +
  geom_smooth(data = filter(GSSgrouped, !is.na(female)), linewidth = 0.25,
            aes(x = age, y = interest / 10, color = female, weight = NULL)) +
  geom_point(data = filter(GSSgrouped, !is.na(female)), size = 0.25,
             aes(x = age, y = interest / 10, color = female, weight = NULL)) +
  scale_y_continuous(name = "General political\ninterest",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_discrete(name = "Age, 2020 GSS, Canada") +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(family = "CM Roman"))
ggsave(plot = ggpubr::ggarrange(
  PlotTimeCES, PlotTimeWVS, PlotTimeWVSCA, PlotTimeGSS,
  nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom"),
  "_graphs/TimeCESWVSGSS.pdf", height = 4.25, width = 5.5)

summary(lm(data = GSS20, formula = interest / 10 ~ female, weights = weight))

### 3.1.2 Political interest by year & gender (CES & WVS) ####
weighted.interest.se <- function(data) {
  weighted.variance <- Hmisc::wtd.var(data$interest,
                                      weight = data$weight)
  sqrt(weighted.variance / length(data$interest))
}

InterestCESGenderData <- data.frame(
  year = as.integer(c(1997, 1997, 2000, 2000, 2004, 2004, 2006, 2006,
                      2008, 2008, 2011, 2011, 2015, 2015, 2019, 2019,
                      2021, 2021)),
  interest = c(Hmisc::wtd.mean(CES97men$interest,
                               weight = CES97men$weight),
               Hmisc::wtd.mean(CES97women$interest,
                               weight = CES97women$weight),
               Hmisc::wtd.mean(CES00men$interest,
                               weight = CES00men$weight),
               Hmisc::wtd.mean(CES00women$interest,
                               weight = CES00women$weight),
               Hmisc::wtd.mean(CES04men$interest,
                               weight = CES04men$weight),
               Hmisc::wtd.mean(CES04women$interest,
                               weight = CES04women$weight),
               Hmisc::wtd.mean(CES06men$interest,
                               weight = CES06men$weight),
               Hmisc::wtd.mean(CES06women$interest,
                               weight = CES06women$weight),
               Hmisc::wtd.mean(CES08men$interest,
                               weight = CES08men$weight),
               Hmisc::wtd.mean(CES08women$interest,
                               weight = CES08women$weight),
               Hmisc::wtd.mean(CES11men$interest,
                               weight = CES11men$weight),
               Hmisc::wtd.mean(CES11women$interest,
                               weight = CES11women$weight),
               Hmisc::wtd.mean(CES15men$interest,
                               weight = CES15men$weight),
               Hmisc::wtd.mean(CES15women$interest,
                               weight = CES15women$weight),
               Hmisc::wtd.mean(CES19men$interest,
                               weight = CES19men$weight),
               Hmisc::wtd.mean(CES19women$interest,
                               weight = CES19women$weight),
               Hmisc::wtd.mean(CES21men$interest,
                               weight = CES21men$weight),
               Hmisc::wtd.mean(CES21women$interest,
                               weight = CES21women$weight)),
  female = as.factor(rep(c(0, 1), 9)))
InterestWVSGenderData <- data.frame(
  year = as.integer(c(1990, 1990, 2000, 2000, 2006, 2006, 2020, 2020)),
  interest = c(Hmisc::wtd.mean(WVSCA90men$interest,
                               weight = WVSCA90men$weight),
               Hmisc::wtd.mean(WVSCA90women$interest,
                               weight = WVSCA90women$weight),
               Hmisc::wtd.mean(WVSCA00men$interest,
                               weight = WVSCA00men$weight),
               Hmisc::wtd.mean(WVSCA00women$interest,
                               weight = WVSCA00women$weight),
               Hmisc::wtd.mean(WVSCA06men$interest,
                               weight = WVSCA06men$weight),
               Hmisc::wtd.mean(WVSCA06women$interest,
                               weight = WVSCA06women$weight),
               Hmisc::wtd.mean(WVSCA20men$interest,
                               weight = WVSCA20men$weight),
               Hmisc::wtd.mean(WVSCA20women$interest,
                               weight = WVSCA20women$weight)),
  female = as.factor(rep(c(0, 1), 4)))
InterestGenderData <- rbind(InterestCESGenderData, InterestWVSGenderData)
InterestGenderData$survey <- c(rep("CES", 18), rep("WVS", 8))
ggplot(InterestGenderData, aes(x = year, y = interest, color = female,
                               linetype = survey)) +
  geom_line() +
  scale_y_continuous(name = "General political interest",
                     limits = c(0, 100)) +
  scale_x_continuous(name = "Year") +
  scale_color_grey(name = "Gender", end = 0.5, labels = c("Men", "Women")) +
  scale_linetype(name = "Survey") +
  theme(text = element_text(family = "CM Roman"))
ggsave("_graphs/InterestYearGender.pdf", height = 4.25, width = 5.5)

summary(lm(data = InterestCESGenderData, formula = interest ~ female))
# in all CES waves: women's political interest = 56%;
# men's political interest = 63%; p<0.05
summary(lm(data = InterestWVSGenderData, formula = interest ~ female))
# in all WVS waves: women's political interest = 47%;
# men's political interest = 56%; p<0.05

### 3.1.3 Political interest by year, gender & race (CES & WVS) ####
GenderEthnicityInterest <- data.frame(
  group = as.factor(c(rep(c("White men", "White women",
                            "Nonwhite men", "Nonwhite women"), 3),
                      "Immigrant men", "Immigrant women",
                      "Nonimmigrant men", "Nonimmigrant women",
                      "White men", "White women",
                      "Nonwhite men", "Nonwhite women",
                      "Immigrant men", "Immigrant women",
                      "Nonimmigrant men", "Nonimmigrant women")),
  interest = c(Hmisc::wtd.mean(WVSCA00whitemen$interest,
                               weight = WVSCA00whitemen$weight),
               Hmisc::wtd.mean(WVSCA00whitewomen$interest,
                               weight = WVSCA00whitewomen$weight),
               Hmisc::wtd.mean(WVSCA00nonwhitemen$interest,
                               weight = WVSCA00nonwhitemen$weight),
               Hmisc::wtd.mean(WVSCA00nonwhitewomen$interest,
                               weight = WVSCA00nonwhitewomen$weight),
               Hmisc::wtd.mean(WVSCA06whitemen$interest,
                               weight = WVSCA06whitemen$weight),
               Hmisc::wtd.mean(WVSCA06whitewomen$interest,
                               weight = WVSCA06whitewomen$weight),
               Hmisc::wtd.mean(WVSCA06nonwhitemen$interest,
                               weight = WVSCA06nonwhitemen$weight),
               Hmisc::wtd.mean(WVSCA06nonwhitewomen$interest,
                               weight = WVSCA06nonwhitewomen$weight),
               Hmisc::wtd.mean(WVSCA20whitemen$interest,
                               weight = WVSCA20whitemen$weight),
               Hmisc::wtd.mean(WVSCA20whitewomen$interest,
                               weight = WVSCA20whitewomen$weight),
               Hmisc::wtd.mean(WVSCA20nonwhitemen$interest,
                               weight = WVSCA20nonwhitemen$weight),
               Hmisc::wtd.mean(WVSCA20nonwhitewomen$interest,
                               weight = WVSCA20nonwhitewomen$weight),
               Hmisc::wtd.mean(WVSCA20immigrantmen$interest,
                               weight = WVSCA20immigrantmen$weight),
               Hmisc::wtd.mean(WVSCA20immigrantwomen$interest,
                               weight = WVSCA20immigrantwomen$weight),
               Hmisc::wtd.mean(WVSCA20nonimmigrantmen$interest,
                               weight = WVSCA20nonimmigrantmen$weight),
               Hmisc::wtd.mean(WVSCA20nonimmigrantwomen$interest,
                               weight = WVSCA20nonimmigrantwomen$weight),
               Hmisc::wtd.mean(CES21whitemen$interest,
                               weight = CES21whitemen$weight),
               Hmisc::wtd.mean(CES21whitewomen$interest,
                               weight = CES21whitewomen$weight),
               Hmisc::wtd.mean(CES21nonwhitemen$interest,
                               weight = CES21nonwhitemen$weight),
               Hmisc::wtd.mean(CES21nonwhitewomen$interest,
                               weight = CES21nonwhitewomen$weight),
               Hmisc::wtd.mean(CES21immigrantmen$interest,
                               weight = CES21immigrantmen$weight),
               Hmisc::wtd.mean(CES21immigrantwomen$interest,
                               weight = CES21immigrantwomen$weight),
               Hmisc::wtd.mean(CES21nonimmigrantmen$interest,
                               weight = CES21nonimmigrantmen$weight),
               Hmisc::wtd.mean(CES21nonimmigrantwomen$interest,
                               weight = CES21nonimmigrantwomen$weight)),
  year = as.factor(c(rep("WVS 2000", 4), rep("WVS 2006", 4),
                     rep("WVS 2020", 8), rep("CES 2021", 8))))
GenderEthnicityInterest$interest.se <- c(
  weighted.interest.se(WVSCA00whitemen),
  weighted.interest.se(WVSCA00whitewomen),
  weighted.interest.se(WVSCA00nonwhitemen),
  weighted.interest.se(WVSCA00nonwhitewomen),
  weighted.interest.se(WVSCA06whitemen),
  weighted.interest.se(WVSCA06whitewomen),
  weighted.interest.se(WVSCA06nonwhitemen),
  weighted.interest.se(WVSCA06nonwhitewomen),
  weighted.interest.se(WVSCA20whitemen),
  weighted.interest.se(WVSCA20whitewomen),
  weighted.interest.se(WVSCA20nonwhitemen),
  weighted.interest.se(WVSCA20nonwhitewomen),
  weighted.interest.se(WVSCA20immigrantmen),
  weighted.interest.se(WVSCA20immigrantwomen),
  weighted.interest.se(WVSCA20nonimmigrantmen),
  weighted.interest.se(WVSCA20nonimmigrantwomen),
  weighted.interest.se(CES21whitemen),
  weighted.interest.se(CES21whitewomen),
  weighted.interest.se(CES21nonwhitemen),
  weighted.interest.se(CES21nonwhitewomen),
  weighted.interest.se(CES21immigrantmen),
  weighted.interest.se(CES21immigrantwomen),
  weighted.interest.se(CES21nonimmigrantmen),
  weighted.interest.se(CES21nonimmigrantwomen))
GenderEthnicityInterest$interest.lb <-
  GenderEthnicityInterest$interest +
  qnorm(0.025) * GenderEthnicityInterest$interest.se
GenderEthnicityInterest$interest.ub <-
  GenderEthnicityInterest$interest +
  qnorm(0.975) * GenderEthnicityInterest$interest.se
GenderEthnicityInterest$group <- factor(
  GenderEthnicityInterest$group,
  levels = c("White men", "White women", "Nonwhite men",
             "Nonwhite women", "Immigrant men", "Immigrant women",
             "Nonimmigrant men", "Nonimmigrant women"))
ggplot(GenderEthnicityInterest, aes(x = group, y = interest,
                                    color = year, )) +
  geom_point(position = position_dodge(width = 0.5), size = 0.75) +
  geom_errorbar(aes(ymin = interest.lb, ymax = interest.ub),
                width = 0.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous(name = "General political interest",
                     limits = c(0, 100)) +
  scale_x_discrete(name = "Group") +
  scale_color_grey(name = "Wave") +
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/InterestWaveGroup.pdf", height = 4.25, width = 5.5)

### 3.1.4 Political interest by race (2017-22 CES & WVS) ####
EthnicityInterestWVS <- data.frame(
  ethnicity = as.factor(c("White", "Black", "West Asian",
                          "Southeast Asian", "Arabic", "South Asian",
                          "Hispanic", "Indigenous", "Chinese",
                          "Filipino", "Korean", "Japanese", "Other")),
  interest = c(Hmisc::wtd.mean(WVSCA20white$interest,
                               weight = WVSCA20white$weight),
               Hmisc::wtd.mean(WVSCA20black$interest,
                               weight = WVSCA20black$weight),
               Hmisc::wtd.mean(WVSCA20westasian$interest,
                               weight = WVSCA20westasian$weight),
               Hmisc::wtd.mean(WVSCA20southeastasian$interest,
                               weight = WVSCA20southeastasian$weight),
               Hmisc::wtd.mean(WVSCA20arabic$interest,
                               weight = WVSCA20arabic$weight),
               Hmisc::wtd.mean(WVSCA20southasian$interest,
                               weight = WVSCA20southasian$weight),
               Hmisc::wtd.mean(WVSCA20hispanic$interest,
                               weight = WVSCA20hispanic$weight),
               Hmisc::wtd.mean(WVSCA20indigenous$interest,
                               weight = WVSCA20indigenous$weight),
               Hmisc::wtd.mean(WVSCA20chinese$interest,
                               weight = WVSCA20chinese$weight),
               Hmisc::wtd.mean(WVSCA20filipino$interest,
                               weight = WVSCA20filipino$weight),
               Hmisc::wtd.mean(WVSCA20korean$interest,
                               weight = WVSCA20korean$weight),
               Hmisc::wtd.mean(WVSCA20japanese$interest,
                               weight = WVSCA20japanese$weight),
               Hmisc::wtd.mean(WVSCA20other$interest,
                               weight = WVSCA20other$weight)))
EthnicityInterestWVS$interest.se <- c(
  weighted.interest.se(WVSCA20white),
  weighted.interest.se(WVSCA20black),
  weighted.interest.se(WVSCA20westasian),
  weighted.interest.se(WVSCA20southeastasian),
  weighted.interest.se(WVSCA20arabic),
  weighted.interest.se(WVSCA20southasian),
  weighted.interest.se(WVSCA20hispanic),
  weighted.interest.se(WVSCA20indigenous),
  weighted.interest.se(WVSCA20chinese),
  weighted.interest.se(WVSCA20filipino),
  weighted.interest.se(WVSCA20korean),
  weighted.interest.se(WVSCA20japanese),
  weighted.interest.se(WVSCA20other))
EthnicityInterestWVS$interest.lb <- EthnicityInterestWVS$interest +
  qnorm(0.025) * EthnicityInterestWVS$interest.se
EthnicityInterestWVS$interest.ub <- EthnicityInterestWVS$interest +
  qnorm(0.975) * EthnicityInterestWVS$interest.se
EthnicityInterestCES <- data.frame(
  ethnicity = as.factor(c("White", "Black", "West Asian",
                          "Southeast Asian", "Arabic", "South Asian",
                          "Hispanic", "Indigenous", "Other")),
  interest = c(Hmisc::wtd.mean(CES21white$interest,
                               weight = CES21white$weight),
               Hmisc::wtd.mean(CES21black$interest,
                               weight = CES21black$weight),
               Hmisc::wtd.mean(CES21westasian$interest,
                               weight = CES21westasian$weight),
               Hmisc::wtd.mean(CES21southeastasian$interest,
                               weight = CES21southeastasian$weight),
               Hmisc::wtd.mean(CES21arabic$interest,
                               weight = CES21arabic$weight),
               Hmisc::wtd.mean(CES21southasian$interest,
                               weight = CES21southasian$weight),
               Hmisc::wtd.mean(CES21hispanic$interest,
                               weight = CES21hispanic$weight),
               Hmisc::wtd.mean(CES21indigenous$interest,
                               weight = CES21indigenous$weight),
               Hmisc::wtd.mean(CES21other$interest,
                               weight = CES21other$weight)))
EthnicityInterestCES$interest.se <- c(
  weighted.interest.se(CES21white),
  weighted.interest.se(CES21black),
  weighted.interest.se(CES21westasian),
  weighted.interest.se(CES21southeastasian),
  weighted.interest.se(CES21arabic),
  weighted.interest.se(CES21southasian),
  weighted.interest.se(CES21hispanic),
  weighted.interest.se(CES21indigenous),
  weighted.interest.se(CES21other))
EthnicityInterestCES$interest.lb <- EthnicityInterestCES$interest +
  qnorm(0.025) * EthnicityInterestCES$interest.se
EthnicityInterestCES$interest.ub <- EthnicityInterestCES$interest +
  qnorm(0.975) * EthnicityInterestCES$interest.se
EthnicityInterest <- rbind(EthnicityInterestCES, EthnicityInterestWVS)
EthnicityInterest$survey <- c(rep("CES 2021", 9), rep("WVS 2020", 13))
EthnicityInterest$ethnicity <- factor(
  EthnicityInterest$ethnicity,
  levels = c("Arabic", "Black", "Chinese", "Filipino", "Hispanic",
             "Indigenous", "Japanese", "Korean", "South Asian",
             "Southeast Asian", "West Asian", "White", "Other"))
ggplot(EthnicityInterest, aes(x = ethnicity, y = interest,
                              color = survey)) +
  geom_point(position = position_dodge(width = 0.75), size = 0.75) +
  geom_errorbar(aes(ymin = interest.lb, ymax = interest.ub),
                width = 0.5, position = position_dodge(width = 0.75)) +
  scale_y_continuous(name = "General political interest",
                     limits = c(0, 100)) +
  scale_x_discrete(name = "Ethnicity") +
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(family = "CM Roman")) +
  scale_color_grey(name = "Survey", end = 0.5)
ggsave("_graphs/InterestEthnicity20_21.pdf", height = 4.25, width = 5.5)

#### 3.2 Chapter 4 ####
### Effect of parents (CCPIS) ####
CCPISBoysLonger <- pivot_longer(CCPISBoys,
                                   cols = c(starts_with("gender_parent_")))
CCPISBoysLonger$sex <- "Boys"
CCPISGirlsLonger <- pivot_longer(CCPISGirls,
                                    cols = c(starts_with("gender_parent_")))
CCPISGirlsLonger$sex <- "Girls"
CCPISLonger <- rbind(CCPISBoysLonger, CCPISGirlsLonger)
CCPISLonger$topic <- case_when(
  CCPISLonger$name == "gender_parent_health" ~ "Health care",
  CCPISLonger$name == "gender_parent_education" ~ "Education",
  CCPISLonger$name == "gender_parent_law" ~ "Law and\ncrime",
  CCPISLonger$name == "gender_parent_foreign" ~ "Internatio-\nnal affairs",
  CCPISLonger$name == "gender_parent_partisan" ~ "Partisan\npolitics")
ggplot(CCPISLonger, aes(x = sex, fill = as.factor(value))) +
  geom_bar(position = "fill") +
  facet_wrap(~topic, ncol = 5) +
  scale_x_discrete("Gender") +
  scale_y_continuous("Percent of students", labels = scales::percent) +
  scale_fill_discrete("Parent who\ndiscusses the\ntopic most often",
                      labels = c("Father", "Mother",
                                 paste0("Don't know/\nPrefer not\nto answer/",
                                        "\nMissing")),
                      type = c("purple", "orange", "white")) +
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/ParentTopics.pdf", width = 5.5, height = 4.25)

CCPISBoysGraph <- CCPISBoys |>
  pivot_longer(cols = c(mother_discuss_clean, father_discuss_clean)) |>
  group_by(name) |>
  mutate(N = n()) |>
  group_by(name, value) |>
  summarise(perc = n() / unique(N)) |>
  filter(!is.na(value))
CCPISBoysGraph$sex <- "Boys"
CCPISGirlsGraph <- CCPISGirls |>
  pivot_longer(cols = c(mother_discuss_clean, father_discuss_clean)) |>
  group_by(name) |>
  mutate(N = n()) |>
  group_by(name, value) |>
  summarise(perc = n() / unique(N)) |>
  filter(!is.na(value))
CCPISGirlsGraph$sex <- "Girls"
CCPISGraph <- rbind(CCPISBoysGraph, CCPISGirlsGraph)
ggplot(CCPISGraph, aes(x = value, y = perc, fill = name)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~sex) +
  scale_x_discrete("Topic most often discussed with parent") +
  scale_y_continuous("Percent of students", labels = scales::percent) +
  scale_fill_discrete("Parent",
                      labels = c("Father", "Mother"),
                      type = c("purple", "orange")) +
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/ParentTopicsMomDad.pdf", width = 5.5, height = 4.25)

#### Regression models ####
Model100 <- nlme::lme(data = CCPISBoys, fixed = interest_health ~
                        gender_parent_health + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model110 <- nlme::lme(data = CCPISGirls, fixed = interest_health ~
                        gender_parent_health + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model120 <- nlme::lme(data = CCPISBoys, fixed = interest_foreign ~
                        gender_parent_foreign + age + age_squared + white +
                        immig + lang, random = ~ 1 | Class,
                      na.action = na.omit)
Model130 <- nlme::lme(data = CCPISGirls, fixed = interest_foreign ~
                        gender_parent_foreign + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model140 <- nlme::lme(data = CCPISBoys, fixed = interest_law ~
                        gender_parent_law + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model150 <- nlme::lme(data = CCPISGirls, fixed = interest_law ~
                        gender_parent_law + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model160 <- nlme::lme(data = CCPISBoys, fixed = interest_education ~
                        gender_parent_education + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model170 <- nlme::lme(data = CCPISGirls, fixed = interest_education ~
                        gender_parent_education + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model180 <- nlme::lme(data = CCPISBoys, fixed = interest_partisan ~
                        gender_parent_partisan + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model190 <- nlme::lme(data = CCPISGirls, fixed = interest_partisan ~
                        gender_parent_partisan + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
CCPISBoysParentLonger <- pivot_longer(CCPISBoys,
                                      cols = starts_with("gender_parent_"))
CCPISBoysParentLonger <- CCPISBoysParentLonger |>
  mutate(interest_all = case_when(
    name == "gender_parent_health" ~ interest_health,
    name == "gender_parent_foreign" ~ interest_foreign,
    name == "gender_parent_law" ~ interest_law,
    name == "gender_parent_education" ~ interest_education,
    name == "gender_parent_partisan" ~ interest_partisan))
ModelAllParentB <- nlme::lme(data = CCPISBoysParentLonger, fixed =
                               interest_all ~ value + age + age_squared +
                               white + immig + lang + agentic + communal,
                             random = ~ 1 | Class, na.action = na.omit)
CCPISGirlsParentLonger <- pivot_longer(CCPISGirls,
                                       cols = starts_with("gender_parent_"))
CCPISGirlsParentLonger <- CCPISGirlsParentLonger |>
  mutate(interest_all = case_when(
    name == "gender_parent_health" ~ interest_health,
    name == "gender_parent_foreign" ~ interest_foreign,
    name == "gender_parent_law" ~ interest_law,
    name == "gender_parent_education" ~ interest_education,
    name == "gender_parent_partisan" ~ interest_partisan))
ModelAllParentG <- nlme::lme(data = CCPISGirlsParentLonger, fixed =
                               interest_all ~ value + age + age_squared +
                               white + immig + lang + agentic + communal,
                             random = ~ 1 | Class, na.action = na.omit)
Models10 <- tibble::tribble(
  ~a, ~b, ~c, ~d, ~e, ~f, ~g,
  "_______________\n**Results among boys**", '', '', '', '', '', '',
  "_______________\n**Results among girls**", '', '', '', '', '', '')
attr(Models10, 'position') <- c(1, 26)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelAllParentB, "Health care" = Model100,
                "International affairs" = Model120, "Law and crime" = Model140,
                "Education" = Model160, "Partisan politics" = Model180),
  "Girls" = list("All" = ModelAllParentG, "Health care" = Model110,
                 "International affairs" = Model130,
                 "Law and crime" = Model150, "Education" = Model170,
                 "Partisan politics" = Model190)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  add_rows = Models10,
  #coef_omit = "age|white|immig|lang|agentic|communal|#school",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            #"Controls: Socio-demographic, personality traits#, schools",
            "Reference Category for Language: Other languages spoken at home",
            "Reference Category for School: School #1"
  ),
  title = paste("Interest in topic by gender of parent who discusses that",
                "topic the most {#tbl-lmeParentAlt}"),
  coef_rename = c(
    "value" = "Mother discusses topic more than father",
    "gender_parent_health" = "Mother discusses topic more than father",
    "gender_parent_foreign" = "Mother discusses topic more than father",
    "gender_parent_law" = "Mother discusses topic more than father",
    "gender_parent_education" = "Mother discusses topic more than father",
    "gender_parent_partisan" = "Mother discusses topic more than father",
    "age" = "Age",
    "age_squared" = "Age squared",
    "white" = "Race (1 = white)",
    "immig" = "Immigrant",
    "langAnglophone" = "English spoken at home",
    "langFrancophone" = "French spoken at home",
    "agentic" = "Agency",
    "communal" = "Communality"))
Model10 <- nlme::lme(data = CCPISBoys, fixed = interest_health ~
                       gender_parent_health, random = ~ 1 | Class,
                     na.action = na.omit)
Model11 <- nlme::lme(data = CCPISGirls, fixed = interest_health ~
                       gender_parent_health, random = ~ 1 | Class,
                     na.action = na.omit)
Model12 <- nlme::lme(data = CCPISBoys, fixed = interest_foreign ~
                       gender_parent_foreign, random = ~ 1 | Class,
                     na.action = na.omit)
Model13 <- nlme::lme(data = CCPISGirls, fixed = interest_foreign ~
                       gender_parent_foreign, random = ~ 1 | Class,
                     na.action = na.omit)
Model14 <- nlme::lme(data = CCPISBoys, fixed = interest_law ~
                       gender_parent_law, random = ~ 1 | Class,
                     na.action = na.omit)
Model15 <- nlme::lme(data = CCPISGirls, fixed = interest_law ~
                       gender_parent_law, random = ~ 1 | Class,
                     na.action = na.omit)
Model16 <- nlme::lme(data = CCPISBoys, fixed = interest_education ~
                       gender_parent_education, random = ~ 1 | Class,
                     na.action = na.omit)
Model17 <- nlme::lme(data = CCPISGirls, fixed = interest_education ~
                       gender_parent_education, random = ~ 1 | Class,
                     na.action = na.omit)
Model18 <- nlme::lme(data = CCPISBoys, fixed = interest_partisan ~
                       gender_parent_partisan, random = ~ 1 | Class,
                     na.action = na.omit)
Model19 <- nlme::lme(data = CCPISGirls, fixed = interest_partisan ~
                       gender_parent_partisan, random = ~ 1 | Class,
                     na.action = na.omit)
ModelParentB <- nlme::lme(data = CCPISBoysParentLonger, fixed =
                            interest_all ~ value,
                          random = ~ 1 | Class, na.action = na.omit)
ModelParentG <- nlme::lme(data = CCPISGirlsParentLonger, fixed =
                            interest_all ~ value,
                          random = ~ 1 | Class, na.action = na.omit)
Models1 <- tibble::tribble(~a, ~b, ~c, ~d, ~e, ~f, ~g,
                           "**Results among boys**", '', '', '', '', '', '',
                           '**Results among girls**', '', '', '', '', '', '')
attr(Models1, 'position') <- c(1, 10)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelParentB, "Health care" = Model10,
                "International affairs" = Model12, "Law and crime" = Model14,
                "Education" = Model16, "Partisan politics" = Model18),
  "Girls" = list("All" = ModelParentG, "Health care" = Model11,
                 "International affairs" = Model13, "Law and crime" = Model15,
                 "Education" = Model17, "Partisan politics" = Model19)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  add_rows = Models1,
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  title = paste("Interest in topic by gender of parent who discusses that",
                "topic the most {#tbl-lmeParent}"),
  coef_rename = c(
    "value" = "Mother discusses topic more than father",
    "gender_parent_health" = "Mother discusses topic more than father",
    "gender_parent_foreign" = "Mother discusses topic more than father",
    "gender_parent_law" = "Mother discusses topic more than father",
    "gender_parent_education" = "Mother discusses topic more than father",
    "gender_parent_partisan" = "Mother discusses topic more than father"))

Model200 <- nlme::lme(data = CCPISBoys, fixed = interest_health ~
                        mother_discuss_health + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model210 <- nlme::lme(data = CCPISGirls, fixed = interest_health ~
                        mother_discuss_health + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model220 <- nlme::lme(data = CCPISBoys, fixed = interest_foreign ~
                        mother_discuss_foreign + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model230 <- nlme::lme(data = CCPISGirls, fixed = interest_foreign ~
                        mother_discuss_foreign + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model240 <- nlme::lme(data = CCPISBoys, fixed = interest_law ~
                        mother_discuss_law + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model250 <- nlme::lme(data = CCPISGirls, fixed = interest_law ~
                        mother_discuss_law + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model260 <- nlme::lme(data = CCPISBoys, fixed = interest_education ~
                        mother_discuss_education + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model270 <- nlme::lme(data = CCPISGirls, fixed = interest_education ~
                        mother_discuss_education + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model280 <- nlme::lme(data = CCPISBoys, fixed = interest_partisan ~
                        mother_discuss_partisan + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model290 <- nlme::lme(data = CCPISGirls, fixed = interest_partisan ~
                        mother_discuss_partisan + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
CCPISBoysMotherLonger <- pivot_longer(CCPISBoys, cols = c(
  "mother_discuss_health", "mother_discuss_foreign", "mother_discuss_law",
  "mother_discuss_education", "mother_discuss_partisan"))
CCPISBoysMotherLonger <- CCPISBoysMotherLonger |>
  mutate(interest_all = case_when(
    name == "mother_discuss_health" ~ interest_health,
    name == "mother_discuss_foreign" ~ interest_foreign,
    name == "mother_discuss_law" ~ interest_law,
    name == "mother_discuss_education" ~ interest_education,
    name == "mother_discuss_partisan" ~ interest_partisan))
ModelAllMotherB <- nlme::lme(data = CCPISBoysMotherLonger, fixed =
                               interest_all ~ value + age + age_squared +
                               white + immig + lang + agentic + communal,
                             random = ~ 1 | Class, na.action = na.omit)
CCPISGirlsMotherLonger <- pivot_longer(CCPISGirls, cols = c(
  "mother_discuss_health", "mother_discuss_foreign", "mother_discuss_law",
  "mother_discuss_education", "mother_discuss_partisan"))
CCPISGirlsMotherLonger <- CCPISGirlsMotherLonger |>
  mutate(interest_all = case_when(
    name == "mother_discuss_health" ~ interest_health,
    name == "mother_discuss_foreign" ~ interest_foreign,
    name == "mother_discuss_law" ~ interest_law,
    name == "mother_discuss_education" ~ interest_education,
    name == "mother_discuss_partisan" ~ interest_partisan))
ModelAllMotherG <- nlme::lme(data = CCPISGirlsMotherLonger, fixed =
                               interest_all ~ value + age + age_squared +
                               white + immig + lang + agentic + communal,
                             random = ~ 1 | Class, na.action = na.omit)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelAllMotherB, "Health care" = Model200,
                "International affairs" = Model220, "Law and crime" = Model240,
                "Education" = Model260, "Partisan politics" = Model280),
  "Girls" = list("All" = ModelAllMotherG, "Health care" = Model210,
                 "International affairs" = Model230,
                 "Law and crime" = Model250, "Education" = Model270,
                 "Partisan politics" = Model290)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  add_rows = Models10,
  #coef_omit = "age|white|immig|lang|agentic|communal|#school",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            #"Controls: Socio-demographic, personality traits#, schools",
            "Reference Category for Language: Other languages spoken at home",
            "Reference Category for School: School #1"
  ),
  title = paste("Interest in topic most often discussed with one's mother",
                "{#tbl-lmeMotherAlt}"),
  coef_rename = c("value" = "Main topic discussed with mother?",
                  "mother_discuss_health" =
                    "Main topic discussed with mother?",
                  "mother_discuss_foreign" =
                    "Main topic discussed with mother?",
                  "mother_discuss_law" =
                    "Main topic discussed with mother?",
                  "mother_discuss_education" =
                    "Main topic discussed with mother?",
                  "mother_discuss_partisan" =
                    "Main topic discussed with mother?",
                  "age" = "Age",
                  "age_squared" = "Age squared",
                  "white" = "Race (1 = white)",
                  "immig" = "Immigrant",
                  "langAnglophone" = "English spoken at home",
                  "langFrancophone" = "French spoken at home",
                  "agentic" = "Agency",
                  "communal" = "Communality"))
Model20 <- nlme::lme(data = CCPISBoys, fixed = interest_health ~
                       mother_discuss_health, random = ~ 1 | Class,
                     na.action = na.omit)
Model21 <- nlme::lme(data = CCPISGirls, fixed = interest_health ~
                       mother_discuss_health, random = ~ 1 | Class,
                     na.action = na.omit)
Model22 <- nlme::lme(data = CCPISBoys, fixed = interest_foreign ~
                       mother_discuss_foreign, random = ~ 1 | Class,
                     na.action = na.omit)
Model23 <- nlme::lme(data = CCPISGirls, fixed = interest_foreign ~
                       mother_discuss_foreign, random = ~ 1 | Class,
                     na.action = na.omit)
Model24 <- nlme::lme(data = CCPISBoys, fixed = interest_law ~
                       mother_discuss_law, random = ~ 1 | Class,
                     na.action = na.omit)
Model25 <- nlme::lme(data = CCPISGirls, fixed = interest_law ~
                       mother_discuss_law, random = ~ 1 | Class,
                     na.action = na.omit)
Model26 <- nlme::lme(data = CCPISBoys, fixed = interest_education ~
                       mother_discuss_education, random = ~ 1 | Class,
                     na.action = na.omit)
Model27 <- nlme::lme(data = CCPISGirls, fixed = interest_education ~
                       mother_discuss_education, random = ~ 1 | Class,
                     na.action = na.omit)
Model28 <- nlme::lme(data = CCPISBoys, fixed = interest_partisan ~
                       mother_discuss_partisan, random = ~ 1 | Class,
                     na.action = na.omit)
Model29 <- nlme::lme(data = CCPISGirls, fixed = interest_partisan ~
                       mother_discuss_partisan, random = ~ 1 | Class,
                     na.action = na.omit)
ModelMotherB <- nlme::lme(data = CCPISBoysMotherLonger, fixed =
                            interest_all ~ value,
                          random = ~ 1 | Class, na.action = na.omit)
ModelMotherG <- nlme::lme(data = CCPISGirlsMotherLonger, fixed =
                            interest_all ~ value,
                          random = ~ 1 | Class, na.action = na.omit)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelMotherB, "Health care" = Model20,
                "International affairs" = Model22, "Law and crime" = Model24,
                "Education" = Model26, "Partisan politics" = Model28),
  "Girls" = list("All" = ModelMotherG, "Health care" = Model21,
                 "International affairs" = Model23, "Law and crime" = Model25,
                 "Education" = Model27, "Partisan politics" = Model29)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  add_rows = Models1,
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  title = paste("Interest in topic most often discussed with one's mother",
                "{#tbl-lmeMother}"),
  coef_rename = c("value" = "Main topic discussed with mother?",
                  "mother_discuss_health" =
                    "Main topic discussed with mother?",
                  "mother_discuss_foreign" =
                    "Main topic discussed with mother?",
                  "mother_discuss_law" =
                    "Main topic discussed with mother?",
                  "mother_discuss_education" =
                    "Main topic discussed with mother?",
                  "mother_discuss_partisan" =
                    "Main topic discussed with mother?"))

Model300 <- nlme::lme(data = CCPISBoys, fixed = interest_health ~
                       father_discuss_health + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model310 <- nlme::lme(data = CCPISGirls, fixed = interest_health ~
                       father_discuss_health + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model320 <- nlme::lme(data = CCPISBoys, fixed = interest_foreign ~
                       father_discuss_foreign + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model330 <- nlme::lme(data = CCPISGirls, fixed = interest_foreign ~
                       father_discuss_foreign + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model340 <- nlme::lme(data = CCPISBoys, fixed = interest_law ~
                       father_discuss_law + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model350 <- nlme::lme(data = CCPISGirls, fixed = interest_law ~
                       father_discuss_law + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model360 <- nlme::lme(data = CCPISBoys, fixed = interest_education ~
                       father_discuss_education + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model370 <- nlme::lme(data = CCPISGirls, fixed = interest_education ~
                       father_discuss_education + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model380 <- nlme::lme(data = CCPISBoys, fixed = interest_partisan ~
                       father_discuss_partisan + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
Model390 <- nlme::lme(data = CCPISGirls, fixed = interest_partisan ~
                       father_discuss_partisan + age + age_squared + white +
                        immig + lang + agentic + communal, random = ~ 1 |
                        Class, na.action = na.omit)
CCPISBoysFatherLonger <- pivot_longer(CCPISBoys, cols = c(
  "father_discuss_health", "father_discuss_foreign", "father_discuss_law",
  "father_discuss_education", "father_discuss_partisan"))
CCPISBoysFatherLonger <- CCPISBoysFatherLonger |>
  mutate(interest_all = case_when(
    name == "father_discuss_health" ~ interest_health,
    name == "father_discuss_foreign" ~ interest_foreign,
    name == "father_discuss_law" ~ interest_law,
    name == "father_discuss_education" ~ interest_education,
    name == "father_discuss_partisan" ~ interest_partisan))
ModelAllFatherB <- nlme::lme(data = CCPISBoysFatherLonger, fixed =
                               interest_all ~ value + age + age_squared +
                               white + immig + lang + agentic + communal,
                             random = ~ 1 | Class, na.action = na.omit)
CCPISGirlsFatherLonger <- pivot_longer(CCPISGirls, cols = c(
  "father_discuss_health", "father_discuss_foreign", "father_discuss_law",
  "father_discuss_education", "father_discuss_partisan"))
CCPISGirlsFatherLonger <- CCPISGirlsFatherLonger |>
  mutate(interest_all = case_when(
    name == "father_discuss_health" ~ interest_health,
    name == "father_discuss_foreign" ~ interest_foreign,
    name == "father_discuss_law" ~ interest_law,
    name == "father_discuss_education" ~ interest_education,
    name == "father_discuss_partisan" ~ interest_partisan))
ModelAllFatherG <- nlme::lme(data = CCPISGirlsFatherLonger, fixed =
                               interest_all ~ value + age + age_squared +
                               white + immig + lang + agentic + communal,
                             random = ~ 1 | Class, na.action = na.omit)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelAllFatherB, "Health care" = Model300,
                "International affairs" = Model320, "Law and crime" = Model340,
                "Education" = Model360, "Partisan politics" = Model380),
  "Girls" = list("All" = ModelAllFatherG, "Health care" = Model310,
                 "International affairs" = Model330,
                 "Law and crime" = Model350, "Education" = Model370,
                 "Partisan politics" = Model390)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  add_rows = Models10,
  #coef_omit = "age|white|immig|lang|agentic|communal|#school",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            #"Controls: Socio-demographic, personality traits#, schools",
            "Reference Category for Language: Other languages spoken at home",
            "Reference Category for School: School #1"
  ),
  title = paste("Interest in topic most often discussed with one's father",
                "{#tbl-lmeFatherAlt}"),
  coef_rename = c("value" = "Main topic discussed with father?",
                  "father_discuss_health" =
                    "Main topic discussed with father?",
                  "father_discuss_foreign" =
                    "Main topic discussed with father?",
                  "father_discuss_law" = "Main topic discussed with father?",
                  "father_discuss_education" =
                    "Main topic discussed with father?",
                  "father_discuss_partisan" =
                    "Main topic discussed with father?",
                  "age" = "Age",
                  "age_squared" = "Age squared",
                  "white" = "Race (1 = white)",
                  "immig" = "Immigrant",
                  "langAnglophone" = "English spoken at home",
                  "langFrancophone" = "French spoken at home",
                  "agentic" = "Agency",
                  "communal" = "Communality"))
Model30 <- nlme::lme(data = CCPISBoys, fixed = interest_health ~
                       father_discuss_health, random = ~ 1 | Class,
                     na.action = na.omit)
Model31 <- nlme::lme(data = CCPISGirls, fixed = interest_health ~
                       father_discuss_health, random = ~ 1 | Class,
                     na.action = na.omit)
Model32 <- nlme::lme(data = CCPISBoys, fixed = interest_foreign ~
                       father_discuss_foreign, random = ~ 1 | Class,
                     na.action = na.omit)
Model33 <- nlme::lme(data = CCPISGirls, fixed = interest_foreign ~
                       father_discuss_foreign, random = ~ 1 | Class,
                     na.action = na.omit)
Model34 <- nlme::lme(data = CCPISBoys, fixed = interest_law ~
                       father_discuss_law, random = ~ 1 | Class,
                     na.action = na.omit)
Model35 <- nlme::lme(data = CCPISGirls, fixed = interest_law ~
                       father_discuss_law, random = ~ 1 | Class,
                     na.action = na.omit)
Model36 <- nlme::lme(data = CCPISBoys, fixed = interest_education ~
                       father_discuss_education, random = ~ 1 | Class,
                     na.action = na.omit)
Model37 <- nlme::lme(data = CCPISGirls, fixed = interest_education ~
                       father_discuss_education, random = ~ 1 | Class,
                     na.action = na.omit)
Model38 <- nlme::lme(data = CCPISBoys, fixed = interest_partisan ~
                       father_discuss_partisan, random = ~ 1 | Class,
                     na.action = na.omit)
Model39 <- nlme::lme(data = CCPISGirls, fixed = interest_partisan ~
                       father_discuss_partisan, random = ~ 1 | Class,
                     na.action = na.omit)
ModelFatherB <- nlme::lme(data = CCPISBoysFatherLonger, fixed =
                            interest_all ~ value,
                          random = ~ 1 | Class, na.action = na.omit)
ModelFatherG <- nlme::lme(data = CCPISGirlsFatherLonger, fixed =
                            interest_all ~ value,
                          random = ~ 1 | Class, na.action = na.omit)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelFatherB, "Health care" = Model30,
                "International affairs" = Model32, "Law and crime" = Model34,
                "Education" = Model36, "Partisan politics" = Model38),
  "Girls" = list("All" = ModelFatherG, "Health care" = Model31,
                 "International affairs" = Model33, "Law and crime" = Model35,
                 "Education" = Model37, "Partisan politics" = Model39)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  add_rows = Models1,
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  title = paste("Interest in topic most often discussed with one's father",
                "{#tbl-lmeFather}"),
  coef_rename = c("value" = "Main topic discussed with father?",
                  "father_discuss_health" =
                    "Main topic discussed with father?",
                  "father_discuss_foreign" =
                    "Main topic discussed with father?",
                  "father_discuss_law" = "Main topic discussed with father?",
                  "father_discuss_education" =
                    "Main topic discussed with father?",
                  "father_discuss_partisan" =
                    "Main topic discussed with father?"))

#### 3.3 Chapter 5 ####
### Effect of peers (CCPIS) ####
CCPISBoysPeerGraph <- CCPISBoys |>
  pivot_longer(cols = c(femalefriends_discuss_clean,
                        malefriends_discuss_clean)) |>
  group_by(name) |>
  mutate(N = n()) |>
  group_by(name, value) |>
  summarise(perc = n() / unique(N)) |>
  filter(!is.na(value))
CCPISBoysPeerGraph$sex <- "Boys"
CCPISGirlsPeerGraph <- CCPISGirls |>
  pivot_longer(cols = c(femalefriends_discuss_clean,
                        malefriends_discuss_clean)) |>
  group_by(name) |>
  mutate(N = n()) |>
  group_by(name, value) |>
  summarise(perc = n() / unique(N)) |>
  filter(!is.na(value))
CCPISGirlsPeerGraph$sex <- "Girls"
CCPISPeerGraph <- rbind(CCPISBoysPeerGraph, CCPISGirlsPeerGraph)
ggplot(CCPISPeerGraph, aes(x = value, y = perc, fill = name)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~sex) +
  scale_x_discrete("Topic most often discussed with peers") +
  scale_y_continuous("Percent of students", labels = scales::percent) +
  scale_fill_discrete("Peers",
                      labels = c("Female", "Male"),
                      type = c("orange", "purple")) +
  theme(axis.text.x = element_text(angle = 90),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/PeersTopics.pdf", width = 5.5, height = 4.25)

#### Regression models ####
Model400 <- nlme::lme(data = CCPISBoys, fixed = interest_health ~
                        femalefriends_discuss_health + age + age_squared +
                        white + immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
Model410 <- nlme::lme(data = CCPISGirls, fixed = interest_health ~
                        femalefriends_discuss_health + age + age_squared +
                        white + immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
Model420 <- nlme::lme(data = CCPISBoys, fixed = interest_foreign ~
                        femalefriends_discuss_foreign + age + age_squared +
                        white + immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
Model430 <- nlme::lme(data = CCPISGirls, fixed = interest_foreign ~
                        femalefriends_discuss_foreign + age + age_squared +
                        white + immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
Model440 <- nlme::lme(data = CCPISBoys, fixed = interest_law ~
                        femalefriends_discuss_law + age + age_squared + white +
                        immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
Model450 <- nlme::lme(data = CCPISGirls, fixed = interest_law ~
                        femalefriends_discuss_law + age + age_squared + white +
                        immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
Model460 <- nlme::lme(data = CCPISBoys, fixed = interest_education ~
                        femalefriends_discuss_education + age + age_squared +
                        white + immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
Model470 <- nlme::lme(data = CCPISGirls, fixed = interest_education ~
                        femalefriends_discuss_education + age + age_squared +
                        white + immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
Model480 <- nlme::lme(data = CCPISBoys, fixed = interest_partisan ~
                        femalefriends_discuss_partisan + age + age_squared +
                        white + immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
Model490 <- nlme::lme(data = CCPISGirls, fixed = interest_partisan ~
                        femalefriends_discuss_partisan + age + age_squared +
                        white + immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
CCPISBoysFemaleFriendLonger <- pivot_longer(CCPISBoys, cols = c(
  "femalefriends_discuss_health", "femalefriends_discuss_foreign",
  "femalefriends_discuss_law", "femalefriends_discuss_education",
  "femalefriends_discuss_partisan"))
CCPISBoysFemaleFriendLonger <- CCPISBoysFemaleFriendLonger |>
  mutate(interest_all = case_when(
    name == "femalefriends_discuss_health" ~ interest_health,
    name == "femalefriends_discuss_foreign" ~ interest_foreign,
    name == "femalefriends_discuss_law" ~ interest_law,
    name == "femalefriends_discuss_education" ~ interest_education,
    name == "femalefriends_discuss_partisan" ~ interest_partisan))
ModelAllFemaleFriendB <- nlme::lme(data = CCPISBoysFemaleFriendLonger, fixed =
                                     interest_all ~ value + age + age_squared +
                                     white + immig + lang + agentic + communal,
                                   random = ~ 1 | Class, na.action = na.omit)
CCPISGirlsFemaleFriendLonger <- pivot_longer(CCPISGirls, cols = c(
  "femalefriends_discuss_health", "femalefriends_discuss_foreign",
  "femalefriends_discuss_law", "femalefriends_discuss_education",
  "femalefriends_discuss_partisan"))
CCPISGirlsFemaleFriendLonger <- CCPISGirlsFemaleFriendLonger |>
  mutate(interest_all = case_when(
    name == "femalefriends_discuss_health" ~ interest_health,
    name == "femalefriends_discuss_foreign" ~ interest_foreign,
    name == "femalefriends_discuss_law" ~ interest_law,
    name == "femalefriends_discuss_education" ~ interest_education,
    name == "femalefriends_discuss_partisan" ~ interest_partisan))
ModelAllFemaleFriendG <- nlme::lme(data = CCPISGirlsFemaleFriendLonger, fixed =
                                     interest_all ~ value + age + age_squared +
                                     white + immig + lang + agentic + communal,
                                   random = ~ 1 | Class, na.action = na.omit)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelAllFemaleFriendB, "Health care" = Model400,
                "International affairs" = Model420, "Law and crime" = Model440,
                "Education" = Model460, "Partisan politics" = Model480),
  "Girls" = list("All" = ModelAllFemaleFriendG, "Health care" = Model410,
                 "International affairs" = Model430, "Law and crime" = Model450,
                 "Education" = Model470, "Partisan politics" = Model490)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  add_rows = Models10,
  #coef_omit = "age|white|immig|lang|agentic|communal|#school",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            #"Controls: Socio-demographic, personality traits#, schools",
            "Reference Category for Language: Other languages spoken at home",
            "Reference Category for School: School #1"
  ),
  title = paste("Interest in topic most often discussed with one's female",
                "friends {#tbl-lmeFemaleFriendsAlt}"),
  coef_rename = c("value" = "Main topic discussed with female friends?",
                  "femalefriends_discuss_health" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_foreign" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_law" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_education" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_partisan" =
                    "Main topic discussed with female friends?",
                  "age" = "Age",
                  "age_squared" = "Age squared",
                  "white" = "Race (1 = white)",
                  "immig" = "Immigrant",
                  "langAnglophone" = "English spoken at home",
                  "langFrancophone" = "French spoken at home",
                  "agentic" = "Agency",
                  "communal" = "Communality"))
Model40 <- nlme::lme(data = CCPISBoys, fixed = interest_health ~
                       femalefriends_discuss_health, random = ~ 1 | Class,
                     na.action = na.omit)
Model41 <- nlme::lme(data = CCPISGirls, fixed = interest_health ~
                       femalefriends_discuss_health, random = ~ 1 | Class,
                     na.action = na.omit)
Model42 <- nlme::lme(data = CCPISBoys, fixed = interest_foreign ~
                       femalefriends_discuss_foreign, random = ~ 1 | Class,
                     na.action = na.omit)
Model43 <- nlme::lme(data = CCPISGirls, fixed = interest_foreign ~
                       femalefriends_discuss_foreign, random = ~ 1 | Class,
                     na.action = na.omit)
Model44 <- nlme::lme(data = CCPISBoys, fixed = interest_law ~
                       femalefriends_discuss_law, random = ~ 1 | Class,
                     na.action = na.omit)
Model45 <- nlme::lme(data = CCPISGirls, fixed = interest_law ~
                       femalefriends_discuss_law, random = ~ 1 | Class,
                     na.action = na.omit)
Model46 <- nlme::lme(data = CCPISBoys, fixed = interest_education ~
                       femalefriends_discuss_education, random = ~ 1 | Class,
                     na.action = na.omit)
Model47 <- nlme::lme(data = CCPISGirls, fixed = interest_education ~
                       femalefriends_discuss_education, random = ~ 1 | Class,
                     na.action = na.omit)
Model48 <- nlme::lme(data = CCPISBoys, fixed = interest_partisan ~
                       femalefriends_discuss_partisan, random = ~ 1 | Class,
                     na.action = na.omit)
Model49 <- nlme::lme(data = CCPISGirls, fixed = interest_partisan ~
                       femalefriends_discuss_partisan, random = ~ 1 | Class,
                     na.action = na.omit)
ModelFemaleFriendB <- nlme::lme(data = CCPISBoysFemaleFriendLonger, fixed =
                                  interest_all ~ value,
                                random = ~ 1 | Class, na.action = na.omit)
ModelFemaleFriendG <- nlme::lme(data = CCPISGirlsFemaleFriendLonger, fixed =
                                  interest_all ~ value,
                                random = ~ 1 | Class, na.action = na.omit)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelFemaleFriendB, "Health care" = Model40,
                "International affairs" = Model42, "Law and crime" = Model44,
                "Education" = Model46, "Partisan politics" = Model48),
  "Girls" = list("All" = ModelFemaleFriendG, "Health care" = Model41,
                 "International affairs" = Model43, "Law and crime" = Model45,
                 "Education" = Model47, "Partisan politics" = Model49)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  add_rows = Models1,
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  title = paste("Interest in topic most often discussed with one's female",
                "friends {#tbl-lmeFemaleFriends}"),
  coef_rename = c("value" = "Main topic discussed with female friends?",
                  "femalefriends_discuss_health" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_foreign" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_law" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_education" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_partisan" =
                    "Main topic discussed with female friends?"))

Model500 <- nlme::lme(data = CCPISBoys, fixed = interest_health ~
                        malefriends_discuss_health + age + age_squared +
                        white + immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
Model510 <- nlme::lme(data = CCPISGirls, fixed = interest_health ~
                        malefriends_discuss_health + age + age_squared +
                        white + immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
Model520 <- nlme::lme(data = CCPISBoys, fixed = interest_foreign ~
                        malefriends_discuss_foreign + age + age_squared +
                        white + immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
Model530 <- nlme::lme(data = CCPISGirls, fixed = interest_foreign ~
                        malefriends_discuss_foreign + age + age_squared +
                        white + immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
Model540 <- nlme::lme(data = CCPISBoys, fixed = interest_law ~
                        malefriends_discuss_law + age + age_squared + white +
                        immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
Model550 <- nlme::lme(data = CCPISGirls, fixed = interest_law ~
                        malefriends_discuss_law + age + age_squared + white +
                        immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
Model560 <- nlme::lme(data = CCPISBoys, fixed = interest_education ~
                        malefriends_discuss_education + age + age_squared +
                        white + immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
Model570 <- nlme::lme(data = CCPISGirls, fixed = interest_education ~
                        malefriends_discuss_education + age + age_squared +
                        white + immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
Model580 <- nlme::lme(data = CCPISBoys, fixed = interest_partisan ~
                        malefriends_discuss_partisan + age + age_squared +
                        white + immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
Model590 <- nlme::lme(data = CCPISGirls, fixed = interest_partisan ~
                        malefriends_discuss_partisan + age + age_squared +
                        white + immig + lang + agentic + communal,
                      random = ~ 1 | Class, na.action = na.omit)
CCPISBoysMaleFriendLonger <- pivot_longer(CCPISBoys, cols = c(
  "malefriends_discuss_health", "malefriends_discuss_foreign",
  "malefriends_discuss_law", "malefriends_discuss_education",
  "malefriends_discuss_partisan"))
CCPISBoysMaleFriendLonger <- CCPISBoysMaleFriendLonger |>
  mutate(interest_all = case_when(
    name == "malefriends_discuss_health" ~ interest_health,
    name == "malefriends_discuss_foreign" ~ interest_foreign,
    name == "malefriends_discuss_law" ~ interest_law,
    name == "malefriends_discuss_education" ~ interest_education,
    name == "malefriends_discuss_partisan" ~ interest_partisan))
ModelAllMaleFriendB <- nlme::lme(data = CCPISBoysMaleFriendLonger, fixed =
                                   interest_all ~ value + age + age_squared +
                                   white + immig + lang + agentic + communal,
                                 random = ~ 1 | Class, na.action = na.omit)
CCPISGirlsMaleFriendLonger <- pivot_longer(CCPISGirls, cols = c(
  "malefriends_discuss_health", "malefriends_discuss_foreign",
  "malefriends_discuss_law", "malefriends_discuss_education",
  "malefriends_discuss_partisan"))
CCPISGirlsMaleFriendLonger <- CCPISGirlsMaleFriendLonger |>
  mutate(interest_all = case_when(
    name == "malefriends_discuss_health" ~ interest_health,
    name == "malefriends_discuss_foreign" ~ interest_foreign,
    name == "malefriends_discuss_law" ~ interest_law,
    name == "malefriends_discuss_education" ~ interest_education,
    name == "malefriends_discuss_partisan" ~ interest_partisan))
ModelAllMaleFriendG <- nlme::lme(data = CCPISGirlsMaleFriendLonger, fixed =
                                   interest_all ~ value + age + age_squared +
                                   white + immig + lang + agentic + communal,
                                 random = ~ 1 | Class, na.action = na.omit)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelAllMaleFriendB, "Health care" = Model500,
                "International affairs" = Model520, "Law and crime" = Model540,
                "Education" = Model560, "Partisan politics" = Model580),
  "Girls" = list("All" = ModelAllMaleFriendG, "Health care" = Model510,
                 "International affairs" = Model530, "Law and crime" = Model550,
                 "Education" = Model570, "Partisan politics" = Model590)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  add_rows = Models10,
  #coef_omit = "age|white|immig|lang|agentic|communal|#school",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            #"Controls: Socio-demographic, personality traits#, schools",
            "Reference Category for Language: Other languages spoken at home",
            "Reference Category for School: School #1"
  ),
  title = paste("Interest in topic most often discussed with one's male",
                "friends {#tbl-lmeMaleFriendsAlt}"),
  coef_rename = c("value" = "Main topic discussed with male friends?",
                  "malefriends_discuss_health" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_foreign" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_law" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_education" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_partisan" =
                    "Main topic discussed with male friends?",
                  "age" = "Age",
                  "age_squared" = "Age squared",
                  "white" = "Race (1 = white)",
                  "immig" = "Immigrant",
                  "langAnglophone" = "English spoken at home",
                  "langFrancophone" = "French spoken at home",
                  "agentic" = "Agency",
                  "communal" = "Communality"))
Model50 <- nlme::lme(data = CCPISBoys, fixed = interest_health ~
                       malefriends_discuss_health, random = ~ 1 | Class,
                     na.action = na.omit)
Model51 <- nlme::lme(data = CCPISGirls, fixed = interest_health ~
                       malefriends_discuss_health, random = ~ 1 | Class,
                     na.action = na.omit)
Model52 <- nlme::lme(data = CCPISBoys, fixed = interest_foreign ~
                       malefriends_discuss_foreign, random = ~ 1 | Class,
                     na.action = na.omit)
Model53 <- nlme::lme(data = CCPISGirls, fixed = interest_foreign ~
                       malefriends_discuss_foreign, random = ~ 1 | Class,
                     na.action = na.omit)
Model54 <- nlme::lme(data = CCPISBoys, fixed = interest_law ~
                       malefriends_discuss_law, random = ~ 1 | Class,
                     na.action = na.omit)
Model55 <- nlme::lme(data = CCPISGirls, fixed = interest_law ~
                       malefriends_discuss_law, random = ~ 1 | Class,
                     na.action = na.omit)
Model56 <- nlme::lme(data = CCPISBoys, fixed = interest_education ~
                       malefriends_discuss_education, random = ~ 1 | Class,
                     na.action = na.omit)
Model57 <- nlme::lme(data = CCPISGirls, fixed = interest_education ~
                       malefriends_discuss_education, random = ~ 1 | Class,
                     na.action = na.omit)
Model58 <- nlme::lme(data = CCPISBoys, fixed = interest_partisan ~
                       malefriends_discuss_partisan, random = ~ 1 | Class,
                     na.action = na.omit)
Model59 <- nlme::lme(data = CCPISGirls, fixed = interest_partisan ~
                       malefriends_discuss_partisan, random = ~ 1 | Class,
                     na.action = na.omit)
ModelMaleFriendB <- nlme::lme(data = CCPISBoysMaleFriendLonger, fixed =
                                interest_all ~ value,
                              random = ~ 1 | Class, na.action = na.omit)
ModelMaleFriendG <- nlme::lme(data = CCPISGirlsMaleFriendLonger, fixed =
                                interest_all ~ value,
                              random = ~ 1 | Class, na.action = na.omit)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelMaleFriendB, "Health care" = Model50,
                "International affairs" = Model52, "Law and crime" = Model54,
                "Education" = Model56, "Partisan politics" = Model58),
  "Girls" = list("All" = ModelMaleFriendG, "Health care" = Model51,
                 "International affairs" = Model53, "Law and crime" = Model55,
                 "Education" = Model57, "Partisan politics" = Model59)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  add_rows = Models1,
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  title = paste("Interest in topic most often discussed with one's male",
                "friends {#tbl-lmeMaleFriends}"),
  coef_rename = c("value" = "Main topic discussed with male friends?",
                  "malefriends_discuss_health" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_foreign" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_law" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_education" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_partisan" =
                    "Main topic discussed with male friends?"))

#### HTML outputs for tables ####
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelAllParentB, "Health care" = Model100,
                "International affairs" = Model120, "Law and crime" = Model140,
                "Education" = Model160, "Partisan politics" = Model180),
  "Girls" = list("All" = ModelAllParentG, "Health care" = Model110,
                 "International affairs" = Model130,
                 "Law and crime" = Model150, "Education" = Model170,
                 "Partisan politics" = Model190)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "_previous/_practice-talk/tbl-lmeParentAlt.png",
  coef_omit = "age|white|immig|lang|agentic|communal",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: Socio-demographic, personality traits"),
  coef_rename = c(
    "value" = "Mother discusses topic more than father",
    "gender_parent_health" = "Mother discusses topic more than father",
    "gender_parent_foreign" = "Mother discusses topic more than father",
    "gender_parent_law" = "Mother discusses topic more than father",
    "gender_parent_education" = "Mother discusses topic more than father",
    "gender_parent_partisan" = "Mother discusses topic more than father"))
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelAllMotherB, "Health care" = Model200,
                "International affairs" = Model220, "Law and crime" = Model240,
                "Education" = Model260, "Partisan politics" = Model280),
  "Girls" = list("All" = ModelAllMotherG, "Health care" = Model210,
                 "International affairs" = Model230,
                 "Law and crime" = Model250, "Education" = Model270,
                 "Partisan politics" = Model290)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "_previous/_practice-talk/tbl-lmeMotherAlt.png",
  coef_omit = "age|white|immig|lang|agentic|communal",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: Socio-demographic, personality traits"),
  coef_rename = c("value" = "Main topic discussed with mother?",
                  "mother_discuss_health" =
                    "Main topic discussed with mother?",
                  "mother_discuss_foreign" =
                    "Main topic discussed with mother?",
                  "mother_discuss_law" =
                    "Main topic discussed with mother?",
                  "mother_discuss_education" =
                    "Main topic discussed with mother?",
                  "mother_discuss_partisan" =
                    "Main topic discussed with mother?"))
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelAllFatherB, "Health care" = Model300,
                "International affairs" = Model320, "Law and crime" = Model340,
                "Education" = Model360, "Partisan politics" = Model380),
  "Girls" = list("All" = ModelAllFatherG, "Health care" = Model310,
                 "International affairs" = Model330,
                 "Law and crime" = Model350, "Education" = Model370,
                 "Partisan politics" = Model390)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "_previous/_practice-talk/tbl-lmeFatherAlt.png",
  coef_omit = "age|white|immig|lang|agentic|communal",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: Socio-demographic, personality traits"),
  coef_rename = c("value" = "Main topic discussed with father?",
                  "father_discuss_health" =
                    "Main topic discussed with father?",
                  "father_discuss_foreign" =
                    "Main topic discussed with father?",
                  "father_discuss_law" = "Main topic discussed with father?",
                  "father_discuss_education" =
                    "Main topic discussed with father?",
                  "father_discuss_partisan" =
                    "Main topic discussed with father?"))
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelAllFemaleFriendB, "Health care" = Model400,
                "International affairs" = Model420, "Law and crime" = Model440,
                "Education" = Model460, "Partisan politics" = Model480),
  "Girls" = list("All" = ModelAllFemaleFriendG, "Health care" = Model410,
                 "International affairs" = Model430, "Law and crime" = Model450,
                 "Education" = Model470, "Partisan politics" = Model490)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "_previous/_practice-talk/tbl-lmeFemaleFriendsAlt.png",
  coef_omit = "age|white|immig|lang|agentic|communal",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: Socio-demographic, personality traits"),
  coef_rename = c("value" = "Main topic discussed with female friends?",
                  "femalefriends_discuss_health" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_foreign" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_law" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_education" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_partisan" =
                    "Main topic discussed with female friends?"))
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelAllMaleFriendB, "Health care" = Model500,
                "International affairs" = Model520, "Law and crime" = Model540,
                "Education" = Model560, "Partisan politics" = Model580),
  "Girls" = list("All" = ModelAllMaleFriendG, "Health care" = Model510,
                 "International affairs" = Model530, "Law and crime" = Model550,
                 "Education" = Model570, "Partisan politics" = Model590)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "_previous/_practice-talk/tbl-lmeMaleFriendsAlt.png",
  coef_omit = "age|white|immig|lang|agentic|communal",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: Socio-demographic, personality traits"),
  coef_rename = c("value" = "Main topic discussed with male friends?",
                  "malefriends_discuss_health" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_foreign" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_law" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_education" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_partisan" =
                    "Main topic discussed with male friends?"))
Model01Young <- nlme::lme(data = CCPISYoung, fixed = interest ~ female +
                            age + white + immig + lang +
                            agentic + communal + school, random = ~ 1 | Class,
                          na.action = na.omit)
Model02Young <- nlme::lme(data = CCPISYoung, fixed = interest_health ~
                            female + age + white +
                            immig + lang + agentic + communal + school,
                          random = ~ 1 | Class, na.action = na.omit)
Model03Young <- nlme::lme(data = CCPISYoung, fixed = interest_foreign ~
                            female + age + white +
                            immig + lang + agentic + communal + school,
                          random = ~ 1 | Class, na.action = na.omit)
Model04Young <- nlme::lme(data = CCPISYoung, fixed = interest_law ~
                            female + age + white +
                            immig + lang + agentic + communal + school,
                          random = ~ 1 | Class, na.action = na.omit)
Model05Young <- nlme::lme(data = CCPISYoung, fixed = interest_education ~
                            female + age + white +
                            immig + lang + agentic + communal + school,
                          random = ~ 1 | Class, na.action = na.omit)
Model06Young <- nlme::lme(data = CCPISYoung, fixed = interest_partisan ~
                            female + age + white +
                            immig + lang + agentic + communal + school,
                          random = ~ 1 | Class, na.action = na.omit)
modelsummary::modelsummary(models = list(
  "Politics (general)" = Model01Young, "Health care" = Model02Young,
  "International affairs" = Model03Young, "Law and crime" = Model04Young,
  "Education" = Model05Young, "Partisan politics" = Model06Young),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  coef_omit = "age|white|immig|lang|agentic|communal|school",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: Socio-demographic, personality traits, schools"),
  output = "_previous/_practice-talk/tbl-lmeInterestYoungCCPISAlt.png",
  coef_rename = c("female1" = "Gender (1 = girl)"))
Model01Old <- nlme::lme(data = CCPISOld, fixed = interest ~ female +
                            age + white + immig + lang +
                            agentic + communal + school, random = ~ 1 | Class,
                          na.action = na.omit)
Model02Old <- nlme::lme(data = CCPISOld, fixed = interest_health ~
                            female + age + white +
                            immig + lang + agentic + communal + school,
                          random = ~ 1 | Class, na.action = na.omit)
Model03Old <- nlme::lme(data = CCPISOld, fixed = interest_foreign ~
                            female + age + white +
                            immig + lang + agentic + communal + school,
                          random = ~ 1 | Class, na.action = na.omit)
Model04Old <- nlme::lme(data = CCPISOld, fixed = interest_law ~
                            female + age + white +
                            immig + lang + agentic + communal + school,
                          random = ~ 1 | Class, na.action = na.omit)
Model05Old <- nlme::lme(data = CCPISOld, fixed = interest_education ~
                            female + age + white +
                            immig + lang + agentic + communal + school,
                          random = ~ 1 | Class, na.action = na.omit)
Model06Old <- nlme::lme(data = CCPISOld, fixed = interest_partisan ~
                            female + age + white +
                            immig + lang + agentic + communal + school,
                          random = ~ 1 | Class, na.action = na.omit)
modelsummary::modelsummary(models = list(
  "Politics (general)" = Model01Old, "Health care" = Model02Old,
  "International affairs" = Model03Old, "Law and crime" = Model04Old,
  "Education" = Model05Old, "Partisan politics" = Model06Old),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  coef_omit = "age|white|immig|lang|agentic|communal|school",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: Socio-demographic, personality traits, schools"),
  output = "_previous/_practice-talk/tbl-lmeInterestOldCCPISAlt.png",
  coef_rename = c("female1" = "Gender (1 = girl)"))

#### Only personality traits added ####
Model01Pers <- nlme::lme(data = CCPIS, fixed = interest ~ female + agentic +
                           communal, random = ~ 1 | Class, na.action = na.omit)
Model02Pers <- nlme::lme(data = CCPIS, fixed = interest_health ~ female + agentic +
                           communal, random = ~ 1 | Class, na.action = na.omit)
Model03Pers <- nlme::lme(data = CCPIS, fixed = interest_foreign ~ female + agentic +
                           communal, random = ~ 1 | Class, na.action = na.omit)
Model04Pers <- nlme::lme(data = CCPIS, fixed = interest_law ~ female + agentic +
                           communal, random = ~ 1 | Class, na.action = na.omit)
Model05Pers <- nlme::lme(data = CCPIS, fixed = interest_education ~ female + agentic +
                           communal, random = ~ 1 | Class, na.action = na.omit)
Model06Pers <- nlme::lme(data = CCPIS, fixed = interest_partisan ~ female + agentic +
                           communal, random = ~ 1 | Class, na.action = na.omit)
modelsummary::modelsummary(models = list(
  "Politics (general)" = Model01Pers, "Health care" = Model02Pers,
  "International affairs" = Model03Pers, "Law and crime" = Model04Pers,
  "Education" = Model05Pers, "Partisan politics" = Model06Pers),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  coef_omit = "agentic|communal",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: Personality traits"),
  output = "_previous/_practice-talk/tbl-lmeInterestPers.png",
  coef_rename = c("female1" = "Gender (1 = girl)"))
Model1Pers <- nlme::lme(data = CCPIS, fixed = interest ~ female + agentic +
                           communal + school, random = ~ 1 | Class,
                        na.action = na.omit)
Model2Pers <- nlme::lme(data = CCPIS, fixed = interest_health ~ female + agentic +
                           communal + school, random = ~ 1 | Class,
                        na.action = na.omit)
Model3Pers <- nlme::lme(data = CCPIS, fixed = interest_foreign ~ female + agentic +
                           communal + school, random = ~ 1 | Class,
                        na.action = na.omit)
Model4Pers <- nlme::lme(data = CCPIS, fixed = interest_law ~ female + agentic +
                           communal + school, random = ~ 1 | Class,
                        na.action = na.omit)
Model5Pers <- nlme::lme(data = CCPIS, fixed = interest_education ~ female + agentic +
                           communal + school, random = ~ 1 | Class,
                        na.action = na.omit)
Model6Pers <- nlme::lme(data = CCPIS, fixed = interest_partisan ~ female + agentic +
                           communal + school, random = ~ 1 | Class,
                        na.action = na.omit)
modelsummary::modelsummary(models = list(
  "Politics (general)" = Model1Pers, "Health care" = Model2Pers,
  "International affairs" = Model3Pers, "Law and crime" = Model4Pers,
  "Education" = Model5Pers, "Partisan politics" = Model6Pers),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  coef_omit = "agentic|communal|school",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: Personality traits, school"),
  output = "_previous/_practice-talk/tbl-lmeInterestPers2.png",
  coef_rename = c("female1" = "Gender (1 = girl)"))
Model10Pers <- nlme::lme(data = CCPIS, fixed = interest ~ female + agentic +
                          communal + school + age + white + immig + lang,
                         random = ~ 1 | Class, na.action = na.omit)
Model20Pers <- nlme::lme(data = CCPIS, fixed = interest_health ~ female + agentic +
                           communal + school + age + white + immig + lang,
                         random = ~ 1 | Class, na.action = na.omit)
Model30Pers <- nlme::lme(data = CCPIS, fixed = interest_foreign ~ female + agentic +
                           communal + school + age + white + immig + lang,
                         random = ~ 1 | Class, na.action = na.omit)
Model40Pers <- nlme::lme(data = CCPIS, fixed = interest_law ~ female + agentic +
                           communal + school + age + white + immig + lang,
                         random = ~ 1 | Class, na.action = na.omit)
Model50Pers <- nlme::lme(data = CCPIS, fixed = interest_education ~ female + agentic +
                           communal + school + age + white + immig + lang,
                         random = ~ 1 | Class, na.action = na.omit)
Model60Pers <- nlme::lme(data = CCPIS, fixed = interest_partisan ~ female + agentic +
                           communal + school + age + white + immig + lang,
                         random = ~ 1 | Class, na.action = na.omit)
modelsummary::modelsummary(models = list(
  "Politics (general)" = Model10Pers, "Health care" = Model20Pers,
  "International affairs" = Model30Pers, "Law and crime" = Model40Pers,
  "Education" = Model50Pers, "Partisan politics" = Model60Pers),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  coef_omit = "agentic|communal|school",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: Socio-economic variables, personality traits, school"),
  output = "_previous/_practice-talk/tbl-lmeInterestPers2.png",
  coef_rename = c("female1" = "Gender (1 = girl)"))

#### No control variables ####
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelParentB, "Health care" = Model10,
                "International affairs" = Model12, "Law and crime" = Model14,
                "Education" = Model16, "Partisan politics" = Model18),
  "Girls" = list("All" = ModelParentG, "Health care" = Model11,
                 "International affairs" = Model13,
                 "Law and crime" = Model15, "Education" = Model17,
                 "Partisan politics" = Model19)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "_previous/_practice-talk/tbl-lmeParentNo.png",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  coef_rename = c(
    "value" = "Mother discusses topic more than father",
    "gender_parent_health" = "Mother discusses topic more than father",
    "gender_parent_foreign" = "Mother discusses topic more than father",
    "gender_parent_law" = "Mother discusses topic more than father",
    "gender_parent_education" = "Mother discusses topic more than father",
    "gender_parent_partisan" = "Mother discusses topic more than father"))
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelMotherB, "Health care" = Model20,
                "International affairs" = Model22, "Law and crime" = Model24,
                "Education" = Model26, "Partisan politics" = Model28),
  "Girls" = list("All" = ModelMotherG, "Health care" = Model21,
                 "International affairs" = Model23,
                 "Law and crime" = Model25, "Education" = Model27,
                 "Partisan politics" = Model29)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "_previous/_practice-talk/tbl-lmeMotherNo.png",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  coef_rename = c("value" = "Main topic discussed with mother?",
                  "mother_discuss_health" =
                    "Main topic discussed with mother?",
                  "mother_discuss_foreign" =
                    "Main topic discussed with mother?",
                  "mother_discuss_law" =
                    "Main topic discussed with mother?",
                  "mother_discuss_education" =
                    "Main topic discussed with mother?",
                  "mother_discuss_partisan" =
                    "Main topic discussed with mother?"))
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelFatherB, "Health care" = Model30,
                "International affairs" = Model32, "Law and crime" = Model34,
                "Education" = Model36, "Partisan politics" = Model38),
  "Girls" = list("All" = ModelFatherG, "Health care" = Model31,
                 "International affairs" = Model33,
                 "Law and crime" = Model35, "Education" = Model37,
                 "Partisan politics" = Model39)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "_previous/_practice-talk/tbl-lmeFatherNo.png",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  coef_rename = c("value" = "Main topic discussed with father?",
                  "father_discuss_health" =
                    "Main topic discussed with father?",
                  "father_discuss_foreign" =
                    "Main topic discussed with father?",
                  "father_discuss_law" = "Main topic discussed with father?",
                  "father_discuss_education" =
                    "Main topic discussed with father?",
                  "father_discuss_partisan" =
                    "Main topic discussed with father?"))
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelFemaleFriendB, "Health care" = Model40,
                "International affairs" = Model42, "Law and crime" = Model44,
                "Education" = Model46, "Partisan politics" = Model48),
  "Girls" = list("All" = ModelFemaleFriendG, "Health care" = Model41,
                 "International affairs" = Model43, "Law and crime" = Model45,
                 "Education" = Model47, "Partisan politics" = Model49)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "_previous/_practice-talk/tbl-lmeFemaleFriendsNo.png",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  coef_rename = c("value" = "Main topic discussed with female friends?",
                  "femalefriends_discuss_health" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_foreign" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_law" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_education" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_partisan" =
                    "Main topic discussed with female friends?"))
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelMaleFriendB, "Health care" = Model50,
                "International affairs" = Model52, "Law and crime" = Model54,
                "Education" = Model56, "Partisan politics" = Model58),
  "Girls" = list("All" = ModelMaleFriendG, "Health care" = Model51,
                 "International affairs" = Model53, "Law and crime" = Model55,
                 "Education" = Model57, "Partisan politics" = Model59)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "_previous/_practice-talk/tbl-lmeMaleFriendsNo.png",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  coef_rename = c("value" = "Main topic discussed with male friends?",
                  "malefriends_discuss_health" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_foreign" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_law" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_education" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_partisan" =
                    "Main topic discussed with male friends?"))
modelsummary::modelsummary(models = list(
  "Politics (general)" = Model1Young, "Health care" = Model2Young,
  "International affairs" = Model3Young, "Law and crime" = Model4Young,
  "Education" = Model5Young, "Partisan politics" = Model6Young),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  output = "_previous/_practice-talk/tbl-lmeInterestYoungCCPISNo.png",
  coef_rename = c("female1" = "Gender (1 = girl)"))
modelsummary::modelsummary(models = list(
  "Politics (general)" = Model1Old, "Health care" = Model2Old,
  "International affairs" = Model3Old, "Law and crime" = Model4Old,
  "Education" = Model5Old, "Partisan politics" = Model6Old),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  output = "_previous/_practice-talk/tbl-lmeInterestOldCCPISNo.png",
  coef_rename = c("female1" = "Gender (1 = girl)"))

#### All reg variables shown (appendix) ####
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelAllParentB, "Health care" = Model100,
                "International affairs" = Model120, "Law and crime" = Model140,
                "Education" = Model160, "Partisan politics" = Model180),
  "Girls" = list("All" = ModelAllParentG, "Health care" = Model110,
                 "International affairs" = Model130,
                 "Law and crime" = Model150, "Education" = Model170,
                 "Partisan politics" = Model190)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "_previous/_practice-talk/tbl-lmeParent.png",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Reference Category for Language: Other languages spoken at home",
            "Reference Category for School: School #1"),
  coef_rename = c(
    "value" = "Mother discusses topic more than father",
    "gender_parent_health" = "Mother discusses topic more than father",
    "gender_parent_foreign" = "Mother discusses topic more than father",
    "gender_parent_law" = "Mother discusses topic more than father",
    "gender_parent_education" = "Mother discusses topic more than father",
    "gender_parent_partisan" = "Mother discusses topic more than father",
    "age" = "Age",
    "age_squared" = "Age squared",
    "white" = "Race (1 = white)",
    "immig" = "Immigrant",
    "langAnglophone" = "English spoken at home",
    "langFrancophone" = "French spoken at home",
    "agentic" = "Agency",
    "communal" = "Communality",
    "schoolCollège Citoyen" = "School #4",
    "schoolCollège mariste de Québec" = "School #3",
    "schoolÉcole de la Rose-des-Vents" = "School #6",
    "schoolÉcole Jean-de-Brébeuf" = "School #2",
    "schoolJaya International High School" = "School #5",
    "schoolRenfrew County DSB Student Senate" = "School #8",
    "schoolUrban Village Academy" = "School #7"))
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelAllMotherB, "Health care" = Model200,
                "International affairs" = Model220, "Law and crime" = Model240,
                "Education" = Model260, "Partisan politics" = Model280),
  "Girls" = list("All" = ModelAllMotherG, "Health care" = Model210,
                 "International affairs" = Model230,
                 "Law and crime" = Model250, "Education" = Model270,
                 "Partisan politics" = Model290)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "_previous/_practice-talk/tbl-lmeMother.png",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Reference Category for Language: Other languages spoken at home",
            "Reference Category for School: School #1"),
  coef_rename = c("value" = "Main topic discussed with mother?",
                  "mother_discuss_health" =
                    "Main topic discussed with mother?",
                  "mother_discuss_foreign" =
                    "Main topic discussed with mother?",
                  "mother_discuss_law" =
                    "Main topic discussed with mother?",
                  "mother_discuss_education" =
                    "Main topic discussed with mother?",
                  "mother_discuss_partisan" =
                    "Main topic discussed with mother?",
                  "age" = "Age",
                  "age_squared" = "Age squared",
                  "white" = "Race (1 = white)",
                  "immig" = "Immigrant",
                  "langAnglophone" = "English spoken at home",
                  "langFrancophone" = "French spoken at home",
                  "agentic" = "Agency",
                  "communal" = "Communality",
                  "schoolCollège Citoyen" = "School #4",
                  "schoolCollège mariste de Québec" = "School #3",
                  "schoolÉcole de la Rose-des-Vents" = "School #6",
                  "schoolÉcole Jean-de-Brébeuf" = "School #2",
                  "schoolJaya International High School" = "School #5",
                  "schoolRenfrew County DSB Student Senate" = "School #8",
                  "schoolUrban Village Academy" = "School #7"))
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelAllFatherB, "Health care" = Model300,
                "International affairs" = Model320, "Law and crime" = Model340,
                "Education" = Model360, "Partisan politics" = Model380),
  "Girls" = list("All" = ModelAllFatherG, "Health care" = Model310,
                 "International affairs" = Model330,
                 "Law and crime" = Model350, "Education" = Model370,
                 "Partisan politics" = Model390)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "_previous/_practice-talk/tbl-lmeFather.png",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Reference Category for Language: Other languages spoken at home",
            "Reference Category for School: School #1"),
  coef_rename = c("value" = "Main topic discussed with father?",
                  "father_discuss_health" =
                    "Main topic discussed with father?",
                  "father_discuss_foreign" =
                    "Main topic discussed with father?",
                  "father_discuss_law" = "Main topic discussed with father?",
                  "father_discuss_education" =
                    "Main topic discussed with father?",
                  "father_discuss_partisan" =
                    "Main topic discussed with father?",
                  "age" = "Age",
                  "age_squared" = "Age squared",
                  "white" = "Race (1 = white)",
                  "immig" = "Immigrant",
                  "langAnglophone" = "English spoken at home",
                  "langFrancophone" = "French spoken at home",
                  "agentic" = "Agency",
                  "communal" = "Communality",
                  "schoolCollège Citoyen" = "School #4",
                  "schoolCollège mariste de Québec" = "School #3",
                  "schoolÉcole de la Rose-des-Vents" = "School #6",
                  "schoolÉcole Jean-de-Brébeuf" = "School #2",
                  "schoolJaya International High School" = "School #5",
                  "schoolRenfrew County DSB Student Senate" = "School #8",
                  "schoolUrban Village Academy" = "School #7"))
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelAllFemaleFriendB, "Health care" = Model400,
                "International affairs" = Model420, "Law and crime" = Model440,
                "Education" = Model460, "Partisan politics" = Model480),
  "Girls" = list("All" = ModelAllFemaleFriendG, "Health care" = Model410,
                 "International affairs" = Model430, "Law and crime" = Model450,
                 "Education" = Model470, "Partisan politics" = Model490)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "_previous/_practice-talk/tbl-lmeFemaleFriends.png",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Reference Category for Language: Other languages spoken at home",
            "Reference Category for School: School #1"),
  coef_rename = c("value" = "Main topic discussed with female friends?",
                  "femalefriends_discuss_health" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_foreign" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_law" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_education" =
                    "Main topic discussed with female friends?",
                  "femalefriends_discuss_partisan" =
                    "Main topic discussed with female friends?",
                  "age" = "Age",
                  "age_squared" = "Age squared",
                  "white" = "Race (1 = white)",
                  "immig" = "Immigrant",
                  "langAnglophone" = "English spoken at home",
                  "langFrancophone" = "French spoken at home",
                  "agentic" = "Agency",
                  "communal" = "Communality",
                  "schoolCollège Citoyen" = "School #4",
                  "schoolCollège mariste de Québec" = "School #3",
                  "schoolÉcole de la Rose-des-Vents" = "School #6",
                  "schoolÉcole Jean-de-Brébeuf" = "School #2",
                  "schoolJaya International High School" = "School #5",
                  "schoolRenfrew County DSB Student Senate" = "School #8",
                  "schoolUrban Village Academy" = "School #7"))
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelAllMaleFriendB, "Health care" = Model500,
                "International affairs" = Model520, "Law and crime" = Model540,
                "Education" = Model560, "Partisan politics" = Model580),
  "Girls" = list("All" = ModelAllMaleFriendG, "Health care" = Model510,
                 "International affairs" = Model530, "Law and crime" = Model550,
                 "Education" = Model570, "Partisan politics" = Model590)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "_previous/_practice-talk/tbl-lmeMaleFriends.png",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Reference Category for Language: Other languages spoken at home",
            "Reference Category for School: School #1"),
  coef_rename = c("value" = "Main topic discussed with male friends?",
                  "malefriends_discuss_health" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_foreign" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_law" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_education" =
                    "Main topic discussed with male friends?",
                  "malefriends_discuss_partisan" =
                    "Main topic discussed with male friends?",
                  "age" = "Age",
                  "age_squared" = "Age squared",
                  "white" = "Race (1 = white)",
                  "immig" = "Immigrant",
                  "langAnglophone" = "English spoken at home",
                  "langFrancophone" = "French spoken at home",
                  "agentic" = "Agency",
                  "communal" = "Communality",
                  "schoolCollège Citoyen" = "School #4",
                  "schoolCollège mariste de Québec" = "School #3",
                  "schoolÉcole de la Rose-des-Vents" = "School #6",
                  "schoolÉcole Jean-de-Brébeuf" = "School #2",
                  "schoolJaya International High School" = "School #5",
                  "schoolRenfrew County DSB Student Senate" = "School #8",
                  "schoolUrban Village Academy" = "School #7"))
modelsummary::modelsummary(models = list(
  "Politics (general)" = Model01Young, "Health care" = Model02Young,
  "International affairs" = Model03Young, "Law and crime" = Model04Young,
  "Education" = Model05Young, "Partisan politics" = Model06Young),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Reference Category for Language: Other languages spoken at home",
            "Reference Category for School: School #1"),
  output = "_previous/_practice-talk/tbl-lmeInterestYoungCCPIS.png",
  coef_rename = c("female1" = "Gender (1 = girl)",
                  "age" = "Age",
                  "age_squared" = "Age squared",
                  "white" = "Race (1 = white)",
                  "immig" = "Immigrant",
                  "langAnglophone" = "English spoken at home",
                  "langFrancophone" = "French spoken at home",
                  "agentic" = "Agency",
                  "communal" = "Communality",
                  "schoolCollège Citoyen" = "School #4",
                  "schoolCollège mariste de Québec" = "School #3",
                  "schoolÉcole de la Rose-des-Vents" = "School #6",
                  "schoolÉcole Jean-de-Brébeuf" = "School #2",
                  "schoolJaya International High School" = "School #5",
                  "schoolRenfrew County DSB Student Senate" = "School #8",
                  "schoolUrban Village Academy" = "School #7"))
modelsummary::modelsummary(models = list(
  "Politics (general)" = Model01Old, "Health care" = Model02Old,
  "International affairs" = Model03Old, "Law and crime" = Model04Old,
  "Education" = Model05Old, "Partisan politics" = Model06Old),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Reference Category for Language: Other languages spoken at home",
            "Reference Category for School: School #1"),
  output = "_previous/_practice-talk/tbl-lmeInterestOldCCPIS.png",
  coef_rename = c("female1" = "Gender (1 = girl)",
                  "age" = "Age",
                  "age_squared" = "Age squared",
                  "white" = "Race (1 = white)",
                  "immig" = "Immigrant",
                  "langAnglophone" = "English spoken at home",
                  "langFrancophone" = "French spoken at home",
                  "agentic" = "Agency",
                  "communal" = "Communality",
                  "schoolCollège Citoyen" = "School #4",
                  "schoolCollège mariste de Québec" = "School #3",
                  "schoolÉcole de la Rose-des-Vents" = "School #6",
                  "schoolÉcole Jean-de-Brébeuf" = "School #2",
                  "schoolJaya International High School" = "School #5",
                  "schoolRenfrew County DSB Student Senate" = "School #8",
                  "schoolUrban Village Academy" = "School #7"))

### 1.1.2 Abandoned (Indices de défavorisation) ####
Defav <- openxlsx::read.xlsx(
  "_data/MinistèreÉducation/Indices-defavorisations_2021-2022.xlsx")
colnames(Defav) <- c("CodeEcole", "Ecole", "X", "Y", "ISFR", "RangSFR", "Z",
                     "AA", "AB", "IMSE", "AC", "RangMSE", "AD", "AE", "AF",
                     "NombreEleves", "AG")
Defav <- Defav[7:3083,]
Defav$ISFR[Defav$ISFR == "Indice du seuil de faible revenu"] <- NA
Defav <- filter(Defav, !is.na(ISFR))
Defav$Ecole_alt <- str_remove_all(Defav$Ecole, "\\s\\(\\d+\\)")
Defav$ISFR <- as.numeric(str_replace_all(Defav$ISFR, ",", "."))
Defav$IMSE <- as.numeric(str_replace_all(Defav$IMSE, ",", "."))
Defav$RangSFR <- as.numeric(str_replace_all(Defav$RangSFR, ",", "."))
Defav$RangMSE <- as.numeric(str_replace_all(Defav$RangMSE, ",", "."))
unique(CCPIS$school)
CCPIS$ISFR <- qdap::mgsub(text.var = CCPIS$school,
                          pattern = Defav$Ecole_alt,
                          replacement = Defav$ISFR)
CCPIS$ISFR <- as.numeric(CCPIS$ISFR)
CCPIS$RangSFR <- qdap::mgsub(text.var = EQ2022$ID_alt,
                             pattern = Defav$Ecole,
                             replacement = Defav$RangSFR)
EQ2022$RangSFR <- as.numeric(EQ2022$RangSFR)
EQ2022$IMSE <- qdap::mgsub(text.var = EQ2022$ID_alt,
                           pattern = Defav$Ecole,
                           replacement = Defav$IMSE)
EQ2022$IMSE <- as.numeric(EQ2022$IMSE)
EQ2022$RangMSE <- qdap::mgsub(text.var = EQ2022$ID_alt,
                              pattern = Defav$Ecole,
                              replacement = Defav$RangMSE)
EQ2022$RangMSE <- as.numeric(EQ2022$RangMSE)
mean(EQ2022$RangSFR, na.rm = TRUE)
mean(EQ2022$RangMSE, na.rm = TRUE)

### Confidence intervals plots ####
CCPISGender <- table(CCPIS$female_alt) |>
  as.data.frame()
CCPISGender$Prop <- CCPISGender$Freq / sum(CCPISGender$Freq)
CCPISGender$SE <- sqrt((CCPISGender$Prop * (1 - CCPISGender$Prop)) /
                         sum(CCPISGender$Freq))
z_critical <- qnorm((1 + 0.95) / 2)
CCPISGender$margin_of_error <- z_critical * CCPISGender$SE
CCPISGender$ci_lower <- CCPISGender$Prop - CCPISGender$margin_of_error
CCPISGender$ci_upper <- CCPISGender$Prop + CCPISGender$margin_of_error
CCPISGender$ci_lower_freq <- CCPISGender$ci_lower * sum(CCPISGender$Freq)
CCPISGender$ci_upper_freq <- CCPISGender$ci_upper * sum(CCPISGender$Freq)
PlotGender <- ggplot(CCPISGender, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = ci_lower_freq, ymax = ci_upper_freq),
                width = 0.1) +
  labs(x = "Gender", y = "Frequency") +
  theme(text = element_text(family = "CM Roman"))
