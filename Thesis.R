#!radian
#devtools::install_github("clessn/clessnverse", force = T)
#extrafont::font_import()
#extrafont::font_install('fontcm')
#extrafont::loadfonts()
#webshot::install_phantomjs() for saving modelsummary to png or jpg
pacman::p_load(tidyverse, extrafont, ggtext, openxlsx, qdap, psy, kableExtra,
               readstata13, haven, anesrake, questionr, ggpubr, nlme,
               modelsummary, Hmisc, scales, ggtext, pwr, lme4, lmtest,
               ggcorrplot, psych, mokken, gt)
delete_rows_na <- function(row, loadings, number_na_allowed = 5) {
  if (sum(is.na(row)) <= number_na_allowed) {
    # Perform element-wise multiplication for each variable and its loading
    variable_contributions <- row * loadings
    return(sum(variable_contributions, na.rm = TRUE) /
           sum(loadings, na.rm = TRUE))
  } else {
    return(NA)
  }
}

### 1. Data cleaning ####
#### 1.1 CPIS ####
CPISEN <- read.csv("_data/CPIS/StudentsEN.csv")
CPISFR1 <- read.csv("_data/CPIS/StudentsFR1.csv")
CPISFR3 <- read.csv("_data/CPIS/StudentsFR3.csv")
CPISBad <- bind_rows(CPISEN, CPISFR1, CPISFR3) |>
  filter(str_detect(StartDate, "^20") & # remove column descriptions
           (Consent == "I do not accept to participate in this survey" |
              Consentement ==
              "Je n'accepte pas de participer à cette recherche ") |
           Q5_1 == "" | is.na(Q5_1))
CPIS <- bind_rows(CPISEN, CPISFR1, CPISFR3) |>
  filter(str_detect(StartDate, "^20") & # remove column descriptions
           (Consent == "I accept to participate in this survey" |
              Consentement == "J'accepte de participer à cette recherche") &
           Q5_1 != "" & !is.na(Q5_1))
CPIS$Class <- CPIS$Classe
CPIS$Class[is.na(CPIS$Class)] <- CPIS$Class.ID[
  is.na(CPIS$Class)]
CPIS$Class <- str_remove_all(CPIS$Class, "\\s+")
CPIS$number <- 1:nrow(CPIS)
Codes <- openxlsx::read.xlsx("_data/CPIS/8digitCodes.xlsx")
CPISManageManually <- CPIS |>
  filter(!(Class %in% Codes$Code.à.8.chiffres)) |>
  dplyr::select(StartDate:EndDate, LocationLatitude:LocationLongitude, Class:Élève,
         number)
# manual check of classrooms numbers which don't match the list to see if I
# can match them with other classes through common latitude/longitude &
# similar star time and end time
CPIS$Class[!(CPIS$Class %in% Codes$Code.à.8.chiffres)] <- c(
  rep("", 9), rep("16554548", 2), "79362677", "41083499", "04120709",
  rep("61150875", 2), rep("40965312", 3), rep("43731111", 2),
  rep("27787836", 4), rep("35043304", 17), rep("56697824", 5), "07892125",
  "26319798", "", rep("46537164", 2), rep("65988237", 3), "53394381",
  "84691593", "", "72249157", "36611780", rep("", 2), "14748972")
CPIS$school <- qdap::mgsub(text.var = CPIS$Class,
                               pattern = Codes$Code.à.8.chiffres,
                               replacement = Codes$École)
CPIS$school[CPIS$school == paste0(
  "Student Senate (https://www.rcdsb.on.ca/en/parents-and-students/",
  "student-senate.aspx)")] <- "Renfrew County DSB Student Senate"
CPIS <- filter(CPIS, school != "")
CPIS$teacher <- qdap::mgsub(text.var = CPIS$Class,
                                pattern = Codes$Code.à.8.chiffres,
                                replacement = Codes$`Nom.de.l'enseignant`)
table(CPIS$school, useNA = "always")
table(CPIS$Class, useNA = "always") # 38 classes, but 6 with 1-2 students
# so 32 classes with at least 9 students (or 30 with at least 12)
table(CPIS$teacher, useNA = "always")
CPIS$province <- NA
CPIS$province[CPIS$school %in% unique(CPIS$school)[4:8]] <-
  "Quebec"
CPIS$province[CPIS$school %in% unique(CPIS$school)[1:3]] <-
  "Ontario"
CPIS$school_lang <- NA
CPIS$school_lang[CPIS$school %in% unique(CPIS$school)[4:8]] <-
  "French"
CPIS$school_lang[CPIS$school %in% unique(CPIS$school)[1:3]] <-
  "English"
CPIS$private <- 1
CPIS$private[CPIS$school %in% c(
  "École Jean-de-Brébeuf", # CSS de la Capitale
  "École de la Rose-des-Vents", # CSS des Draveurs
  "Renfrew County DSB Student Senate")] <- 0 # public
table(CPIS$province, useNA = "always")
table(CPIS$school_lang, useNA = "always")
table(CPIS$private, useNA = "always")
CPIS$city <- case_when(
  CPIS$school == "Collège Citoyen" ~ "Laval",
  CPIS$school == "Collège Boisbriand 2016" ~ "Boisbriand",
  CPIS$school == "École Jean-de-Brébeuf" ~ "Québec",
  CPIS$school == "École de la Rose-des-Vents" ~ "Cantley",
  CPIS$school == "Renfrew County DSB Student Senate" ~ "Pembroke",
  CPIS$school == "Collège mariste de Québec" ~ "Québec",
  CPIS$school == "Urban Village Academy" ~ "Barrie",
  CPIS$school == "Jaya International High School" ~ "Mississauga")
table(CPIS$city, useNA = "always")
names(CPIS)[20:77] <- c(
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
  "lang_aboriginal", "lang_other", "ethnicity", "ethnicity_other", "born_canada",
  "sexrole_independent", "sexrole_passive", "sexrole_competitive",
  "sexrole_easydecisions", "sexrole_giveup", "sexrole_selfconfident",
  "sexrole_inferior", "sexrole_underpressure", "sexrole_emotional",
  "sexrole_devote", "sexrole_gentle", "sexrole_helpful", "sexrole_kind",
  "sexrole_awarefeelings", "sexrole_understanding", "sexrole_warm", "email")
CPIS$female <- NA
CPIS$female[CPIS$gender %in% c("A boy", "Un garçon")] <- 0
CPIS$female[CPIS$gender %in% c("A girl", "Une fille")] <- 1
CPIS$female <- as.factor(CPIS$female)
table(CPIS$female, useNA = "always") # 17 "other", 33 did not answer
CPIS$female_alt <- NA
CPIS$female_alt[CPIS$gender %in% c("A boy", "Un garçon")] <- "Boy"
CPIS$female_alt[CPIS$gender %in% c("A girl", "Une fille")] <- "Girl"
CPIS$female_alt[
  CPIS$gender ==
    "Autre (ex.: trans, non-binaire, bispirituel, gender-queer)"] <- "Other"
CPIS$female_alt2 <- CPIS$female_alt
CPIS$female_alt2[CPIS$female_alt == "Girl"] <- "Girls"
CPIS$female_alt2[CPIS$female_alt == "Boy"] <- "Boys"
CPIS$female_alt <- as.factor(CPIS$female_alt)
CPIS$female_alt2 <- as.factor(CPIS$female_alt2)
CPIS$age <- 2022 - as.numeric(CPIS$yob)
CPIS$age[CPIS$age > 18 | CPIS$age < 9] <- NA
table(CPIS$age, useNA = "always")
CPIS$age_squared <- CPIS$age ^ 2
CPIS$lang <- "Allophone"
CPIS$lang[CPIS$language == "Français"] <- "Francophone"
CPIS$lang[CPIS$language %in% c("English", "Anglais")] <- "Anglophone"
CPIS$lang[CPIS$language %in% c(
  "Don’t know/Prefer not to answer", "Je ne sais pas/Préfère ne pas répondre",
  "")] <- NA
table(CPIS$lang, useNA = "always")
CPIS$immig <- NA
CPIS$immig[CPIS$born_canada %in% c("Yes", "Oui")] <- 0
CPIS$immig[CPIS$born_canada %in% c("No", "Non")] <- 1
CPIS$ethn[CPIS$ethnicity %in% c("Blanc(he)", "White")] <- "White"
CPIS$ethn[CPIS$ethnicity == "Noir(e)"] <- "Black"
CPIS$ethn[CPIS$ethnicity == paste(
  "Asiatique occidental(e) (e.g., Iranien(ne), Afghan(e), etc.)")] <-
  "West Asian"
CPIS$ethn[CPIS$ethnicity == paste(
  "Asiatique du Sud-Est (ex.: Vietnamien(ne), Cambodgien(ne), Laotien(ne),",
  "Thaïlandais(e), etc.)")] <- "Southeast Asian"
CPIS$ethn[CPIS$ethnicity == "Arabe"] <- "Arabic"
CPIS$ethn[CPIS$ethnicity == paste(
  "Sud-Asiatique (ex.: Indien(ne) de l’Inde, Pakistanais(e),",
  "Sri-Lankais(e), etc.)")] <- "South Asian"
CPIS$ethn[CPIS$ethnicity == "Latino-Américain(e)"] <- "Hispanic"
CPIS$ethn[CPIS$ethnicity == paste(
  "Première Nation (Indien(ne) de l’Amérique du Nord), Métis(se) ou",
  "Inuk (Inuit)")] <- "Indigenous"
CPIS$ethn[CPIS$ethnicity %in% c("Chinese", "Chinois(e)")] <- "Chinese"
CPIS$ethn[CPIS$ethnicity == "Philippin(e)"] <- "Filipino"
CPIS$ethn[CPIS$ethnicity == "Coréen(ne)"] <- "Korean"
CPIS$ethn[CPIS$ethnicity == "Japonais(e)"] <- "Japanese"
CPIS$ethn[CPIS$ethnicity %in% c("Autre (veuillez spécifier)",
                                        "Other (please specify)")] <- "Other"
table(CPIS$ethn, useNA = "always")
CPIS$white <- 0
CPIS$white[CPIS$ethn == "White"] <- 1
CPIS$white[is.na(CPIS$ethn)] <- NA
table(CPIS$white, useNA = "always")
CPIS$interest <- as.numeric(CPIS$interest)
CPIS$interest_health <- as.numeric(CPIS$interest_health)
CPIS$interest_foreign <- as.numeric(CPIS$interest_foreign)
CPIS$interest_law <- as.numeric(CPIS$interest_law)
CPIS$interest_education <- as.numeric(CPIS$interest_education)
CPIS$interest_partisan <- as.numeric(CPIS$interest_partisan)
CPIS$gender_parent_health <- NA
CPIS$gender_parent_health[
  CPIS$parent_health %in% c("Mon père", "Father")] <- 0
CPIS$gender_parent_health[
  CPIS$parent_health %in% c("Ma mère", "Mother")] <- 1
table(CPIS$gender_parent_health, useNA = "always")
CPIS$gender_parent_education <- NA
CPIS$gender_parent_education[
  CPIS$parent_education %in% c("Mon père", "Father")] <- 0
CPIS$gender_parent_education[
  CPIS$parent_education %in% c("Ma mère", "Mother")] <- 1
table(CPIS$gender_parent_education, useNA = "always")
CPIS$gender_parent_law <- NA
CPIS$gender_parent_law[
  CPIS$parent_law %in% c("Mon père", "Father")] <- 0
CPIS$gender_parent_law[
  CPIS$parent_law %in% c("Ma mère", "Mother")] <- 1
table(CPIS$gender_parent_law, useNA = "always")
CPIS$gender_parent_foreign <- NA
CPIS$gender_parent_foreign[
  CPIS$parent_foreign %in% c("Mon père", "Father")] <- 0
CPIS$gender_parent_foreign[
  CPIS$parent_foreign %in% c("Ma mère", "Mother")] <- 1
table(CPIS$gender_parent_foreign, useNA = "always")
CPIS$gender_parent_partisan <- NA
CPIS$gender_parent_partisan[
  CPIS$parent_partisan %in% c("Mon père", "Father")] <- 0
CPIS$gender_parent_partisan[
  CPIS$parent_partisan %in% c("Ma mère", "Mother")] <- 1
table(CPIS$gender_parent_partisan, useNA = "always")
CPIS$mother_discuss_clean <- CPIS$mother_discuss
CPIS$mother_discuss_clean[CPIS$mother_discuss %in% c(
  "Je ne sais pas/Préfère ne pas répondre",
  "Don't know/Prefer not to answer", "")] <- NA
CPIS$mother_discuss_clean[CPIS$mother_discuss == "Santé"] <- "Health care"
CPIS$mother_discuss_clean[
  CPIS$mother_discuss == "Affaires internationales"] <-
  "International affairs"
CPIS$mother_discuss_clean[CPIS$mother_discuss == "Loi et crime"] <-
  "Law and crime"
CPIS$mother_discuss_clean[CPIS$mother_discuss == "Éducation"] <- "Education"
CPIS$mother_discuss_clean[CPIS$mother_discuss == "Politique partisane"] <-
  "Partisan politics"
table(CPIS$mother_discuss_clean, useNA = "always")
CPIS$mother_discuss_health <- 0
CPIS$mother_discuss_health[CPIS$mother_discuss_clean == "Health care"] <- 1
CPIS$mother_discuss_health[is.na(CPIS$mother_discuss_clean)] <- NA
table(CPIS$mother_discuss_health, useNA = "always")
CPIS$mother_discuss_foreign <- 0
CPIS$mother_discuss_foreign[CPIS$mother_discuss_clean ==
                               "International affairs"] <- 1
CPIS$mother_discuss_foreign[is.na(CPIS$mother_discuss_clean)] <- NA
table(CPIS$mother_discuss_foreign, useNA = "always")
CPIS$mother_discuss_law <- 0
CPIS$mother_discuss_law[CPIS$mother_discuss_clean == "Law and crime"] <- 1
CPIS$mother_discuss_law[is.na(CPIS$mother_discuss_clean)] <- NA
table(CPIS$mother_discuss_law, useNA = "always")
CPIS$mother_discuss_education <- 0
CPIS$mother_discuss_education[CPIS$mother_discuss_clean == "Education"] <- 1
CPIS$mother_discuss_education[is.na(CPIS$mother_discuss_clean)] <- NA
table(CPIS$mother_discuss_education, useNA = "always")
CPIS$mother_discuss_partisan <- 0
CPIS$mother_discuss_partisan[CPIS$mother_discuss_clean ==
                                "Partisan politics"] <- 1
CPIS$mother_discuss_partisan[is.na(CPIS$mother_discuss_clean)] <- NA
table(CPIS$mother_discuss_partisan, useNA = "always")
CPIS$father_discuss_clean <- CPIS$father_discuss
CPIS$father_discuss_clean[CPIS$father_discuss %in% c(
  "Je ne sais pas/Préfère ne pas répondre",
  "Don't know/Prefer not to answer", "")] <- NA
CPIS$father_discuss_clean[CPIS$father_discuss == "Santé"] <- "Health care"
CPIS$father_discuss_clean[
  CPIS$father_discuss == "Affaires internationales"] <-
  "International affairs"
CPIS$father_discuss_clean[CPIS$father_discuss == "Loi et crime"] <-
  "Law and crime"
CPIS$father_discuss_clean[CPIS$father_discuss == "Éducation"] <- "Education"
CPIS$father_discuss_clean[CPIS$father_discuss == "Politique partisane"] <-
  "Partisan politics"
table(CPIS$father_discuss_clean, useNA = "always")
CPIS$father_discuss_health <- 0
CPIS$father_discuss_health[CPIS$father_discuss_clean == "Health care"] <- 1
CPIS$father_discuss_health[is.na(CPIS$father_discuss_clean)] <- NA
table(CPIS$father_discuss_health, useNA = "always")
CPIS$father_discuss_foreign <- 0
CPIS$father_discuss_foreign[CPIS$father_discuss_clean ==
                               "International affairs"] <- 1
CPIS$father_discuss_foreign[is.na(CPIS$father_discuss_clean)] <- NA
table(CPIS$father_discuss_foreign, useNA = "always")
CPIS$father_discuss_law <- 0
CPIS$father_discuss_law[CPIS$father_discuss_clean == "Law and crime"] <- 1
CPIS$father_discuss_law[is.na(CPIS$father_discuss_clean)] <- NA
table(CPIS$father_discuss_law, useNA = "always")
CPIS$father_discuss_education <- 0
CPIS$father_discuss_education[CPIS$father_discuss_clean == "Education"] <- 1
CPIS$father_discuss_education[is.na(CPIS$father_discuss_clean)] <- NA
table(CPIS$father_discuss_education, useNA = "always")
CPIS$father_discuss_partisan <- 0
CPIS$father_discuss_partisan[CPIS$father_discuss_clean ==
                                "Partisan politics"] <- 1
CPIS$father_discuss_partisan[is.na(CPIS$father_discuss_clean)] <- NA
table(CPIS$father_discuss_partisan, useNA = "always")
CPIS$peers_female <- NA
CPIS$peers_female[CPIS$friends_gender %in% c("Garçons", "Boys")] <- 0
CPIS$peers_female[CPIS$friends_gender %in% c("Filles", "Girls")] <- 1
table(CPIS$peers_female, useNA = "always")
CPIS$malefriends_discuss_clean <- CPIS$malefriends_discuss
CPIS$malefriends_discuss_clean[CPIS$malefriends_discuss %in% c(
  "Je ne sais pas/Préfère ne pas répondre",
  "Don't know/Prefer not to answer", "")] <- NA
CPIS$malefriends_discuss_clean[CPIS$malefriends_discuss ==
                                  "Santé"] <- "Health care"
CPIS$malefriends_discuss_clean[
  CPIS$malefriends_discuss == "Affaires internationales"] <-
  "International affairs"
CPIS$malefriends_discuss_clean[CPIS$malefriends_discuss ==
                                  "Loi et crime"] <-
  "Law and crime"
CPIS$malefriends_discuss_clean[CPIS$malefriends_discuss ==
                                  "Éducation"] <- "Education"
CPIS$malefriends_discuss_clean[CPIS$malefriends_discuss ==
                                  "Politique partisane"] <-
  "Partisan politics"
table(CPIS$malefriends_discuss_clean, useNA = "always")
CPIS$malefriends_discuss_health <- 0
CPIS$malefriends_discuss_health[CPIS$malefriends_discuss_clean %in% c(
  "Santé", "Health care")] <- 1
CPIS$malefriends_discuss_health[
  is.na(CPIS$malefriends_discuss_clean)] <- NA
table(CPIS$malefriends_discuss_health, useNA = "always")
CPIS$malefriends_discuss_foreign <- 0
CPIS$malefriends_discuss_foreign[
  CPIS$malefriends_discuss_clean %in% c(
    "Affaires internationales", "International affairs")] <- 1
CPIS$malefriends_discuss_foreign[
  is.na(CPIS$malefriends_discuss_clean)] <- NA
table(CPIS$malefriends_discuss_foreign, useNA = "always")
CPIS$malefriends_discuss_law <- 0
CPIS$malefriends_discuss_law[CPIS$malefriends_discuss_clean %in% c(
  "Loi et crime", "Law and crime")] <- 1
CPIS$malefriends_discuss_law[
  is.na(CPIS$malefriends_discuss_clean)] <- NA
table(CPIS$malefriends_discuss_law, useNA = "always")
CPIS$malefriends_discuss_education <- 0
CPIS$malefriends_discuss_education[
  CPIS$malefriends_discuss_clean %in% c("Éducation", "Education")] <- 1
CPIS$malefriends_discuss_education[
  is.na(CPIS$malefriends_discuss_clean)] <- NA
table(CPIS$malefriends_discuss_education, useNA = "always")
CPIS$malefriends_discuss_partisan <- 0
CPIS$malefriends_discuss_partisan[
  CPIS$malefriends_discuss_clean %in% c(
    "Politique partisane", "Partisan politics")] <- 1
CPIS$malefriends_discuss_partisan[
  is.na(CPIS$malefriends_discuss_clean)] <- NA
table(CPIS$malefriends_discuss_partisan, useNA = "always")
CPIS$femalefriends_discuss_clean <- CPIS$femalefriends_discuss
CPIS$femalefriends_discuss_clean[CPIS$femalefriends_discuss %in% c(
  "Je ne sais pas/Préfère ne pas répondre",
  "Don't know/Prefer not to answer", "")] <- NA
CPIS$femalefriends_discuss_clean[CPIS$femalefriends_discuss ==
                                    "Santé"] <- "Health care"
CPIS$femalefriends_discuss_clean[
  CPIS$femalefriends_discuss == "Affaires internationales"] <-
  "International affairs"
CPIS$femalefriends_discuss_clean[CPIS$femalefriends_discuss ==
                                    "Loi et crime"] <-
  "Law and crime"
CPIS$femalefriends_discuss_clean[CPIS$femalefriends_discuss ==
                                    "Éducation"] <- "Education"
CPIS$femalefriends_discuss_clean[CPIS$femalefriends_discuss ==
                                    "Politique partisane"] <-
  "Partisan politics"
table(CPIS$femalefriends_discuss_clean, useNA = "always")
CPIS$femalefriends_discuss_health <- 0
CPIS$femalefriends_discuss_health[
  CPIS$femalefriends_discuss_clean %in% c("Santé", "Health care")] <- 1
CPIS$femalefriends_discuss_health[
  is.na(CPIS$femalefriends_discuss_clean)] <- NA
table(CPIS$femalefriends_discuss_health, useNA = "always")
CPIS$femalefriends_discuss_foreign <- 0
CPIS$femalefriends_discuss_foreign[
  CPIS$femalefriends_discuss_clean %in% c(
    "Affaires internationales", "International affairs")] <- 1
CPIS$femalefriends_discuss_foreign[
  is.na(CPIS$femalefriends_discuss_clean)] <- NA
table(CPIS$femalefriends_discuss_foreign, useNA = "always")
CPIS$femalefriends_discuss_law <- 0
CPIS$femalefriends_discuss_law[
  CPIS$femalefriends_discuss_clean %in% c(
    "Loi et crime", "Law and crime")] <- 1
CPIS$femalefriends_discuss_law[
  is.na(CPIS$femalefriends_discuss_clean)] <- NA
table(CPIS$femalefriends_discuss_law, useNA = "always")
CPIS$femalefriends_discuss_education <- 0
CPIS$femalefriends_discuss_education[
  CPIS$femalefriends_discuss_clean %in% c("Éducation", "Education")] <- 1
CPIS$femalefriends_discuss_education[
  is.na(CPIS$femalefriends_discuss_clean)] <- NA
table(CPIS$femalefriends_discuss_education, useNA = "always")
CPIS$femalefriends_discuss_partisan <- 0
CPIS$femalefriends_discuss_partisan[
  CPIS$femalefriends_discuss_clean %in% c(
    "Politique partisane", "Partisan politics")] <- 1
CPIS$femalefriends_discuss_partisan[is.na(
  CPIS$femalefriends_discuss_clean)] <- NA
table(CPIS$femalefriends_discuss_partisan, useNA = "always")
CPIS$teacher_discuss_clean <- CPIS$teacher_discuss
CPIS$teacher_discuss_clean[
  CPIS$teacher_discuss %in% c("", "Don't know/Prefer not to answer",
                               "Je ne sais pas/Préfère ne pas répondre")] <- NA
table(CPIS$teacher_discuss_clean)
CPIS$teacher_discuss_health <- 0
CPIS$teacher_discuss_health[is.na(CPIS$teacher_discuss_clean)] <- NA
CPIS$teacher_discuss_health[CPIS$teacher_discuss_clean %in% c(
  "Santé", "Health care")] <- 1
CPIS$teacher_discuss_foreign <- 0
CPIS$teacher_discuss_foreign[is.na(CPIS$teacher_discuss_clean)] <- NA
CPIS$teacher_discuss_foreign[CPIS$teacher_discuss_clean %in% c(
  "Affaires internationales", "International affairs")] <- 1
CPIS$teacher_discuss_law <- 0
CPIS$teacher_discuss_law[is.na(CPIS$teacher_discuss_clean)] <- NA
CPIS$teacher_discuss_law[CPIS$teacher_discuss_clean %in% c(
  "Loi et crime", "Law and crime")] <- 1
CPIS$teacher_discuss_education <- 0
CPIS$teacher_discuss_education[is.na(CPIS$teacher_discuss_clean)] <- NA
CPIS$teacher_discuss_education[CPIS$teacher_discuss_clean %in% c(
  "Éducation", "Education")] <- 1
CPIS$teacher_discuss_partisan <- 0
CPIS$teacher_discuss_partisan[is.na(CPIS$teacher_discuss_clean)] <- NA
CPIS$teacher_discuss_partisan[CPIS$teacher_discuss_clean %in% c(
  "Politique partisane", "Partisan politics")] <- 1
CPIS$influencer_discuss_clean <- CPIS$influencer_discuss
CPIS$influencer_discuss_clean[
  CPIS$influencer_discuss %in% c(
    "", "Don't know/Prefer not to answer",
    "Je ne sais pas/Préfère ne pas répondre")] <- NA
table(CPIS$influencer_discuss_clean)
CPIS$influencer_discuss_health <- 0
CPIS$influencer_discuss_health[is.na(CPIS$influencer_discuss_clean)] <- NA
CPIS$influencer_discuss_health[CPIS$influencer_discuss_clean %in% c(
  "Santé", "Health care")] <- 1
CPIS$influencer_discuss_foreign <- 0
CPIS$influencer_discuss_foreign[is.na(CPIS$influencer_discuss_clean)] <- NA
CPIS$influencer_discuss_foreign[CPIS$influencer_discuss_clean %in% c(
  "Affaires internationales", "International affairs")] <- 1
CPIS$influencer_discuss_law <- 0
CPIS$influencer_discuss_law[is.na(CPIS$influencer_discuss_clean)] <- NA
CPIS$influencer_discuss_law[CPIS$influencer_discuss_clean %in% c(
  "Loi et crime", "Law and crime")] <- 1
CPIS$influencer_discuss_education <- 0
CPIS$influencer_discuss_education[is.na(CPIS$influencer_discuss_clean)] <- NA
CPIS$influencer_discuss_education[CPIS$influencer_discuss_clean %in% c(
  "Éducation", "Education")] <- 1
CPIS$influencer_discuss_partisan <- 0
CPIS$influencer_discuss_partisan[is.na(CPIS$influencer_discuss_clean)] <- NA
CPIS$influencer_discuss_partisan[CPIS$influencer_discuss_clean %in% c(
  "Politique partisane", "Partisan politics")] <- 1
CPIS[, 61:76] <- map(CPIS[, 61:76], as.numeric)
CPIS[, 61:76] <- apply(CPIS[, 61:76], 2, function(x) {(x - 1) / 4})
CPIS$sexrole_easydecisions_rev <- 1 - CPIS$sexrole_easydecisions
CPIS$fam_situation_alt <- CPIS$fam_situation
CPIS$fam_situation_alt[CPIS$fam_situation == "Une mère uniquement"] <-
  "One mother only"
CPIS$fam_situation_alt[CPIS$fam_situation == "Un père uniquement"] <-
  "One father only"
CPIS$fam_situation_alt[CPIS$fam_situation == "Deux mères"] <-
  "Two mothers"
CPIS$fam_situation_alt[CPIS$fam_situation == "Deux pères"] <-
  "Two fathers"
CPIS$fam_situation_alt[CPIS$fam_situation == "Autre"] <- "Other"
CPIS$fam_situation_alt[CPIS$fam_situation ==
                             "Une mère, un père et au moins un beau-parent"] <-
  "Mother, father\nand stepparent(s)"
CPIS$fam_situation_alt[CPIS$fam_situation == paste(
  "One mother, one father and at least one stepparent")] <-
  "Mother, father\nand stepparent(s)"
CPIS$fam_situation_alt[CPIS$fam_situation ==
                             "Une mère, un père et aucun beau-parent"] <-
  "Mother and father"
CPIS$fam_situation_alt[CPIS$fam_situation ==
                             "One mother, one father and no stepparents"] <-
  "Mother and father"
table(CPIS$fam_situation_alt, useNA = "always")
CPIS$friends_gender_alt <- CPIS$friends_gender
CPIS$friends_gender_alt[CPIS$friends_gender == "Filles"] <- "Girls"
CPIS$friends_gender_alt[CPIS$friends_gender == "Garçons"] <- "Boys"
CPIS$friends_gender_alt[CPIS$friends_gender ==
                              "Environ autant des deux genres"] <-
  "About the same\nfor both genders"
CPIS$friends_gender_alt[CPIS$friends_gender ==
                              "About the same for both genders"] <-
  "About the same\nfor both genders"
CPIS$friends_gender_alt[CPIS$friends_gender %in% c(
  "Don't know/Prefer not to answer", "",
  "Je ne sais pas/Préfère ne pas répondre")] <- NA
table(CPIS$friends_gender_alt, useNA = "always")
CPIS$parent_discuss_alt <- NA
CPIS$parent_discuss_alt[CPIS$parent_discuss %in% c("Ma mère", "Mother")] <- 1
CPIS$parent_discuss_alt[CPIS$parent_discuss %in% c(
  "Mon père", "Father")] <- 0
table(CPIS$parent_discuss_alt, useNA = "always")

CPIS$teacher_gender_alt <- CPIS$teacher_gender
CPIS$teacher_gender_alt[CPIS$teacher_gender %in% c(
  "A boy", "Un homme")] <- "A man"
CPIS$teacher_gender_alt[CPIS$teacher_gender %in% c(
  "A girl", "Une femme")] <- "A woman"
CPIS$teacher_gender_alt[
  CPIS$teacher_gender ==
    "Autre (ex.: trans, non-binaire, bispirituel, gender-queer)"] <-
  "Other (e.g. Trans,\nnon-binary, two-\nspirit, gender-queer)"
CPIS$teacher_gender_alt[
  CPIS$teacher_gender ==
    "Other (e.g. Trans, non-binary, two-spirit, gender-queer)"] <-
  "Other (e.g. Trans,\nnon-binary, two-\nspirit, gender-queer)"
CPIS$teacher_gender_alt[CPIS$teacher_gender == ""] <- NA
table(CPIS$teacher_gender_alt, useNA = "always")
CPIS$influencer_gender_alt <- CPIS$influencer_gender
CPIS$influencer_gender_alt[CPIS$influencer_gender %in% c(
  "A boy", "Un homme")] <- "A man"
CPIS$influencer_gender_alt[CPIS$influencer_gender %in% c(
  "A girl", "Une femme")] <- "A woman"
CPIS$influencer_gender_alt[
  CPIS$influencer_gender ==
    "Autre (ex.: trans, non-binaire, bispirituel, gender-queer)"] <-
  "Other"
CPIS$influencer_gender_alt[
  CPIS$influencer_gender ==
    "Other (e.g. Trans, non-binary, two-spirit, gender-queer)"] <-
  "Other"
CPIS$influencer_gender_alt[CPIS$influencer_gender == ""] <- NA
table(CPIS$influencer_gender_alt, useNA = "always")
CPIS$influencer_gender_congruence <- ifelse(
  CPIS$influencer_gender_alt == "A woman" & CPIS$female_alt == "Girl", 1, ifelse(
    CPIS$influencer_gender_alt == "A man" & CPIS$female_alt == "Boy", 1, ifelse(
      CPIS$influencer_gender_alt == "Other" & CPIS$female_alt == "Other", 1, 0)))
table(CPIS$influencer_gender_congruence, useNA = "always")
CPIS$samegenderinfluencer_discuss_health <- CPIS$influencer_discuss_health
CPIS$samegenderinfluencer_discuss_health[
  CPIS$influencer_gender_congruence == 0] <- NA
CPIS$othergenderinfluencer_discuss_health <- CPIS$influencer_discuss_health
CPIS$othergenderinfluencer_discuss_health[
  CPIS$influencer_gender_congruence == 1] <- NA
CPIS$samegenderinfluencer_discuss_foreign <- CPIS$influencer_discuss_foreign
CPIS$samegenderinfluencer_discuss_foreign[
  CPIS$influencer_gender_congruence == 0] <- NA
CPIS$othergenderinfluencer_discuss_foreign <- CPIS$influencer_discuss_foreign
CPIS$othergenderinfluencer_discuss_foreign[
  CPIS$influencer_gender_congruence == 1] <- NA
CPIS$samegenderinfluencer_discuss_law <- CPIS$influencer_discuss_law
CPIS$samegenderinfluencer_discuss_law[
  CPIS$influencer_gender_congruence == 0] <- NA
CPIS$othergenderinfluencer_discuss_law <- CPIS$influencer_discuss_law
CPIS$othergenderinfluencer_discuss_law[
  CPIS$influencer_gender_congruence == 1] <- NA
CPIS$samegenderinfluencer_discuss_education <- CPIS$influencer_discuss_education
CPIS$samegenderinfluencer_discuss_education[
  CPIS$influencer_gender_congruence == 0] <- NA
CPIS$othergenderinfluencer_discuss_education <- CPIS$influencer_discuss_education
CPIS$othergenderinfluencer_discuss_education[
  CPIS$influencer_gender_congruence == 1] <- NA
CPIS$samegenderinfluencer_discuss_partisan <- CPIS$influencer_discuss_partisan
CPIS$samegenderinfluencer_discuss_partisan[
  CPIS$influencer_gender_congruence == 0] <- NA
CPIS$othergenderinfluencer_discuss_partisan <- CPIS$influencer_discuss_partisan
CPIS$othergenderinfluencer_discuss_partisan[
  CPIS$influencer_gender_congruence == 1] <- NA
political <- function(new, old) {
  new <- NA
  new[old %in% c("Not political", "Pas politique")] <- 0
  new[old %in% c("Political", "Politique")] <- 1
  return(new)
}
CPIS$lockdown_political_alt <- political(
  old = CPIS$lockdown_political, new = CPIS$lockdown_political_alt)
CPIS$nurses_political_alt <- political(
  old = CPIS$nurses_political, new = CPIS$nurses_political_alt)
CPIS$china_political_alt <- political(
  old = CPIS$china_political, new = CPIS$china_political_alt)
CPIS$ukraine_political_alt <- political(
  old = CPIS$ukraine_political, new = CPIS$ukraine_political_alt)
CPIS$police_political_alt <- political(
  old = CPIS$police_political, new = CPIS$police_political_alt)
CPIS$crime_political_alt <- political(
  old = CPIS$crime_political, new = CPIS$crime_political_alt)
CPIS$tuition_political_alt <- political(
  old = CPIS$tuition_political, new = CPIS$tuition_political_alt)
CPIS$privateschool_political_alt <- political(
  old = CPIS$privateschool_political, new = CPIS$privateschool_political_alt)
CPIS$elections_political_alt <- political(
  old = CPIS$elections_political, new = CPIS$elections_political_alt)
CPIS$parties_political_alt <- political(
  old = CPIS$parties_political, new = CPIS$parties_political_alt)

##### 1.1.1 Factor analysis ####
AgencyScale <- na.omit(CPIS[, c(
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
       aes(x = reorder(AgencyVariableNames, desc(AgencyVariableNames)),
           y = AgencyFactorLoadings)) +
  coord_flip() +
  geom_bar(stat = "identity", colour = "black", fill = "black", linewidth = 1,
           width = 0.4) +
  geom_text(aes(label = as.character(round(AgencyFactorLoadings, digits = 2))),
            vjust = 0.35, hjust = -0.3, size = 6, family = "CM Roman") +
  geom_hline(yintercept = 0.3, colour = "gray", linetype = "longdash") +
  annotate("text", label = paste("Cronbach's alpha =", as.character(
    AgencyCronbach)), x = 2.25, y = 0.85, size = 6, family = "CM Roman") +
  annotate("text", label = paste("First eigenvalue =", as.character(
    AgencyFirstEigenvalue)), x = 1.75, y = 0.85, size = 6,
    family = "CM Roman") +
  scale_y_continuous(name = "Factor loadings", limits = c(-0.1, 1),
                     breaks = seq(-0.1, 1, by = 0.1)) +
  xlab("") +
  theme_linedraw() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 18.5),
        axis.title.x = element_text(hjust = 0.3, vjust = -0.17),
        panel.grid = element_blank(),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/AgencyScale.pdf", width = 11, height = 4.25)

# Apply the function to each row and create a new column agentic
agencyVariables <- c("sexrole_independent", "sexrole_passive",
                     "sexrole_competitive", "sexrole_easydecisions_rev",
                     "sexrole_giveup", "sexrole_selfconfident",
                     "sexrole_inferior", "sexrole_underpressure")
CPIS$agentic <- apply(CPIS[agencyVariables], 1, delete_rows_na,
                       loadings = AgencyFactorLoadings)
length(na.omit(CPIS$agentic)) / nrow(CPIS) * 100 # 90% available data
CommunalityScale <- na.omit(CPIS[, c(
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
       aes(x = reorder(CommunalityVariableNames, desc(CommunalityVariableNames)),
           y = CommunalityFactorLoadings)) +
  coord_flip() +
  geom_bar(stat = "identity", colour = "black", fill = "black", linewidth = 1,
           width = 0.4) +
  geom_text(aes(label = as.character(round(
    CommunalityFactorLoadings, digits = 2))), vjust = 0.35, hjust = -0.3,
    family = "CM Roman", size = 6) +
  geom_hline(yintercept = 0.3, colour = "gray", linetype = "longdash") +
  annotate("text", label = paste("Cronbach's alpha =", as.character(
    CommunalityCronbach)), x = 1.25, y = 0.85, size = 6,
    family = "CM Roman") +
  annotate("text", label = paste("First eigenvalue =", as.character(
    CommunalityFirstEigenvalue)), x = 0.75, y = 0.85, size = 6,
    family = "CM Roman") +
  scale_y_continuous(name = "Factor loadings", limits = c(-0.1, 1),
                     breaks = seq(-0.1, 1, by = 0.1)) +
  xlab("") +
  theme_linedraw() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 18.5),
        axis.title.x = element_text(hjust = 0.3, vjust = -0.17),
        panel.grid = element_blank(),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/CommunalityScale.pdf", width = 11, height = 4.25)
# Apply the function to each row and create a new column agentic
communalVariables <- c("sexrole_emotional", "sexrole_devote", "sexrole_gentle",
                       "sexrole_helpful", "sexrole_kind",
                       "sexrole_awarefeelings", "sexrole_understanding",
                       "sexrole_warm")
CPIS$communal <- apply(CPIS[communalVariables], 1, delete_rows_na,
                        loadings = CommunalityFactorLoadings)
 # 0 = not communal, 1 = communal
length(na.omit(CPIS$communal)) / nrow(CPIS) * 100 # 89% available data

##### 1.1.2 Open-ended question analysis ####
CPISOpen <- openxlsx::read.xlsx("_data/CPIS/CPISReview_CoderFinal.xlsx",
 colNames = F)
names(CPISOpen) <- CPISOpen[1, ]
CPISOpen <- CPISOpen[2:nrow(CPISOpen), ]
CPISOpen$female <- CPIS$female
CPISOpenM <- filter(CPISOpen, female == 0)
CPISOpenMTab <- data.frame(freq = colSums(map_dfr(CPISOpenM[2:(ncol(CPISOpenM) - 1)],
 as.numeric), na.rm = T))
CPISOpenMTab$subject <- rownames(CPISOpenMTab)
CPISOpenMTab$female <- "Boys"
CPISOpenMTab$perc <- 100 * CPISOpenMTab$freq / nrow(CPISOpenM)
CPISOpenW <- filter(CPISOpen, female == 1)
CPISOpenWTab <- data.frame(freq = colSums(map_dfr(CPISOpenW[2:(ncol(CPISOpenW) - 1)],
 as.numeric), na.rm = T))
CPISOpenWTab$subject <- rownames(CPISOpenWTab)
CPISOpenWTab$female <- "Girls"
CPISOpenWTab$perc <- 100 * CPISOpenWTab$freq / nrow(CPISOpenW)
CPISOpenTab <- bind_rows(CPISOpenMTab, CPISOpenWTab)
ggplot(CPISOpenTab, aes(x = perc, y = reorder(subject, perc), fill = female)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual("Gender", values = c("black", "white")) +
  labs(x = "% of answers", y = "Category of Answer") +
  theme_linedraw() +
  theme(axis.text = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        axis.title = element_text(size = 18.5),
        axis.title.x = element_text(hjust = 0.3, vjust = -0.17),
        panel.grid = element_blank(),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/CPISOpenQuestion.pdf", width = 11, height = 8.5)

##### 1.1.3 Creation of sub-datasets ####
CPISBoys <- filter(CPIS, female == 0)
CPISGirls <- filter(CPIS, female == 1)
CPISYoung <- CPIS |> filter(age > 9 & age <= 15)
CPISOld <- CPIS |> filter(age >= 16 & age < 19)
CPISYoung$agegrp <- "Ages 10-15"
CPISOld$agegrp <- "Ages 16-18"
CPISOldYoung <- rbind(CPISYoung, CPISOld)
CPISOldYoung$agegrp <- factor(CPISOldYoung$agegrp, levels = c(
  "Ages 10-15", "Ages 16-18"))
CPISYoungBoys <- filter(CPISYoung, female == 0)
CPISYoungGirls <- filter(CPISYoung, female == 1)
CPISOldBoys <- filter(CPISOld, female == 0)
CPISOldGirls <- filter(CPISOld, female == 1)
mean(CPIS$agentic, na.rm= T)
mean(CPIS$communal, na.rm= T)
CPISReview <- data.frame(pol_meaning = CPIS$pol_meaning)
CPISReview$governing_government <- NA
CPISReview$power <- NA
CPISReview$communication_speeches <- NA
CPISReview$leadership <- NA
CPISReview$conflict <- NA
CPISReview$disagreement <- NA
CPISReview$controversy_debate <- NA
CPISReview$competition <- NA
CPISReview$partisan_game <- NA
CPISReview$elections_voting_democracy <- NA
CPISReview$rules_laws <- NA
CPISReview$legislating <- NA
CPISReview$specific_political_issue <- NA
CPISReview$cooperation <- NA
CPISReview$political_parties <- NA
CPISReview$resource_distribution <- NA
CPISReview$interest_groups <- NA
CPISReview$specific_people <- NA
CPISReview$mistake_other_concept <- NA
CPISReview$too_vague <- NA
CPISReview$no_real_answer <- NA
#openxlsx::write.xlsx(CPISReview, "_data/CPIS/CPISReview.xlsx")

#### 1.2 Census ####
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
Census21$ethn <- Census21$DPGRSUM
Census21$ethn[Census21$ethn == 88] <- NA
Census21$white[Census21$ethn == 1] <- 2
Census21$white[Census21$ethn > 1] <- 1
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
prop.table(table(CensusAdult$ethn)) # 70.2% white
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
prop.table(table(CensusQcAdult$ethn)) # 82.1% white
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
prop.table(table(CensusTeen$ethn)) # 59.8% white

# Full data
prop.table(table(Census21$ses_female))
18226240/36991980 # female
prop.table(table(Census21$age, useNA = "always"))
1831195/36991980 # age group 0-4
prop.table(table(Census21$ethn))
26689275/36328480 # not visible minority... doesn't match with white
1547870/36328480 # quite close to #4 (Black)
2571400/36328480 # quite close to #2 (South Asian)

#### 1.3 Datagotchi PES ####
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
DG$ethn <- NA
DG$ethn[DG$ses_ethn %in% c("Blanc", "White")] <- "White"
DG$ethn[DG$ses_ethn == "Noir"] <- "Black"
DG$ethn[DG$ses_ethn %in% c("Asian", "Asiatique")] <- "Asian"
DG$ethn[DG$ses_ethn %in% c("Arab", "Arabe")] <- "Arabic"
DG$ethn[DG$ses_ethn %in% c("Hispanic", "Hispanique")] <- "Hispanic"
DG$ethn[DG$ses_ethn == "Autochtone"] <- "Indigenous"
DG$ethn[DG$ses_ethn %in% c("Autre", "Other")] <- "Other"
table(DG$ethn, useNA = "always")
DG$white <- 0
DG$white[DG$ethn == "White"] <- 1
DG$white[is.na(DG$ethn)] <- NA
table(DG$white, useNA = "always")
DG$white <- 0
DG$white[DG$ses_ethn %in% c("Blanc", "White")] <- 1
DG$white[is.na(DG$ses_ethn) | DG$ses_ethn == ""] <- NA
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
  "Technical, college\nor CEGEP"
DG$education[DG$education ==
               "Technical, community college, CEGEP or College classique"] <-
  "Technical, college\nor CEGEP"
DG$education[DG$education == "Doctorat"] <- "Doctorate"
DG$education[DG$education == "Maîtrise"] <- "Master's degree"
DG$education[DG$education == "Aucune scolarité"] <- "No schooling"
DG$education[DG$education == "École primaire"] <- "Elementary school"
DG$education[DG$education == "École secondaire"] <- "High school"
DG$education[DG$education == ""] <- NA
DG$education <- factor(DG$education, levels = c(
  "No schooling", "Elementary school", "High school",
  "Technical, college\nor CEGEP", "Bachelor's degree", "Master's degree",
  "Doctorate"))
table(DG$education, useNA = "always")
DG$ses_education <- NA
DG$ses_education[DG$education %in% c(
  "No schooling", "Elementary school", "High school")] <- 1
DG$ses_education[DG$education == "Technical, college\nor CEGEP"] <- 2
DG$ses_education[DG$education %in% c(
  "Bachelor's degree", "Doctorate", "Master's degree")] <- 3
DG$income <- DG$ses_income
DG$income[DG$income == "Aucun revenu"] <- "No income"
DG$income[DG$income %in% c("1$ à 30 000$", "$1 to $30 000")] <- "$1-30,000"
DG$income[DG$income %in% c("30 001$ à 60 000$", "$30 001 to $60 000")] <-
 "$30,001-60,000"
DG$income[DG$income %in% c("60 001$ à 90 000$", "$60 001 to $90 000")] <-
 "$60,001-90,000"
DG$income[DG$income %in% c("90 001 à 110 000$", "$90 001 to $110 000")] <-
 "$90,001-110,000"
DG$income[DG$income %in% c("110 001$ à 150 000$", "$110 001 to $150 000")] <-
 "$110,001-150,000"
DG$income[DG$income %in% c("150 001$ à 200 000$", "$150 001 to $200 000")] <-
 "$150,001-200,000"
DG$income[DG$income %in% c("Plus de 200 000$", "More than $200 000")] <-
 "$200,000+"
DG$income[DG$income == ""] <- NA
DG$income <- factor(DG$income, levels = c(
  "No income", "$1-30,000", "$30,001-60,000", "$60,001-90,000",
  "$90,001-110,000", "$110,001-150,000", "$150,001-200,000", "$200,000+"))
table(DG$income, useNA = "always")
DG$income_low <- NA
DG$income_low[DG$income %in% c(
  "$110,001-150,000", "$150,001-200,000", "$60,001-90,000",
  "$90,001-110,000", "$200,000+")] <- 0
DG$income_low[DG$income %in% c(
  "$1-30,000", "$30,001-60,000", "No income")] <- 1
DG$income_mid <- NA
DG$income_mid[DG$income %in% c(
  "$1-30,000", "$30,001-60,000", "No income", "$150,001-200,000",
  "$200,000+")] <- 0
DG$income_mid[DG$income %in% c(
  "$110,001-150,000", "$60,001-90,000", "$90,001-110,000")] <- 1
DG$income_high <- NA
DG$income_high[DG$income %in% c(
  "$1-30,000", "$30,001-60,000", "No income", "$110,001-150,000",
  "$60,001-90,000", "$90,001-110,000")] <- 0
DG$income_high[DG$income %in% c("$150,001-200,000", "$200,000+")] <- 1
DG$ses_income <- NA
DG$ses_income[DG$income %in% c(
  "$1-30,000", "$30,001-60,000", "No income")] <- 1
DG$ses_income[DG$income %in% c(
  "$110,001-150,000", "$60,001-90,000", "$90,001-110,000")] <- 2
DG$ses_income[DG$income %in% c(
  "$150,001-200,000", "More than $200,000")] <- 3
DG$interest <- as.numeric(DG$pol_interest_1)
DG$interest_health <- as.numeric(DG$issues_interest_1)
DG$interest_foreign <- as.numeric(DG$issues_interest_2)
DG$interest_law <- as.numeric(DG$issues_interest_3)
DG$interest_education <- as.numeric(DG$issues_interest_4)
DG$interest_partisan <- as.numeric(DG$issues_interest_5)

##### 1.3.1 Weighting ####
calculate_unweighted_props <- function(data, variable) {
  data |> # calculate proportions for one variable
    dplyr::select({{variable}}) |>
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
    dplyr::select(name, value, n, prop)
  return(DataProp) # calculate proportions for multiple variables
}
add_raking_weights_column_5_var <- function(
    popData, sampleData, variable1, variable2, variable3, variable4,
    variable5) {
  sampleData$mergeId <- 1:nrow(sampleData) # add a variable for row number
  subsetRaking <- sampleData |> # keep only relevant variables
    dplyr::select(mergeId, {{variable1}}, {{variable2}}, {{variable3}},
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
  sampleData$weight <- raking$weightvec # add raking weights column
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
summary(DG$weight)
prop.table(table(CensusQcAdult$ses_female))
prop.table(table(DG$ses_female))
prop.table(questionr::wtd.table(DG$ses_female, weights = DG$weight))
prop.table(table(CensusQcAdult$ses_education))
prop.table(table(DG$ses_education))
prop.table(questionr::wtd.table(DG$ses_education, weights = DG$weight))
prop.table(table(CensusQcAdult$ses_franco))
prop.table(table(DG$ses_franco))
prop.table(questionr::wtd.table(DG$ses_franco, weights = DG$weight))
prop.table(table(CensusQcAdult$ses_age))
prop.table(table(DG$ses_age))
prop.table(questionr::wtd.table(DG$ses_age, weights = DG$weight))
prop.table(table(CensusQcAdult$ses_income))
prop.table(table(DG$ses_income))
prop.table(questionr::wtd.table(DG$ses_income, weights = DG$weight))

#### 1.4 CES ####
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
CES97$weight <- CES97$cpsnwgt1
summary(CES97$weight)
CES97men <- filter(CES97, female == 0)
CES97women <- filter(CES97, female == 1)
CES97$mode <- "CPS phone"

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
#CES00$weight <- CES00$weshhwgt
#CES00$weight <- CES00$wespwgt
#CES00$weight <- CES00$wesnwgt
CES00$weight <- CES00$ceshhwgt
#CES00$weight <- CES00$rocnwgt
#CES00$weight <- CES00$cesnwgt
#CES00$weight <- CES00$cespwgt
summary(CES00$weight)
CES00$mode <- "CPS phone"
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
#CES04$weight <- CES04$ces04_CESPWGT
#CES04$weight <- CES04$ces04_CESNWGT
#CES04$weight <- CES04$ces04_RDDHHWGT
#CES04$weight <- CES04$ces04_ROCNWGT
CES04$weight <- CES04$ces04_CESHHWGT
summary(CES04$weight)
CES04$mode <- "CPS phone"
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
#CES06$weight <- CES06$ces06_CESPWGT
#CES06$weight <- CES06$ces06_CESNWGT
#CES06$weight <- CES06$ces06_RDDHHWGT
CES06$weight <- CES06$ces06_CESHHWGT
summary(CES06$weight)
CES06$mode <- "CPS phone"
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
#CES08$weight <- CES08$ces08_PROVWGT
CES08$weight <- CES08$ces08_NATWGT
summary(CES08$weight)
CES08$mode <- "CPS phone"
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
#CES11$weight <- CES11$NATIONAL_WGTPOP11
#CES11$weight <- CES11$WeightBYNadults_and_TotPopn
CES11$weight <- CES11$HOUSEHOLD_WGTSAMP11
#CES11$weight <- CES11$PROVINCIAL_WEIGHT11
summary(CES11$weight)
CES11men <- filter(CES11, female == 0)
CES11women <- filter(CES11, female == 1)
CES11$mode <- "PES phone"

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
CES15$weight <- CES15$CombWgt
CES15men <- filter(CES15, female == 0)
CES15women <- filter(CES15, female == 1)
CES15$original_mode <- CES15$mode
CES15$mode <- NA
table(CES15$original_mode) # compared to Breton et al. 2015 table 1
CES15$mode[CES15$original_mode == 0 & is.na(CES15$p_date)] <- "CPS phone"
CES15$mode[CES15$original_mode == 1 & is.na(CES15$p_date)] <- "CPS online"
CES15$mode[CES15$original_mode == 0 & !is.na(CES15$p_date)] <- "PES phone"
CES15$mode[CES15$original_mode == 1 & !is.na(CES15$p_date)] <- "PES online"
# but political interest questions were asked in the PES only, so CPS phone/
# online will not appear in the graph
table(CES15$mode, useNA = "always")

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
table(CES19online$cps19_interest_gen_1, CES19online$pes19_interest_1,
 useNA = "always")
CES19online$mode <- "CPS online"
table(CES19online$pes19_interest_1)
# but political interest questions in the PES were excluded since they come from the same respondents

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
table(CES19phone$mode_PES, useNA = "always")
CES19phone$mode <- NA
CES19phone$mode[is.na(CES19phone$mode_PES)] <- "CPS phone"
CES19phone$mode[CES19phone$mode_PES == "(1) CATI"] <- "PES phone"
CES19phone$mode[CES19phone$mode_PES == "(2) Web"] <- "PES online"
# but political interest questions were asked in the PES only, so CPS phone
# will not appear in the graph

CES19 <- data.frame(
  id = c(CES19online$cps19_ResponseId, CES19phone$sample_id),
  # merge online + phone data
  sample = c(rep("online", length(CES19online$cps19_ResponseId)),
           rep("phone", length(CES19phone$sample_id))),
  mode = as.factor(c(CES19online$mode, CES19phone$mode)),
  female = as.factor(c(CES19online$female, CES19phone$female)),
  interest = as.integer(c(CES19online$interest, CES19phone$interest)),
  age = c(CES19online$age, CES19phone$age),
  weight = c(CES19online$cps19_weight_general_all,
             CES19phone$weight_CES))
CES19men <- filter(CES19, female == 0)
CES19women <- filter(CES19, female == 1)

CES21 <- readstata13::read.dta13("_data/CES/CES21/CES21.dta")
CES21$ethn <- NA
CES21$ethn[
  CES21$cps21_vismin_1 == -99 & CES21$cps21_vismin_2 == -99 &
    CES21$cps21_vismin_3 == -99 & CES21$cps21_vismin_4 == -99 &
    CES21$cps21_vismin_5 == -99 & CES21$cps21_vismin_6 == -99 &
    CES21$cps21_vismin_7 == -99 & CES21$cps21_vismin_8 == -99 &
    CES21$cps21_vismin_9 == 1 & CES21$cps21_vismin_10 == -99 &
    CES21$cps21_vismin_1 == -99] <- "White"
CES21$ethn[
  CES21$cps21_vismin_1 == -99 & CES21$cps21_vismin_2 == -99 &
    CES21$cps21_vismin_3 == 1 & CES21$cps21_vismin_4 == -99 &
    CES21$cps21_vismin_5 == -99 & CES21$cps21_vismin_6 == -99 &
    CES21$cps21_vismin_7 == -99 & CES21$cps21_vismin_8 == -99 &
    CES21$cps21_vismin_9 == -99 & CES21$cps21_vismin_10 == -99 &
    CES21$cps21_vismin_1 == -99] <- "Black"
CES21$ethn[
  CES21$cps21_vismin_1 == -99 & CES21$cps21_vismin_2 == -99 &
    CES21$cps21_vismin_3 == -99 & CES21$cps21_vismin_4 == -99 &
    CES21$cps21_vismin_5 == -99 & CES21$cps21_vismin_6 == -99 &
    CES21$cps21_vismin_7 == -99 & CES21$cps21_vismin_8 == 1 &
    CES21$cps21_vismin_9 == -99 & CES21$cps21_vismin_10 == -99 &
    CES21$cps21_vismin_1 == -99] <- "West Asian"
CES21$ethn[
  CES21$cps21_vismin_1 == -99 & CES21$cps21_vismin_2 == -99 &
    CES21$cps21_vismin_3 == -99 & CES21$cps21_vismin_4 == -99 &
    CES21$cps21_vismin_5 == -99 & CES21$cps21_vismin_6 == -99 &
    CES21$cps21_vismin_7 == 1 & CES21$cps21_vismin_8 == -99 &
    CES21$cps21_vismin_9 == -99 & CES21$cps21_vismin_10 == -99 &
    CES21$cps21_vismin_1 == -99] <- "Southeast Asian"
CES21$ethn[
  CES21$cps21_vismin_1 == 1 & CES21$cps21_vismin_2 == -99 &
    CES21$cps21_vismin_3 == -99 & CES21$cps21_vismin_4 == -99 &
    CES21$cps21_vismin_5 == -99 & CES21$cps21_vismin_6 == -99 &
    CES21$cps21_vismin_7 == -99 & CES21$cps21_vismin_8 == -99 &
    CES21$cps21_vismin_9 == -99 & CES21$cps21_vismin_10 == -99 &
    CES21$cps21_vismin_1 == -99] <- "Arabic"
CES21$ethn[
  CES21$cps21_vismin_1 == -99 & CES21$cps21_vismin_2 == -99 &
    CES21$cps21_vismin_3 == -99 & CES21$cps21_vismin_4 == -99 &
    CES21$cps21_vismin_5 == -99 & CES21$cps21_vismin_6 == 1 &
    CES21$cps21_vismin_7 == -99 & CES21$cps21_vismin_8 == -99 &
    CES21$cps21_vismin_9 == -99 & CES21$cps21_vismin_10 == -99 &
    CES21$cps21_vismin_1 == -99] <- "South Asian"
CES21$ethn[
  CES21$cps21_vismin_1 == -99 & CES21$cps21_vismin_2 == -99 &
    CES21$cps21_vismin_3 == -99 & CES21$cps21_vismin_4 == -99 &
    CES21$cps21_vismin_5 == 1 & CES21$cps21_vismin_6 == -99 &
    CES21$cps21_vismin_7 == -99 & CES21$cps21_vismin_8 == -99 &
    CES21$cps21_vismin_9 == -99 & CES21$cps21_vismin_10 == -99 &
    CES21$cps21_vismin_1 == -99] <- "Hispanic"
CES21$ethn[
  CES21$cps21_vismin_1 == -99 & CES21$cps21_vismin_2 == -99 &
    CES21$cps21_vismin_3 == -99 & CES21$cps21_vismin_4 == 1 &
    CES21$cps21_vismin_5 == -99 & CES21$cps21_vismin_6 == -99 &
    CES21$cps21_vismin_7 == -99 & CES21$cps21_vismin_8 == -99 &
    CES21$cps21_vismin_9 == -99 & CES21$cps21_vismin_10 == -99 &
    CES21$cps21_vismin_1 == -99] <- "Indigenous"
CES21$ethn[
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
CES21$mode <- "CPS online"
CES21$immig <- ifelse(CES21$cps21_bornin_canada == "Yes", 1,
                      ifelse(CES21$cps21_bornin_canada == "No", 2, NA))
CES21$weight <- CES21$cps21_weight_general_all
CES21$education <- as.character(CES21$cps21_education)
CES21$education[
  CES21$education == "Don't know/ Prefer not to answer"] <- NA
CES21$education[CES21$education == paste0(
  "Some elementary school")] <-
  "Elementary school"
CES21$education[CES21$education == paste0(
  "Completed elementary school")] <-
  "Elementary school"
CES21$education[CES21$education == paste0(
  "Some secondary/ high school")] <-
  "Elementary school"
CES21$education[CES21$education == paste0(
  "Completed secondary/ high school")] <-
  "High school"
CES21$education[CES21$education == paste0(
  "Some technical, community college, CEGEP, College Classique")] <-
  "High school"
CES21$education[CES21$education == paste0(
  "Completed technical, community college, CEGEP, College Classique")] <-
  "Technical, college\nor CEGEP"
CES21$education[CES21$education == paste0(
  "Professional degree or doctorate")] <-
  "Doctorate"
CES21$education <- factor(CES21$education, levels = c(
  "No schooling", "Elementary school", "High school",
  "Technical, college\nor CEGEP", "Bachelor's degree", "Master's degree",
  "Doctorate"))
CES21$income <- NA
CES21$income[CES21$cps21_income_number < 1] <- "No income"
CES21$income[CES21$cps21_income_number >= 1 &
               CES21$cps21_income_number < 30000] <- "$1-30,000"
CES21$income[CES21$cps21_income_number >= 30000 &
               CES21$cps21_income_number < 60000] <- "$30,001-60,000"
CES21$income[CES21$cps21_income_number >= 60000 &
               CES21$cps21_income_number < 90000] <- "$60,001-90,000"
CES21$income[CES21$cps21_income_number >= 90000 &
               CES21$cps21_income_number < 110000] <- "$90,001-110,000"
CES21$income[CES21$cps21_income_number >= 110000 &
               CES21$cps21_income_number < 150000] <- "$110,001-150,000"
CES21$income[CES21$cps21_income_number >= 150000 &
               CES21$cps21_income_number < 200000] <- "$150,001-200,000"
CES21$income[CES21$cps21_income_number >= 200000] <- "$200,000+"
CES21$income <- factor(CES21$income, levels = c(
  "No income", "$1-30,000", "$30,001-60,000", "$60,001-90,000",
  "$90,001-110,000", "$110,001-150,000", "$150,001-200,000",
  "$200,000+"))
CES21$province <- as.character(CES21$pes21_province) # no data about territories
CES21$province[CES21$province == "Newfoundland and Labrador"] <-
  "N&L"
CES21$province[CES21$province == "Northwest Territories"] <-
  "NWT"
CES21$province[CES21$province == "Prince Edward Island"] <-
  "PEI"
CES21$province <- as.factor(CES21$province)
CES97$year <- 1997
CES00$year <- 2000
CES04$year <- 2004
CES06$year <- 2006
CES08$year <- 2008
CES11$year <- 2011
CES15$year <- 2015
CES19$year <- 2019
CES21$year <- 2021
CES21$internal_efficacy <- case_when(
  CES21$cps21_govt_confusing == "Strongly agree" ~ 0,
  CES21$cps21_govt_confusing == "Somewhat agree" ~ (1 / 3),
  CES21$cps21_govt_confusing == "Somewhat disagree" ~ (2 / 3),
  CES21$cps21_govt_confusing == "Strongly disagree" ~ 1)
CES21$external_efficacy_pre <- case_when(
  CES21$cps21_govt_say == "Strongly agree" ~ 0,
  CES21$cps21_govt_say == "Somewhat agree" ~ (1 / 3),
  CES21$cps21_govt_say == "Somewhat disagree" ~ (2 / 3),
  CES21$cps21_govt_say == "Strongly disagree" ~ 1)
CES21$external_efficacy_post <- case_when(
  CES21$pes21_govtcare == "Strongly agree" ~ 0,
  CES21$pes21_govtcare == "Somewhat agree" ~ 0.25,
  CES21$pes21_govtcare == "Neither agree nor disagree" ~ 0.5,
  CES21$pes21_govtcare == "Somewhat disagree" ~ 0.75,
  CES21$pes21_govtcare == "Strongly disagree" ~ 1)
CES21$external_efficacy <- CES21$external_efficacy_pre
CES21$know_premier_name <- 0
CES21$know_premier_name[CES21$cps21_premier_name %in% c(
  "Sandy Silver_(21)", "Caroline Cochrane_(19)", "Joe Savikataaq_(23)",
  "John Horgan_(9)", "Jason Kenney_(15)", "Scott Moe_(13)",
  "Brian Pallister_(1)", "Doug Ford_(25)", "François Legault_(3)",
  "Blaine Higgs_(7)", "Dennis King_(11)", "Tim Houston_(6)",
  "Andrew Furey_(17)")] <- 1
CES21$know_premier_name[CES21$cps21_premier_name %in% c(
  "I don't know", "Prefer not to answer")] <- NA
table(CES21$know_premier_name, useNA = "always")
CES21$know_finmin_name <- 0
CES21$know_finmin_name[CES21$cps21_finmin_name == "Chrystia Freeland"] <- 1
CES21$know_finmin_name[CES21$cps21_finmin_name %in% c(
  "I don't know", "Prefer not to answer")] <- NA
table(CES21$know_finmin_name, useNA = "always")
CES21$know_govgen_name <- 0
CES21$know_govgen_name[CES21$cps21_govgen_name == "Mary Simon"] <- 1
CES21$know_govgen_name[CES21$cps21_govgen_name %in% c(
  "I don't know", "Prefer not to answer")] <- NA
table(CES21$know_govgen_name, useNA = "always")
CES21$cps21_lr_parties_1[CES21$cps21_lr_parties_1 == -99] <- NA
CES21$cps21_lr_parties_2[CES21$cps21_lr_parties_2 == -99] <- NA
CES21$cps21_lr_parties_3[CES21$cps21_lr_parties_3 == -99] <- NA
CES21$cps21_lr_parties_4[CES21$cps21_lr_parties_4 == -99] <- NA
CES21$cps21_lr_parties_5[CES21$cps21_lr_parties_5 == -99] <- NA
CES21$cps21_lr_parties_7[CES21$cps21_lr_parties_7 == -99] <- NA
CES21Educated <- filter(CES21, education %in% c(
  "Bachelor's degree", "Master's degree", "Doctorate"))
CES21$doesnt_know_lpc_pos <- abs(
  CES21$cps21_lr_parties_1 - mean(CES21Educated$cps21_lr_parties_1, na.rm = T))
CES21$doesnt_know_cpc_pos <- abs(
  CES21$cps21_lr_parties_2 - mean(CES21Educated$cps21_lr_parties_2, na.rm = T))
CES21$doesnt_know_ndp_pos <- abs(
  CES21$cps21_lr_parties_3 - mean(CES21Educated$cps21_lr_parties_3, na.rm = T))
CES21$doesnt_know_bq_pos <- abs(
  CES21$cps21_lr_parties_4 - mean(CES21Educated$cps21_lr_parties_4, na.rm = T))
CES21$doesnt_know_gpc_pos <- abs(
  CES21$cps21_lr_parties_5 - mean(CES21Educated$cps21_lr_parties_5, na.rm = T))
CES21$doesnt_know_ppc_pos <- abs(
  CES21$cps21_lr_parties_7 - mean(CES21Educated$cps21_lr_parties_7, na.rm = T))
clean_participation_scale <- function(x){
  CES21$x <- CES21[[x]]
  case_when(CES21$x == "More than five times" ~ 1,
            CES21$x == "A few times" ~ (2 / 3),
            CES21$x == "Just once" ~ (1 / 3),
            CES21$x == "Never" ~ 0)
}
CES21$volunteer_community <- clean_participation_scale("cps21_volunteer")
CES21$attend_meeting_speech <- clean_participation_scale("pes21_partic1_1")
CES21$attend_protest <- clean_participation_scale("pes21_partic1_2")
CES21$boycott <- clean_participation_scale("pes21_partic1_3")
CES21$petition <- clean_participation_scale("pes21_partic1_4")
CES21$politician_social_media <- clean_participation_scale("pes21_partic2_1")
CES21$volunteer_partisan <- clean_participation_scale("pes21_partic2_2")
CES21$contact_politician <- clean_participation_scale("pes21_partic2_3")
CES21$donation_partisan <- clean_participation_scale("pes21_partic2_4")
CES21$donation_cause <- clean_participation_scale("pes21_partic3_1")
CES21$group_partic <- clean_participation_scale("pes21_partic3_2")
CES21$comment_politics <- clean_participation_scale("pes21_partic3_3")
CES21$talk_issue_social_media <- clean_participation_scale("pes21_partic3_4")

##### 1.4.1 Factor analysis #####
EfficacyScale <- na.omit(CES21[, c(
  "internal_efficacy", "external_efficacy_pre", "external_efficacy_post")])
EfficacyCronbach <- round(as.numeric(psy::cronbach(EfficacyScale)[3]),
                          digits = 2)
EfficacyFactorAnalysis <- factanal(EfficacyScale, factors = 1)
EfficacyVariableNames <- c(
  paste("Sometimes, politics and\ngovernment seem so\ncomplicated that a",
        "person\nlike me can't really\nunderstand what's going on"),
  "People like me don't\nhave any say about\nwhat the government does",
  "The government does not\ncare much about\nwhat people\nlike me think")
EfficacyFactorLoadings <- EfficacyFactorAnalysis$loadings[, 1]
EfficacyFirstEigenvalue <- round(eigen(cor(EfficacyScale))$values[1],
                                 digits = 2)
ggplot(data.frame(EfficacyVariableNames, EfficacyFactorLoadings),
       aes(x = reorder(EfficacyVariableNames, desc(EfficacyVariableNames)),
           y = EfficacyFactorLoadings)) +
  coord_flip() +
  geom_bar(stat = "identity", colour = "black", fill = "black", linewidth = 1,
           width = 0.4) +
  geom_text(aes(label = as.character(round(
    EfficacyFactorLoadings, digits = 2))), vjust = 0.35, hjust = -0.3,
    family = "CM Roman", size = 6) +
  geom_hline(yintercept = 0.3, colour = "gray", linetype = "longdash") +
  annotate("text", label = paste("Cronbach's alpha =", as.character(
    EfficacyCronbach)), x = 1.1, y = 0.85, size = 6,
    family = "CM Roman") +
  annotate("text", label = paste("First eigenvalue =", as.character(
    EfficacyFirstEigenvalue)), x = 0.9, y = 0.85, size = 6,
    family = "CM Roman") +
  scale_y_continuous(name = "Factor loadings", limits = c(-0.1, 1),
                     breaks = seq(-0.1, 1, by = 0.1)) +
  xlab("") +
  theme_linedraw() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 18.5),
        axis.title.x = element_text(hjust = 0.3, vjust = -0.17),
        panel.grid = element_blank(),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/EfficacyScale.pdf", width = 11, height = 4.25)
# Apply the function to each row and create a new column agentic
efficacyVariables <- c("internal_efficacy", "external_efficacy_pre",
                       "external_efficacy_post")
CES21$efficacy <- apply(CES21[efficacyVariables], 1, delete_rows_na,
                        loadings = EfficacyFactorLoadings,
                        number_na_allowed = 1)
# 0 = efficacious, 1 = not efficacious, at least two answers out of 3 questions
length(na.omit(CES21$efficacy)) / nrow(CES21) * 100 # 98% available data

KnowScale <- na.omit(CES21[, c(
  "know_premier_name", "know_finmin_name", "know_govgen_name")])
KnowLoevinger <- round(as.numeric(unlist(
  mokken::coefH(KnowScale))["H.Scale H"]), digits = 2)
KnowFactorAnalysis <- factanal(KnowScale, factors = 1)
KnowVariableNames <- c("Name of Provincial Premier", "Name of Finance Minister",
                       "Name of Governor General")
KnowFactorLoadings <- KnowFactorAnalysis$loadings[, 1]
KnowFirstEigenvalue <- round(eigen(cor(KnowScale))$values[1],
                                 digits = 2)
ggplot(data.frame(KnowVariableNames, KnowFactorLoadings),
       aes(x = reorder(KnowVariableNames, desc(KnowVariableNames)),
           y = KnowFactorLoadings)) +
  coord_flip() +
  geom_bar(stat = "identity", colour = "black", fill = "black", linewidth = 1,
           width = 0.4) +
  geom_text(aes(label = as.character(round(
    KnowFactorLoadings, digits = 2))), vjust = 0.35, hjust = -0.3,
    family = "CM Roman", size = 6) +
  geom_hline(yintercept = 0.3, colour = "gray", linetype = "longdash") +
  annotate("text", label = paste("Loevinger's H =", as.character(
    KnowLoevinger)), x = 1.1, y = 0.85, size = 6,
    family = "CM Roman") +
  annotate("text", label = paste("First eigenvalue =", as.character(
    KnowFirstEigenvalue)), x = 0.9, y = 0.85, size = 6,
    family = "CM Roman") +
  scale_y_continuous(name = "Factor loadings", limits = c(-0.1, 1),
                     breaks = seq(-0.1, 1, by = 0.1)) +
  xlab("") +
  theme_linedraw() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 18.5),
        axis.title.x = element_text(hjust = 0.3, vjust = -0.17),
        panel.grid = element_blank(),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/KnowScale.pdf", width = 11, height = 4.25)
# Apply the function to each row and create a new column agentic
knowVariables <- c("know_premier_name", "know_finmin_name",
                   "know_govgen_name")
CES21$knowledge <- apply(CES21[knowVariables], 1, delete_rows_na,
                         loadings = KnowFactorLoadings,
                         number_na_allowed = 1)
# 0 = efficacious, 1 = not efficacious, at least two answers out of 3 questions
length(na.omit(CES21$knowledge)) / nrow(CES21) * 100 # 84% available data

KnowPartyScale <- na.omit(CES21[, c(
  "doesnt_know_lpc_pos", "doesnt_know_cpc_pos", "doesnt_know_ndp_pos",
  "doesnt_know_bq_pos", "doesnt_know_gpc_pos", "doesnt_know_ppc_pos")])
KnowPartyLoevinger <- round(as.numeric(unlist(
  mokken::coefH(round(KnowPartyScale, 0)))["H.Scale H"]), digits = 2)
KnowPartyFactorAnalysis <- factanal(KnowPartyScale, factors = 1)
KnowPartyVariableNames <- c(
  "Knows LPC's ideology", "Knows CPC's ideology", "Knows NDP's ideology",
  "Knows BQ's ideology", "Knows GPC's ideology", "Knows PPC's ideology")
KnowPartyFactorLoadings <- KnowPartyFactorAnalysis$loadings[, 1]
KnowPartyFirstEigenvalue <- round(eigen(cor(KnowPartyScale))$values[1],
 digits = 2)
ggplot(data.frame(KnowPartyVariableNames, KnowPartyFactorLoadings),
       aes(x = reorder(KnowPartyVariableNames, desc(KnowPartyVariableNames)),
           y = KnowPartyFactorLoadings)) +
  coord_flip() +
  geom_bar(stat = "identity", colour = "black", fill = "black", linewidth = 1,
           width = 0.4) +
  geom_text(aes(label = as.character(round(
    KnowPartyFactorLoadings, digits = 2))), vjust = 0.35, hjust = -0.3,
    family = "CM Roman", size = 6) +
  geom_hline(yintercept = 0.3, colour = "gray", linetype = "longdash") +
  annotate("text", label = paste("Loevinger's H =", as.character(
    KnowPartyLoevinger)), x = 1.3, y = 0.85, size = 6,
    family = "CM Roman") +
  annotate("text", label = paste("First eigenvalue =", as.character(
    KnowPartyFirstEigenvalue)), x = 0.9, y = 0.85, size = 6,
    family = "CM Roman") +
  scale_y_continuous(name = "Factor loadings", limits = c(-0.1, 1),
                     breaks = seq(-0.1, 1, by = 0.1)) +
  xlab("") +
  theme_linedraw() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 18.5),
        axis.title.x = element_text(hjust = 0.3, vjust = -0.17),
        panel.grid = element_blank(),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/KnowPartyScale.pdf", width = 11, height = 4.25)
knowPartyVariables <- c(
  "doesnt_know_lpc_pos", "doesnt_know_cpc_pos", "doesnt_know_ndp_pos",
  "doesnt_know_bq_pos", "doesnt_know_gpc_pos", "doesnt_know_ppc_pos")
CES21$knowparty <- apply(CES21[knowPartyVariables], 1, delete_rows_na,
                         loadings = KnowPartyFactorLoadings,
                         number_na_allowed = 1)
length(na.omit(CES21$knowparty)) / nrow(CES21) * 100 # 65% available data

ParticScale <- na.omit(CES21[, c(
  "volunteer_community", "attend_meeting_speech", "attend_protest", "boycott",
  "petition", "politician_social_media", "volunteer_partisan",
  "contact_politician", "donation_partisan", "donation_cause", "group_partic",
  "comment_politics", "talk_issue_social_media")])
ParticCronbach <- round(as.numeric(psy::cronbach(ParticScale)[3]),
                        digits = 2)
ParticFactorAnalysis <- factanal(ParticScale, factors = 1)
ParticVariableNames <- c(
  "Volunteer for group/organization", "Attend political meeting/speech",
  "Attend protest", "Boycott", "Sign petition",
  "Follow politician social media", "Volunteer politician",
  "Contact elected official", "Donation to candidate", "Donation to cause",
  "Group active membership", "Comment political content",
  "Discuss politics social media")
ParticFactorLoadings <- ParticFactorAnalysis$loadings[, 1]
ParticFirstEigenvalue <- round(eigen(cor(ParticScale))$values[1],
                               digits = 2)
ggplot(data.frame(ParticVariableNames, ParticFactorLoadings),
       aes(x = reorder(ParticVariableNames, desc(ParticVariableNames)),
           y = ParticFactorLoadings)) +
  coord_flip() +
  geom_bar(stat = "identity", colour = "black", fill = "black", linewidth = 1,
           width = 0.4) +
  geom_text(aes(label = as.character(round(
    ParticFactorLoadings, digits = 2))), vjust = 0.35, hjust = -0.3,
    family = "CM Roman", size = 6) +
  geom_hline(yintercept = 0.3, colour = "gray", linetype = "longdash") +
  annotate("text", label = paste("Cronbach's alpha =", as.character(
    ParticCronbach)), x = 4, y = 0.825, size = 6,
    family = "CM Roman") +
  annotate("text", label = paste("First eigenvalue =", as.character(
    ParticFirstEigenvalue)), x = 2, y = 0.825, size = 6,
    family = "CM Roman") +
  scale_y_continuous(name = "Factor loadings", limits = c(-0.1, 1),
                     breaks = seq(-0.1, 1, by = 0.1)) +
  xlab("") +
  theme_linedraw() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 18.5),
        axis.title.x = element_text(hjust = 0.3, vjust = -0.17),
        panel.grid = element_blank(),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/ParticScale.pdf", width = 11, height = 4.25)
# Apply the function to each row and create a new column agentic
particVariables <- c(
  "volunteer_community", "attend_meeting_speech", "attend_protest", "boycott",
  "petition", "politician_social_media", "volunteer_partisan",
  "contact_politician", "donation_partisan", "donation_cause", "group_partic",
  "comment_politics", "talk_issue_social_media")
CES21$participation <- apply(CES21[particVariables], 1, delete_rows_na,
                         loadings = ParticFactorLoadings,
                         number_na_allowed = 10)
# 0 = efficacious, 1 = not efficacious, at least two answers out of 3 questions
length(na.omit(CES21$participation)) / nrow(CES21) * 100 # 71% available data

ParticPartisanScale <- na.omit(CES21[, c(
  "attend_meeting_speech", "politician_social_media", "volunteer_partisan",
  "contact_politician", "donation_partisan")])
ParticPartisanCronbach <- round(as.numeric(psy::cronbach(ParticPartisanScale)[3]),
  digits = 2)
ParticPartisanFactorAnalysis <- factanal(ParticPartisanScale, factors = 1)
ParticPartisanVariableNames <- c(
  "Attend political meeting/speech", "Follow politician social media",
  "Volunteer politician", "Contact elected official", "Donation to candidate")
ParticPartisanFactorLoadings <- ParticPartisanFactorAnalysis$loadings[, 1]
ParticPartisanFirstEigenvalue <- round(
  eigen(cor(ParticPartisanScale))$values[1], digits = 2)
ggplot(data.frame(ParticPartisanVariableNames, ParticPartisanFactorLoadings),
       aes(x = reorder(ParticPartisanVariableNames,
        desc(ParticPartisanVariableNames)),
           y = ParticPartisanFactorLoadings)) +
  coord_flip() +
  geom_bar(stat = "identity", colour = "black", fill = "black", linewidth = 1,
           width = 0.4) +
  geom_text(aes(label = as.character(round(
    ParticPartisanFactorLoadings, digits = 2))), vjust = 0.35, hjust = -0.3,
    family = "CM Roman", size = 6) +
  geom_hline(yintercept = 0.3, colour = "gray", linetype = "longdash") +
  annotate("text", label = paste("Cronbach's alpha =", as.character(
    ParticPartisanCronbach)), x = 4, y = 0.825, size = 6,
    family = "CM Roman") +
  annotate("text", label = paste("First eigenvalue =", as.character(
    ParticPartisanFirstEigenvalue)), x = 2, y = 0.825, size = 6,
    family = "CM Roman") +
  scale_y_continuous(name = "Factor loadings", limits = c(-0.1, 1),
                     breaks = seq(-0.1, 1, by = 0.1)) +
  xlab("") +
  theme_linedraw() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 18.5),
        axis.title.x = element_text(hjust = 0.3, vjust = -0.17),
        panel.grid = element_blank(),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/ParticPartisanScale.pdf", width = 11, height = 4.25)
particPartisanVariables <- c(
  "attend_meeting_speech", "politician_social_media", "volunteer_partisan",
  "contact_politician", "donation_partisan")
CES21$particPartisan <- apply(CES21[particPartisanVariables], 1, delete_rows_na,
                         loadings = ParticPartisanFactorLoadings,
                         number_na_allowed = 2)
length(na.omit(CES21$particPartisan)) / nrow(CES21) * 100 # 71% available data

ParticNonPartisanScale <- na.omit(CES21[, c(
  "volunteer_community", "attend_protest", "boycott", "petition",
  "donation_cause", "group_partic", "comment_politics",
  "talk_issue_social_media")])
ParticNonPartisanCronbach <- round(
  as.numeric(psy::cronbach(ParticNonPartisanScale)[3]), digits = 2)
ParticNonPartisanFactorAnalysis <- factanal(ParticNonPartisanScale,
 factors = 1)
ParticNonPartisanVariableNames <- c(
  "Volunteer for group/organization", "Attend protest", "Boycott",
  "Sign petition", "Donation to cause", "Group active membership",
  "Comment political content", "Discuss politics social media")
ParticNonPartisanFactorLoadings <-
 ParticNonPartisanFactorAnalysis$loadings[, 1]
ParticNonPartisanFirstEigenvalue <- round(
  eigen(cor(ParticNonPartisanScale))$values[1], digits = 2)
ggplot(data.frame(ParticNonPartisanVariableNames,
 ParticNonPartisanFactorLoadings),
       aes(x = reorder(ParticNonPartisanVariableNames,
        desc(ParticNonPartisanVariableNames)),
           y = ParticNonPartisanFactorLoadings)) +
  coord_flip() +
  geom_bar(stat = "identity", colour = "black", fill = "black", linewidth = 1,
           width = 0.4) +
  geom_text(aes(label = as.character(round(
    ParticNonPartisanFactorLoadings, digits = 2))), vjust = 0.35, hjust = -0.3,
    family = "CM Roman", size = 6) +
  geom_hline(yintercept = 0.3, colour = "gray", linetype = "longdash") +
  annotate("text", label = paste("Cronbach's alpha =", as.character(
    ParticNonPartisanCronbach)), x = 4, y = 0.825, size = 6,
    family = "CM Roman") +
  annotate("text", label = paste("First eigenvalue =", as.character(
    ParticNonPartisanFirstEigenvalue)), x = 2, y = 0.825, size = 6,
    family = "CM Roman") +
  scale_y_continuous(name = "Factor loadings", limits = c(-0.1, 1),
                     breaks = seq(-0.1, 1, by = 0.1)) +
  xlab("") +
  theme_linedraw() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 18.5),
        axis.title.x = element_text(hjust = 0.3, vjust = -0.17),
        panel.grid = element_blank(),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/ParticNonPartisanScale.pdf", width = 11, height = 4.25)
particNonPartisanVariables <- c(
  "volunteer_community", "attend_protest", "boycott", "petition",
  "donation_cause", "group_partic", "comment_politics",
  "talk_issue_social_media")
CES21$particNonPartisan <- apply(
  CES21[particNonPartisanVariables], 1, delete_rows_na,
  loadings = ParticNonPartisanFactorLoadings, number_na_allowed = 5)
length(na.omit(CES21$particNonPartisan)) / nrow(CES21) * 100 # 71% available data

CES21men <- filter(CES21, female == 0)
CES21women <- filter(CES21, female == 1)
CES21white <- filter(CES21, ethn == "White")
CES21black <- filter(CES21, ethn == "Black")
CES21westasian <- filter(CES21, ethn == "West Asian")
CES21southeastasian <- filter(CES21, ethn == "Southeast Asian")
CES21arabic <- filter(CES21, ethn == "Arabic")
CES21southasian <- filter(CES21, ethn == "South Asian")
CES21hispanic <- filter(CES21, ethn == "Hispanic")
CES21indigenous <- filter(CES21, ethn == "Indigenous")
CES21other <- filter(CES21, ethn == "Other")
CES21whitemen <- filter(CES21, female == 0 & ethn == "White")
CES21whitewomen <- filter(CES21, female == 1 & ethn == "White")
CES21nonwhitemen <- filter(CES21, female == 0 & ethn != "White")
CES21nonwhitewomen <- filter(CES21, female == 1 & ethn != "White")
CES21immigrantmen <- filter(CES21, female == 0 & immig == 2)
CES21immigrantwomen <- filter(CES21, female == 1 & immig == 2)
CES21nonimmigrantmen <- filter(CES21, female == 0 & immig == 1)
CES21nonimmigrantwomen <- filter(CES21, female == 1 & immig == 1)
prop.table(table(CES21$female))
prop.table(table(CES21$lang))
prop.table(table(CES21$education))
prop.table(table(CES21$immig))
prop.table(table(CES21$ethn))
cumsum(prop.table(table(CES21$income)))
cumsum(prop.table(table(CES21$age)))
prop.table(table(CES21$province))
summary(CES21$weight)
CES97clean <- dplyr::select(CES97, interest, female, year, age, mode, weight)
CES00clean <- dplyr::select(CES00, interest, female, year, age, mode, weight)
CES04clean <- dplyr::select(CES04, interest, female, year, age, mode, weight)
CES06clean <- dplyr::select(CES06, interest, female, year, age, mode, weight)
CES08clean <- dplyr::select(CES08, interest, female, year, age, mode, weight)
CES11clean <- dplyr::select(CES11, interest, female, year, age, mode, weight)
CES15clean <- dplyr::select(CES15, interest, female, year, age, mode, weight)
CES19clean <- dplyr::select(CES19, interest, female, year, age, mode, weight)
CES21clean <- dplyr::select(CES21, interest, female, year, age, mode, weight)
CES <- bind_rows(CES97clean, CES00clean, CES04clean, CES06clean, CES08clean,
                 CES11clean, CES15clean, CES19clean, CES21clean)

#### 1.5 WVS ####
WVS <- readRDS("_data/WVS/WVS_TimeSeries_1981_2022_Rds_v3_0.rds")
table(WVS$COUNTRY_ALPHA)
Countries <- read.csv("_data/countries_codes_and_coordinates.csv")
 # https://gist.github.com/cpl/3dc2d19137588d9ae202d67233715478
Countries$Alpha.3.code <- str_remove_all(Countries$Alpha.3.code, "\\s+")
table(Countries$Alpha.3.code)
WVS <- left_join(WVS, Countries, join_by(COUNTRY_ALPHA == Alpha.3.code))
WVS$country <- WVS$Country
WVS$ethn <- NA
WVS$ethn[WVS$X051 == 124001] <- "White"
WVS$ethn[WVS$X051 == 124002] <- "Black"
WVS$ethn[WVS$X051 == 124003] <- "West Asian"
WVS$ethn[WVS$X051 == 124004] <- "Southeast Asian"
WVS$ethn[WVS$X051 == 124005] <- "Arabic"
WVS$ethn[WVS$X051 == 124006] <- "South Asian"
WVS$ethn[WVS$X051 == 124007] <- "Hispanic"
WVS$ethn[WVS$X051 == 124008] <- "Indigenous"
WVS$ethn[WVS$X051 == 124009] <- "Chinese"
WVS$ethn[WVS$X051 == 124010] <- "Filipino"
WVS$ethn[WVS$X051 == 124011] <- "Korean"
WVS$ethn[WVS$X051 == 124012] <- "Japanese"
WVS$ethn[WVS$X051 == 124999] <- "Other"
WVS$ethn <- as.factor(WVS$ethn)
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
WVS$education[WVS$education == 0] <- "No schooling"
WVS$education[WVS$education == 1] <- "Elementary school"
WVS$education[WVS$education %in% c(2, 3)] <- "High school"
WVS$education[WVS$education %in% c(4, 5)] <- "Technical, college\nor CEGEP"
WVS$education[WVS$education == 6] <- "Bachelor's degree"
WVS$education[WVS$education == 7] <- "Master's degree"
WVS$education[WVS$education == 8] <- "Doctorate"
WVS$education <- factor(WVS$education, levels = c(
  "No schooling", "Elementary school", "High school",
  "Technical, college\nor CEGEP", "Bachelor's degree", "Master's degree",
  "Doctorate"))
WVS$province <- NA
WVS$province[as.numeric(WVS$X048ISO) == 124001] <- "Alberta"
WVS$province[as.numeric(WVS$X048ISO) == 124002] <- "British Columbia"
WVS$province[as.numeric(WVS$X048ISO) == 124003] <- "Manitoba"
WVS$province[as.numeric(WVS$X048ISO) == 124004] <- "New Brunswick"
WVS$province[as.numeric(WVS$X048ISO) == 124005] <- "N&L"
WVS$province[as.numeric(WVS$X048ISO) == 124006] <- "Nova Scotia"
WVS$province[as.numeric(WVS$X048ISO) == 124007] <- "Ontario"
WVS$province[as.numeric(WVS$X048ISO) == 124008] <- "PEI"
WVS$province[as.numeric(WVS$X048ISO) == 124009] <- "Quebec"
WVS$province[as.numeric(WVS$X048ISO) == 124010] <- "Saskatchewan"
WVS$province[as.numeric(WVS$X048ISO) == 124011] <- "NWT"
WVS$province[as.numeric(WVS$X048ISO) == 124012] <- "Nunavut"
WVS$province[as.numeric(WVS$X048ISO) == 124013] <- "Yukon"
WVS$province <- as.factor(WVS$province)
WVS$weight <- as.numeric(WVS$S017)
WVSWave7 <- filter(WVS, S020 %in% seq(2017, 2022))
length(table(WVSWave7$COUNTRY_ALPHA))
WVSWave7$canada <- 0
WVSWave7$canada[WVSWave7$COUNTRY_ALPHA == "CAN"] <- 1
WVSCA <- filter(WVS, COUNTRY_ALPHA == "CAN")
WVSCA90 <- filter(WVSCA, S020 == 1990) # no ethn variable
WVSCA90men <- filter(WVSCA90, female == 0)
WVSCA90women <- filter(WVSCA90, female == 1)
WVSCA00 <- filter(WVSCA, S020 == 2000)
WVSCA00men <- filter(WVSCA00, female == 0)
WVSCA00women <- filter(WVSCA00, female == 1)
WVSCA00whitemen <- filter(WVSCA00, female == 0 & ethn == "White")
WVSCA00whitewomen <- filter(WVSCA00, female == 1 &
                              ethn == "White")
WVSCA00nonwhitemen <- filter(WVSCA00, female == 0 &
                               ethn != "White")
WVSCA00nonwhitewomen <- filter(WVSCA00, female == 1 &
                                 ethn != "White")
WVSCA06 <- filter(WVSCA, S020 == 2006)
WVSCA06men <- filter(WVSCA06, female == 0)
WVSCA06women <- filter(WVSCA06, female == 1)
WVSCA06whitemen <- filter(WVSCA06, female == 0 & ethn == "White")
WVSCA06whitewomen <- filter(WVSCA06, female == 1 &
                              ethn == "White")
WVSCA06nonwhitemen <- filter(WVSCA06, female == 0 &
                               ethn != "White")
WVSCA06nonwhitewomen <- filter(WVSCA06, female == 1 &
                                 ethn != "White")
WVSCA20 <- filter(WVSCA, S020 == 2020)
WVSCA20men <- filter(WVSCA20, female == 0)
WVSCA20women <- filter(WVSCA20, female == 1)
WVSCA20white <- filter(WVSCA20, ethn == "White")
WVSCA20black <- filter(WVSCA20, ethn == "Black")
WVSCA20westasian <- filter(WVSCA20, ethn == "West Asian")
WVSCA20southeastasian <- filter(WVSCA20, ethn == "Southeast Asian")
WVSCA20arabic <- filter(WVSCA20, ethn == "Arabic")
WVSCA20southasian <- filter(WVSCA20, ethn == "South Asian")
WVSCA20hispanic <- filter(WVSCA20, ethn == "Hispanic")
WVSCA20indigenous <- filter(WVSCA20, ethn == "Indigenous")
WVSCA20chinese <- filter(WVSCA20, ethn == "Chinese")
WVSCA20filipino <- filter(WVSCA20, ethn == "Filipino")
WVSCA20korean <- filter(WVSCA20, ethn == "Korean")
WVSCA20japanese <- filter(WVSCA20, ethn == "Japanese")
WVSCA20other <- filter(WVSCA20, ethn == "Other")
WVSCA20whitemen <- filter(WVSCA20, female == 0 & ethn == "White")
WVSCA20whitewomen <- filter(WVSCA20, female == 1 &
                              ethn == "White")
WVSCA20nonwhitemen <- filter(WVSCA20, female == 0 &
                               ethn != "White")
WVSCA20nonwhitewomen <- filter(WVSCA20, female == 1 &
                                 ethn != "White")
WVSCA20immigrantmen <- filter(WVSCA20, female == 0 & immig == 2)
WVSCA20immigrantwomen <- filter(WVSCA20, female == 1 & immig == 2)
WVSCA20nonimmigrantmen <- filter(WVSCA20, female == 0 & immig == 1)
WVSCA20nonimmigrantwomen <- filter(WVSCA20, female == 1 & immig == 1)
prop.table(table(WVSCA20$female))
prop.table(table(WVSCA20$lang))
prop.table(table(WVSCA20$education))
prop.table(table(WVSCA20$immig))
prop.table(table(WVSCA20$ethn))
cumsum(prop.table(table(WVSCA20$age)))
prop.table(table(WVSCA20$province))
summary(WVSCA20$weight)

#### 1.6 GSS ####
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
  GSS13$education ==
    "Less than high school diploma or its equivalent"] <- "No schooling"
GSS13$education[
  GSS13$education ==
    "High school diploma or a high school equivalency certificate"] <-
  "High school"
GSS13$education[
  GSS13$education ==
    "College/CEGEP/other non-university certificate or diploma"] <-
  "Technical, college\nor CEGEP"
GSS13$education[
  GSS13$education ==
    "University certificate or diploma below the bachelor's level"] <-
  "Technical, college\nor CEGEP"
GSS13$education[
  GSS13$education == "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)"] <-
  "Bachelor's degree"
GSS13$education[
  GSS13$education ==
    "University certificate, diploma, degree above the BA level"] <-
  "Master's degree"
GSS13$education <- factor(GSS13$education, levels = c(
  "No schooling", "High school",
  "Technical, college\nor CEGEP", "Bachelor's degree", "Master's degree"))
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
GSS20$education[GSS20$education == 1] <- "No schooling"
GSS20$education[GSS20$education == 2] <- "High school"
GSS20$education[GSS20$education %in% c(3, 4)] <- "Technical, college\nor CEGEP"
GSS20$education[GSS20$education == 5] <- "Technical, college\nor CEGEP"
GSS20$education[GSS20$education == 6] <- "Bachelor's degree"
GSS20$education[GSS20$education == 7] <- "Master's degree"
GSS20$education[GSS20$education == 99] <- NA
GSS20$education <- factor(GSS20$education, levels = c(
  "No schooling", "High school",
  "Technical, college\nor CEGEP", "Bachelor's degree", "Master's degree"))
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
GSS20$income[GSS20$income == 1] <- "$24,999-"
GSS20$income[GSS20$income == 2] <- "$25,000-49,999"
GSS20$income[GSS20$income == 3] <- "$50,000-74,999"
GSS20$income[GSS20$income == 4] <- "$75,000-99,999"
GSS20$income[GSS20$income == 5] <- "$100,000+"
GSS20$income <- factor(GSS20$income, levels = c(
  "$24,999-", "$25,000-49,999", "$50,000-74,999",
  "$75,000-99,999", "$100,000+"))
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
GSS20$province[GSS20$province == 10] <- "N&L"
GSS20$province[GSS20$province == 11] <- "PEI"
GSS20$province[GSS20$province == 12] <- "Nova Scotia"
GSS20$province[GSS20$province == 13] <- "New Brunswick"
GSS20$province[GSS20$province == 24] <- "Quebec"
GSS20$province[GSS20$province == 35] <- "Ontario"
GSS20$province[GSS20$province == 46] <- "Manitoba"
GSS20$province[GSS20$province == 47] <- "Saskatchewan"
GSS20$province[GSS20$province == 48] <- "Alberta"
GSS20$province[GSS20$province == 59] <- "British Columbia"
GSS20$ethn <- NA
GSS20$ethn[GSS20$VISMIN_C == 3] <- "Black"
GSS20$ethn[GSS20$VISMIN_C == 8] <- "West Asian"
GSS20$ethn[GSS20$VISMIN_C == 7] <- "Southeast Asian"
GSS20$ethn[GSS20$VISMIN_C == 5] <- "Arabic"
GSS20$ethn[GSS20$VISMIN_C == 1] <- "South Asian"
GSS20$ethn[GSS20$VISMIN_C == 6] <- "Hispanic"
GSS20$ethn[GSS20$VISMIN_C == 2] <- "Chinese"
GSS20$ethn[GSS20$VISMIN_C == 4] <- "Filipino"
GSS20$ethn[GSS20$VISMIN_C == 9] <- "Other"
GSS20$ethn[GSS20$VISMIN_C == 10] <- "White"
GSS20$ethn[GSS20$ABM_01A == 2] <- "Indigenous"
GSS20$ethn <- as.factor(GSS20$ethn)
GSS20$weight <- GSS20$WGHT_PER / (sum(GSS20$WGHT_PER) / nrow(GSS20))
 # 10.0000 - 32631.0308 before adjustment
prop.table(table(GSS20$female))
prop.table(table(GSS20$lang))
prop.table(table(GSS20$education))
prop.table(table(GSS20$immig))
cumsum(prop.table(table(GSS20$income)))
cumsum(prop.table(table(GSS20$age)))
prop.table(table(GSS20$province))
prop.table(table(GSS20$ethn))
summary(GSS20$weight)
GSS13men <- filter(GSS13, female == 0)
GSS13women <- filter(GSS13, female == 1)
GSS20men <- filter(GSS20, female == 0)
GSS20women <- filter(GSS20, female == 1)
GSS13immigrantmen <- filter(GSS13, female == 0 & immig == 1)
GSS13immigrantwomen <- filter(GSS13, female == 1 & immig == 1)
GSS13nonimmigrantmen <- filter(GSS13, female == 0 & immig == 0)
GSS13nonimmigrantwomen <- filter(GSS13, female == 1 & immig == 0)
GSS20whitemen <- filter(GSS20, female == 0 & ethn == "White")
GSS20whitewomen <- filter(GSS20, female == 1 & ethn == "White")
GSS20nonwhitemen <- filter(GSS20, female == 0 & ethn != "White")
GSS20nonwhitewomen <- filter(GSS20, female == 1 & ethn != "White")
GSS20immigrantmen <- filter(GSS20, female == 0 & immig == 1)
GSS20immigrantwomen <- filter(GSS20, female == 1 & immig == 1)
GSS20nonimmigrantmen <- filter(GSS20, female == 0 & immig == 0)
GSS20nonimmigrantwomen <- filter(GSS20, female == 1 & immig == 0)

### 2. Descriptive statistics graphs ####
#### 2.1 CPIS ####
PlotGender <- CPIS |>
  filter(!is.na(female_alt)) |>
  ggplot(aes(x = female_alt)) +
  geom_bar() +
  labs(x = "Gender", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotAge <- ggplot(CPIS, aes(x = age)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Age", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotEthnicity <- CPIS |>
  filter(!is.na(ethn)) |>
  ggplot(aes(x = ethn)) +
  geom_bar() +
  labs(x = "Ethnicity", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotLanguage <- CPIS |>
  filter(!is.na(lang)) |>
  ggplot(aes(x = lang)) +
  geom_bar() +
  labs(x = "Language spoken at home", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotImmigrant <- CPIS |>
  filter(!is.na(immig)) |>
  ggplot(aes(x = as.factor(immig))) +
  geom_bar() +
  labs(x = "Born in Canada?", y = "Frequency") +
  theme_minimal() +
  scale_x_discrete(labels = c("Yes", "No")) +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotAgentic <- ggplot(CPIS, aes(x = agentic)) +
  geom_histogram(breaks = seq(0, 1, 0.1)) +
  labs(x = "Agency scale score", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotCommunal <- ggplot(CPIS, aes(x = communal)) +
  geom_histogram(breaks = seq(0, 1, 0.1)) +
  labs(x = "Communality scale score", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotFamSituation <- CPIS |>
  ggplot(aes(x = as.factor(fam_situation_alt))) +
  geom_bar() +
  labs(x = "Family situation", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
ggsave(plot = ggpubr::ggarrange(
  PlotGender, PlotAge, PlotEthnicity, PlotLanguage, PlotImmigrant, PlotFamSituation, nrow = 2, ncol = 3),
  "_graphs/CPISDescriptive.pdf", width = 11, height = 8.5)

filter(CPIS, !is.na(teacher_gender_alt) & !is.na(female)) |>
  ggplot(aes(x = as.factor(teacher_gender_alt))) +
  geom_bar() +
  facet_grid(~female_alt2) +
  labs(x = "Gender of liked teacher", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
filter(CPIS, !is.na(influencer_gender_alt) & !is.na(female)) |>
  ggplot(aes(x = as.factor(influencer_gender_alt))) +
  geom_bar() +
  facet_grid(~female_alt2) +
  labs(x = "Gender of follower influencer", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))

PlotInterest <- ggplot(CPIS, aes(x = interest)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 120)) +
  labs(x = "General\npolitical interest", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotHealth <- ggplot(CPIS, aes(x = interest_health)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 120)) +
  labs(x = "Interest in\nhealth care", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotForeign <- ggplot(CPIS, aes(x = interest_foreign)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 120)) +
  labs(x = "Interest in\ninternational relations", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotLaw <- ggplot(CPIS, aes(x = interest_law)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 120)) +
  labs(x = "Interest in\nlaw and crime", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotEducation <- ggplot(CPIS, aes(x = interest_education)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 120)) +
  labs(x = "Interest in\neducation", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotPartisan <- ggplot(CPIS, aes(x = interest_partisan)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 120)) +
  labs(x = "Interest in\npartisan politics", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
ggsave(plot = ggpubr::ggarrange(
  PlotInterest, PlotHealth, PlotForeign, PlotLaw, PlotEducation, PlotPartisan,
  nrow = 2, ncol = 3), width = 11, height = 8.5,
  "_graphs/CPISInterest.pdf")
sd(CPIS$interest, na.rm = T)
sd(CPIS$interest_health, na.rm = T)
sd(CPIS$interest_foreign, na.rm = T)
sd(CPIS$interest_law, na.rm = T)
sd(CPIS$interest_education, na.rm = T)
sd(CPIS$interest_partisan, na.rm = T)

PoliticalGraphData <- pivot_longer(
  CPISOldYoung, cols = lockdown_political_alt:parties_political_alt,
  names_to = "name", values_to = "value") |>
  filter(!is.na(value)) |>
  group_by(name, agegrp) |>
  summarise(value = mean(value, na.rm = T),
            n = n())
PoliticalGraphData$name_full <- case_when(
  PoliticalGraphData$name == "lockdown_political_alt" ~ "Pandemic restrictions",
  PoliticalGraphData$name == "nurses_political_alt" ~
    "Working conditions of nurses",
  PoliticalGraphData$name == "china_political_alt" ~
    "Diplomatic disputes between Canada and China",
  PoliticalGraphData$name == "ukraine_political_alt" ~ "Ukrainian war",
  PoliticalGraphData$name == "police_political_alt" ~ "Police funding",
  PoliticalGraphData$name == "crime_political_alt" ~ "Sentences for violent crimes",
  PoliticalGraphData$name == "tuition_political_alt" ~ "University tuition",
  PoliticalGraphData$name == "privateschool_political_alt" ~
    "Funding of public and private schools",
  PoliticalGraphData$name == "elections_political_alt" ~ "Federal elections",
  PoliticalGraphData$name == "parties_political_alt" ~ "Political parties")
value.se <- sd(PoliticalGraphData$value) / sqrt(PoliticalGraphData$n)
PoliticalGraphData$value.lb <-
  PoliticalGraphData$value + qnorm(0.025) * value.se
PoliticalGraphData$value.ub <-
  PoliticalGraphData$value + qnorm(0.975) * value.se
ggplot(PoliticalGraphData, aes(x = value, y = reorder(name_full, value))) +
  geom_point() +
  facet_wrap(~agegrp) +
  geom_errorbar(aes(xmin = value.lb, xmax = value.ub), width = 0.5) +
  geom_vline(xintercept = 0.5) +
  scale_x_continuous("Average view\nof students", limits = c(0, 1),
                     breaks = c(0.1, 0.9),
                     labels = c("Non-political", "Political")) +
  scale_y_discrete("Issue") +
  theme_minimal() +
  theme(strip.text.x = element_text(size = 17.5),
        axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/CPISPolitical.pdf", width = 11, height = 4.25)

#### 2.2 Datagotchi PES, CES, WVS and GSS ####
PlotAgeDG <- ggplot(DG, aes(x = age)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Age", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotGenderDG <- DG |>
  filter(!is.na(female_alt)) |>
  ggplot(aes(x = female_alt)) +
  geom_bar() +
  labs(x = "Gender", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotLanguageDG <- DG |>
  filter(!is.na(lang)) |>
  ggplot(aes(x = lang)) +
  geom_bar() +
  labs(x = "Language of the survey", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotEthnicityDG <- DG |>
  filter(!is.na(ethn)) |>
  ggplot(aes(x = ethn)) +
  geom_bar() +
  labs(x = "Ethnicity", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotImmigrantDG <- DG |>
  filter(!is.na(immig)) |>
  ggplot(aes(x = as.factor(immig))) +
  geom_bar() +
  scale_x_discrete(labels = c("Yes", "No")) +
  labs(x = "Born in Canada?", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotIncomeDG <- DG |>
  filter(!is.na(income)) |>
  ggplot(aes(x = as.factor(income))) +
  geom_bar() +
  labs(x = "Household yearly income", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotEducationDG <- DG |>
  filter(!is.na(education)) |>
  ggplot(aes(x = as.factor(education))) +
  geom_bar() +
  labs(x = "Level of education", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
ggsave(plot = ggpubr::ggarrange(
  PlotGenderDG, PlotAgeDG, PlotEthnicityDG, PlotLanguageDG, PlotImmigrantDG,
  PlotIncomeDG, PlotEducationDG, nrow = 3, ncol = 3),
  "_graphs/DGDescriptive.pdf", width = 11, height = 12.75)
WVSWave7 |>
  group_by(country) |>
  reframe(
    interest_male = weighted.mean(
      interest[as.numeric(female) == 1], na.rm = T, w = weight[
        as.numeric(female) == 1]),
    interest_female = weighted.mean(
      interest[as.numeric(female) == 2], na.rm = T, w = weight[
        as.numeric(female) == 2])) |>
  ggplot(aes(x = interest_male - interest_female, y = reorder(country, interest_male - interest_female))) +
  geom_bar(stat = "identity") +
  scale_x_continuous(paste("Gender gap in general political interest\nLeft:",
                           "women more interested; right: men more interested")) +
  scale_y_discrete("Country") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/InterestGapByCountry.pdf", width = 11, height = 12.75)

PlotAgeCES <- CES21 |>
  filter(!is.na(age)) |>
  ggplot(aes(x = age)) +
  geom_bar() +
  labs(x = "Age", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotGenderCES <- CES21 |>
  filter(!is.na(female_alt)) |>
  ggplot(aes(x = female_alt)) +
  geom_bar() +
  labs(x = "Gender", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotLanguageCES <- CES21 |>
  filter(!is.na(lang)) |>
  ggplot(aes(x = lang)) +
  geom_bar() +
  labs(x = "Language spoken at home", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotImmigrantCES <- CES21 |>
  filter(!is.na(immig)) |>
  ggplot(aes(x = as.factor(immig))) +
  geom_bar() +
  scale_x_discrete(labels = c("Yes", "No")) +
  labs(x = "Born in Canada?", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotIncomeCES <- CES21 |>
  filter(!is.na(income)) |>
  ggplot(aes(x = as.factor(income))) +
  geom_bar() +
  labs(x = "Household yearly income", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotEducationCES <- CES21 |>
  filter(!is.na(education)) |>
  ggplot(aes(x = as.factor(education))) +
  geom_bar() +
  labs(x = "Level of education", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotProvinceCES <- CES21 |>
  filter(!is.na(province)) |>
  ggplot(aes(x = as.factor(province))) +
  geom_bar() +
  labs(x = "Province of residence", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotEthnicityCES <- CES21 |>
  filter(!is.na(ethn)) |>
  ggplot(aes(x = as.factor(ethn))) +
  geom_bar() +
  labs(x = "Ethnicity", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
ggsave(plot = ggpubr::ggarrange(
  PlotGenderCES, PlotAgeCES, PlotLanguageCES, PlotImmigrantCES,
  PlotIncomeCES, PlotEducationCES, PlotEthnicityCES, PlotProvinceCES,
  nrow = 3, ncol = 3),
  "_graphs/CESDescriptive.pdf", width = 11, height = 12.75)

PlotAgeWVS <- WVSCA20 |>
  filter(!is.na(age)) |>
  ggplot(aes(x = age)) +
  geom_bar() +
  labs(x = "Age", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotGenderWVS <- WVSCA20 |>
  filter(!is.na(female_alt)) |>
  ggplot(aes(x = female_alt)) +
  geom_bar() +
  labs(x = "Gender", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotLanguageWVS <- WVSCA20 |>
  filter(!is.na(lang)) |>
  ggplot(aes(x = lang)) +
  geom_bar() +
  labs(x = "Language spoken at home", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotImmigrantWVS <- WVSCA20 |>
  filter(!is.na(immig)) |>
  ggplot(aes(x = as.factor(immig))) +
  geom_bar() +
  scale_x_discrete(labels = c("Yes", "No")) +
  labs(x = "Born in Canada?", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotEducationWVS <- WVSCA20 |>
  filter(!is.na(education)) |>
  ggplot(aes(x = as.factor(education))) +
  geom_bar() +
  labs(x = "Level of education", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotProvinceWVS <- WVSCA20 |>
  filter(!is.na(province)) |>
  ggplot(aes(x = as.factor(province))) +
  geom_bar() +
  labs(x = "Province of residence", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotEthnicityWVS <- WVSCA20 |>
  filter(!is.na(ethn)) |>
  ggplot(aes(x = as.factor(ethn))) +
  geom_bar() +
  labs(x = "Ethnicity", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
ggsave(plot = ggpubr::ggarrange(
  PlotGenderWVS, PlotAgeWVS, PlotLanguageWVS, PlotImmigrantWVS,
  PlotEducationWVS, PlotEthnicityWVS, PlotProvinceWVS, nrow = 3, ncol = 3),
  "_graphs/WVSDescriptive.pdf", width = 11, height = 12.75)

PlotAgeGSS <- GSS20 |>
  filter(!is.na(age)) |>
  ggplot(aes(x = age)) +
  geom_bar() +
  labs(x = "Age", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotGenderGSS <- GSS20 |>
  filter(!is.na(female_alt)) |>
  ggplot(aes(x = female_alt)) +
  geom_bar() +
  labs(x = "Gender", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotLanguageGSS <- GSS20 |>
  filter(!is.na(lang)) |>
  ggplot(aes(x = lang)) +
  geom_bar() +
  labs(x = "Language spoken at home", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotImmigrantGSS <- GSS20 |>
  filter(!is.na(immig)) |>
  ggplot(aes(x = as.factor(immig))) +
  geom_bar() +
  scale_x_discrete(labels = c("Yes", "No")) +
  labs(x = "Born in Canada?", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotIncomeGSS <- GSS20 |>
  filter(!is.na(income)) |>
  ggplot(aes(x = as.factor(income))) +
  geom_bar() +
  labs(x = "Household yearly income", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotEducationGSS <- GSS20 |>
  filter(!is.na(education)) |>
  ggplot(aes(x = as.factor(education))) +
  geom_bar() +
  labs(x = "Level of education", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotProvinceGSS <- GSS20 |>
  filter(!is.na(province)) |>
  ggplot(aes(x = as.factor(province))) +
  geom_bar() +
  labs(x = "Province of residence", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
PlotEthnicityGSS <- GSS20 |>
  filter(!is.na(ethn)) |>
  ggplot(aes(x = as.factor(ethn))) +
  geom_bar() +
  labs(x = "Ethnicity", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "CM Roman"))
ggsave(plot = ggpubr::ggarrange(
  PlotGenderGSS, PlotAgeGSS, PlotLanguageGSS, PlotImmigrantGSS,
  PlotIncomeGSS, PlotEducationGSS, PlotEthnicityGSS, PlotProvinceGSS,
  nrow = 3, ncol = 3),
  "_graphs/GSSDescriptive.pdf", width = 11, height = 12.75)

PlotInterestDG <- ggplot(DG, aes(x = interest, weight = weight)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 450)) +
  labs(x = "General\npolitical interest", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotHealthDG <- ggplot(DG, aes(x = interest_health, weight = weight)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 450)) +
  labs(x = "Interest in\nhealth care", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotForeignDG <- ggplot(DG, aes(x = interest_foreign, weight = weight)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 450)) +
  labs(x = "Interest in\ninternational affairs", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotLawDG <- ggplot(DG, aes(x = interest_law, weight = weight)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 450)) +
  labs(x = "Interest in\nlaw and crime", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotEducationDG <- ggplot(DG, aes(x = interest_education,
                                  weight = weight)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 450)) +
  labs(x = "Interest in\neducation", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotPartisanDG <- ggplot(DG, aes(x = interest_partisan,
                                 weight = weight)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(limits = c(0, 450)) +
  labs(x = "Interest in\npartisan politics", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
ggsave(plot = ggpubr::ggarrange(
  PlotInterestDG, PlotHealthDG, PlotForeignDG, PlotLawDG, PlotEducationDG,
  PlotPartisanDG, nrow = 2, ncol = 3), width = 11, height = 8.5,
  "_graphs/DGInterest.pdf")
sd(DG$interest, na.rm = T)
sd(DG$interest_health, na.rm = T)
sd(DG$interest_foreign, na.rm = T)
sd(DG$interest_law, na.rm = T)
sd(DG$interest_education, na.rm = T)
sd(DG$interest_partisan, na.rm = T)

PlotInterestCES <- ggplot(CES21, aes(x = interest / 10, weight = weight)) +
  geom_histogram(binwidth = 1) +
  labs(x = "General political interest -\n2021 CES", y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotInterestWVS <- ggplot(WVSCA20, aes(x = interest / 10, weight = weight)) +
  geom_histogram(binwidth = (10/3)) +
  scale_x_continuous(breaks = seq(0, 10, by = 2.5)) +
  labs(x = "General political interest -\n2020 WVS - Canada",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotInterestGSS <- ggplot(GSS20, aes(x = interest / 10, weight = weight)) +
  geom_histogram(binwidth = (10/3)) +
  scale_x_continuous(breaks = seq(0, 10, by = 2.5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "General political interest -\n2020 GSS - Canada",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
ggsave(plot = ggpubr::ggarrange(
  PlotInterestCES, PlotInterestWVS, PlotInterestGSS,
  nrow = 2, ncol = 2), width = 11, height = 8.5,
  "_graphs/CESWVSGSSInterest.pdf")
mean(CPIS$interest, na.rm=T)
mean(CPIS$interest_law, na.rm=T)
mean(CPIS$interest_education, na.rm=T)
mean(CPIS$interest_foreign, na.rm=T)
mean(CPIS$interest_partisan, na.rm=T)
mean(CPIS$interest_health, na.rm=T)
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

### 3. Multivariate analysis graphs ####
#### 3.1 Political interest by age & gender (all) ####
CPISGrouped <- CPIS |>
  group_by(age, female) |>
  summarise(interest = mean(interest, na.rm = TRUE))
CPISGroupedCategory <- CPIS |>
  group_by(age, female) |>
  summarise(interest_health = mean(interest_health, na.rm = TRUE),
            interest_foreign = mean(interest_foreign, na.rm = TRUE),
            interest_law = mean(interest_law, na.rm = TRUE),
            interest_education = mean(interest_education, na.rm = TRUE),
            interest_partisan = mean(interest_partisan, na.rm = TRUE))

Plot1 <- ggplot(filter(CPIS, !is.na(female)),
       aes(x = age, y = interest, color = female)) +
  geom_point(data = filter(CPISGrouped, !is.na(female)), size = 0.25,
             aes(x = age, y = interest, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age") +
  scale_color_grey(name = "", end = 0.5, labels = c("Boys", "Girls")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        text = element_text(family = "CM Roman")) +
  ggtitle("General")
Plot2 <- ggplot(filter(CPIS, !is.na(female)),
       aes(x = age, y = interest_health, color = female)) +
  geom_point(data = filter(CPISGroupedCategory, !is.na(female)), size = 0.25,
             aes(x = age, y = interest_health, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age") +
  scale_color_grey(name = "", end = 0.5, labels = c("Boys", "Girls")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        legend.position = "none",
        text = element_text(family = "CM Roman")) +
  ggtitle("Health care")
Plot3 <- ggplot(filter(CPIS, !is.na(female)),
                aes(x = age, y = interest_foreign, color = female)) +
  geom_point(data = filter(CPISGroupedCategory, !is.na(female)), size = 0.25,
             aes(x = age, y = interest_foreign, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age") +
  scale_color_grey(name = "", end = 0.5, labels = c("Boys", "Girls")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        legend.position = "none",
        text = element_text(family = "CM Roman")) +
  ggtitle("International affairs")
Plot4 <- ggplot(filter(CPIS, !is.na(female)),
                aes(x = age, y = interest_law, color = female)) +
  geom_point(data = filter(CPISGroupedCategory, !is.na(female)), size = 0.25,
             aes(x = age, y = interest_law, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age") +
  scale_color_grey(name = "", end = 0.5, labels = c("Boys", "Girls")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        legend.position = "none",
        text = element_text(family = "CM Roman")) +
  ggtitle("Law and crime")
Plot5 <- ggplot(filter(CPIS, !is.na(female)),
                aes(x = age, y = interest_education, color = female)) +
  geom_point(data = filter(CPISGroupedCategory, !is.na(female)),
             size = 0.25, aes(x = age, y = interest_education, color = female,
                              weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age") +
  scale_color_grey(name = "", end = 0.5, labels = c("Boys", "Girls")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        legend.position = "none",
        text = element_text(family = "CM Roman")) +
  ggtitle("Education")
Plot6 <- ggplot(filter(CPIS, !is.na(female)),
                aes(x = age, y = interest_partisan, color = female)) +
  geom_point(data = filter(CPISGroupedCategory, !is.na(female)),
             size = 0.25, aes(x = age, y = interest_partisan, color = female,
                              weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age") +
  scale_color_grey(name = "", end = 0.5, labels = c("Boys", "Girls")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        legend.position = "none",
        text = element_text(family = "CM Roman")) +
  ggtitle("Partisan politics")
ggsave(plot = ggpubr::ggarrange(Plot1, Plot2, Plot3, Plot4, Plot5, Plot6,
       nrow = 2, ncol = 3, common.legend = TRUE, legend = "bottom"),
       "_graphs/InterestAgeGenderCPIS.pdf", width = 11, height = 8.5)

DGgrouped <- DG |>
  dplyr::group_by(age, female) |>
  dplyr::summarise(
    interest = weighted.mean(interest, w = weight, na.rm = TRUE))
GroupedDGCategory <- DG |>
  group_by(age, female) |>
  summarise(interest_health = weighted.mean(
              interest_health, w = weight, na.rm = TRUE),
            interest_foreign = weighted.mean(
              interest_foreign, w = weight, na.rm = TRUE),
            interest_law = weighted.mean(
              interest_law, w = weight, na.rm = TRUE),
            interest_education = weighted.mean(
              interest_education, w = weight, na.rm = TRUE),
            interest_partisan = weighted.mean(
              interest_partisan, w = weight, na.rm = TRUE))
DGPlot1 <- ggplot(filter(DG, !is.na(female)),
                  aes(x = age, y = interest, color = female,
                      weight = weight)) +
  geom_point(data = filter(DGgrouped, !is.na(female)), size = 0.25,
             aes(x = age, y = interest, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        text = element_text(family = "CM Roman")) +
  ggtitle("General")
DGPlot2 <- ggplot(filter(DG, !is.na(female)),
                  aes(x = age, y = interest_health, color = female,
                      weight = weight)) +
  geom_point(data = filter(GroupedDGCategory, !is.na(female)), size = 0.25,
             aes(x = age, y = interest_health, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        legend.position = "none",
        text = element_text(family = "CM Roman")) +
  ggtitle("Health care")
DGPlot3 <- ggplot(filter(DG, !is.na(female)),
                  aes(x = age, y = interest_foreign, color = female,
                      weight = weight)) +
  geom_point(data = filter(GroupedDGCategory, !is.na(female)), size = 0.25,
             aes(x = age, y = interest_foreign, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        legend.position = "none",
        text = element_text(family = "CM Roman")) +
  ggtitle("International affairs")
DGPlot4 <- ggplot(filter(DG, !is.na(female)),
                  aes(x = age, y = interest_law, color = female,
                      weight = weight)) +
  geom_point(data = filter(GroupedDGCategory, !is.na(female)), size = 0.25,
             aes(x = age, y = interest_law, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        legend.position = "none",
        text = element_text(family = "CM Roman")) +
  ggtitle("Law and crime")
DGPlot5 <- ggplot(filter(DG, !is.na(female)),
                  aes(x = age, y = interest_education, color = female,
                      weight = weight)) +
  geom_point(data = filter(GroupedDGCategory, !is.na(female)), size = 0.25,
             aes(x = age, y = interest_education, color = female,
                 weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        legend.position = "none",
        text = element_text(family = "CM Roman")) +
  ggtitle("Education")
DGPlot6 <- ggplot(filter(DG, !is.na(female)),
                  aes(x = age, y = interest_partisan, color = female,
                      weight = weight)) +
  geom_point(data = filter(GroupedDGCategory, !is.na(female)), size = 0.25,
             aes(x = age, y = interest_partisan, color = female,
                 weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        legend.position = "none",
        text = element_text(family = "CM Roman")) +
  ggtitle("Partisan politics")
ggsave(plot = ggpubr::ggarrange(
  DGPlot1, DGPlot2, DGPlot3, DGPlot4, DGPlot5, DGPlot6, nrow = 2, ncol = 3,
  common.legend = TRUE, legend = "bottom"),
  "_graphs/InterestAgeGenderDG.pdf", width = 11, height = 8.5)

CES21grouped <- CES21 |>
  group_by(age, female) |>
  reframe(interest = weighted.mean(interest, w = weight, na.rm = TRUE),
          efficacy = weighted.mean(efficacy, w = weight, na.rm = TRUE),
          internal_efficacy = weighted.mean(
            internal_efficacy, w = weight, na.rm = TRUE),
          external_efficacy = weighted.mean(
            external_efficacy, w = weight, na.rm = TRUE),
          knowledge = weighted.mean(knowledge, w = weight, na.rm = TRUE),
          knowparty = weighted.mean(knowparty, w = weight, na.rm = TRUE),
          participation = weighted.mean(
            participation, w = weight, na.rm = TRUE),
          particPartisan = weighted.mean(
            particPartisan, w = weight, na.rm = TRUE),
          particNonPartisan = weighted.mean(
            particNonPartisan, w = weight, na.rm = TRUE))
PlotTimeCES <- ggplot(filter(CES21, !is.na(female)),
       aes(x = age, y = interest / 10, color = female, weight = weight)) +
  geom_point(data = filter(CES21grouped, !is.na(female)), size = 0.25,
             aes(x = age, y = interest / 10, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2.5)) +
  scale_x_continuous(name = "Age, 2021 CES", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        text = element_text(family = "CM Roman")) +
  ggtitle("General political interest")

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
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2.5)) +
  scale_x_continuous(name = "Age, 2020 WVS, Canada", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman")) +
  ggtitle("General political interest")

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
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2.5)) +
  scale_x_continuous(name = "Age, 2017-22 WVS", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman")) +
  ggtitle("General political interest")

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
  geom_smooth(data = filter(GSSgrouped, !is.na(female)), linewidth = 0.25, method = "lm",
            aes(x = age, y = interest / 10, color = female, weight = NULL)) +
  geom_point(data = filter(GSSgrouped, !is.na(female)), size = 0.25,
             aes(x = age, y = interest / 10, color = female, weight = NULL)) +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2.5)) +
  scale_x_discrete(name = "Age, 2020 GSS, Canada") +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90),
        text = element_text(family = "CM Roman")) +
  ggtitle("General political interest")
ggsave(plot = ggpubr::ggarrange(
  PlotTimeCES, PlotTimeWVS, PlotTimeWVSCA, PlotTimeGSS,
  nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom"),
  "_graphs/TimeCESWVSGSS.pdf", width = 11, height = 8.5)
summary(lm(data = GSS20, formula = interest / 10 ~ female, weights = weight))

PlotTimeInterest <- PlotTimeCES +
  scale_x_continuous(name = "Age", limits = c(18, 105))
PlotTimeEfficacy <- ggplot(filter(CES21, !is.na(female)),
       aes(x = age, y = efficacy * 10, color = female, weight = weight)) +
  geom_point(data = filter(CES21grouped, !is.na(female)), size = 0.25,
             aes(x = age, y = efficacy * 10, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "Political efficacy",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2.5)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
PlotTimeInternalEfficacy <- ggplot(
  filter(CES21, !is.na(female)),
  aes(x = age, y = internal_efficacy * 10, color = female, weight = weight)) +
  geom_point(data = filter(CES21grouped, !is.na(female)), size = 0.25,
             aes(x = age, y = internal_efficacy * 10, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2.5)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        text = element_text(family = "CM Roman")) +
  ggtitle("Internal political efficacy")
PlotTimeExternalEfficacy <- ggplot(
  filter(CES21, !is.na(female)),
  aes(x = age, y = external_efficacy * 10, color = female, weight = weight)) +
  geom_point(data = filter(CES21grouped, !is.na(female)), size = 0.25,
             aes(x = age, y = external_efficacy * 10, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2.5)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman")) +
  ggtitle("External political efficacy")
PlotTimeKnowledge <- ggplot(filter(CES21, !is.na(female)),
       aes(x = age, y = knowledge * 10, color = female, weight = weight)) +
  geom_point(data = filter(CES21grouped, !is.na(female)), size = 0.25,
             aes(x = age, y = knowledge * 10, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2.5)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman")) +
  ggtitle("Knowledge of political\nfigures' names")
PlotTimeKnowParty <- ggplot(filter(CES21, !is.na(female)),
       aes(x = age, y = knowparty, color = female, weight = weight)) +
  geom_point(data = filter(CES21grouped, !is.na(female)), size = 0.25,
             aes(x = age, y = knowparty * 10, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2.5)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman")) +
  ggtitle("Percentage point error in\npolitical parties' positions")
PlotTimeParticipation <- ggplot(filter(CES21, !is.na(female)),
       aes(x = age, y = participation * 10, color = female, weight = weight)) +
  geom_point(data = filter(CES21grouped, !is.na(female)), size = 0.25,
             aes(x = age, y = participation * 10, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2.5)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman")) +
  ggtitle("Political participation")
PlotTimePartisanParticipation <- ggplot(filter(CES21, !is.na(female)),
       aes(x = age, y = particPartisan * 10, color = female, weight = weight)) +
  geom_point(data = filter(CES21grouped, !is.na(female)), size = 0.25,
             aes(x = age, y = particPartisan * 10, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2.5)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman")) +
  ggtitle("Partisan political participation")
PlotTimeNonPartisanParticipation <- ggplot(filter(CES21, !is.na(female)),
       aes(x = age, y = particNonPartisan * 10, color = female, weight = weight)) +
  geom_point(data = filter(CES21grouped, !is.na(female)), size = 0.25,
             aes(x = age, y = particNonPartisan * 10, color = female, weight = NULL)) +
  geom_smooth(method = "loess") +
  scale_y_continuous(name = "",
                     limits = c(0, 10), breaks = seq(0, 10, by = 2.5)) +
  scale_x_continuous(name = "Age", limits = c(18, 105)) +
  scale_color_grey(name = "", end = 0.5, labels = c("Men", "Women")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        plot.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman")) +
  ggtitle("Non-partisan political participation")
ggsave(plot = ggpubr::ggarrange(
  PlotTimeInternalEfficacy, PlotTimeExternalEfficacy, PlotTimeKnowledge,
  PlotTimeKnowParty, PlotTimeParticipation, nrow = 3, ncol = 2,
  common.legend = TRUE, legend = "bottom"),
  "_graphs/TimePoliticalEngagement.pdf", width = 11, height = 12.75)
ggsave(plot = ggpubr::ggarrange(
  PlotTimePartisanParticipation, PlotTimeNonPartisanParticipation,
  common.legend = TRUE, legend = "bottom"),
  "_graphs/TimePoliticalParticipation.pdf", width = 11, height = 4.25)

cor.test(CES21$interest, CES21$internal_efficacy, conf.level = 0.999)
cor.test(CES21$interest, CES21$external_efficacy, conf.level = 0.999)
cor.test(CES21$interest, CES21$knowledge, conf.level = 0.999)
cor.test(CES21$interest, CES21$participation, conf.level = 0.999)

##### 3.1.1 Political engagement correlations ####
CES21numeric <- mutate_all(CES21, ~as.numeric(factor(.)))
missing_vars <- sapply(CES21numeric, function(x) mean(is.na(x)) > 0.75)
# Remove variables with more than 75% missing values
CES21numeric <- CES21numeric[, !missing_vars]
zero_sd_vars <- sapply(CES21numeric, function(x) sd(x, na.rm = TRUE) == 0)
# Remove variables with zero standard deviation
CES21numeric <- CES21numeric[, !zero_sd_vars] |>
  dplyr::select(-interest, -cps21_interest_gen_1)
corTests <- map(CES21numeric, cor.test, y = CES21$interest)
CorrelateVariablesData <- data.frame(
  lapply(CES21numeric, function(x) as.numeric(as.character(x))))
varNames <- names(CorrelateVariablesData)
CorrelateData <- data.frame(varNames)
CorrelateData$pearCor <- map(corTests, `[`("estimate")) |>
  map_dbl(`[`("cor"))
CorrelateData$confIntLB <- map(corTests, `[`("conf.int")) |>
  map_dbl(`[`(1))
CorrelateData$confIntUB <- map(corTests, `[`("conf.int")) |>
  map_dbl(`[`(2))
CorrelateData$absPearCor <- abs(CorrelateData$pearCor)
GraphData <- CorrelateData |>
  arrange(absPearCor) |>
  na.omit() |>
  top_n(100)
ggplot(GraphData, aes(x = reorder(varNames, absPearCor), y = pearCor)) +
  geom_point() +
  geom_errorbar(aes(ymin = confIntLB, ymax = confIntUB)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_discrete("") +
  scale_y_continuous("Pearson's correlation") +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "CM Roman"))
ggsave("_graphs/CorTests.pdf", width = 11, height = 14.25)
#ModelInternalEfficacy <- lm(data = CES21, interest ~ internal_efficacy)
#ModelExternalEfficacy <- lm(data = CES21, interest ~ external_efficacy)
#ModelKnowledge <- lm(data = CES21, interest ~ knowledge)
#ModelParticipation <- lm(data = CES21, interest ~ participation)
#EngagementModels <- list(ModelInternalEfficacy, ModelExternalEfficacy,
#                         ModelKnowledge, ModelParticipation)
#print(map(EngagementModels, ~bptest(.x)[[4]]))
# # Breusch-Pagan test for heteroscedasticity. All values below 0.05
#print(map(EngagementModels, ~dwtest(.x)[[4]]))
# # Durbin-Watson test for autocorrelation. All values above 0.05
#summary(lm(data = CES21, interest ~ internal_efficacy, weights = weight))
#summary(lm(data = CES21, interest ~ external_efficacy, weights = weight))
#summary(lm(data = CES21, interest ~ knowledge, weights = weight))
#summary(lm(data = CES21, interest ~ participation, weights = weight))

#### 3.2 Political interest by gender and province ####
prop.table(table(GSS20$province, GSS20$female, GSS20$interest))
ProvinceData <- CES21 |>
  filter(!is.na(female) & !is.na(province)) |>
  group_by(province, female) |>
  summarise(interest_ces = weighted.mean(interest, w = weight, na.rm = TRUE)) |>
  filter(!is.na(interest_ces))
WVSCAProv <- WVSCA20 |>
  filter(!is.na(female) & !is.na(province)) |>
  group_by(province, female) |>
  summarise(interest = weighted.mean(interest, w = weight, na.rm = TRUE))
ProvinceData$interest_wvsca <- WVSCAProv$interest
GSSProv <- GSS20 |>
  filter(!is.na(female) & !is.na(province)) |>
  group_by(province, female) |>
  summarise(interest = weighted.mean(interest, w = weight, na.rm = TRUE))
ProvinceData$interest_gss <- GSSProv$interest
#openxlsx::write.xlsx(ProvinceData, "_data/InterestByGenderAndProvince.xlsx")

#### 3.3 Political interest by gender gap by year and age ####
CESGapYearAge <- CES |>
  filter(!is.na(female) & !is.na(weight)) |>
  group_by(female, year, age, mode) |>
  reframe(interest = weighted.mean(interest, w = weight, na.rm = TRUE),
          n = n()) |>
  group_by(year, age, mode) |>
  reframe(interest_gap = interest[female == 1] - interest[female == 0],
          n = sum(n, na.rm = T))
ggplot(CESGapYearAge, aes(x = age, y = interest_gap / 10, weight = n,
                          linetype = as.factor(mode))) +
  facet_wrap(~year) +
  geom_smooth() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(name = "General political interest gender gap") +
  scale_x_continuous(name = "Age, CES") +
  scale_linetype_manual("Survey mode", values = c(
    "CPS phone" = "dashed", "CPS online" = "solid",
    "PES phone" = "twodash", "PES online" = "dotted")) +
  theme_minimal() +
  theme(strip.text.x = element_text(size = 17.5),
        axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/CESGapYearAge.pdf", width = 11, height = 12.75)

#### 3.4 Political interest by year & gender (CES & WVS) ####
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
InterestGSSGenderData <- data.frame(
  year = as.integer(c(2013, 2013, 2020, 2020)),
  interest = c(Hmisc::wtd.mean(GSS13men$interest, weight = GSS13men$weight),
              Hmisc::wtd.mean(GSS13women$interest, weight = GSS13women$weight),
              Hmisc::wtd.mean(GSS20men$interest, weight = GSS20men$weight),
              Hmisc::wtd.mean(GSS20women$interest,
              weight = GSS20women$weight)),
  female = as.factor(rep(c(0, 1), 2)))
InterestGenderData <- rbind(InterestCESGenderData, InterestWVSGenderData,
  InterestGSSGenderData)
InterestGenderData$survey <- c(rep("CES", 18), rep("WVS", 8), rep("GSS", 4))
InterestGenderData$interest.se <- c(
  weighted.interest.se(CES97men),
  weighted.interest.se(CES97women),
  weighted.interest.se(CES00men),
  weighted.interest.se(CES00women),
  weighted.interest.se(CES04men),
  weighted.interest.se(CES04women),
  weighted.interest.se(CES06men),
  weighted.interest.se(CES06women),
  weighted.interest.se(CES08men),
  weighted.interest.se(CES08women),
  weighted.interest.se(CES11men),
  weighted.interest.se(CES11women),
  weighted.interest.se(CES15men),
  weighted.interest.se(CES15women),
  weighted.interest.se(CES19men),
  weighted.interest.se(CES19women),
  weighted.interest.se(CES21men),
  weighted.interest.se(CES21women),
  weighted.interest.se(WVSCA90men),
  weighted.interest.se(WVSCA90women),
  weighted.interest.se(WVSCA00men),
  weighted.interest.se(WVSCA00women),
  weighted.interest.se(WVSCA06men),
  weighted.interest.se(WVSCA06women),
  weighted.interest.se(WVSCA20men),
  weighted.interest.se(WVSCA20women),
  weighted.interest.se(GSS13men),
  weighted.interest.se(GSS13women),
  weighted.interest.se(GSS20men),
  weighted.interest.se(GSS20women))
InterestGenderData$interest.lb <-
  InterestGenderData$interest + qnorm(0.025) * InterestGenderData$interest.se
InterestGenderData$interest.ub <-
  InterestGenderData$interest + qnorm(0.975) * InterestGenderData$interest.se
ggplot(InterestGenderData, aes(x = year, y = interest / 10, color = female,
                               linetype = survey, group = interaction(female, survey))) +
  geom_line() +
  geom_ribbon(aes(ymin = interest.lb / 10, ymax = interest.ub / 10), alpha = 0.25,
              color = NA) +
  scale_y_continuous(name = "General political interest",
                     limits = c(0, 10)) +
  scale_x_continuous(name = "Year") +
  scale_color_grey(name = "Gender", end = 0.75, labels = c("Men", "Women")) +
  scale_linetype(name = "Survey") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/InterestYearGender.pdf", width = 11, height = 4.25)

summary(lm(data = InterestCESGenderData, formula = interest ~ female))
# in all CES waves: women's political interest = 56%;
# men's political interest = 63%; p<0.05
summary(lm(data = InterestWVSGenderData, formula = interest ~ female))
# in all WVS waves: women's political interest = 47%;
# men's political interest = 56%; p<0.05
summary(lm(data = InterestGSSGenderData, formula = interest ~ female))
# in all GSS waves: women's political interest = 56%;
# men's political interest = 62%; N.S.

#### 3.5 Political interest by year, gender & ethnicity (CES & WVS) ####
whites <- c("White\nmen", "White\nwomen", "Nonwhite\nmen", "Nonwhite\nwomen")
immigs <- c("Immigrant\nmen", "Immigrant\nwomen", "Nonimmigrant\nmen",
            "Nonimmigrant\nwomen")
GenderEthnicityInterest <- data.frame(
  group = as.factor(c(rep(whites, 3), immigs, whites, rep(immigs, 2),
                     whites, immigs)),
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
                               weight = CES21nonimmigrantwomen$weight),
               Hmisc::wtd.mean(GSS13immigrantmen$interest,
                               weight = GSS13immigrantmen$weight),
               Hmisc::wtd.mean(GSS13immigrantwomen$interest,
                               weight = GSS13immigrantwomen$weight),
               Hmisc::wtd.mean(GSS13nonimmigrantmen$interest,
                               weight = GSS13nonimmigrantmen$weight),
               Hmisc::wtd.mean(GSS13nonimmigrantwomen$interest,
                               weight = GSS13nonimmigrantwomen$weight),
               Hmisc::wtd.mean(GSS20whitemen$interest,
                               weight = GSS20whitemen$weight),
               Hmisc::wtd.mean(GSS20whitewomen$interest,
                               weight = GSS20whitewomen$weight),
               Hmisc::wtd.mean(GSS20nonwhitemen$interest,
                               weight = GSS20nonwhitemen$weight),
               Hmisc::wtd.mean(GSS20nonwhitewomen$interest,
                               weight = GSS20nonwhitewomen$weight),
               Hmisc::wtd.mean(GSS20immigrantmen$interest,
                               weight = GSS20immigrantmen$weight),
               Hmisc::wtd.mean(GSS20immigrantwomen$interest,
                               weight = GSS20immigrantwomen$weight),
               Hmisc::wtd.mean(GSS20nonimmigrantmen$interest,
                               weight = GSS20nonimmigrantmen$weight),
               Hmisc::wtd.mean(GSS20nonimmigrantwomen$interest,
                               weight = GSS20nonimmigrantwomen$weight)),
  year = as.factor(c(rep("WVS 2000", 4), rep("WVS 2006", 4),
                     rep("WVS 2020", 8), rep("CES 2021", 8),
                     rep("GSS 2013", 4), rep("GSS 2020", 8))))
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
  weighted.interest.se(CES21nonimmigrantwomen),
  weighted.interest.se(GSS13immigrantmen),
  weighted.interest.se(GSS13immigrantwomen),
  weighted.interest.se(GSS13nonimmigrantmen),
  weighted.interest.se(GSS13nonimmigrantwomen),
  weighted.interest.se(GSS20whitemen),
  weighted.interest.se(GSS20whitewomen),
  weighted.interest.se(GSS20nonwhitemen),
  weighted.interest.se(GSS20nonwhitewomen),
  weighted.interest.se(GSS20immigrantmen),
  weighted.interest.se(GSS20immigrantwomen),
  weighted.interest.se(GSS20nonimmigrantmen),
  weighted.interest.se(GSS20nonimmigrantwomen))
GenderEthnicityInterest$interest.lb <-
  GenderEthnicityInterest$interest +
  qnorm(0.025) * GenderEthnicityInterest$interest.se
GenderEthnicityInterest$interest.ub <-
  GenderEthnicityInterest$interest +
  qnorm(0.975) * GenderEthnicityInterest$interest.se
GenderEthnicityInterest$group <- factor(
  GenderEthnicityInterest$group,
  levels = c("White\nmen", "White\nwomen", "Nonwhite\nmen",
             "Nonwhite\nwomen", "Immigrant\nmen", "Immigrant\nwomen",
             "Nonimmigrant\nmen", "Nonimmigrant\nwomen"))
ggplot(GenderEthnicityInterest, aes(x = group, y = interest / 10,
                                    color = year, shape = year)) +
  geom_point(position = position_dodge(width = 0.5), size = 0.75) +
  geom_errorbar(aes(ymin = interest.lb / 10, ymax = interest.ub / 10),
                width = 0, position = position_dodge(width = 0.5)) +
  scale_y_continuous(name = "General political interest",
                     limits = c(0, 10)) +
  scale_x_discrete(name = "Group") +
  scale_color_manual(name = "Wave", values = c(
    "CES 2021" = "grey20", "GSS 2013" = "grey50", "GSS 2020" = "grey20",
    "WVS 2000" = "grey80", "WVS 2006" = "grey50", "WVS 2020" = "grey20")) +
  scale_shape_manual(name = "Wave", values = c(
    "CES 2021" = 1, "GSS 2013" = 2, "GSS 2020" = 2,
    "WVS 2000" = 3, "WVS 2006" = 3, "WVS 2020" = 3)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/InterestWaveGroup.pdf", width = 11, height = 8.5)

#### 3.6 Political interest by ethnicity (2017-22 CES & WVS) ####
EthnicityInterestWVS <- data.frame(
  ethn = as.factor(c("White", "Black", "West Asian",
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
  ethn = as.factor(c("White", "Black", "West Asian",
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
EthnicityInterest$ethn <- factor(
  EthnicityInterest$ethn,
  levels = c("Arabic", "Black", "Chinese", "Filipino", "Hispanic",
             "Indigenous", "Japanese", "Korean", "South Asian",
             "Southeast Asian", "West Asian", "White", "Other"))
ggplot(EthnicityInterest, aes(x = ethn, y = interest,
                              color = survey)) +
  geom_point(position = position_dodge(width = 0.75), size = 0.75) +
  geom_errorbar(aes(ymin = interest.lb, ymax = interest.ub),
                width = 0.5, position = position_dodge(width = 0.75)) +
  scale_y_continuous(name = "General\npolitical interest",
                     limits = c(0, 100)) +
  scale_x_discrete(name = "Ethnicity") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90),
        text = element_text(family = "CM Roman")) +
  scale_color_grey(name = "Survey", end = 0.5)
ggsave("_graphs/InterestEthnicity20_21.pdf", width = 11, height = 4.25)

#### 3.7 Parents' discussion of topics by child gender ####
CPISBoysLonger <- pivot_longer(CPISBoys,
                                cols = c(starts_with(c("gender_parent_",
                                                      "parent_discuss_alt"))))
CPISBoysLonger$sex <- "Boys"
CPISGirlsLonger <- pivot_longer(CPISGirls,
                                    cols = c(starts_with(c("gender_parent_",
                                                           "parent_discuss_alt"))))
CPISGirlsLonger$sex <- "Girls"
CPISLonger <- rbind(CPISBoysLonger, CPISGirlsLonger)
CPISLonger$topic <- case_when(
  CPISLonger$name == "gender_parent_health" ~ "Health\ncare",
  CPISLonger$name == "gender_parent_education" ~ "Education",
  CPISLonger$name == "gender_parent_law" ~ "Law and\ncrime",
  CPISLonger$name == "gender_parent_foreign" ~ "Inter-\nnational\naffairs",
  CPISLonger$name == "gender_parent_partisan" ~ "Partisan\npolitics",
  CPISLonger$name == "parent_discuss_alt" ~ "All\ndiscussions")
ggplot(CPISLonger, aes(x = sex, fill = as.factor(value))) +
  geom_bar(position = "fill") +
  facet_wrap(~topic, ncol = 6) +
  scale_x_discrete("Gender") +
  scale_y_continuous("Percent of students", labels = scales::percent) +
  scale_fill_discrete("Parent who\ndiscusses the\ntopic most often",
                      labels = c("Father", "Mother",
                                 paste0("Don't know/\nPrefer not\nto answer/",
                                        "\nMissing")),
                      type = c("purple", "orange", "white")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.x = element_text(angle = 45),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/ParentTopics.pdf", width = 11, height = 4.25)
ggplot(CPISLonger, aes(x = sex, fill = as.factor(value))) +
  geom_bar(position = "fill") +
  facet_wrap(~topic, ncol = 6) +
  scale_x_discrete("Gender") +
  scale_y_continuous("Percent of students", labels = scales::percent) +
  scale_fill_grey("Parent who\ndiscusses the\ntopic most often",
                      labels = c("Father", "Mother",
                                 paste0("Don't know/\nPrefer not\nto answer/",
                                        "\nMissing")),
                      start = 0.2, end = 0.6, na.value = "grey90") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.x = element_text(angle = 45),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/ParentTopicsGrey.pdf", width = 11, height = 4.25)
CPISLongerTest <- filter(CPISLonger, topic == "All\ndiscussions")
prop.table(table(CPISLongerTest$value, CPISLongerTest$sex, useNA = "always"), margin = 2)
CPISLongerTest2 <- filter(CPISLonger, topic == "Health\ncare")
prop.table(table(CPISLongerTest2$value, CPISLongerTest2$sex, useNA = "always"), margin = 2)
CPISLongerTest3 <- filter(CPISLonger, topic == "Education")
prop.table(table(CPISLongerTest3$value, CPISLongerTest3$sex, useNA = "always"), margin = 2)

CPISBoysGraph <- CPISBoys |>
  pivot_longer(cols = c(mother_discuss_clean, father_discuss_clean)) |>
  group_by(name) |>
  mutate(N = n()) |>
  group_by(name, value) |>
  summarise(perc = n() / unique(N)) |>
  filter(!is.na(value))
CPISBoysGraph$sex <- "Boys"
CPISGirlsGraph <- CPISGirls |>
  pivot_longer(cols = c(mother_discuss_clean, father_discuss_clean)) |>
  group_by(name) |>
  mutate(N = n()) |>
  group_by(name, value) |>
  summarise(perc = n() / unique(N)) |>
  filter(!is.na(value))
CPISGirlsGraph$sex <- "Girls"
CPISGraph <- rbind(CPISBoysGraph, CPISGirlsGraph)
ggplot(CPISGraph, aes(x = value, y = perc, fill = name)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~sex) +
  scale_x_discrete("Topic most often discussed with parent") +
  scale_y_continuous("Percent of students", labels = scales::percent) +
  scale_fill_discrete("Parent",
                      labels = c("Father", "Mother"),
                      type = c("purple", "orange")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/ParentTopicsMomDad.pdf", width = 11, height = 4.25)
ggplot(CPISGraph, aes(x = value, y = perc, fill = name)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~sex) +
  scale_x_discrete("Topic most often discussed with parent") +
  scale_y_continuous("Percent of students", labels = scales::percent) +
  scale_fill_grey("Parent",
                      labels = c("Father", "Mother")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/ParentTopicsMomDadGrey.pdf", width = 11, height = 4.25)

#### 3.8 Peers' discussion of topics by child gender ####
CPISBoysPeerGraph <- CPISBoys |>
  pivot_longer(cols = c(femalefriends_discuss_clean,
                        malefriends_discuss_clean)) |>
  group_by(name) |>
  mutate(N = n()) |>
  group_by(name, value) |>
  summarise(perc = n() / unique(N)) |>
  filter(!is.na(value))
CPISBoysPeerGraph$sex <- "Boys"
CPISGirlsPeerGraph <- CPISGirls |>
  pivot_longer(cols = c(femalefriends_discuss_clean,
                        malefriends_discuss_clean)) |>
  group_by(name) |>
  mutate(N = n()) |>
  group_by(name, value) |>
  summarise(perc = n() / unique(N)) |>
  filter(!is.na(value))
CPISGirlsPeerGraph$sex <- "Girls"
CPISPeerGraph <- rbind(CPISBoysPeerGraph, CPISGirlsPeerGraph)
ggplot(CPISPeerGraph, aes(x = value, y = perc, fill = name)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~sex) +
  scale_x_discrete("Topic most often discussed") +
  scale_y_continuous("Percent of students", labels = scales::percent) +
  scale_fill_discrete("Friends' gender",
                      labels = c("Female", "Male"),
                      type = c("orange", "purple")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/PeersTopics.pdf", width = 11, height = 4.25)
ggplot(CPISPeerGraph, aes(x = value, y = perc, fill = name)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_wrap(~sex) +
  scale_x_discrete("Topic most often discussed") +
  scale_y_continuous("Percent of students", labels = scales::percent) +
  scale_fill_grey("Friends' gender", start = 0.8, end = 0.2,
                      labels = c("Female", "Male")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/PeersTopicsGrey.pdf", width = 11, height = 4.25)
filter(CPISOldYoung, !is.na(friends_gender_alt) & !is.na(female)) |>
  ggplot(aes(x = female_alt, fill = as.factor(friends_gender_alt))) +
  geom_bar(position = "fill") +
  facet_wrap(~agegrp) +
  scale_x_discrete("Gender") +
  scale_y_continuous("Percent of students", labels = scales::percent) +
  scale_fill_grey("Gender of most friends", na.value = "grey") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/PeersGenderAge.pdf", width = 11, height = 4.25)
CPISOldYoungBoys <- filter(CPISOldYoung, female == 0)
CPISOldYoungGirls <- filter(CPISOldYoung, female == 1)
prop.table(table(CPISOldYoungBoys$agegrp,
 CPISOldYoungBoys$friends_gender_alt), margin = 1)
prop.table(table(CPISOldYoungGirls$agegrp,
 CPISOldYoungGirls$friends_gender_alt), margin = 1)

### 4. Regression models ####
#### 4.1 Create longer versions of each dataset ####
longer_parent <- function(data) {
  pivot_longer(data, cols = starts_with("gender_parent_"),
    names_to = "name_parent", values_to = "value_parent") |>
  mutate(interest_all = case_when(
    name_parent == "gender_parent_health" ~ interest_health,
    name_parent == "gender_parent_foreign" ~ interest_foreign,
    name_parent == "gender_parent_law" ~ interest_law,
    name_parent == "gender_parent_education" ~ interest_education,
    name_parent == "gender_parent_partisan" ~ interest_partisan))
}
CPISParentLonger <- longer_parent(CPIS)
CPISBoysParentLonger <- longer_parent(CPISBoys)
CPISGirlsParentLonger <- longer_parent(CPISGirls)
CPISYoungBoysParentLonger <- longer_parent(CPISYoungBoys)
CPISYoungGirlsParentLonger <- longer_parent(CPISYoungGirls)
CPISOldBoysParentLonger <- longer_parent(CPISOldBoys)
CPISOldGirlsParentLonger <- longer_parent(CPISOldGirls)
longer_mother <- function(data) {
  pivot_longer(data, cols = c(
    "mother_discuss_health", "mother_discuss_foreign", "mother_discuss_law",
    "mother_discuss_education", "mother_discuss_partisan"),
    names_to = "name_mother", values_to = "value_mother") |>
  mutate(interest_all = case_when(
    name_mother == "mother_discuss_health" ~ interest_health,
    name_mother == "mother_discuss_foreign" ~ interest_foreign,
    name_mother == "mother_discuss_law" ~ interest_law,
    name_mother == "mother_discuss_education" ~ interest_education,
    name_mother == "mother_discuss_partisan" ~ interest_partisan))
}
CPISMotherLonger <- longer_mother(CPIS)
CPISBoysMotherLonger <- longer_mother(CPISBoys)
CPISGirlsMotherLonger <- longer_mother(CPISGirls)
CPISYoungBoysMotherLonger <- longer_mother(CPISYoungBoys)
CPISYoungGirlsMotherLonger <- longer_mother(CPISYoungGirls)
CPISOldBoysMotherLonger <- longer_mother(CPISOldBoys)
CPISOldGirlsMotherLonger <- longer_mother(CPISOldGirls)
longer_father <- function(data) {
  pivot_longer(data, cols = c(
    "father_discuss_health", "father_discuss_foreign", "father_discuss_law",
    "father_discuss_education", "father_discuss_partisan"),
    names_to = "name_father", values_to = "value_father") |>
  mutate(interest_all = case_when(
    name_father == "father_discuss_health" ~ interest_health,
    name_father == "father_discuss_foreign" ~ interest_foreign,
    name_father == "father_discuss_law" ~ interest_law,
    name_father == "father_discuss_education" ~ interest_education,
    name_father == "father_discuss_partisan" ~ interest_partisan))
}
CPISFatherLonger <- longer_father(CPIS)
CPISBoysFatherLonger <- longer_father(CPISBoys)
CPISGirlsFatherLonger <- longer_father(CPISGirls)
CPISYoungBoysFatherLonger <- longer_father(CPISYoungBoys)
CPISYoungGirlsFatherLonger <- longer_father(CPISYoungGirls)
CPISOldBoysFatherLonger <- longer_father(CPISOldBoys)
CPISOldGirlsFatherLonger <- longer_father(CPISOldGirls)
longer_femalefriends <- function(data) {
  pivot_longer(data, cols = c(
    "femalefriends_discuss_health", "femalefriends_discuss_foreign",
    "femalefriends_discuss_law", "femalefriends_discuss_education",
    "femalefriends_discuss_partisan"),
    names_to = "name_femalefriends", values_to = "value_femalefriends") |>
  mutate(interest_all = case_when(
    name_femalefriends == "femalefriends_discuss_health" ~ interest_health,
    name_femalefriends == "femalefriends_discuss_foreign" ~ interest_foreign,
    name_femalefriends == "femalefriends_discuss_law" ~ interest_law,
    name_femalefriends == "femalefriends_discuss_education" ~
     interest_education,
    name_femalefriends == "femalefriends_discuss_partisan" ~
     interest_partisan))
}
CPISFemaleFriendsLonger <- longer_femalefriends(CPIS)
CPISBoysFemaleFriendsLonger <- longer_femalefriends(CPISBoys)
CPISGirlsFemaleFriendsLonger <- longer_femalefriends(CPISGirls)
CPISYoungBoysFemaleFriendsLonger <- longer_femalefriends(CPISYoungBoys)
CPISYoungGirlsFemaleFriendsLonger <- longer_femalefriends(CPISYoungGirls)
CPISOldBoysFemaleFriendsLonger <- longer_femalefriends(CPISOldBoys)
CPISOldGirlsFemaleFriendsLonger <- longer_femalefriends(CPISOldGirls)
longer_malefriends <- function(data) {
  pivot_longer(data, cols = c(
    "malefriends_discuss_health", "malefriends_discuss_foreign",
    "malefriends_discuss_law", "malefriends_discuss_education",
    "malefriends_discuss_partisan"),
    names_to = "name_malefriends", values_to = "value_malefriends") |>
  mutate(interest_all = case_when(
    name_malefriends == "malefriends_discuss_health" ~ interest_health,
    name_malefriends == "malefriends_discuss_foreign" ~ interest_foreign,
    name_malefriends == "malefriends_discuss_law" ~ interest_law,
    name_malefriends == "malefriends_discuss_education" ~ interest_education,
    name_malefriends == "malefriends_discuss_partisan" ~ interest_partisan))
}
CPISMaleFriendsLonger <- longer_malefriends(CPIS)
CPISBoysMaleFriendsLonger <- longer_malefriends(CPISBoys)
CPISGirlsMaleFriendsLonger <- longer_malefriends(CPISGirls)
CPISYoungBoysMaleFriendsLonger <- longer_malefriends(CPISYoungBoys)
CPISYoungGirlsMaleFriendsLonger <- longer_malefriends(CPISYoungGirls)
CPISOldBoysMaleFriendsLonger <- longer_malefriends(CPISOldBoys)
CPISOldGirlsMaleFriendsLonger <- longer_malefriends(CPISOldGirls)
longer_teacher <- function(data) {
  pivot_longer(data, cols = c(
    "teacher_discuss_health", "teacher_discuss_foreign",
    "teacher_discuss_law", "teacher_discuss_education",
    "teacher_discuss_partisan"),
    names_to = "name_teacher", values_to = "value_teacher") |>
  mutate(interest_all = case_when(
    name_teacher == "teacher_discuss_health" ~ interest_health,
    name_teacher == "teacher_discuss_foreign" ~ interest_foreign,
    name_teacher == "teacher_discuss_law" ~ interest_law,
    name_teacher == "teacher_discuss_education" ~ interest_education,
    name_teacher == "teacher_discuss_partisan" ~ interest_partisan))
}
CPISTeacherLonger <- longer_teacher(CPIS)
CPISBoysTeacherLonger <- longer_teacher(CPISBoys)
CPISGirlsTeacherLonger <- longer_teacher(CPISGirls)
CPISYoungBoysTeacherLonger <- longer_teacher(CPISYoungBoys)
CPISYoungGirlsTeacherLonger <- longer_teacher(CPISYoungGirls)
CPISOldBoysTeacherLonger <- longer_teacher(CPISOldBoys)
CPISOldGirlsTeacherLonger <- longer_teacher(CPISOldGirls)
longer_influencer <- function(data) {
  pivot_longer(data, cols = c(
    "influencer_discuss_health", "influencer_discuss_foreign",
    "influencer_discuss_law", "influencer_discuss_education",
    "influencer_discuss_partisan"),
    names_to = "name_influencer", values_to = "value_influencer") |>
  mutate(interest_all = case_when(
    name_influencer == "influencer_discuss_health" ~ interest_health,
    name_influencer == "influencer_discuss_foreign" ~ interest_foreign,
    name_influencer == "influencer_discuss_law" ~ interest_law,
    name_influencer == "influencer_discuss_education" ~ interest_education,
    name_influencer == "influencer_discuss_partisan" ~ interest_partisan))
}
CPISInfluencerLonger <- longer_influencer(CPIS)
CPISBoysInfluencerLonger <- longer_influencer(CPISBoys)
CPISGirlsInfluencerLonger <- longer_influencer(CPISGirls)
CPISYoungBoysInfluencerLonger <- longer_influencer(CPISYoungBoys)
CPISYoungGirlsInfluencerLonger <- longer_influencer(CPISYoungGirls)
CPISOldBoysInfluencerLonger <- longer_influencer(CPISOldBoys)
CPISOldGirlsInfluencerLonger <- longer_influencer(CPISOldGirls)
CPISAgentsLonger <- cbind(
  CPISMotherLonger, CPISFatherLonger, CPISFemaleFriendsLonger,
  CPISMaleFriendsLonger, CPISTeacherLonger, CPISInfluencerLonger)
CPISAgentsLonger <- CPISAgentsLonger[,
 !duplicated(colnames(CPISAgentsLonger), fromLast = TRUE)]
CPISBoysAgentsLonger <- cbind(
  CPISBoysMotherLonger, CPISBoysFatherLonger, CPISBoysFemaleFriendsLonger,
  CPISBoysMaleFriendsLonger, CPISBoysTeacherLonger,
  CPISBoysInfluencerLonger)
CPISBoysAgentsLonger <- CPISBoysAgentsLonger[,
 !duplicated(colnames(CPISBoysAgentsLonger), fromLast = TRUE)]
CPISGirlsAgentsLonger <- cbind(
  CPISGirlsMotherLonger, CPISGirlsFatherLonger,
  CPISGirlsFemaleFriendsLonger, CPISGirlsMaleFriendsLonger,
  CPISGirlsTeacherLonger, CPISGirlsInfluencerLonger)
CPISGirlsAgentsLonger <- CPISGirlsAgentsLonger[,
 !duplicated(colnames(CPISGirlsAgentsLonger), fromLast = TRUE)]
CPISYoungBoysAgentsLonger <- cbind(
  CPISYoungBoysMotherLonger, CPISYoungBoysFatherLonger,
  CPISYoungBoysFemaleFriendsLonger, CPISYoungBoysMaleFriendsLonger,
  CPISYoungBoysTeacherLonger, CPISYoungBoysInfluencerLonger)
CPISYoungBoysAgentsLonger <- CPISYoungBoysAgentsLonger[,
 !duplicated(colnames(CPISYoungBoysAgentsLonger), fromLast = TRUE)]
CPISYoungGirlsAgentsLonger <- cbind(
  CPISYoungGirlsMotherLonger, CPISYoungGirlsFatherLonger,
  CPISYoungGirlsFemaleFriendsLonger, CPISYoungGirlsMaleFriendsLonger,
  CPISYoungGirlsTeacherLonger, CPISYoungGirlsInfluencerLonger)
CPISYoungGirlsAgentsLonger <- CPISYoungGirlsAgentsLonger[,
 !duplicated(colnames(CPISYoungGirlsAgentsLonger), fromLast = TRUE)]
CPISOldBoysAgentsLonger <- cbind(
  CPISOldBoysMotherLonger, CPISOldBoysFatherLonger,
  CPISOldBoysFemaleFriendsLonger, CPISOldBoysMaleFriendsLonger,
  CPISOldBoysTeacherLonger, CPISOldBoysInfluencerLonger)
CPISOldBoysAgentsLonger <- CPISOldBoysAgentsLonger[,
 !duplicated(colnames(CPISOldBoysAgentsLonger), fromLast = TRUE)]
CPISOldGirlsAgentsLonger <- cbind(
  CPISOldGirlsMotherLonger, CPISOldGirlsFatherLonger,
  CPISOldGirlsFemaleFriendsLonger, CPISOldGirlsMaleFriendsLonger,
  CPISOldGirlsTeacherLonger, CPISOldGirlsInfluencerLonger)
CPISOldGirlsAgentsLonger <- CPISOldGirlsAgentsLonger[,
 !duplicated(colnames(CPISOldGirlsAgentsLonger), fromLast = TRUE)]
longer_samegenderinfluencer <- function(data) {
  pivot_longer(data, cols = c(
    "samegenderinfluencer_discuss_health",
    "samegenderinfluencer_discuss_foreign",
    "samegenderinfluencer_discuss_law",
    "samegenderinfluencer_discuss_education",
    "samegenderinfluencer_discuss_partisan"),
    names_to = "name_influencer", values_to = "value_influencer") |>
  mutate(interest_all = case_when(
    name_influencer == "samegenderinfluencer_discuss_health" ~
     interest_health,
    name_influencer == "samegenderinfluencer_discuss_foreign" ~
     interest_foreign,
    name_influencer == "samegenderinfluencer_discuss_law" ~
     interest_law,
    name_influencer == "samegenderinfluencer_discuss_education" ~
     interest_education,
    name_influencer == "samegenderinfluencer_discuss_partisan" ~
     interest_partisan))
}
CPISSameGenderInfluencerLonger <- longer_samegenderinfluencer(CPIS)
longer_othergenderinfluencer <- function(data) {
  pivot_longer(data, cols = c(
    "othergenderinfluencer_discuss_health",
    "othergenderinfluencer_discuss_foreign",
    "othergenderinfluencer_discuss_law",
    "othergenderinfluencer_discuss_education",
    "othergenderinfluencer_discuss_partisan"),
    names_to = "name_influencer", values_to = "value_influencer") |>
  mutate(interest_all = case_when(
    name_influencer == "othergenderinfluencer_discuss_health" ~
     interest_health,
    name_influencer == "othergenderinfluencer_discuss_foreign" ~
     interest_foreign,
    name_influencer == "othergenderinfluencer_discuss_law" ~
     interest_law,
    name_influencer == "othergenderinfluencer_discuss_education" ~
     interest_education,
    name_influencer == "othergenderinfluencer_discuss_partisan" ~
     interest_partisan))
}
CPISOtherGenderInfluencerLonger <- longer_othergenderinfluencer(CPIS)

#### 4.2 Create empty models ####
ModelInterest <- nlme::lme(data = CPIS, fixed = interest ~ 1,
                    random = ~ 1 | Class, na.action = na.omit)
ModelInterestEffects <- nlme::VarCorr(ModelInterest)
100 * as.numeric(ModelInterestEffects[1]) / (
  as.numeric(ModelInterestEffects[1]) + as.numeric(ModelInterestEffects[2]))
# ~6.2% of variance in political interest is located at the classroom level
ModelHealth <- nlme::lme(data = CPIS, fixed = interest_health ~ 1,
                    random = ~ 1 | Class, na.action = na.omit)
ModelHealthEffects <- nlme::VarCorr(ModelHealth)
100 * as.numeric(ModelHealthEffects[1]) / (as.numeric(ModelHealthEffects[1]) +
                                             as.numeric(ModelHealthEffects[2]))
# ~4.6% of variance in interest in health care is located at the classroom level
ModelForeign <- nlme::lme(data = CPIS, fixed = interest_foreign ~ 1,
                         random = ~ 1 | Class, na.action = na.omit)
ModelForeignEffects <- nlme::VarCorr(ModelForeign)
100 * as.numeric(ModelForeignEffects[1]) / (
  as.numeric(ModelForeignEffects[1]) + as.numeric(ModelForeignEffects[2]))
# ~3.6% of variance in interest in international affairs is located at the
# classroom level
ModelLaw <- nlme::lme(data = CPIS, fixed = interest_law ~ 1,
                          random = ~ 1 | Class, na.action = na.omit)
ModelLawEffects <- nlme::VarCorr(ModelLaw)
100 * as.numeric(ModelLawEffects[1]) / (as.numeric(ModelLawEffects[1]) +
                                              as.numeric(ModelLawEffects[2]))
# ~1.4% of variance in interest in law and crime is located at the classroom
# level
ModelEducation <- nlme::lme(data = CPIS, fixed = interest_education ~ 1,
                          random = ~ 1 | Class, na.action = na.omit)
ModelEducationEffects <- nlme::VarCorr(ModelEducation)
100 * as.numeric(ModelEducationEffects[1]) / (as.numeric(ModelEducationEffects[1]) +
                                              as.numeric(ModelEducationEffects[2]))
# ~8.1% of variance in interest in education is located at the classroom level
ModelPartisan <- nlme::lme(data = CPIS, fixed = interest_partisan ~ 1,
                          random = ~ 1 | Class, na.action = na.omit)
ModelPartisanEffects <- nlme::VarCorr(ModelPartisan)
100 * as.numeric(ModelPartisanEffects[1]) / (as.numeric(ModelPartisanEffects[1]) +
                                              as.numeric(ModelPartisanEffects[2]))
# ~2.6% of variance in interest in partisan politics is located at the
# classroom level
ModelInterestYoung <- nlme::lme(data = CPISYoung, fixed = interest ~
                                1, random = ~ 1 | Class, na.action = na.omit)
ModelInterestYoungEffects <- nlme::VarCorr(ModelInterestYoung)
100 * as.numeric(ModelInterestYoungEffects[1]) / (
  as.numeric(ModelInterestYoungEffects[1]) +
  as.numeric(ModelInterestYoungEffects[2]))
# ~7% of variance in political interest is located at the classroom level among
# students aged 10-15
ModelInterestOld <- nlme::lme(data = CPISOld, fixed = interest ~
                              1, random = ~ 1 | Class, na.action = na.omit)
ModelInterestOldEffects <- nlme::VarCorr(ModelInterestOld)
100 * as.numeric(ModelInterestOldEffects[1]) / (
  as.numeric(ModelInterestOldEffects[1]) +
  as.numeric(ModelInterestOldEffects[2]))
# ~1.2% of variance in political interest is located at the classroom level
# among students aged 16-18

#### 4.3 Create models with no controls ####
lme_no_ctrl <- function(data, x, y) {
  data$x <- data[[x]]
  data$y <- data[[y]]
  nlme::lme(data = data, fixed = y ~ x, random = ~ 1 | Class, na.action = na.omit)
}
ModelInterestGender <- lme_no_ctrl(data = CPIS, x = "female", y = "interest")
summary(ModelInterestGender)
# girls' political interest = 4.1/10; boys' political interest = 4.6/10; p<0.05
ModelHealthGender <- lme_no_ctrl(
  data = CPIS, x = "female", y = "interest_health")
summary(ModelHealthGender) # N.S.
ModelForeignGender <- lme_no_ctrl(
  data = CPIS, x = "female", y = "interest_foreign")
summary(ModelForeignGender) # p<0.001
ModelLawGender <- lme_no_ctrl(
  data = CPIS, x = "female", y = "interest_law")
summary(ModelLawGender) # p<0.05
ModelEducationGender <- lme_no_ctrl(
  data = CPIS, x = "female", y = "interest_education")
summary(ModelEducationGender) # N.S.
ModelPartisanGender <- lme_no_ctrl(
  data = CPIS, x = "female", y = "interest_partisan")
summary(ModelPartisanGender) # p<0.001
ModelInterestGenderYoung <- lme_no_ctrl(
  data = CPISYoung, x = "female", y = "interest")
ModelHealthGenderYoung <- lme_no_ctrl(
  data = CPISYoung, x = "female", y = "interest_health")
ModelForeignGenderYoung <- lme_no_ctrl(
  data = CPISYoung, x = "female", y = "interest_foreign")
ModelLawGenderYoung <- lme_no_ctrl(
  data = CPISYoung, x = "female", y = "interest_law")
ModelEducationGenderYoung <- lme_no_ctrl(
  data = CPISYoung, x = "female", y = "interest_education")
ModelPartisanGenderYoung <- lme_no_ctrl(
  data = CPISYoung, x = "female", y = "interest_partisan")
ModelInterestGenderOld <- lme_no_ctrl(
  data = CPISOld, x = "female", y = "interest")
ModelHealthGenderOld <- lme_no_ctrl(
  data = CPISOld, x = "female", y = "interest_health")
ModelForeignGenderOld <- lme_no_ctrl(
  data = CPISOld, x = "female", y = "interest_foreign")
ModelLawGenderOld <- lme_no_ctrl(
  data = CPISOld, x = "female", y = "interest_law")
ModelEducationGenderOld <- lme_no_ctrl(
  data = CPISOld, x = "female", y = "interest_education")
ModelPartisanGenderOld <- lme_no_ctrl(
  data = CPISOld, x = "female", y = "interest_partisan")
ModelInterestGenderDG <- lm(data = DG, formula = interest ~ female)
# women's political interest = 6.9/10; men's political interest = 7.7; p<0.001
ModelHealthGenderDG <- lm(data = DG, formula = interest_health ~ female)
# if one of these tests gives p<0.05, I should use WLS instead of WLS
ModelForeignGenderDG <- lm(data = DG, formula = interest_foreign ~ female)
ModelLawGenderDG <- lm(data = DG, formula = interest_law ~ female)
ModelEducationGenderDG <- lm(data = DG, formula = interest_education ~ female)
ModelPartisanGenderDG <- lm(data = DG, formula = interest_partisan ~ female)
ModelBoysHealthGenderParent <- lme_no_ctrl(
  data = CPISBoys, x = "gender_parent_health", y = "interest_health")
ModelGirlsHealthGenderParent <- lme_no_ctrl(
  data = CPISGirls, x = "gender_parent_health", y = "interest_health")
ModelBoysForeignGenderParent <- lme_no_ctrl(
  data = CPISBoys, x = "gender_parent_foreign", y = "interest_foreign")
ModelGirlsForeignGenderParent <- lme_no_ctrl(
  data = CPISGirls, x = "gender_parent_foreign", y = "interest_foreign")
ModelBoysLawGenderParent <- lme_no_ctrl(
  data = CPISBoys, x = "gender_parent_law", y = "interest_law")
ModelGirlsLawGenderParent <- lme_no_ctrl(
  data = CPISGirls, x = "gender_parent_law", y = "interest_law")
ModelBoysEducationGenderParent <- lme_no_ctrl(
  data = CPISBoys, x = "gender_parent_education", y = "interest_education")
ModelGirlsEducationGenderParent <- lme_no_ctrl(
  data = CPISGirls, x = "gender_parent_education", y = "interest_education")
ModelBoysPartisanGenderParent <- lme_no_ctrl(
  data = CPISBoys, x = "gender_parent_partisan", y = "interest_partisan")
ModelGirlsPartisanGenderParent <- lme_no_ctrl(
  data = CPISGirls, x = "gender_parent_partisan", y = "interest_partisan")
ModelBoysAllGenderParent <- lme_no_ctrl(
  data = CPISBoysParentLonger, x = "value_parent", y = "interest_all")
ModelGirlsAllGenderParent <- lme_no_ctrl(
  data = CPISGirlsParentLonger, x = "value_parent", y = "interest_all")
ModelYoungBoysGenderParent <- lme_no_ctrl(
  data = CPISYoungBoysParentLonger, x = "value_parent", y = "interest_all")
ModelYoungGirlsGenderParent <- lme_no_ctrl(
  data = CPISYoungGirlsParentLonger, x = "value_parent", y = "interest_all")
ModelOldBoysGenderParent <- lme_no_ctrl(
  data = CPISOldBoysParentLonger, x = "value_parent", y = "interest_all")
ModelOldGirlsGenderParent <- lme_no_ctrl(
  data = CPISOldGirlsParentLonger, x = "value_parent", y = "interest_all")
ModelBoysHealthMother <- lme_no_ctrl(
  data = CPISBoys, x = "mother_discuss_health", y = "interest_health")
ModelGirlsHealthMother <- lme_no_ctrl(
  data = CPISGirls, x = "mother_discuss_health", y = "interest_health")
ModelBoysForeignMother <- lme_no_ctrl(
  data = CPISBoys, x = "mother_discuss_foreign", y = "interest_foreign")
ModelGirlsForeignMother <- lme_no_ctrl(
  data = CPISGirls, x = "mother_discuss_foreign", y = "interest_foreign")
ModelBoysLawMother <- lme_no_ctrl(
  data = CPISBoys, x = "mother_discuss_law", y = "interest_law")
ModelGirlsLawMother <- lme_no_ctrl(
  data = CPISGirls, x = "mother_discuss_law", y = "interest_law")
ModelBoysEducationMother <- lme_no_ctrl(
  data = CPISBoys, x = "mother_discuss_education", y = "interest_education")
ModelGirlsEducationMother <- lme_no_ctrl(
  data = CPISGirls, x = "mother_discuss_education", y = "interest_education")
ModelBoysPartisanMother <- lme_no_ctrl(
  data = CPISBoys, x = "mother_discuss_partisan", y = "interest_partisan")
ModelGirlsPartisanMother <- lme_no_ctrl(
  data = CPISGirls, x = "mother_discuss_partisan", y = "interest_partisan")
ModelBoysAllMother <- lme_no_ctrl(
  data = CPISBoysAgentsLonger, x = "value_mother", y = "interest_all")
ModelGirlsAllMother <- lme_no_ctrl(
  data = CPISGirlsAgentsLonger, x = "value_mother", y = "interest_all")
ModelYoungBoysMother <- lme_no_ctrl(
  data = CPISYoungBoysMotherLonger, x = "value_mother", y = "interest_all")
ModelYoungGirlsMother <- lme_no_ctrl(
  data = CPISYoungGirlsMotherLonger, x = "value_mother", y = "interest_all")
ModelOldBoysMother <- lme_no_ctrl(
  data = CPISOldBoysMotherLonger, x = "value_mother", y = "interest_all")
ModelOldGirlsMother <- lme_no_ctrl(
  data = CPISOldGirlsMotherLonger, x = "value_mother", y = "interest_all")
ModelBoysHealthFather <- lme_no_ctrl(
  data = CPISBoys, x = "father_discuss_health", y = "interest_health")
ModelGirlsHealthFather <- lme_no_ctrl(
  data = CPISGirls, x = "father_discuss_health", y = "interest_health")
ModelBoysForeignFather <- lme_no_ctrl(
  data = CPISBoys, x = "father_discuss_foreign", y = "interest_foreign")
ModelGirlsForeignFather <- lme_no_ctrl(
  data = CPISGirls, x = "father_discuss_foreign", y = "interest_foreign")
ModelBoysLawFather <- lme_no_ctrl(
  data = CPISBoys, x = "father_discuss_law", y = "interest_law")
ModelGirlsLawFather <- lme_no_ctrl(
  data = CPISGirls, x = "father_discuss_law", y = "interest_law")
ModelBoysEducationFather <- lme_no_ctrl(
  data = CPISBoys, x = "father_discuss_education", y = "interest_education")
ModelGirlsEducationFather <- lme_no_ctrl(
  data = CPISGirls, x = "father_discuss_education", y = "interest_education")
ModelBoysPartisanFather <- lme_no_ctrl(
  data = CPISBoys, x = "father_discuss_partisan", y = "interest_partisan")
ModelGirlsPartisanFather <- lme_no_ctrl(
  data = CPISGirls, x = "father_discuss_partisan", y = "interest_partisan")
ModelBoysAllFather <- lme_no_ctrl(
  data = CPISBoysAgentsLonger, x = "value_father", y = "interest_all")
ModelGirlsAllFather <- lme_no_ctrl(
  data = CPISGirlsAgentsLonger, x = "value_father", y = "interest_all")
ModelYoungBoysFather <- lme_no_ctrl(
  data = CPISYoungBoysFatherLonger, x = "value_father", y = "interest_all")
ModelYoungGirlsFather <- lme_no_ctrl(
  data = CPISYoungGirlsFatherLonger, x = "value_father", y = "interest_all")
ModelOldBoysFather <- lme_no_ctrl(
  data = CPISOldBoysFatherLonger, x = "value_father", y = "interest_all")
ModelOldGirlsFather <- lme_no_ctrl(
  data = CPISOldGirlsFatherLonger, x = "value_father", y = "interest_all")
ModelBoysHealthFemaleFriends <- lme_no_ctrl(
  data = CPISBoys, x = "femalefriends_discuss_health", y = "interest_health")
ModelGirlsHealthFemaleFriends <- lme_no_ctrl(
  data = CPISGirls, x = "femalefriends_discuss_health", y = "interest_health")
ModelBoysForeignFemaleFriends <- lme_no_ctrl(
  data = CPISBoys, x = "femalefriends_discuss_foreign", y = "interest_foreign")
ModelGirlsForeignFemaleFriends <- lme_no_ctrl(
  data = CPISGirls, x = "femalefriends_discuss_foreign", y = "interest_foreign")
ModelBoysLawFemaleFriends <- lme_no_ctrl(
  data = CPISBoys, x = "femalefriends_discuss_law", y = "interest_law")
ModelGirlsLawFemaleFriends <- lme_no_ctrl(
  data = CPISGirls, x = "femalefriends_discuss_law", y = "interest_law")
ModelBoysEducationFemaleFriends <- lme_no_ctrl(
  data = CPISBoys, x = "femalefriends_discuss_education", y = "interest_education")
ModelGirlsEducationFemaleFriends <- lme_no_ctrl(
  data = CPISGirls, x = "femalefriends_discuss_education", y = "interest_education")
ModelBoysPartisanFemaleFriends <- lme_no_ctrl(
  data = CPISBoys, x = "femalefriends_discuss_partisan", y = "interest_partisan")
ModelGirlsPartisanFemaleFriends <- lme_no_ctrl(
  data = CPISGirls, x = "femalefriends_discuss_partisan", y = "interest_partisan")
ModelBoysAllFemaleFriends <- lme_no_ctrl(
  data = CPISBoysAgentsLonger, x = "value_femalefriends", y = "interest_all")
ModelGirlsAllFemaleFriends <- lme_no_ctrl(
  data = CPISGirlsAgentsLonger, x = "value_femalefriends", y = "interest_all")
ModelYoungBoysFemaleFriends <- lme_no_ctrl(
  data = CPISYoungBoysFemaleFriendsLonger, x = "value_femalefriends", y = "interest_all")
ModelYoungGirlsFemaleFriends <- lme_no_ctrl(
  data = CPISYoungGirlsFemaleFriendsLonger, x = "value_femalefriends", y = "interest_all")
ModelOldBoysFemaleFriends <- lme_no_ctrl(
  data = CPISOldBoysFemaleFriendsLonger, x = "value_femalefriends", y = "interest_all")
ModelOldGirlsFemaleFriends <- lme_no_ctrl(
  data = CPISOldGirlsFemaleFriendsLonger, x = "value_femalefriends", y = "interest_all")
ModelBoysHealthMaleFriends <- lme_no_ctrl(
  data = CPISBoys, x = "malefriends_discuss_health", y = "interest_health")
ModelGirlsHealthMaleFriends <- lme_no_ctrl(
  data = CPISGirls, x = "malefriends_discuss_health", y = "interest_health")
ModelBoysForeignMaleFriends <- lme_no_ctrl(
  data = CPISBoys, x = "malefriends_discuss_foreign", y = "interest_foreign")
ModelGirlsForeignMaleFriends <- lme_no_ctrl(
  data = CPISGirls, x = "malefriends_discuss_foreign", y = "interest_foreign")
ModelBoysLawMaleFriends <- lme_no_ctrl(
  data = CPISBoys, x = "malefriends_discuss_law", y = "interest_law")
ModelGirlsLawMaleFriends <- lme_no_ctrl(
  data = CPISGirls, x = "malefriends_discuss_law", y = "interest_law")
ModelBoysEducationMaleFriends <- lme_no_ctrl(
  data = CPISBoys, x = "malefriends_discuss_education", y = "interest_education")
ModelGirlsEducationMaleFriends <- lme_no_ctrl(
  data = CPISGirls, x = "malefriends_discuss_education", y = "interest_education")
ModelBoysPartisanMaleFriends <- lme_no_ctrl(
  data = CPISBoys, x = "malefriends_discuss_partisan", y = "interest_partisan")
ModelGirlsPartisanMaleFriends <- lme_no_ctrl(
  data = CPISGirls, x = "malefriends_discuss_partisan", y = "interest_partisan")
ModelBoysAllMaleFriends <- lme_no_ctrl(
  data = CPISBoysAgentsLonger, x = "value_malefriends", y = "interest_all")
ModelGirlsAllMaleFriends <- lme_no_ctrl(
  data = CPISGirlsAgentsLonger, x = "value_malefriends", y = "interest_all")
ModelYoungBoysMaleFriends <- lme_no_ctrl(
  data = CPISYoungBoysMaleFriendsLonger, x = "value_malefriends", y = "interest_all")
ModelYoungGirlsMaleFriends <- lme_no_ctrl(
  data = CPISYoungGirlsMaleFriendsLonger, x = "value_malefriends", y = "interest_all")
ModelOldBoysMaleFriends <- lme_no_ctrl(
  data = CPISOldBoysMaleFriendsLonger, x = "value_malefriends", y = "interest_all")
ModelOldGirlsMaleFriends <- lme_no_ctrl(
  data = CPISOldGirlsMaleFriendsLonger, x = "value_malefriends", y = "interest_all")
ModelHealthSameGenderInfluencer <- lme_no_ctrl(
  data = CPIS, x = "samegenderinfluencer_discuss_health", y = "interest_health")
ModelHealthOtherGenderInfluencer <- lme_no_ctrl(
  data = CPIS, x = "othergenderinfluencer_discuss_health", y = "interest_health")
ModelForeignSameGenderInfluencer <- lme_no_ctrl(
  data = CPIS, x = "samegenderinfluencer_discuss_foreign", y = "interest_foreign")
ModelForeignOtherGenderInfluencer <- lme_no_ctrl(
  data = CPIS, x = "othergenderinfluencer_discuss_foreign", y = "interest_foreign")
ModelLawSameGenderInfluencer <- lme_no_ctrl(
  data = CPIS, x = "samegenderinfluencer_discuss_law", y = "interest_law")
ModelLawOtherGenderInfluencer <- lme_no_ctrl(
  data = CPIS, x = "othergenderinfluencer_discuss_law", y = "interest_law")
ModelEducationSameGenderInfluencer <- lme_no_ctrl(
  data = CPIS, x = "samegenderinfluencer_discuss_education", y = "interest_education")
ModelEducationOtherGenderInfluencer <- lme_no_ctrl(
  data = CPIS, x = "othergenderinfluencer_discuss_education", y = "interest_education")
ModelPartisanSameGenderInfluencer <- lme_no_ctrl(
  data = CPIS, x = "samegenderinfluencer_discuss_partisan", y = "interest_partisan")
ModelPartisanOtherGenderInfluencer <- lme_no_ctrl(
  data = CPIS, x = "othergenderinfluencer_discuss_partisan", y = "interest_partisan")
ModelAllSameGenderInfluencer <- lme_no_ctrl(
  data = CPISSameGenderInfluencerLonger, x = "value_influencer", y = "interest_all")
ModelAllOtherGenderInfluencer <- lme_no_ctrl(
  data = CPISOtherGenderInfluencerLonger, x = "value_influencer", y = "interest_all")

#### 4.4 Create models with SES only ####
lme_ses <- function(data, x, y) {
  data$x <- data[[x]]
  data$y <- data[[y]]
  nlme::lme(data = data, fixed = y ~ x + age + white + immig + lang,
            random = ~ 1 | Class, na.action = na.omit)
}
ModelInterestGenderSES <- lme_ses(data = CPIS, x = "female", y = "interest")
ModelHealthGenderSES <- lme_ses(data = CPIS, x = "female",
 y = "interest_health")
ModelForeignGenderSES <- lme_ses(data = CPIS, x = "female",
 y = "interest_foreign")
ModelLawGenderSES <- lme_ses(data = CPIS, x = "female",
 y = "interest_law")
ModelEducationGenderSES <- lme_ses(data = CPIS, x = "female",
 y = "interest_education")
ModelPartisanGenderSES <- lme_ses(data = CPIS, x = "female",
 y = "interest_partisan")
ModelBoysHealthMotherSES <- lme_ses(data = CPISBoys,
 x = "mother_discuss_health", y = "interest_health")
ModelBoysForeignMotherSES <- lme_ses(data = CPISBoys,
 x = "mother_discuss_foreign", y = "interest_foreign")
ModelBoysLawMotherSES <- lme_ses(data = CPISBoys, x = "mother_discuss_law",
 y = "interest_law")
ModelBoysEducationMotherSES <- lme_ses(data = CPISBoys,
 x = "mother_discuss_education", y = "interest_education")
ModelBoysPartisanMotherSES <- lme_ses(data = CPISBoys,
 x = "mother_discuss_partisan", y = "interest_partisan")
ModelBoysAllMotherSES <- lme_ses(data = CPISBoysMotherLonger,
 x = "value_mother", y = "interest_all")
ModelBoysHealthFatherSES <- lme_ses(data = CPISBoys,
 x = "father_discuss_health", y = "interest_health")
ModelBoysForeignFatherSES <- lme_ses(data = CPISBoys,
 x = "father_discuss_foreign", y = "interest_foreign")
ModelBoysLawFatherSES <- lme_ses(data = CPISBoys, x = "father_discuss_law",
 y = "interest_law")
ModelBoysEducationFatherSES <- lme_ses(data = CPISBoys,
 x = "father_discuss_education", y = "interest_education")
ModelBoysPartisanFatherSES <- lme_ses(data = CPISBoys,
 x = "father_discuss_partisan", y = "interest_partisan")
ModelBoysAllFatherSES <- lme_ses(data = CPISBoysFatherLonger,
 x = "value_father", y = "interest_all")
ModelGirlsHealthMotherSES <- lme_ses(data = CPISGirls,
 x = "mother_discuss_health", y = "interest_health")
ModelGirlsForeignMotherSES <- lme_ses(data = CPISGirls,
 x = "mother_discuss_foreign", y = "interest_foreign")
ModelGirlsLawMotherSES <- lme_ses(data = CPISGirls, x = "mother_discuss_law",
 y = "interest_law")
ModelGirlsEducationMotherSES <- lme_ses(data = CPISGirls,
 x = "mother_discuss_education", y = "interest_education")
ModelGirlsPartisanMotherSES <- lme_ses(data = CPISGirls,
 x = "mother_discuss_partisan", y = "interest_partisan")
ModelGirlsAllMotherSES <- lme_ses(data = CPISGirlsMotherLonger,
 x = "value_mother", y = "interest_all")
ModelGirlsHealthFatherSES <- lme_ses(data = CPISGirls,
 x = "father_discuss_health", y = "interest_health")
ModelGirlsForeignFatherSES <- lme_ses(data = CPISGirls,
 x = "father_discuss_foreign", y = "interest_foreign")
ModelGirlsLawFatherSES <- lme_ses(data = CPISGirls, x = "father_discuss_law",
 y = "interest_law")
ModelGirlsEducationFatherSES <- lme_ses(data = CPISGirls,
 x = "father_discuss_education", y = "interest_education")
ModelGirlsPartisanFatherSES <- lme_ses(data = CPISGirls,
 x = "father_discuss_partisan", y = "interest_partisan")
ModelGirlsAllFatherSES <- lme_ses(data = CPISGirlsFatherLonger,
 x = "value_father", y = "interest_all")
ModelBoysHealthMaleFriendsSES <- lme_ses(data = CPISBoys,
 x = "malefriends_discuss_health", y = "interest_health")
ModelBoysForeignMaleFriendsSES <- lme_ses(data = CPISBoys,
 x = "malefriends_discuss_foreign", y = "interest_foreign")
ModelBoysLawMaleFriendsSES <- lme_ses(data = CPISBoys, x = "malefriends_discuss_law",
 y = "interest_law")
ModelBoysEducationMaleFriendsSES <- lme_ses(data = CPISBoys,
 x = "malefriends_discuss_education", y = "interest_education")
ModelBoysPartisanMaleFriendsSES <- lme_ses(data = CPISBoys,
 x = "malefriends_discuss_partisan", y = "interest_partisan")
ModelBoysAllMaleFriendsSES <- lme_ses(data = CPISBoysMaleFriendsLonger,
 x = "value_malefriends", y = "interest_all")
ModelBoysHealthFemaleFriendsSES <- lme_ses(data = CPISBoys,
 x = "femalefriends_discuss_health", y = "interest_health")
ModelBoysForeignFemaleFriendsSES <- lme_ses(data = CPISBoys,
 x = "femalefriends_discuss_foreign", y = "interest_foreign")
ModelBoysLawFemaleFriendsSES <- lme_ses(data = CPISBoys, x = "femalefriends_discuss_law",
 y = "interest_law")
ModelBoysEducationFemaleFriendsSES <- lme_ses(data = CPISBoys,
 x = "femalefriends_discuss_education", y = "interest_education")
ModelBoysPartisanFemaleFriendsSES <- lme_ses(data = CPISBoys,
 x = "femalefriends_discuss_partisan", y = "interest_partisan")
ModelBoysAllFemaleFriendsSES <- lme_ses(data = CPISBoysFemaleFriendsLonger,
 x = "value_femalefriends", y = "interest_all")
ModelGirlsHealthMaleFriendsSES <- lme_ses(data = CPISGirls,
 x = "malefriends_discuss_health", y = "interest_health")
ModelGirlsForeignMaleFriendsSES <- lme_ses(data = CPISGirls,
 x = "malefriends_discuss_foreign", y = "interest_foreign")
ModelGirlsLawMaleFriendsSES <- lme_ses(data = CPISGirls, x = "malefriends_discuss_law",
 y = "interest_law")
ModelGirlsEducationMaleFriendsSES <- lme_ses(data = CPISGirls,
 x = "malefriends_discuss_education", y = "interest_education")
ModelGirlsPartisanMaleFriendsSES <- lme_ses(data = CPISGirls,
 x = "malefriends_discuss_partisan", y = "interest_partisan")
ModelGirlsAllMaleFriendsSES <- lme_ses(data = CPISGirlsMaleFriendsLonger,
 x = "value_malefriends", y = "interest_all")
ModelGirlsHealthFemaleFriendsSES <- lme_ses(data = CPISGirls,
 x = "femalefriends_discuss_health", y = "interest_health")
ModelGirlsForeignFemaleFriendsSES <- lme_ses(data = CPISGirls,
 x = "femalefriends_discuss_foreign", y = "interest_foreign")
ModelGirlsLawFemaleFriendsSES <- lme_ses(data = CPISGirls, x = "femalefriends_discuss_law",
 y = "interest_law")
ModelGirlsEducationFemaleFriendsSES <- lme_ses(data = CPISGirls,
 x = "femalefriends_discuss_education", y = "interest_education")
ModelGirlsPartisanFemaleFriendsSES <- lme_ses(data = CPISGirls,
 x = "femalefriends_discuss_partisan", y = "interest_partisan")
ModelGirlsAllFemaleFriendsSES <- lme_ses(data = CPISGirlsFemaleFriendsLonger,
 x = "value_femalefriends", y = "interest_all")
lme_ses_dg <- function(data, y) {
  data$y <- data[[y]]
  lm(data = data, formula = y ~ female + age + white + immig + lang +
     income_mid + income_high + educ_mid + educ_high)
}
lme_ses_dg_weighted <- function(data, y) {
  data$y <- data[[y]]
  lm(data = data, formula = y ~ female + age + white + immig + lang +
     income_mid + income_high + educ_mid + educ_high, weights = weight)
}
ModelInterestGenderDGSES <- lme_ses_dg(data = DG, y = "interest")
ModelHealthGenderDGSES <- lme_ses_dg_weighted(data = DG, y = "interest_health")
ModelForeignGenderDGSES <- lme_ses_dg_weighted(
  data = DG, y = "interest_foreign")
ModelLawGenderDGSES <- lme_ses_dg(data = DG, y = "interest_law")
ModelEducationGenderDGSES <- lme_ses_dg_weighted(
  data = DG, y = "interest_education")
ModelPartisanGenderDGSES <- lme_ses_dg_weighted(
  data = DG, y = "interest_partisan")

#### 4.5 Create models with SES and interactions ####
lme_ses_interactions <- function(data, y) {
  data$y <- data[[y]]
  nlme::lme(data = data, fixed = y ~ female * age + age_squared +
            female * white + immig + lang,
            random = ~ 1 | Class, na.action = na.omit)
}
ModelInterestGenderSESInterac <- lme_ses_interactions(
  data = CPIS, y = "interest")
ModelHealthGenderSESInterac <- lme_ses_interactions(
  data = CPIS, y = "interest_health")
ModelForeignGenderSESInterac <- lme_ses_interactions(
  data = CPIS, y = "interest_foreign")
ModelLawGenderSESInterac <- lme_ses_interactions(
  data = CPIS, y = "interest_law")
ModelEducationGenderSESInterac <- lme_ses_interactions(
  data = CPIS, y = "interest_education")
ModelPartisanGenderSESInterac <- lme_ses_interactions(
  data = CPIS, y = "interest_partisan")
lme_ses_interactions_parent <- function(data, x, y) {
  data$x <- data[[x]]
  data$y <- data[[y]]
  nlme::lme(data = data, fixed = y ~ x + age + age_squared +
            white + immig + lang,
            random = ~ 1 | Class, na.action = na.omit)
}
ModelBoysHealthGenderParentSESInterac <- lme_ses_interactions_parent(
  data = CPISBoys, x = "gender_parent_health", y = "interest_health")
ModelGirlsHealthGenderParentSESInterac <- lme_ses_interactions_parent(
  data = CPISGirls, x = "gender_parent_health", y = "interest_health")
ModelBoysForeignGenderParentSESInterac <- lme_ses_interactions_parent(
  data = CPISBoys, x = "gender_parent_foreign", y = "interest_foreign")
ModelGirlsForeignGenderParentSESInterac <- lme_ses_interactions_parent(
  data = CPISGirls, x = "gender_parent_foreign", y = "interest_foreign")
ModelBoysLawGenderParentSESInterac <- lme_ses_interactions_parent(
  data = CPISBoys, x = "gender_parent_law", y = "interest_law")
ModelGirlsLawGenderParentSESInterac <- lme_ses_interactions_parent(
  data = CPISGirls, x = "gender_parent_law", y = "interest_law")
ModelBoysEducationGenderParentSESInterac <- lme_ses_interactions_parent(
  data = CPISBoys, x = "gender_parent_education", y = "interest_education")
ModelGirlsEducationGenderParentSESInterac <- lme_ses_interactions_parent(
  data = CPISGirls, x = "gender_parent_education", y = "interest_education")
ModelBoysPartisanGenderParentSESInterac <- lme_ses_interactions_parent(
  data = CPISBoys, x = "gender_parent_partisan", y = "interest_partisan")
ModelGirlsPartisanGenderParentSESInterac <- lme_ses_interactions_parent(
  data = CPISGirls, x = "gender_parent_partisan", y = "interest_partisan")
ModelBoysAllGenderParentSESInterac <- lme_ses_interactions_parent(
  data = CPISBoysParentLonger, x = "value_parent", y = "interest_all")
ModelGirlsAllGenderParentSESInterac <- lme_ses_interactions_parent(
  data = CPISGirlsParentLonger, x = "value_parent", y = "interest_all")
ModelYoungBoysGenderParentSESInterac <- lme_ses_interactions_parent(
  data = CPISYoungBoysParentLonger, x = "value_parent", y = "interest_all")
ModelYoungGirlsGenderParentSESInterac <- lme_ses_interactions_parent(
  data = CPISYoungGirlsParentLonger, x = "value_parent", y = "interest_all")
ModelOldBoysGenderParentSESInterac <- lme_ses_interactions_parent(
  data = CPISOldBoysParentLonger, x = "value_parent", y = "interest_all")
ModelOldGirlsGenderParentSESInterac <- lme_ses_interactions_parent(
  data = CPISOldGirlsParentLonger, x = "value_parent", y = "interest_all")
lme_ses_agesquared_dg_weighted <- function(data, y) {
  data$y <- data[[y]]
  lm(data = data, formula = y ~ female + age + age_squared + white +
     immig + lang + income_mid + income_high + educ_mid + educ_high,
     weights = weight)
}
lme_ses_femaleage_dg_weighted <- function(data, y) {
  data$y <- data[[y]]
  lm(data = data, formula = y ~ female * age + white +
     immig + lang + income_mid + income_high + educ_mid + educ_high,
     weights = weight)
}
lme_ses_femalewhite_dg_weighted <- function(data, y) {
  data$y <- data[[y]]
  lm(data = data, formula = y ~ female * white + age +
     immig + lang + income_mid + income_high + educ_mid + educ_high,
     weights = weight)
}
summary(lme_ses_agesquared_dg_weighted(
  data = DG, y = "interest_education"))
summary(lme_ses_femaleage_dg_weighted(
  data = DG, y = "interest_education"))
summary(lme_ses_femalewhite_dg_weighted(
  data = DG, y = "interest_education"))
lme_ses_interactions_dg <- function(data, y) {
  data$y <- data[[y]]
  lm(data = data, formula = y ~ female * age + age_squared + female * white +
     immig + lang + income_mid + income_high + educ_mid + educ_high)
}
lme_ses_interactions_dg_weighted <- function(data, y) {
  data$y <- data[[y]]
  lm(data = data, formula = y ~ female * age + age_squared + female * white +
     immig + lang + income_mid + income_high + educ_mid + educ_high,
     weights = weight)
}
ModelInterestGenderDGSESInterac <- lme_ses_interactions_dg(
  data = DG, y = "interest")
ModelHealthGenderDGSESInterac <- lme_ses_interactions_dg_weighted(
  data = DG, y = "interest_health")
ModelForeignGenderDGSESInterac <- lme_ses_interactions_dg_weighted(
  data = DG, y = "interest_foreign")
ModelLawGenderDGSESInterac <- lme_ses_interactions_dg(
  data = DG, y = "interest_law")
ModelEducationGenderDGSESInterac <- lme_ses_interactions_dg_weighted(
  data = DG, y = "interest_education")
ModelPartisanGenderDGSESInterac <- lme_ses_interactions_dg_weighted(
  data = DG, y = "interest_partisan")
lme_ses_boysgirls <- function(data, x, y) {
  data$x <- data[[x]]
  data$y <- data[[y]]
  nlme::lme(data = data, fixed = y ~ x + female * age + age_squared +
            female * white + immig + lang, random = ~ 1 |
            Class, na.action = na.omit)
}
ModelHealthGenderParentSES <- lme_ses_boysgirls(
  data = CPIS, x = "gender_parent_health", y = "interest_health")
ModelForeignGenderParentSES <- lme_ses_boysgirls(
  data = CPIS, x = "gender_parent_foreign", y = "interest_foreign")
ModelLawGenderParentSES <- lme_ses_boysgirls(
  data = CPIS, x = "gender_parent_law", y = "interest_law")
ModelEducationGenderParentSES <- lme_ses_boysgirls(
  data = CPIS, x = "gender_parent_education", y = "interest_education")
ModelPartisanGenderParentSES <- lme_ses_boysgirls(
  data = CPIS, x = "gender_parent_partisan", y = "interest_partisan")
ModelAllGenderParentSES <- lme_ses_boysgirls(
  data = CPISParentLonger, x = "value_parent", y = "interest_all")

#### 4.6 Create models with SES, interactions and all agents ####
lme_ses_allagents <- function(data, x1, x2, x3, x4, x5, x6, y) {
  data$x1 <- data[[x1]]
  data$x2 <- data[[x2]]
  data$x3 <- data[[x3]]
  data$x4 <- data[[x4]]
  data$x5 <- data[[x5]]
  data$x6 <- data[[x6]]
  data$y <- data[[y]]
  nlme::lme(data = data, fixed = y ~ x1 + x2 + x3 + x4 + x5 + x6 + age +
            age_squared + white + immig + lang,
            random = ~ 1 | Class, na.action = na.omit)
}
ModelBoysHealthAgentsSES <- lme_ses_allagents(
  data = CPISBoys, x1 = "mother_discuss_health", x2 = "father_discuss_health",
  x3 = "femalefriends_discuss_health", x4 = "malefriends_discuss_health",
  x5 = "teacher_discuss_health", x6 = "influencer_discuss_health",
  y = "interest_health")
ModelGirlsHealthAgentsSES <- lme_ses_allagents(
  data = CPISGirls, x1 = "mother_discuss_health", x2 = "father_discuss_health",
  x3 = "femalefriends_discuss_health", x4 = "malefriends_discuss_health",
  x5 = "teacher_discuss_health", x6 = "influencer_discuss_health",
  y = "interest_health")
ModelBoysForeignAgentsSES <- lme_ses_allagents(
  data = CPISBoys, x1 = "mother_discuss_foreign", x2 = "father_discuss_foreign",
  x3 = "femalefriends_discuss_foreign", x4 = "malefriends_discuss_foreign",
  x5 = "teacher_discuss_foreign", x6 = "influencer_discuss_foreign",
  y = "interest_foreign")
ModelGirlsForeignAgentsSES <- lme_ses_allagents(
  data = CPISGirls, x1 = "mother_discuss_foreign", x2 = "father_discuss_foreign",
  x3 = "femalefriends_discuss_foreign", x4 = "malefriends_discuss_foreign",
  x5 = "teacher_discuss_foreign", x6 = "influencer_discuss_foreign",
  y = "interest_foreign")
ModelBoysLawAgentsSES <- lme_ses_allagents(
  data = CPISBoys, x1 = "mother_discuss_law", x2 = "father_discuss_law",
  x3 = "femalefriends_discuss_law", x4 = "malefriends_discuss_law",
  x5 = "teacher_discuss_law", x6 = "influencer_discuss_law",
  y = "interest_law")
ModelGirlsLawAgentsSES <- lme_ses_allagents(
  data = CPISGirls, x1 = "mother_discuss_law", x2 = "father_discuss_law",
  x3 = "femalefriends_discuss_law", x4 = "malefriends_discuss_law",
  x5 = "teacher_discuss_law", x6 = "influencer_discuss_law",
  y = "interest_law")
ModelBoysEducationAgentsSES <- lme_ses_allagents(
  data = CPISBoys, x1 = "mother_discuss_education", x2 = "father_discuss_education",
  x3 = "femalefriends_discuss_education", x4 = "malefriends_discuss_education",
  x5 = "teacher_discuss_education", x6 = "influencer_discuss_education",
  y = "interest_education")
ModelGirlsEducationAgentsSES <- lme_ses_allagents(
  data = CPISGirls, x1 = "mother_discuss_education", x2 = "father_discuss_education",
  x3 = "femalefriends_discuss_education", x4 = "malefriends_discuss_education",
  x5 = "teacher_discuss_education", x6 = "influencer_discuss_education",
  y = "interest_education")
ModelBoysPartisanAgentsSES <- lme_ses_allagents(
  data = CPISBoys, x1 = "mother_discuss_partisan", x2 = "father_discuss_partisan",
  x3 = "femalefriends_discuss_partisan", x4 = "malefriends_discuss_partisan",
  x5 = "teacher_discuss_partisan", x6 = "influencer_discuss_partisan",
  y = "interest_partisan")
#ModelGirlsPartisanAgentsSES <- lme_ses_allagents(
#  data = CPISGirls, x1 = "mother_discuss_partisan", x2 = "father_discuss_partisan",
#  x3 = "femalefriends_discuss_partisan", x4 = "malefriends_discuss_partisan",
#  x5 = "teacher_discuss_partisan", x6 = "influencer_discuss_partisan",
#  y = "interest_partisan") # error
ModelGirlsPartisanAgentsSES <- lme4::lmer(
  data = CPISGirls, formula = interest_partisan ~ mother_discuss_partisan +
  father_discuss_partisan + femalefriends_discuss_partisan +
  malefriends_discuss_partisan + teacher_discuss_partisan +
  influencer_discuss_partisan + age + age_squared + white + immig + lang +
  (1 | Class), na.action = na.omit)
ModelBoysAllAgentsSES <- lme_ses_allagents(
  data = CPISBoysAgentsLonger, x1 = "value_mother", x2 = "value_father",
  x3 = "value_femalefriends", x4 = "value_malefriends", x5 = "value_teacher",
  x6 = "value_influencer", y = "interest_all")
ModelGirlsAllAgentsSES <- lme_ses_allagents(
  data = CPISGirlsAgentsLonger, x1 = "value_mother", x2 = "value_father",
  x3 = "value_femalefriends", x4 = "value_malefriends", x5 = "value_teacher",
  x6 = "value_influencer", y = "interest_all")
ModelYoungBoysAgentsSES <- lme_ses_allagents(
  data = CPISYoungBoysAgentsLonger, x1 = "value_mother", x2 = "value_father",
  x3 = "value_femalefriends", x4 = "value_malefriends", x5 = "value_teacher",
  x6 = "value_influencer", y = "interest_all")
ModelYoungGirlsAgentsSES <- lme_ses_allagents(
  data = CPISYoungGirlsAgentsLonger, x1 = "value_mother", x2 = "value_father",
  x3 = "value_femalefriends", x4 = "value_malefriends", x5 = "value_teacher",
  x6 = "value_influencer", y = "interest_all")
ModelOldBoysAgentsSES <- lme_ses_allagents(
  data = CPISOldBoysAgentsLonger, x1 = "value_mother", x2 = "value_father",
  x3 = "value_femalefriends", x4 = "value_malefriends", x5 = "value_teacher",
  x6 = "value_influencer", y = "interest_all")
ModelOldGirlsAgentsSES <- lme_ses_allagents(
  data = CPISOldGirlsAgentsLonger, x1 = "value_mother", x2 = "value_father",
  x3 = "value_femalefriends", x4 = "value_malefriends", x5 = "value_teacher",
  x6 = "value_influencer", y = "interest_all")
lme_ses_allagents_boysgirls <- function(
  data, x1, x2, x3, x4, x5, x6, y) {
    data$x1 <- data[[x1]]
    data$x2 <- data[[x2]]
    data$x3 <- data[[x3]]
    data$x4 <- data[[x4]]
    data$x5 <- data[[x5]]
    data$x6 <- data[[x6]]
    data$y <- data[[y]]
    nlme::lme(data = data, fixed = y ~ female * x1 + female * x2 + female * x3 +
              female * x4 + female * x5 + female * x6 + female * age +
              age_squared + female * white + immig + lang,
              random = ~ 1 | Class, na.action = na.omit)
}
ModelHealthAgentsSES <- lme_ses_allagents_boysgirls(
  data = CPIS, x1 = "mother_discuss_health", x2 = "father_discuss_health",
  x3 = "femalefriends_discuss_health", x4 = "malefriends_discuss_health",
  x5 = "teacher_discuss_health", x6 = "influencer_discuss_health",
  y = "interest_health")
ModelForeignAgentsSES <- lme_ses_allagents_boysgirls(
  data = CPIS, x1 = "mother_discuss_foreign", x2 = "father_discuss_foreign",
  x3 = "femalefriends_discuss_foreign", x4 = "malefriends_discuss_foreign",
  x5 = "teacher_discuss_foreign", x6 = "influencer_discuss_foreign",
  y = "interest_foreign")
ModelLawAgentsSES <- lme_ses_allagents_boysgirls(
  data = CPIS, x1 = "mother_discuss_law", x2 = "father_discuss_law",
  x3 = "femalefriends_discuss_law", x4 = "malefriends_discuss_law",
  x5 = "teacher_discuss_law", x6 = "influencer_discuss_law",
  y = "interest_law")
ModelEducationAgentsSES <- lme_ses_allagents_boysgirls(
  data = CPIS, x1 = "mother_discuss_education", x2 = "father_discuss_education",
  x3 = "femalefriends_discuss_education", x4 = "malefriends_discuss_education",
  x5 = "teacher_discuss_education", x6 = "influencer_discuss_education",
  y = "interest_education")
#ModelPartisanAgentsSES <- lme_ses_allagents_boysgirls(
#  data = CPIS, x1 = "mother_discuss_partisan", x2 = "father_discuss_partisan",
#  x3 = "femalefriends_discuss_partisan", x4 = "malefriends_discuss_partisan",
#  x5 = "teacher_discuss_partisan", x6 = "influencer_discuss_partisan",
#  y = "interest_partisan")
ModelPartisanAgentsSES <- lme4::lmer(
  data = CPIS, formula = interest_partisan ~ female * mother_discuss_partisan +
  female * father_discuss_partisan + female * femalefriends_discuss_partisan +
  female * malefriends_discuss_partisan + female * teacher_discuss_partisan +
  female * influencer_discuss_partisan + female * age + age_squared +
  female * white + immig + lang + (1 | Class), na.action = na.omit)
ModelAllAgentsSES <- lme_ses_allagents_boysgirls(
  data = CPISAgentsLonger, x1 = "value_mother", x2 = "value_father",
  x3 = "value_femalefriends", x4 = "value_malefriends", x5 = "value_teacher",
  x6 = "value_influencer", y = "interest_all")

#### 4.7 Create models with SES and personality ####
lme_ses_personality <- function(data, x, y) {
  data$x <- data[[x]]
  data$y <- data[[y]]
  nlme::lme(data = data, fixed = y ~ x + age + white + immig + lang + agentic +
            communal, random = ~ 1 | Class, na.action = na.omit)
}
ModelInterestGenderSESPersonality <- lme_ses_personality(
  data = CPIS, x = "female", y = "interest")
ModelHealthGenderSESPersonality <- lme_ses_personality(
  data = CPIS, x = "female", y = "interest_health")
ModelForeignGenderSESPersonality <- lme_ses_personality(
  data = CPIS, x = "female", y = "interest_foreign")
ModelLawGenderSESPersonality <- lme_ses_personality(
  data = CPIS, x = "female", y = "interest_law")
ModelEducationGenderSESPersonality <- lme_ses_personality(
  data = CPIS, x = "female", y = "interest_education")
ModelPartisanGenderSESPersonality <- lme_ses_personality(
  data = CPIS, x = "female", y = "interest_partisan")
ModelBoysHealthGenderParentCtrl <- lme_ses_personality(
  data = CPISBoys, x = "gender_parent_health", y = "interest_health")
ModelGirlsHealthGenderParentCtrl <- lme_ses_personality(
  data = CPISGirls, x = "gender_parent_health", y = "interest_health")
ModelBoysForeignGenderParentCtrl <- lme_ses_personality(
  data = CPISBoys, x = "gender_parent_foreign", y = "interest_foreign")
ModelGirlsForeignGenderParentCtrl <- lme_ses_personality(
  data = CPISGirls, x = "gender_parent_foreign", y = "interest_foreign")
ModelBoysLawGenderParentCtrl <- lme_ses_personality(
  data = CPISBoys, x = "gender_parent_law", y = "interest_law")
ModelGirlsLawGenderParentCtrl <- lme_ses_personality(
  data = CPISGirls, x = "gender_parent_law", y = "interest_law")
ModelBoysEducationGenderParentCtrl <- lme_ses_personality(
  data = CPISBoys, x = "gender_parent_education", y = "interest_education")
ModelGirlsEducationGenderParentCtrl <- lme_ses_personality(
  data = CPISGirls, x = "gender_parent_education", y = "interest_education")
ModelBoysPartisanGenderParentCtrl <- lme_ses_personality(
  data = CPISBoys, x = "gender_parent_partisan", y = "interest_partisan")
ModelGirlsPartisanGenderParentCtrl <- lme_ses_personality(
  data = CPISGirls, x = "gender_parent_partisan", y = "interest_partisan")
ModelBoysAllGenderParentCtrl <- lme_ses_personality(
  data = CPISBoysParentLonger, x = "value_parent", y = "interest_all")
ModelGirlsAllGenderParentCtrl <- lme_ses_personality(
  data = CPISGirlsParentLonger, x = "value_parent", y = "interest_all")
ModelYoungBoysGenderParentCtrl <- lme_ses_personality(
  data = CPISYoungBoysParentLonger, x = "value_parent", y = "interest_all")
ModelYoungGirlsGenderParentCtrl <- lme_ses_personality(
  data = CPISYoungGirlsParentLonger, x = "value_parent", y = "interest_all")
ModelOldBoysGenderParentCtrl <- lme_ses_personality(
  data = CPISOldBoysParentLonger, x = "value_parent", y = "interest_all")
ModelOldGirlsGenderParentCtrl <- lme_ses_personality(
  data = CPISOldGirlsParentLonger, x = "value_parent", y = "interest_all")
ModelBoysHealthMotherSESPersonality <- lme_ses_personality(
  data = CPISBoys, x = "mother_discuss_health", y = "interest_health")
ModelBoysForeignMotherSESPersonality <- lme_ses_personality(
  data = CPISBoys, x = "mother_discuss_foreign", y = "interest_foreign")
ModelBoysLawMotherSESPersonality <- lme_ses_personality(
  data = CPISBoys, x = "mother_discuss_law", y = "interest_law")
ModelBoysEducationMotherSESPersonality <- lme_ses_personality(
  data = CPISBoys, x = "mother_discuss_education", y = "interest_education")
ModelBoysPartisanMotherSESPersonality <- lme_ses_personality(
  data = CPISBoys, x = "mother_discuss_partisan", y = "interest_partisan")
ModelBoysAllMotherSESPersonality <- lme_ses_personality(
  data = CPISBoysMotherLonger, x = "value_mother", y = "interest_all")
ModelBoysHealthFatherSESPersonality <- lme_ses_personality(
  data = CPISBoys, x = "father_discuss_health", y = "interest_health")
ModelBoysForeignFatherSESPersonality <- lme_ses_personality(
  data = CPISBoys, x = "father_discuss_foreign", y = "interest_foreign")
ModelBoysLawFatherSESPersonality <- lme_ses_personality(
  data = CPISBoys, x = "father_discuss_law", y = "interest_law")
ModelBoysEducationFatherSESPersonality <- lme_ses_personality(
  data = CPISBoys, x = "father_discuss_education", y = "interest_education")
ModelBoysPartisanFatherSESPersonality <- lme_ses_personality(
  data = CPISBoys, x = "father_discuss_partisan", y = "interest_partisan")
ModelBoysAllFatherSESPersonality <- lme_ses_personality(
  data = CPISBoysFatherLonger, x = "value_father", y = "interest_all")
ModelGirlsHealthMotherSESPersonality <- lme_ses_personality(
  data = CPISGirls, x = "mother_discuss_health", y = "interest_health")
ModelGirlsForeignMotherSESPersonality <- lme_ses_personality(
  data = CPISGirls, x = "mother_discuss_foreign", y = "interest_foreign")
ModelGirlsLawMotherSESPersonality <- lme_ses_personality(
  data = CPISGirls, x = "mother_discuss_law", y = "interest_law")
ModelGirlsEducationMotherSESPersonality <- lme_ses_personality(
  data = CPISGirls, x = "mother_discuss_education", y = "interest_education")
ModelGirlsPartisanMotherSESPersonality <- lme_ses_personality(
  data = CPISGirls, x = "mother_discuss_partisan", y = "interest_partisan")
ModelGirlsAllMotherSESPersonality <- lme_ses_personality(
  data = CPISGirlsMotherLonger, x = "value_mother", y = "interest_all")
ModelGirlsHealthFatherSESPersonality <- lme_ses_personality(
  data = CPISGirls, x = "father_discuss_health", y = "interest_health")
ModelGirlsForeignFatherSESPersonality <- lme_ses_personality(
  data = CPISGirls, x = "father_discuss_foreign", y = "interest_foreign")
ModelGirlsLawFatherSESPersonality <- lme_ses_personality(
  data = CPISGirls, x = "father_discuss_law", y = "interest_law")
ModelGirlsEducationFatherSESPersonality <- lme_ses_personality(
  data = CPISGirls, x = "father_discuss_education", y = "interest_education")
ModelGirlsPartisanFatherSESPersonality <- lme_ses_personality(
  data = CPISGirls, x = "father_discuss_partisan", y = "interest_partisan")
ModelGirlsAllFatherSESPersonality <- lme_ses_personality(
  data = CPISGirlsFatherLonger, x = "value_father", y = "interest_all")
ModelBoysHealthMaleFriendsSESPersonality <- lme_ses_personality(data = CPISBoys,
 x = "malefriends_discuss_health", y = "interest_health")
ModelBoysForeignMaleFriendsSESPersonality <- lme_ses_personality(data = CPISBoys,
 x = "malefriends_discuss_foreign", y = "interest_foreign")
ModelBoysLawMaleFriendsSESPersonality <- lme_ses_personality(data = CPISBoys, x = "malefriends_discuss_law",
 y = "interest_law")
ModelBoysEducationMaleFriendsSESPersonality <- lme_ses_personality(data = CPISBoys,
 x = "malefriends_discuss_education", y = "interest_education")
ModelBoysPartisanMaleFriendsSESPersonality <- lme_ses_personality(data = CPISBoys,
 x = "malefriends_discuss_partisan", y = "interest_partisan")
ModelBoysAllMaleFriendsSESPersonality <- lme_ses_personality(data = CPISBoysMaleFriendsLonger,
 x = "value_malefriends", y = "interest_all")
ModelBoysHealthFemaleFriendsSESPersonality <- lme_ses_personality(data = CPISBoys,
 x = "femalefriends_discuss_health", y = "interest_health")
ModelBoysForeignFemaleFriendsSESPersonality <- lme_ses_personality(data = CPISBoys,
 x = "femalefriends_discuss_foreign", y = "interest_foreign")
ModelBoysLawFemaleFriendsSESPersonality <- lme_ses_personality(data = CPISBoys, x = "femalefriends_discuss_law",
 y = "interest_law")
ModelBoysEducationFemaleFriendsSESPersonality <- lme_ses_personality(data = CPISBoys,
 x = "femalefriends_discuss_education", y = "interest_education")
ModelBoysPartisanFemaleFriendsSESPersonality <- lme_ses_personality(data = CPISBoys,
 x = "femalefriends_discuss_partisan", y = "interest_partisan")
ModelBoysAllFemaleFriendsSESPersonality <- lme_ses_personality(data = CPISBoysFemaleFriendsLonger,
 x = "value_femalefriends", y = "interest_all")
ModelGirlsHealthMaleFriendsSESPersonality <- lme_ses_personality(data = CPISGirls,
 x = "malefriends_discuss_health", y = "interest_health")
ModelGirlsForeignMaleFriendsSESPersonality <- lme_ses_personality(data = CPISGirls,
 x = "malefriends_discuss_foreign", y = "interest_foreign")
ModelGirlsLawMaleFriendsSESPersonality <- lme_ses_personality(data = CPISGirls, x = "malefriends_discuss_law",
 y = "interest_law")
ModelGirlsEducationMaleFriendsSESPersonality <- lme_ses_personality(data = CPISGirls,
 x = "malefriends_discuss_education", y = "interest_education")
ModelGirlsPartisanMaleFriendsSESPersonality <- lme_ses_personality(data = CPISGirls,
 x = "malefriends_discuss_partisan", y = "interest_partisan")
ModelGirlsAllMaleFriendsSESPersonality <- lme_ses_personality(data = CPISGirlsMaleFriendsLonger,
 x = "value_malefriends", y = "interest_all")
ModelGirlsHealthFemaleFriendsSESPersonality <- lme_ses_personality(data = CPISGirls,
 x = "femalefriends_discuss_health", y = "interest_health")
ModelGirlsForeignFemaleFriendsSESPersonality <- lme_ses_personality(data = CPISGirls,
 x = "femalefriends_discuss_foreign", y = "interest_foreign")
ModelGirlsLawFemaleFriendsSESPersonality <- lme_ses_personality(data = CPISGirls, x = "femalefriends_discuss_law",
 y = "interest_law")
ModelGirlsEducationFemaleFriendsSESPersonality <- lme_ses_personality(data = CPISGirls,
 x = "femalefriends_discuss_education", y = "interest_education")
ModelGirlsPartisanFemaleFriendsSESPersonality <- lme_ses_personality(data = CPISGirls,
 x = "femalefriends_discuss_partisan", y = "interest_partisan")
ModelGirlsAllFemaleFriendsSESPersonality <- lme_ses_personality(data = CPISGirlsFemaleFriendsLonger,
 x = "value_femalefriends", y = "interest_all")

#### 4.8 Create models with SES, personality and interactions ####
lme_ses_personality_boysgirls <- function(data, x, y) {
  data$x <- data[[x]]
  data$y <- data[[y]]
  nlme::lme(data = data, fixed = y ~ x + female * age + age_squared +
            female * white + immig + lang + agentic + communal, random = ~ 1 |
            Class, na.action = na.omit)
}
ModelHealthGenderParentCtrl <- lme_ses_personality_boysgirls(
  data = CPIS, x = "gender_parent_health", y = "interest_health")
ModelForeignGenderParentCtrl <- lme_ses_personality_boysgirls(
  data = CPIS, x = "gender_parent_foreign", y = "interest_foreign")
ModelLawGenderParentCtrl <- lme_ses_personality_boysgirls(
  data = CPIS, x = "gender_parent_law", y = "interest_law")
ModelEducationGenderParentCtrl <- lme_ses_personality_boysgirls(
  data = CPIS, x = "gender_parent_education", y = "interest_education")
ModelPartisanGenderParentCtrl <- lme_ses_personality_boysgirls(
  data = CPIS, x = "gender_parent_partisan", y = "interest_partisan")
ModelAllGenderParentCtrl <- lme_ses_personality_boysgirls(
  data = CPISParentLonger, x = "value_parent", y = "interest_all")
lme_ses_personality_agesquared <- function(data, y) {
  data$y <- data[[y]]
  nlme::lme(data = data, fixed = y ~ female + age + age_squared +
            white + immig + lang + agentic + communal,
            random = ~ 1 | Class, na.action = na.omit)
}
lme_ses_personality_femaleage <- function(data, y) {
  data$y <- data[[y]]
  nlme::lme(data = data, fixed = y ~ female * age +
            white + immig + lang + agentic + communal,
            random = ~ 1 | Class, na.action = na.omit)
}
lme_ses_personality_femalewhite <- function(data, y) {
  data$y <- data[[y]]
  nlme::lme(data = data, fixed = y ~ female * white + age +
            immig + lang + agentic + communal,
            random = ~ 1 | Class, na.action = na.omit)
}
summary(lme_ses_personality_agesquared(
  data = CPIS, y = "interest_partisan"))
summary(lme_ses_personality_femaleage(
  data = CPIS, y = "interest_partisan"))
summary(lme_ses_personality_femalewhite(
  data = CPIS, y = "interest_partisan"))
summary(lme_ses_personality_agesquared(
  data = CPIS, y = "interest_foreign"))
summary(lme_ses_personality_femaleage(
  data = CPIS, y = "interest_foreign"))
summary(lme_ses_personality_femalewhite(
  data = CPIS, y = "interest_foreign"))
lme_ses_personality_interactions <- function(data, y) {
  data$y <- data[[y]]
  nlme::lme(data = data, fixed = y ~ female * age + age_squared +
            female * white + immig + lang + agentic + communal,
            random = ~ 1 | Class, na.action = na.omit)
}
ModelInterestGenderCtrl <- lme_ses_personality_interactions(
  data = CPIS, y = "interest")
ModelHealthGenderCtrl <- lme_ses_personality_interactions(
  data = CPIS, y = "interest_health")
ModelForeignGenderCtrl <- lme_ses_personality_interactions(
  data = CPIS, y = "interest_foreign")
ModelLawGenderCtrl <- lme_ses_personality_interactions(
  data = CPIS, y = "interest_law")
ModelEducationGenderCtrl <- lme_ses_personality_interactions(
  data = CPIS, y = "interest_education")
ModelPartisanGenderCtrl <- lme_ses_personality_interactions(
  data = CPIS, y = "interest_partisan")
ModelInterestGenderYoungCtrl <- lme_ses_personality_interactions(
  data = CPISYoung, y = "interest")
ModelHealthGenderYoungCtrl <- lme_ses_personality_interactions(
  data = CPISYoung, y = "interest_health")
ModelForeignGenderYoungCtrl <- lme_ses_personality_interactions(
  data = CPISYoung, y = "interest_foreign")
ModelLawGenderYoungCtrl <- lme_ses_personality_interactions(
  data = CPISYoung, y = "interest_law")
ModelEducationGenderYoungCtrl <- lme_ses_personality_interactions(
  data = CPISYoung, y = "interest_education")
ModelPartisanGenderYoungCtrl <- lme_ses_personality_interactions(
  data = CPISYoung, y = "interest_partisan")
ModelInterestGenderOldCtrl <- lme_ses_personality_interactions(
  data = CPISOld, y = "interest")
ModelHealthGenderOldCtrl <- lme_ses_personality_interactions(
  data = CPISOld, y = "interest_health")
ModelForeignGenderOldCtrl <- lme_ses_personality_interactions(
  data = CPISOld, y = "interest_foreign")
ModelLawGenderOldCtrl <- lme_ses_personality_interactions(
  data = CPISOld, y = "interest_law")
ModelEducationGenderOldCtrl <- lme_ses_personality_interactions(
  data = CPISOld, y = "interest_education")
ModelPartisanGenderOldCtrl <- lme_ses_personality_interactions(
  data = CPISOld, y = "interest_partisan")
lme_ses_personality_allagents <- function(data, x1, x2, x3, x4, x5, x6, y) {
  data$x1 <- data[[x1]]
  data$x2 <- data[[x2]]
  data$x3 <- data[[x3]]
  data$x4 <- data[[x4]]
  data$x5 <- data[[x5]]
  data$x6 <- data[[x6]]
  data$y <- data[[y]]
  nlme::lme(data = data, fixed = y ~ x1 + x2 + x3 + x4 + x5 + x6 + age +
            age_squared + white + immig + lang + agentic + communal,
            random = ~ 1 | Class, na.action = na.omit)
}
ModelBoysHealthAgentsCtrl <- lme_ses_personality_allagents(
  data = CPISBoys, x1 = "mother_discuss_health", x2 = "father_discuss_health",
  x3 = "femalefriends_discuss_health", x4 = "malefriends_discuss_health",
  x5 = "teacher_discuss_health", x6 = "influencer_discuss_health",
  y = "interest_health")
ModelGirlsHealthAgentsCtrl <- lme_ses_personality_allagents(
  data = CPISGirls, x1 = "mother_discuss_health", x2 = "father_discuss_health",
  x3 = "femalefriends_discuss_health", x4 = "malefriends_discuss_health",
  x5 = "teacher_discuss_health", x6 = "influencer_discuss_health",
  y = "interest_health")
ModelBoysForeignAgentsCtrl <- lme_ses_personality_allagents(
  data = CPISBoys, x1 = "mother_discuss_foreign", x2 = "father_discuss_foreign",
  x3 = "femalefriends_discuss_foreign", x4 = "malefriends_discuss_foreign",
  x5 = "teacher_discuss_foreign", x6 = "influencer_discuss_foreign",
  y = "interest_foreign")
ModelGirlsForeignAgentsCtrl <- lme_ses_personality_allagents(
  data = CPISGirls, x1 = "mother_discuss_foreign", x2 = "father_discuss_foreign",
  x3 = "femalefriends_discuss_foreign", x4 = "malefriends_discuss_foreign",
  x5 = "teacher_discuss_foreign", x6 = "influencer_discuss_foreign",
  y = "interest_foreign")
ModelBoysLawAgentsCtrl <- lme_ses_personality_allagents(
  data = CPISBoys, x1 = "mother_discuss_law", x2 = "father_discuss_law",
  x3 = "femalefriends_discuss_law", x4 = "malefriends_discuss_law",
  x5 = "teacher_discuss_law", x6 = "influencer_discuss_law",
  y = "interest_law")
ModelGirlsLawAgentsCtrl <- lme_ses_personality_allagents(
  data = CPISGirls, x1 = "mother_discuss_law", x2 = "father_discuss_law",
  x3 = "femalefriends_discuss_law", x4 = "malefriends_discuss_law",
  x5 = "teacher_discuss_law", x6 = "influencer_discuss_law",
  y = "interest_law")
ModelBoysEducationAgentsCtrl <- lme_ses_personality_allagents(
  data = CPISBoys, x1 = "mother_discuss_education", x2 = "father_discuss_education",
  x3 = "femalefriends_discuss_education", x4 = "malefriends_discuss_education",
  x5 = "teacher_discuss_education", x6 = "influencer_discuss_education",
  y = "interest_education")
ModelGirlsEducationAgentsCtrl <- lme_ses_personality_allagents(
  data = CPISGirls, x1 = "mother_discuss_education", x2 = "father_discuss_education",
  x3 = "femalefriends_discuss_education", x4 = "malefriends_discuss_education",
  x5 = "teacher_discuss_education", x6 = "influencer_discuss_education",
  y = "interest_education")
ModelBoysPartisanAgentsCtrl <- lme_ses_personality_allagents(
  data = CPISBoys, x1 = "mother_discuss_partisan", x2 = "father_discuss_partisan",
  x3 = "femalefriends_discuss_partisan", x4 = "malefriends_discuss_partisan",
  x5 = "teacher_discuss_partisan", x6 = "influencer_discuss_partisan",
  y = "interest_partisan")
#ModelGirlsPartisanAgentsCtrl <- lme_ses_personality_allagents(
#  data = CPISGirls, x1 = "mother_discuss_partisan", x2 = "father_discuss_partisan",
#  x3 = "femalefriends_discuss_partisan", x4 = "malefriends_discuss_partisan",
#  x5 = "teacher_discuss_partisan", x6 = "influencer_discuss_partisan",
#  y = "interest_partisan") # error
ModelGirlsPartisanAgentsCtrl <- lme4::lmer(
  data = CPISGirls, formula = interest_partisan ~ mother_discuss_partisan +
  father_discuss_partisan + femalefriends_discuss_partisan +
  malefriends_discuss_partisan + teacher_discuss_partisan +
  influencer_discuss_partisan + age + age_squared + white + immig + lang +
  agentic + communal + (1 | Class), na.action = na.omit)
ModelBoysAllAgentsCtrl <- lme_ses_personality_allagents(
  data = CPISBoysAgentsLonger, x1 = "value_mother", x2 = "value_father",
  x3 = "value_femalefriends", x4 = "value_malefriends", x5 = "value_teacher",
  x6 = "value_influencer", y = "interest_all")
ModelGirlsAllAgentsCtrl <- lme_ses_personality_allagents(
  data = CPISGirlsAgentsLonger, x1 = "value_mother", x2 = "value_father",
  x3 = "value_femalefriends", x4 = "value_malefriends", x5 = "value_teacher",
  x6 = "value_influencer", y = "interest_all")
ModelYoungBoysAgentsCtrl <- lme_ses_personality_allagents(
  data = CPISYoungBoysAgentsLonger, x1 = "value_mother", x2 = "value_father",
  x3 = "value_femalefriends", x4 = "value_malefriends", x5 = "value_teacher",
  x6 = "value_influencer", y = "interest_all")
ModelYoungGirlsAgentsCtrl <- lme_ses_personality_allagents(
  data = CPISYoungGirlsAgentsLonger, x1 = "value_mother", x2 = "value_father",
  x3 = "value_femalefriends", x4 = "value_malefriends", x5 = "value_teacher",
  x6 = "value_influencer", y = "interest_all")
# ModelOldBoysAgentsCtrl <- lme_ses_personality_allagents(
#   data = CPISOldBoysAgentsLonger, x1 = "value_mother", x2 = "value_father",
#   x3 = "value_femalefriends", x4 = "value_malefriends", y = "interest_all")
ModelOldBoysAgentsCtrl <- lme4::lmer(
  data = CPISOldBoysAgentsLonger, formula = interest_all ~ value_mother +
  value_father + value_femalefriends + value_malefriends + value_teacher +
  value_influencer + age + age_squared + white + immig + lang + agentic +
  communal + (1 | Class), na.action = na.omit)
ModelOldGirlsAgentsCtrl <- lme_ses_personality_allagents(
  data = CPISOldGirlsAgentsLonger, x1 = "value_mother", x2 = "value_father",
  x3 = "value_femalefriends", x4 = "value_malefriends", x5 = "value_teacher",
  x6 = "value_influencer", y = "interest_all")
lme_ses_personality_allagents_boysgirls <- function(
  data, x1, x2, x3, x4, x5, x6, y) {
    data$x1 <- data[[x1]]
    data$x2 <- data[[x2]]
    data$x3 <- data[[x3]]
    data$x4 <- data[[x4]]
    data$x5 <- data[[x5]]
    data$x6 <- data[[x6]]
    data$y <- data[[y]]
    nlme::lme(data = data, fixed = y ~ female * x1 + female * x2 + female * x3 +
              female * x4 + female * x5 + female * x6 + female * age +
              age_squared + female * white + immig + lang + agentic + communal,
              random = ~ 1 | Class, na.action = na.omit)
}
ModelHealthAgentsCtrl <- lme_ses_personality_allagents_boysgirls(
  data = CPIS, x1 = "mother_discuss_health", x2 = "father_discuss_health",
  x3 = "femalefriends_discuss_health", x4 = "malefriends_discuss_health",
  x5 = "teacher_discuss_health", x6 = "influencer_discuss_health",
  y = "interest_health")
ModelForeignAgentsCtrl <- lme_ses_personality_allagents_boysgirls(
  data = CPIS, x1 = "mother_discuss_foreign", x2 = "father_discuss_foreign",
  x3 = "femalefriends_discuss_foreign", x4 = "malefriends_discuss_foreign",
  x5 = "teacher_discuss_foreign", x6 = "influencer_discuss_foreign",
  y = "interest_foreign")
ModelLawAgentsCtrl <- lme_ses_personality_allagents_boysgirls(
  data = CPIS, x1 = "mother_discuss_law", x2 = "father_discuss_law",
  x3 = "femalefriends_discuss_law", x4 = "malefriends_discuss_law",
  x5 = "teacher_discuss_law", x6 = "influencer_discuss_law",
  y = "interest_law")
ModelEducationAgentsCtrl <- lme_ses_personality_allagents_boysgirls(
  data = CPIS, x1 = "mother_discuss_education", x2 = "father_discuss_education",
  x3 = "femalefriends_discuss_education", x4 = "malefriends_discuss_education",
  x5 = "teacher_discuss_education", x6 = "influencer_discuss_education",
  y = "interest_education")
ModelPartisanAgentsCtrl <- lme4::lmer(
  data = CPIS, formula = interest_partisan ~ female * mother_discuss_partisan +
  female * father_discuss_partisan + female * femalefriends_discuss_partisan +
  female * malefriends_discuss_partisan + female * teacher_discuss_partisan +
  female * influencer_discuss_partisan + female * age + age_squared +
  female * white + immig + lang + agentic + communal + (1 | Class),
  na.action = na.omit)
ModelAllAgentsCtrl <- lme_ses_personality_allagents_boysgirls(
  data = CPISAgentsLonger, x1 = "value_mother", x2 = "value_father",
  x3 = "value_femalefriends", x4 = "value_malefriends", x5 = "value_teacher",
  x6 = "value_influencer", y = "interest_all")

#### 4.9 Test models for multicollinearity, heteroskedasticity and autocorrelation ####
CPISModels <- list(
  ModelInterestGender, ModelHealthGender, ModelForeignGender,
  ModelLawGender, ModelEducationGender, ModelPartisanGender,
  ModelInterestGenderSES, ModelHealthGenderSES, ModelForeignGenderSES,
  ModelLawGenderSES, ModelEducationGenderSES, ModelPartisanGenderSES,
  ModelInterestGenderSESPersonality, ModelHealthGenderSESPersonality,
  ModelForeignGenderSESPersonality, ModelLawGenderSESPersonality,
  ModelEducationGenderSESPersonality, ModelPartisanGenderSESPersonality,
  ModelInterestGenderSESInterac, ModelHealthGenderSESInterac,
  ModelForeignGenderSESInterac, ModelLawGenderSESInterac,
  ModelEducationGenderSESInterac, ModelPartisanGenderSESInterac,
  ModelInterestGenderCtrl, ModelHealthGenderCtrl,
  ModelForeignGenderCtrl, ModelLawGenderCtrl,
  ModelEducationGenderCtrl, ModelPartisanGenderCtrl)
CPISAgentsModels <- list(
  ModelBoysHealthGenderParent, ModelBoysForeignGenderParent,
  ModelBoysLawGenderParent, ModelBoysEducationGenderParent,
  ModelBoysPartisanGenderParent, ModelBoysAllGenderParent,
  ModelGirlsHealthGenderParent, ModelGirlsForeignGenderParent,
  ModelGirlsLawGenderParent, ModelGirlsEducationGenderParent,
  ModelGirlsPartisanGenderParent, ModelGirlsAllGenderParent,
  ModelBoysHealthMother, ModelBoysForeignMother,
  ModelBoysLawMother, ModelBoysEducationMother,
  ModelBoysPartisanMother, ModelBoysAllMother,
  ModelGirlsHealthMother, ModelGirlsForeignMother,
  ModelGirlsLawMother, ModelGirlsEducationMother,
  ModelGirlsPartisanMother, ModelGirlsAllMother,
  ModelBoysHealthFather, ModelBoysForeignFather,
  ModelBoysLawFather, ModelBoysEducationFather,
  ModelBoysPartisanFather, ModelBoysAllFather,
  ModelGirlsHealthFather, ModelGirlsForeignFather,
  ModelGirlsLawFather, ModelGirlsEducationFather,
  ModelGirlsPartisanFather, ModelGirlsAllFather,
  ModelBoysHealthFemaleFriends, ModelBoysForeignFemaleFriends,
  ModelBoysLawFemaleFriends, ModelBoysEducationFemaleFriends,
  ModelBoysPartisanFemaleFriends, ModelBoysAllFemaleFriends,
  ModelGirlsHealthFemaleFriends, ModelGirlsForeignFemaleFriends,
  ModelGirlsLawFemaleFriends, ModelGirlsEducationFemaleFriends,
  ModelGirlsPartisanFemaleFriends, ModelGirlsAllFemaleFriends,
  ModelBoysHealthMaleFriends, ModelBoysForeignMaleFriends,
  ModelBoysLawMaleFriends, ModelBoysEducationMaleFriends,
  ModelBoysPartisanMaleFriends, ModelBoysAllMaleFriends,
  ModelGirlsHealthMaleFriends, ModelGirlsForeignMaleFriends,
  ModelGirlsLawMaleFriends, ModelGirlsEducationMaleFriends,
  ModelGirlsPartisanMaleFriends, ModelGirlsAllMaleFriends,
  ModelBoysHealthMotherSES, ModelBoysForeignMotherSES,
  ModelBoysLawMotherSES, ModelBoysEducationMotherSES,
  ModelBoysPartisanMotherSES, ModelBoysAllMotherSES,
  ModelBoysHealthFatherSES, ModelBoysForeignFatherSES,
  ModelBoysLawFatherSES, ModelBoysEducationFatherSES,
  ModelBoysPartisanFatherSES, ModelBoysAllFatherSES,
  ModelGirlsHealthMotherSES, ModelGirlsForeignMotherSES,
  ModelGirlsLawMotherSES, ModelGirlsEducationMotherSES,
  ModelGirlsPartisanMotherSES, ModelGirlsAllMotherSES,
  ModelGirlsHealthFatherSES, ModelGirlsForeignFatherSES,
  ModelGirlsLawFatherSES, ModelGirlsEducationFatherSES,
  ModelGirlsPartisanFatherSES, ModelGirlsAllFatherSES,
  ModelBoysHealthMotherSESPersonality, ModelBoysForeignMotherSESPersonality,
  ModelBoysLawMotherSESPersonality, ModelBoysEducationMotherSESPersonality,
  ModelBoysPartisanMotherSESPersonality, ModelBoysAllMotherSESPersonality,
  ModelBoysHealthFatherSESPersonality, ModelBoysForeignFatherSESPersonality,
  ModelBoysLawFatherSESPersonality, ModelBoysEducationFatherSESPersonality,
  ModelBoysPartisanFatherSESPersonality, ModelBoysAllFatherSESPersonality,
  ModelGirlsHealthMotherSESPersonality, ModelGirlsForeignMotherSESPersonality,
  ModelGirlsLawMotherSESPersonality, ModelGirlsEducationMotherSESPersonality,
  ModelGirlsPartisanMotherSESPersonality, ModelGirlsAllMotherSESPersonality,
  ModelGirlsHealthFatherSESPersonality, ModelGirlsForeignFatherSESPersonality,
  ModelGirlsLawFatherSESPersonality, ModelGirlsEducationFatherSESPersonality,
  ModelGirlsPartisanFatherSESPersonality, ModelGirlsAllFatherSESPersonality,
  ModelBoysHealthGenderParentCtrl, ModelBoysForeignGenderParentCtrl,
  ModelBoysLawGenderParentCtrl, ModelBoysEducationGenderParentCtrl,
  ModelBoysPartisanGenderParentCtrl, ModelBoysAllGenderParentCtrl,
  ModelGirlsHealthGenderParentCtrl, ModelGirlsForeignGenderParentCtrl,
  ModelGirlsLawGenderParentCtrl, ModelGirlsEducationGenderParentCtrl,
  ModelGirlsPartisanGenderParentCtrl, ModelGirlsAllGenderParentCtrl,
  ModelBoysHealthAgentsCtrl, ModelBoysForeignAgentsCtrl,
  ModelBoysLawAgentsCtrl, ModelBoysEducationAgentsCtrl,
  ModelBoysPartisanAgentsCtrl, ModelBoysAllAgentsCtrl,
  ModelGirlsHealthAgentsCtrl, ModelGirlsForeignAgentsCtrl,
  ModelGirlsLawAgentsCtrl, ModelGirlsEducationAgentsCtrl,
  ModelGirlsPartisanAgentsCtrl, ModelGirlsAllAgentsCtrl,
  ModelHealthGenderParentCtrl, ModelForeignGenderParentCtrl,
  ModelLawGenderParentCtrl, ModelEducationGenderParentCtrl,
  ModelPartisanGenderParentCtrl, ModelAllGenderParentCtrl,
  ModelHealthAgentsCtrl, ModelForeignAgentsCtrl,
  ModelLawAgentsCtrl, ModelEducationAgentsCtrl,
  ModelPartisanAgentsCtrl, ModelAllAgentsCtrl)
  CPISAgentsModels2 <- list(
    ModelBoysHealthMaleFriendsSES, ModelBoysForeignMaleFriendsSES,
  ModelBoysLawMaleFriendsSES, ModelBoysEducationMaleFriendsSES,
  ModelBoysPartisanMaleFriendsSES, ModelBoysAllMaleFriendsSES,
  ModelBoysHealthFemaleFriendsSES, ModelBoysForeignFemaleFriendsSES,
  ModelBoysLawFemaleFriendsSES, ModelBoysEducationFemaleFriendsSES,
  ModelBoysPartisanFemaleFriendsSES, ModelBoysAllFemaleFriendsSES,
  ModelGirlsHealthMaleFriendsSES, ModelGirlsForeignMaleFriendsSES,
  ModelGirlsLawMaleFriendsSES, ModelGirlsEducationMaleFriendsSES,
  ModelGirlsPartisanMaleFriendsSES, ModelGirlsAllMaleFriendsSES,
  ModelGirlsHealthFemaleFriendsSES, ModelGirlsForeignFemaleFriendsSES,
  ModelGirlsLawFemaleFriendsSES, ModelGirlsEducationFemaleFriendsSES,
  ModelGirlsPartisanFemaleFriendsSES, ModelGirlsAllFemaleFriendsSES,
  ModelBoysHealthMaleFriendsSESPersonality, ModelBoysForeignMaleFriendsSESPersonality,
  ModelBoysLawMaleFriendsSESPersonality, ModelBoysEducationMaleFriendsSESPersonality,
  ModelBoysPartisanMaleFriendsSESPersonality, ModelBoysAllMaleFriendsSESPersonality,
  ModelBoysHealthFemaleFriendsSESPersonality, ModelBoysForeignFemaleFriendsSESPersonality,
  ModelBoysLawFemaleFriendsSESPersonality, ModelBoysEducationFemaleFriendsSESPersonality,
  ModelBoysPartisanFemaleFriendsSESPersonality, ModelBoysAllFemaleFriendsSESPersonality,
  ModelGirlsHealthMaleFriendsSESPersonality, ModelGirlsForeignMaleFriendsSESPersonality,
  ModelGirlsLawMaleFriendsSESPersonality, ModelGirlsEducationMaleFriendsSESPersonality,
  ModelGirlsPartisanMaleFriendsSESPersonality, ModelGirlsAllMaleFriendsSESPersonality,
  ModelGirlsHealthFemaleFriendsSESPersonality, ModelGirlsForeignFemaleFriendsSESPersonality,
  ModelGirlsLawFemaleFriendsSESPersonality, ModelGirlsEducationFemaleFriendsSESPersonality,
  ModelGirlsPartisanFemaleFriendsSESPersonality, ModelGirlsAllFemaleFriendsSESPersonality)
DGModels <- list(
  ModelInterestGenderDG, ModelHealthGenderDG, ModelForeignGenderDG,
  ModelLawGenderDG, ModelEducationGenderDG, ModelPartisanGenderDG,
  ModelInterestGenderDGSES, ModelHealthGenderDGSES, ModelForeignGenderDGSES,
  ModelLawGenderDGSES, ModelEducationGenderDGSES, ModelPartisanGenderDGSES,
  ModelInterestGenderDGSESInterac, ModelHealthGenderDGSESInterac,
  ModelForeignGenderDGSESInterac, ModelLawGenderDGSESInterac,
  ModelEducationGenderDGSESInterac, ModelPartisanGenderDGSESInterac)
map(CPISModels[7:length(CPISModels)], car::vif)
map(CPISAgentsModels[61:length(CPISAgentsModels)], car::vif)
map(CPISAgentsModels2, car::vif)
map(DGModels[7:length(DGModels)], car::vif)
print(map(DGModels, ~bptest(.x)[[4]]))
 # Breusch-Pagan test for heteroscedasticity
#print(map(DGModels, ~dwtest(.x)[[4]]))
 # Durbin-Watson test for autocorrelation. All values above 0.05

#### 4.10 Create regression tables ####
modelsummary::modelsummary(models = list(
  "Without Controls" = list(
    "Politics (general)" = ModelInterestGender,
    "Health care" = ModelHealthGender,
    "International affairs" = ModelForeignGender,
    "Law and crime" = ModelLawGender,
    "Education" = ModelEducationGender,
    "Partisan politics" = ModelPartisanGender),
  "With Controls" = list(
    "Politics (general)" = ModelInterestGenderSES,
    "Health care" = ModelHealthGenderSES,
    "International affairs" = ModelForeignGenderSES,
    "Law and crime" = ModelLawGenderSES,
    "Education" = ModelEducationGenderSES,
    "Partisan politics" = ModelPartisanGenderSES)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Reference Category for Language: Other languages spoken at home"),
  title = paste("Interest in Topic by Gender, CPIS \\label{tab:lmeInterestCPIS}"),
  coef_rename = c(
    "x1" = "Gender (1 = girl)",
    "female1" = "Gender (1 = girl)",
    "age" = "Age",
    "white" = "Ethnicity (1 = white)",
    "immig" = "Immigrant",
    "langAnglophone" = "English spoken at home",
    "langFrancophone" = "French spoken at home"),
  output = "latex") |>
    kableExtra::kable_styling(font_size = 6, full_width = FALSE)
modelsummary::modelsummary(models = list(
  "Ages 10--15" = list(
    "Politics (general)" = ModelInterestGenderYoung,
    "Health care" = ModelHealthGenderYoung,
    "International affairs" = ModelForeignGenderYoung,
    "Law and crime" = ModelLawGenderYoung,
    "Education" = ModelEducationGenderYoung,
    "Partisan politics" = ModelPartisanGenderYoung),
  "Ages 16--18" = list(
    "Politics (general)" = ModelInterestGenderOld,
    "Health care" = ModelHealthGenderOld,
    "International affairs" = ModelForeignGenderOld,
    "Law and crime" = ModelLawGenderOld,
    "Education" = ModelEducationGenderOld,
    "Partisan politics" = ModelPartisanGenderOld)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  title = paste("Interest in Topic by Gender and Age Group, CPIS",
               "\\label{tab:lmeInterestYoungOldCPIS}"),
  coef_rename = c("x1" = "Gender (1 = girl)"),
  output = "latex") |>
  gt::tab_options(table.font.size = "small")  # Adjust size as needed
modelsummary::modelsummary(models = list(
  "Ages 10--15" = list(
    "Politics (general)" = ModelInterestGenderYoungCtrl,
    "Health care" = ModelHealthGenderYoungCtrl,
    "International affairs" = ModelForeignGenderYoungCtrl,
    "Law and crime" = ModelLawGenderYoungCtrl,
    "Education" = ModelEducationGenderYoungCtrl,
    "Partisan politics" = ModelPartisanGenderYoungCtrl),
  "Ages 16--18" = list(
    "Politics (general)" = ModelInterestGenderOldCtrl,
    "Health care" = ModelHealthGenderOldCtrl,
    "International affairs" = ModelForeignGenderOldCtrl,
    "Law and crime" = ModelLawGenderOldCtrl,
    "Education" = ModelEducationGenderOldCtrl,
    "Partisan politics" = ModelPartisanGenderOldCtrl)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Reference Category for Language: Other languages spoken at home"),
  title = paste("Interest in Topic by Gender and Age Group, CPIS",
               "\\label{tab:lmeInterestYoungOldCPISCtrl}"),
  coef_rename = c(
    "female1" = "Gender (1 = girl)",
    "age" = "Age",
    "age_squared" = "Age squared",
    "white" = "Ethnicity (1 = white)",
    "immig" = "Immigrant",
    "langAnglophone" = "English spoken at home",
    "langFrancophone" = "French spoken at home",
    "agentic" = "Agency",
    "communal" = "Communality"),
  output = "latex") |>
    kableExtra::kable_styling(font_size = 6, full_width = FALSE)
modelsummary::modelsummary(models = list(
  "Without Controls" = list(
    "Politics (general)" = ModelInterestGenderDG,
    "Health care" = ModelHealthGenderDG,
    "International affairs" = ModelForeignGenderDG,
    "Law and crime" = ModelLawGenderDG,
    "Education" = ModelEducationGenderDG,
    "Partisan politics" = ModelPartisanGenderDG),
  "With Controls" = list(
    "Politics (general)" = ModelInterestGenderDGSES,
    "Health care" = ModelHealthGenderDGSES,
    "International affairs" = ModelForeignGenderDGSES,
    "Law and crime" = ModelLawGenderDGSES,
    "Education" = ModelEducationGenderDGSES,
    "Partisan politics" = ModelPartisanGenderDGSES)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  notes = c("Without controls: Ordinary least squares (OLS) regressions",
           paste("With controls: OLS for Politics (general) and Law and Crime;",
                 "Weighted least squares (WLS) for other regressions")),
  title = paste("Interest in Topic by Gender, Datagotchi PES \\label{tab:olsInterestDG}"),
  coef_rename = c(
    "female1" = "Gender (1 = women)",
    "age" = "Age",
    "white" = "Ethnicity (1 = white)",
    "immig" = "Immigrant",
    "langFrench" = "French spoken at home",
    "income_mid" = "Income between \\$60,000 and \\$150,000",
    "income_high" = "Income above \\$150,000",
    "educ_mid" = "Education: college",
    "educ_high" = "Education: university"),
  output = "latex") |>
    kableExtra::kable_styling(font_size = 6, full_width = FALSE)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelBoysAllGenderParent,
                "Health care" = ModelBoysHealthGenderParent,
                "International affairs" = ModelBoysForeignGenderParent,
                "Law and crime" = ModelBoysLawGenderParent,
                "Education" = ModelBoysEducationGenderParent,
                "Partisan politics" = ModelBoysPartisanGenderParent),
  "Girls" = list("All" = ModelGirlsAllGenderParent,
                 "Health care" = ModelGirlsHealthGenderParent,
                 "International affairs" = ModelGirlsForeignGenderParent,
                 "Law and crime" = ModelGirlsLawGenderParent,
                 "Education" = ModelGirlsEducationGenderParent,
                 "Partisan politics" = ModelGirlsPartisanGenderParent)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "latex",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  title = paste("Interest in Topic by Gender of Parent who Discusses that",
                "Topic the Most \\label{tab:lmeParent}"),
  coef_rename = c("x" = "Mother discusses topic more than father")) |>
    kableExtra::kable_styling(font_size = 6, full_width = FALSE)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelBoysAllGenderParentCtrl,
                "Health care" = ModelBoysHealthGenderParentCtrl,
                "International affairs" = ModelBoysForeignGenderParentCtrl,
                "Law and crime" = ModelBoysLawGenderParentCtrl,
                "Education" = ModelBoysEducationGenderParentCtrl,
                "Partisan politics" = ModelBoysPartisanGenderParentCtrl),
  "Girls" = list("All" = ModelGirlsAllGenderParentCtrl,
                 "Health care" = ModelGirlsHealthGenderParentCtrl,
                 "International affairs" = ModelGirlsForeignGenderParentCtrl,
                 "Law and crime" = ModelGirlsLawGenderParentCtrl,
                 "Education" = ModelGirlsEducationGenderParentCtrl,
                 "Partisan politics" = ModelGirlsPartisanGenderParentCtrl)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "latex",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Reference Category for Language: Other languages spoken at home"),
  title = paste("Interest in Topic by Gender of Parent who Discusses that",
                "Topic the Most \\label{tab:lmeParentCtrl}"),
  coef_rename = c(
    "x" = "Mother discusses topic more than father",
    "age" = "Age",
    "age_squared" = "Age squared",
    "white" = "Ethnicity (1 = white)",
    "immig" = "Immigrant",
    "langAnglophone" = "English spoken at home",
    "langFrancophone" = "French spoken at home",
    "agentic" = "Agency",
    "communal" = "Communality")) |>
    kableExtra::kable_styling(font_size = 6, full_width = FALSE)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelBoysAllGenderParentSESInterac,
                "Health care" = ModelBoysHealthGenderParentSESInterac,
                "International affairs" = ModelBoysForeignGenderParentSESInterac,
                "Law and crime" = ModelBoysLawGenderParentSESInterac,
                "Education" = ModelBoysEducationGenderParentSESInterac,
                "Partisan politics" = ModelBoysPartisanGenderParentSESInterac),
  "Girls" = list("All" = ModelGirlsAllGenderParentSESInterac,
                 "Health care" = ModelGirlsHealthGenderParentSESInterac,
                 "International affairs" = ModelGirlsForeignGenderParentSESInterac,
                 "Law and crime" = ModelGirlsLawGenderParentSESInterac,
                 "Education" = ModelGirlsEducationGenderParentSESInterac,
                 "Partisan politics" = ModelGirlsPartisanGenderParentSESInterac)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "latex",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Reference Category for Language: Other languages spoken at home"),
  title = paste("Interest in Topic by Gender of Parent who Discusses that",
                "Topic the Most (With Interactions) \\label{tab:lmeParentSESInterac}"),
  coef_rename = c(
    "female1" = "Gender (1 = girl)",
    "x" = "Mother discusses topic more than father",
    "age" = "Age",
    "age_squared" = "Age squared",
    "white" = "Ethnicity (1 = white)",
    "immig" = "Immigrant",
    "langAnglophone" = "English spoken at home",
    "langFrancophone" = "French spoken at home",
    "agentic" = "Agency",
    "communal" = "Communality")) |>
  kableExtra::kable_styling(font_size = 6, full_width = FALSE)
result <- modelsummary::modelsummary(models = list(
  "All" = ModelAllGenderParentSES,
  "Health care" = ModelHealthGenderParentSES,
  "International affairs" = ModelForeignGenderParentSES,
  "Law and crime" = ModelLawGenderParentSES,
  "Education" = ModelEducationGenderParentSES,
  "Partisan politics" = ModelPartisanGenderParentSES),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "latex",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Reference Category for Language: Other languages spoken at home"),
  title = paste("Interest in Topic by Gender of Parent who Discusses that",
                "Topic the Most (With Interactions) \\label{tab:lmeParentBoysGirls}"),
  coef_rename = c(
    "female1" = "Gender (1 = girl)",
    "x" = "Mother discusses topic more than father",
    "age" = "Age",
    "age_squared" = "Age squared",
    "white" = "Ethnicity (1 = white)",
    "immig" = "Immigrant",
    "langAnglophone" = "English spoken at home",
    "langFrancophone" = "French spoken at home",
    "agentic" = "Agency",
    "communal" = "Communality")) |>
  kableExtra::kable_styling(font_size = 6, full_width = FALSE)
result <- gsub("\\\\num\\{([^}]+)\\}", "\\1", result)
result <- gsub("estimates? & ", "\\1", result)
result <- gsub("ccccccccc", "lcccccc", result)
result <- gsub("part & term & statistic", "\\1", result)
result <- gsub("gof & ", "\\\\hspace{1em}", result)
result <- gsub("multicolumn\\{9\\}", "multicolumn\\{7\\}", result)
result <- gsub(" & std.error", "\\\\hspace{1em}", result)
result <- gsub("SD", "\\\\hspace{1em}SD", result)
result <- gsub("R2 Marg. & ", "R2 Marg.", result)
gsub("Num.Obs. & ", "Num.Obs.", result)
result2 <- modelsummary::modelsummary(models = list(
  "All" = ModelAllGenderParentCtrl,
  "Health care" = ModelHealthGenderParentCtrl,
  "International affairs" = ModelForeignGenderParentCtrl,
  "Law and crime" = ModelLawGenderParentCtrl,
  "Education" = ModelEducationGenderParentCtrl,
  "Partisan politics" = ModelPartisanGenderParentCtrl),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "latex",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Reference Category for Language: Other languages spoken at home"),
  title = paste("Interest in Topic by Gender of Parent who Discusses that",
                "Topic the Most (With Interactions) \\label{tab:lmeParentCtrlInterac}"),
  coef_rename = c(
    "female1" = "Gender (1 = girl)",
    "x" = "Mother discusses topic more than father",
    "age" = "Age",
    "age_squared" = "Age squared",
    "white" = "Ethnicity (1 = white)",
    "immig" = "Immigrant",
    "langAnglophone" = "English spoken at home",
    "langFrancophone" = "French spoken at home",
    "agentic" = "Agency",
    "communal" = "Communality")) |>
  kableExtra::kable_styling(font_size = 6, full_width = FALSE)
result2 <- gsub("\\\\num\\{([^}]+)\\}", "\\1", result2)
result2 <- gsub("estimates? & ", "\\1", result2)
result2 <- gsub("ccccccccc", "lcccccc", result2)
result2 <- gsub("part & term & statistic", "\\1", result2)
result2 <- gsub("gof & ", "\\\\hspace{1em}", result2)
result2 <- gsub("multicolumn\\{9\\}", "multicolumn\\{7\\}", result2)
result2 <- gsub(" & std.error", "\\\\hspace{1em}", result2)
result2 <- gsub("SD", "\\\\hspace{1em}SD", result2)
result2 <- gsub("R2 Marg. & ", "R2 Marg.", result2)
gsub("Num.Obs. & ", "Num.Obs.", result2)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelBoysAllMother,
                "Health care" = ModelBoysHealthMother,
                "International affairs" = ModelBoysForeignMother,
                "Law and crime" = ModelBoysLawMother,
                "Education" = ModelBoysEducationMother,
                "Partisan politics" = ModelBoysPartisanMother),
  "Girls" = list("All" = ModelGirlsAllMother,
                 "Health care" = ModelGirlsHealthMother,
                 "International affairs" = ModelGirlsForeignMother,
                 "Law and crime" = ModelGirlsLawMother,
                 "Education" = ModelGirlsEducationMother,
                 "Partisan politics" = ModelGirlsPartisanMother)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "latex",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  title = paste("Interest in Topic Most Often Discussed with One's Mother",
                "\\label{tab:lmeMother}"),
  coef_rename = c("x" = "Topic most discussed with mother?")) |>
    kableExtra::kable_styling(font_size = 6, full_width = FALSE)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelBoysAllFather,
                "Health care" = ModelBoysHealthFather,
                "International affairs" = ModelBoysForeignFather,
                "Law and crime" = ModelBoysLawFather,
                "Education" = ModelBoysEducationFather,
                "Partisan politics" = ModelBoysPartisanFather),
  "Girls" = list("All" = ModelGirlsAllFather,
                 "Health care" = ModelGirlsHealthFather,
                 "International affairs" = ModelGirlsForeignFather,
                 "Law and crime" = ModelGirlsLawFather,
                 "Education" = ModelGirlsEducationFather,
                 "Partisan politics" = ModelGirlsPartisanFather)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "latex",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  title = paste("Interest in Topic Most Often Discussed with One's Father",
                "\\label{tab:lmeFather}"),
  coef_rename = c("x" = "Topic most discussed with father?")) |>
    kableExtra::kable_styling(font_size = 6, full_width = FALSE)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelBoysAllFemaleFriends,
                "Health care" = ModelBoysHealthFemaleFriends,
                "International affairs" = ModelBoysForeignFemaleFriends,
                "Law and crime" = ModelBoysLawFemaleFriends,
                "Education" = ModelBoysEducationFemaleFriends,
                "Partisan politics" = ModelBoysPartisanFemaleFriends),
  "Girls" = list("All" = ModelGirlsAllFemaleFriends,
                 "Health care" = ModelGirlsHealthFemaleFriends,
                 "International affairs" = ModelGirlsForeignFemaleFriends,
                 "Law and crime" = ModelGirlsLawFemaleFriends,
                 "Education" = ModelGirlsEducationFemaleFriends,
                 "Partisan politics" = ModelGirlsPartisanFemaleFriends)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "latex",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  title = paste("Interest in Topic Most Often Discussed with one's Female",
                "Friends \\label{tab:lmeFemaleFriends}"),
  coef_rename = c("x" = "Topic most discussed with female friends?")) |>
    kableExtra::kable_styling(font_size = 6, full_width = FALSE)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelBoysAllMaleFriends,
                "Health care" = ModelBoysHealthMaleFriends,
                "International affairs" = ModelBoysForeignMaleFriends,
                "Law and crime" = ModelBoysLawMaleFriends,
                "Education" = ModelBoysEducationMaleFriends,
                "Partisan politics" = ModelBoysPartisanMaleFriends),
  "Girls" = list("All" = ModelGirlsAllMaleFriends,
                 "Health care" = ModelGirlsHealthMaleFriends,
                 "International affairs" = ModelGirlsForeignMaleFriends,
                 "Law and crime" = ModelGirlsLawMaleFriends,
                 "Education" = ModelGirlsEducationMaleFriends,
                 "Partisan politics" = ModelGirlsPartisanMaleFriends)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "latex",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  title = paste("Interest in Topic Most Often Discussed with One's Male",
                "Friends \\label{tab:lmeMaleFriends}"),
  coef_rename = c("x" = "Topic most discussed with male friends?")) |>
    kableExtra::kable_styling(font_size = 6, full_width = FALSE)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelBoysAllAgentsSES,
                "Health care" = ModelBoysHealthAgentsSES,
                "International affairs" = ModelBoysForeignAgentsSES,
                "Law and crime" = ModelBoysLawAgentsSES,
                "Education" = ModelBoysEducationAgentsSES,
                "Partisan politics" = ModelBoysPartisanAgentsSES),
  "Girls" = list("All" = ModelGirlsAllAgentsSES,
                 "Health care" = ModelGirlsHealthAgentsSES,
                 "International affairs" = ModelGirlsForeignAgentsSES,
                 "Law and crime" = ModelGirlsLawAgentsSES,
                 "Education" = ModelGirlsEducationAgentsSES,
                 "Partisan politics" = ModelGirlsPartisanAgentsSES)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "latex",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Reference Category for Language: Other languages spoken at home"),
  title = paste("Interest in Topic Most Often Discussed with Socialization Agents",
                "\\label{tab:lmeAgentsSES}"),
  coef_rename = c("x1" = "Topic most discussed with mother?",
                  "x2" = "Topic most discussed with father?",
                  "x3" = "Topic most discussed with female friends?",
                  "x4" = "Topic most discussed with male friends?",
                  "x5" = "Topic most discussed by teacher?",
                  "x6" = "Topic most discussed by social media influencer?",
                  "mother_discuss_partisan" =
                   "Topic most discussed with mother?",
                  "father_discuss_partisan" =
                   "Topic most discussed with father?",
                  "femalefriends_discuss_partisan" =
                   "Topic most discussed with female friends?",
                  "malefriends_discuss_partisan" =
                   "Topic most discussed with male friends?",
                  "teacher_discuss_partisan" =
                   "Topic most discussed by teacher?",
                  "influencer_discuss_partisan" =
                   "Topic most discussed by social media influencer?",
                  "age" = "Age",
                  "age_squared" = "Age squared",
                  "white" = "Ethnicity (1 = white)",
                  "immig" = "Immigrant",
                  "langAnglophone" = "English spoken at home",
                  "langFrancophone" = "French spoken at home")) |>
    kableExtra::kable_styling(font_size = 6, full_width = FALSE)
modelsummary::modelsummary(models = list(
  "Boys" = list("All" = ModelBoysAllAgentsCtrl,
                "Health care" = ModelBoysHealthAgentsCtrl,
                "International affairs" = ModelBoysForeignAgentsCtrl,
                "Law and crime" = ModelBoysLawAgentsCtrl,
                "Education" = ModelBoysEducationAgentsCtrl,
                "Partisan politics" = ModelBoysPartisanAgentsCtrl),
  "Girls" = list("All" = ModelGirlsAllAgentsCtrl,
                 "Health care" = ModelGirlsHealthAgentsCtrl,
                 "International affairs" = ModelGirlsForeignAgentsCtrl,
                 "Law and crime" = ModelGirlsLawAgentsCtrl,
                 "Education" = ModelGirlsEducationAgentsCtrl,
                 "Partisan politics" = ModelGirlsPartisanAgentsCtrl)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "latex",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Reference Category for Language: Other languages spoken at home"),
  title = paste("Interest in Topic Most Often Discussed with Socialization Agents",
                "\\label{tab:lmeAgentsCtrl}"),
  coef_rename = c("x1" = "Topic most discussed with mother?",
                  "x2" = "Topic most discussed with father?",
                  "x3" = "Topic most discussed with female friends?",
                  "x4" = "Topic most discussed with male friends?",
                  "x5" = "Topic most discussed by teacher?",
                  "x6" = "Topic most discussed by social media influencer?",
                  "mother_discuss_partisan" =
                   "Topic most discussed with mother?",
                  "father_discuss_partisan" =
                   "Topic most discussed with father?",
                  "femalefriends_discuss_partisan" =
                   "Topic most discussed with female friends?",
                  "malefriends_discuss_partisan" =
                   "Topic most discussed with male friends?",
                  "teacher_discuss_partisan" =
                   "Topic most discussed by teacher?",
                  "influencer_discuss_partisan" =
                   "Topic most discussed by social media influencer?",
                  "age" = "Age",
                  "age_squared" = "Age squared",
                  "white" = "Ethnicity (1 = white)",
                  "immig" = "Immigrant",
                  "langAnglophone" = "English spoken at home",
                  "langFrancophone" = "French spoken at home",
                  "agentic" = "Agency",
                  "communal" = "Communality")) |>
    kableExtra::kable_styling(font_size = 6, full_width = FALSE)
result3 <- modelsummary::modelsummary(models = list(
  "All" = ModelAllAgentsSES,
  "Health care" = ModelHealthAgentsSES,
  "International affairs" = ModelForeignAgentsSES,
  "Law and crime" = ModelLawAgentsSES,
  "Education" = ModelEducationAgentsSES,
  "Partisan politics" = ModelPartisanAgentsSES),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "latex",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Reference Category for Language: Other languages spoken at home"),
  title = paste("Interest in Topic Most Often Discussed with Socialization Agents",
                "(With Interactions) \\label{tab:lmeAgentsBoysGirls}"),
  coef_rename = c("female1" = "Gender (1 = girl)",
                  "x1" = "Topic most discussed with mother?",
                  "x2" = "Topic most discussed with father?",
                  "x3" = "Topic most discussed with female friends?",
                  "x4" = "Topic most discussed with male friends?",
                  "x5" = "Topic most discussed by teacher?",
                  "x6" = "Topic most discussed by social media influencer?",
                  "mother_discuss_partisan" = "Topic most discussed with mother?",
                  "father_discuss_partisan" = "Topic most discussed with father?",
                  "femalefriends_discuss_partisan" = "Topic most discussed with female friends?",
                  "malefriends_discuss_partisan" = "Topic most discussed with male friends?",
                  "teacher_discuss_partisan" = "Topic most discussed by teacher?",
                  "influencer_discuss_partisan" = "Topic most discussed by social media influencer?",
                  "age" = "Age",
                  "age_squared" = "Age squared",
                  "white" = "Ethnicity (1 = white)",
                  "immig" = "Immigrant",
                  "langAnglophone" = "English spoken at home",
                  "langFrancophone" = "French spoken at home")) |>
    kableExtra::kable_styling(font_size = 6, full_width = FALSE)
result3 <- gsub("\\\\num\\{([^}]+)\\}", "\\1", result3)
result3 <- gsub("estimates? & ", "\\1", result3)
result3 <- gsub("ccccccccc", "lcccccc", result3)
result3 <- gsub("part & term & statistic", "\\1", result3)
result3 <- gsub("gof & ", "\\\\hspace{1em}", result3)
result3 <- gsub("multicolumn\\{9\\}", "multicolumn\\{7\\}", result3)
result3 <- gsub(" & std.error", "\\\\hspace{1em}", result3)
result3 <- gsub("SD", "\\\\hspace{1em}SD", result3)
result3 <- gsub("R2 Marg. & ", "R2 Marg.", result3)
gsub("Num.Obs. & ", "Num.Obs.", result3)
result4 <- modelsummary::modelsummary(models = list(
  "All" = ModelAllAgentsCtrl,
  "Health care" = ModelHealthAgentsCtrl,
  "International affairs" = ModelForeignAgentsCtrl,
  "Law and crime" = ModelLawAgentsCtrl,
  "Education" = ModelEducationAgentsCtrl,
  "Partisan politics" = ModelPartisanAgentsCtrl),
  stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "latex",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Reference Category for Language: Other languages spoken at home"),
  title = paste("Interest in Topic Most Often Discussed with Socialization Agents",
                "(With Interactions) \\label{tab:lmeAgentsCtrlInterac}"),
  coef_rename = c("female1" = "Gender (1 = girl)",
                  "x1" = "Topic most discussed with mother?",
                  "x2" = "Topic most discussed with father?",
                  "x3" = "Topic most discussed with female friends?",
                  "x4" = "Topic most discussed with male friends?",
                  "x5" = "Topic most discussed by teacher?",
                  "x6" = "Topic most discussed by social media influencer?",
                  "mother_discuss_partisan" = "Topic most discussed with mother?",
                  "father_discuss_partisan" = "Topic most discussed with father?",
                  "femalefriends_discuss_partisan" = "Topic most discussed with female friends?",
                  "malefriends_discuss_partisan" = "Topic most discussed with male friends?",
                  "teacher_discuss_partisan" = "Topic most discussed by teacher?",
                  "influencer_discuss_partisan" = "Topic most discussed by social media influencer?",
                  "age" = "Age",
                  "age_squared" = "Age squared",
                  "white" = "Ethnicity (1 = white)",
                  "immig" = "Immigrant",
                  "langAnglophone" = "English spoken at home",
                  "langFrancophone" = "French spoken at home",
                  "agentic" = "Agency",
                  "communal" = "Communality")) |>
    kableExtra::kable_styling(font_size = 6, full_width = FALSE)
result4 <- gsub("\\\\num\\{([^}]+)\\}", "\\1", result4)
result4 <- gsub("estimates? & ", "\\1", result4)
result4 <- gsub("ccccccccc", "lcccccc", result4)
result4 <- gsub("part & term & statistic", "\\1", result4)
result4 <- gsub("gof & ", "\\\\hspace{1em}", result4)
result4 <- gsub("multicolumn\\{9\\}", "multicolumn\\{7\\}", result4)
result4 <- gsub(" & std.error", "\\\\hspace{1em}", result4)
result4 <- gsub("SD", "\\\\hspace{1em}SD", result4)
result4 <- gsub("R2 Marg. & ", "R2 Marg.", result4)
gsub("Num.Obs. & ", "Num.Obs.", result4)
modelsummary::modelsummary(models = list(
  "Same-Gender Influencers" = list(
    "All" = ModelAllSameGenderInfluencer,
    "Health care" = ModelHealthSameGenderInfluencer,
    "International affairs" = ModelForeignSameGenderInfluencer,
    "Law and crime" = ModelLawSameGenderInfluencer,
    "Education" = ModelEducationSameGenderInfluencer,
    "Partisan politics" = ModelPartisanSameGenderInfluencer),
  "Other-Gender Influencers" = list(
    "All" = ModelAllOtherGenderInfluencer,
    "Health care" = ModelHealthOtherGenderInfluencer,
    "International affairs" = ModelForeignOtherGenderInfluencer,
    "Law and crime" = ModelLawOtherGenderInfluencer,
    "Education" = ModelEducationOtherGenderInfluencer,
    "Partisan politics" = ModelPartisanOtherGenderInfluencer)),
  shape = "rbind", stars = TRUE, gof_omit = "(IC)|(RMSE)|(R2 Cond.)",
  output = "latex",
  notes = c("Method: Multilevel linear regression",
            "Fixed Effects: Classroom",
            "Controls: None"),
  title = paste("Interest in Topic by Gender Congruence of Influencer who",
                "Discusses that Topic \\label{tab:lmeInfluencer}"),
  coef_rename = c("x" = "Topic most discussed with influencer?")) |>
    kableExtra::kable_styling(font_size = 6, full_width = FALSE)

#### 4.11 Create confidence intervals graphs ####
get_ci <- function(model, var_order, level = 0.95) {
  mod_int <- nlme::intervals(model, which = "fixed", level = level)
  c(mod_int[1]$fixed[var_order + 1,]) # extract est. and ci for 1st IV only
}
GenderCPISData <- data.frame(
  pred_interest = get_ci(ModelInterestGender, 1),
  pred_interest_ses = get_ci(ModelInterestGenderSES, 1),
  pred_interest_ses_interac = get_ci(ModelInterestGenderSESInterac, 1),
  pred_health = get_ci(ModelHealthGender, 1),
  pred_health_ses = get_ci(ModelHealthGenderSES, 1),
  pred_health_ses_interac = get_ci(ModelHealthGenderSESInterac, 1),
  pred_foreign = get_ci(ModelForeignGender, 1),
  pred_foreign_ses = get_ci(ModelForeignGenderSES, 1),
  pred_foreign_ses_interac = get_ci(ModelForeignGenderSESInterac, 1),
  pred_law = get_ci(ModelLawGender, 1),
  pred_law_ses = get_ci(ModelLawGenderSES, 1),
  pred_law_ses_interac = get_ci(ModelLawGenderSESInterac, 1),
  pred_education = get_ci(ModelEducationGender, 1),
  pred_education_ses = get_ci(ModelEducationGenderSES, 1),
  pred_education_ses_interac = get_ci(ModelEducationGenderSESInterac, 1),
  pred_partisan = get_ci(ModelPartisanGender, 1),
  pred_partisan_ses = get_ci(ModelPartisanGenderSES, 1),
  pred_partisan_ses_interac = get_ci(ModelPartisanGenderSESInterac, 1))
rownames(GenderCPISData) <- c("ci_l", "pred", "ci_u")
GenderCPISData <- as.data.frame(t(GenderCPISData))
GenderCPISData$ctrl <- rep(c(
  "Without Controls", "With Controls for SES",
  "With Controls for SES\nand Interactions"), 6)
GenderCPISData$ctrl <- factor(GenderCPISData$ctrl, levels = c(
  "Without Controls", "With Controls for SES",
  "With Controls for SES\nand Interactions"))
GenderCPISData$topic <- c(
  rep("Politics (general)", 3), rep("Health care", 3),
  rep("International affairs", 3), rep("Law and crime", 3),
  rep("Education", 3), rep("Partisan politics", 3))
ggplot(GenderCPISData, aes(x = pred, y = topic)) +
  geom_point() +
  facet_wrap(~ ctrl) +
  geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = 0.5) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_x_continuous("\nGender most interested in that topic (Boys <--------> Girls)",
                     breaks = c(-5, 0, 5)) +
  scale_y_discrete("Topics", limits = rev) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.y = ggtext::element_markdown(
          color = c("red", rep("black", 5))),
          text = element_text(family = "CM Roman"))
ggsave("_graphs/GenderCPIS.pdf", width = 11, height = 4.25)
GenderCPISYOData <- data.frame(
  pred_interest_y = get_ci(ModelInterestGenderYoung, 1),
  pred_interest_o = get_ci(ModelInterestGenderOld, 1),
  pred_health_y = get_ci(ModelHealthGenderYoung, 1),
  pred_health_o = get_ci(ModelHealthGenderOld, 1),
  pred_foreign_y = get_ci(ModelForeignGenderYoung, 1),
  pred_foreign_o = get_ci(ModelForeignGenderOld, 1),
  pred_law_y = get_ci(ModelLawGenderYoung, 1),
  pred_law_o = get_ci(ModelLawGenderOld, 1),
  pred_education_y = get_ci(ModelEducationGenderYoung, 1),
  pred_education_o = get_ci(ModelEducationGenderOld, 1),
  pred_partisan_y = get_ci(ModelPartisanGenderYoung, 1),
  pred_partisan_o = get_ci(ModelPartisanGenderOld, 1))
rownames(GenderCPISYOData) <- c("ci_l", "pred", "ci_u")
GenderCPISYOData <- as.data.frame(t(GenderCPISYOData))
GenderCPISYOData$age <- rep(c("10-15", "16-18"), 6)
GenderCPISYOData$age <- factor(GenderCPISYOData$age, levels = c(
  "10-15", "16-18"))
GenderCPISYOData$topic <- c(
  rep("Politics (general)", 2), rep("Health care", 2),
  rep("International affairs", 2), rep("Law and crime", 2),
  rep("Education", 2), rep("Partisan politics", 2))
ggplot(GenderCPISYOData, aes(x = pred, y = topic)) +
  geom_point() +
  facet_wrap(~ age) +
  geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = 0.5) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_x_continuous("\nGender most interested in that topic (Boys <--------> Girls)") +
  scale_y_discrete("Topics", limits = rev) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.y = ggtext::element_markdown(
          color = c("red", rep("black", 5))),
          text = element_text(family = "CM Roman"))
ggsave("_graphs/GenderCPISYO.pdf", width = 11, height = 4.25)
get_ci_lm <- function(model, var_order, level = 0.95) {
  mod_int <- confint(model, level = level)
  c(mod_int[var_order + 1, 1], model[[1]][var_order + 1],
    mod_int[var_order + 1, 2])
# extract est. and ci for 1st IV only
}
GenderDGData <- data.frame(
  pred_interest = get_ci_lm(ModelInterestGenderDG, 1),
  pred_interest_ses = get_ci_lm(ModelInterestGenderDGSES, 1),
  pred_interest_ctrl = get_ci_lm(ModelInterestGenderDGSESInterac, 1),
  pred_health = get_ci_lm(ModelHealthGenderDG, 1),
  pred_health_ses = get_ci_lm(ModelHealthGenderDGSES, 1),
  pred_health_ctrl = get_ci_lm(ModelHealthGenderDGSESInterac, 1),
  pred_foreign = get_ci_lm(ModelForeignGenderDG, 1),
  pred_foreign_ses = get_ci_lm(ModelForeignGenderDGSES, 1),
  pred_foreign_ctrl = get_ci_lm(ModelForeignGenderDGSESInterac, 1),
  pred_law = get_ci_lm(ModelLawGenderDG, 1),
  pred_law_ses = get_ci_lm(ModelLawGenderDGSES, 1),
  pred_law_ctrl = get_ci_lm(ModelLawGenderDGSESInterac, 1),
  pred_education = get_ci_lm(ModelEducationGenderDG, 1),
  pred_education_ses = get_ci_lm(ModelEducationGenderDGSES, 1),
  pred_education_ctrl = get_ci_lm(ModelEducationGenderDGSESInterac, 1),
  pred_partisan = get_ci_lm(ModelPartisanGenderDG, 1),
  pred_partisan_ses = get_ci_lm(ModelPartisanGenderDGSES, 1),
  pred_partisan_ctrl = get_ci_lm(ModelPartisanGenderDGSESInterac, 1))
rownames(GenderDGData) <- c("ci_l", "pred", "ci_u")
GenderDGData <- as.data.frame(t(GenderDGData))
GenderDGData$ctrl <- rep(c("Without Controls", "With Controls for SES",
                           "With Controls for SES\nand Interactions"), 6)
GenderDGData$ctrl <- factor(GenderDGData$ctrl, levels = c(
  "Without Controls", "With Controls for SES",
  "With Controls for SES\nand Interactions"))
GenderDGData$topic <- c(
  rep("Politics (general)", 3), rep("Health care", 3),
  rep("International affairs", 3), rep("Law and crime", 3),
  rep("Education", 3), rep("Partisan politics", 3))
ggplot(GenderDGData, aes(x = pred, y = topic)) +
  geom_point() +
  facet_wrap(~ ctrl) +
  geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = 0.5) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_x_continuous("\nGender most interested in that topic (Men <--------> Women)") +
  scale_y_discrete("Topics", limits = rev) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.y = ggtext::element_markdown(
          color = c("red", rep("black", 5))),
          text = element_text(family = "CM Roman"))
ggsave("_graphs/GenderDG.pdf", width = 11, height = 4.25)
GenderParentData <- data.frame(
  pred_health_b = get_ci(ModelBoysHealthGenderParent, 1),
  pred_health_g = get_ci(ModelGirlsHealthGenderParent, 1),
  pred_foreign_b = get_ci(ModelBoysForeignGenderParent, 1),
  pred_foreign_g = get_ci(ModelGirlsForeignGenderParent, 1),
  pred_law_b = get_ci(ModelBoysLawGenderParent, 1),
  pred_law_g = get_ci(ModelGirlsLawGenderParent, 1),
  pred_education_b = get_ci(ModelBoysEducationGenderParent, 1),
  pred_education_g = get_ci(ModelGirlsEducationGenderParent, 1),
  pred_partisan_b = get_ci(ModelBoysPartisanGenderParent, 1),
  pred_partisan_g = get_ci(ModelGirlsPartisanGenderParent, 1),
  pred_all_b = get_ci(ModelBoysAllGenderParent, 1),
  pred_all_g = get_ci(ModelGirlsAllGenderParent, 1))
rownames(GenderParentData) <- c("ci_l", "pred", "ci_u")
GenderParentData <- as.data.frame(t(GenderParentData))
GenderParentData$gender <- rep(c("Boys", "Girls"), 6)
GenderParentData$topic <- c(
  rep("Health care", 2), rep("International affairs", 2),
  rep("Law and crime", 2), rep("Education", 2), rep("Partisan politics", 2),
  rep("All topics", 2))
ggplot(GenderParentData, aes(x = pred, y = topic)) +
  geom_point() +
  facet_wrap(~ gender) +
  geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = 0.5) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_x_continuous("\nGender of parent who discusses that topic the most",
                     breaks = 0, labels = "Father<---------->Mother") +
  scale_y_discrete("Topics", limits = rev) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.y = ggtext::element_markdown(
          color = c(rep("black", 5), "red")),
          text = element_text(family = "CM Roman"))
ggsave("_graphs/GenderParent.pdf", width = 11, height = 4.25)
GenderParentSESData <- data.frame(
  pred_health_b = get_ci(ModelBoysHealthGenderParentSESInterac, 1),
  pred_health_g = get_ci(ModelGirlsHealthGenderParentSESInterac, 1),
  pred_foreign_b = get_ci(ModelBoysForeignGenderParentSESInterac, 1),
  pred_foreign_g = get_ci(ModelGirlsForeignGenderParentSESInterac, 1),
  pred_law_b = get_ci(ModelBoysLawGenderParentSESInterac, 1),
  pred_law_g = get_ci(ModelGirlsLawGenderParentSESInterac, 1),
  pred_education_b = get_ci(ModelBoysEducationGenderParentSESInterac, 1),
  pred_education_g = get_ci(ModelGirlsEducationGenderParentSESInterac, 1),
  pred_partisan_b = get_ci(ModelBoysPartisanGenderParentSESInterac, 1),
  pred_partisan_g = get_ci(ModelGirlsPartisanGenderParentSESInterac, 1),
  pred_all_b = get_ci(ModelBoysAllGenderParentSESInterac, 1),
  pred_all_g = get_ci(ModelGirlsAllGenderParentSESInterac, 1))
rownames(GenderParentSESData) <- c("ci_l", "pred", "ci_u")
GenderParentSESData <- as.data.frame(t(GenderParentSESData))
GenderParentSESData$gender <- rep(c("Boys", "Girls"), 6)
GenderParentSESData$topic <- c(
  rep("Health care", 2), rep("International affairs", 2),
  rep("Law and crime", 2), rep("Education", 2), rep("Partisan politics", 2),
  rep("All topics", 2))
ggplot(GenderParentSESData, aes(x = pred, y = topic)) +
  geom_point() +
  facet_wrap(~ gender) +
  geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = 0.5) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_x_continuous("\nGender of parent who discusses that topic the most",
                     breaks = 0, labels = "Father<---------->Mother") +
  scale_y_discrete("Topics", limits = rev) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.y = ggtext::element_markdown(
          color = c(rep("black", 5), "red")),
          text = element_text(family = "CM Roman"))
ggsave("_graphs/GenderParentSES.pdf", width = 11, height = 4.25)
GenderParentCtrlData <- data.frame(
  pred_health_b = get_ci(ModelBoysHealthGenderParentCtrl, 1),
  pred_health_g = get_ci(ModelGirlsHealthGenderParentCtrl, 1),
  pred_foreign_b = get_ci(ModelBoysForeignGenderParentCtrl, 1),
  pred_foreign_g = get_ci(ModelGirlsForeignGenderParentCtrl, 1),
  pred_law_b = get_ci(ModelBoysLawGenderParentCtrl, 1),
  pred_law_g = get_ci(ModelGirlsLawGenderParentCtrl, 1),
  pred_education_b = get_ci(ModelBoysEducationGenderParentCtrl, 1),
  pred_education_g = get_ci(ModelGirlsEducationGenderParentCtrl, 1),
  pred_partisan_b = get_ci(ModelBoysPartisanGenderParentCtrl, 1),
  pred_partisan_g = get_ci(ModelGirlsPartisanGenderParentCtrl, 1),
  pred_all_b = get_ci(ModelBoysAllGenderParentCtrl, 1),
  pred_all_g = get_ci(ModelGirlsAllGenderParentCtrl, 1))
rownames(GenderParentCtrlData) <- c("ci_l", "pred", "ci_u")
GenderParentCtrlData <- as.data.frame(t(GenderParentCtrlData))
GenderParentCtrlData$gender <- rep(c("Boys", "Girls"), 6)
GenderParentCtrlData$topic <- c(
  rep("Health care", 2), rep("International affairs", 2),
  rep("Law and crime", 2), rep("Education", 2), rep("Partisan politics", 2),
  rep("All topics", 2))
ggplot(GenderParentCtrlData, aes(x = pred, y = topic)) +
  geom_point() +
  facet_wrap(~ gender) +
  geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = 0.5) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_x_continuous("\nGender of parent who discusses that topic the most",
                     breaks = 0, labels = "Father<---------->Mother") +
  scale_y_discrete("Topics", limits = rev) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.y = ggtext::element_markdown(
          color = c(rep("black", 5), "red")),
          text = element_text(family = "CM Roman"))
ggsave("_graphs/GenderParentCtrl.pdf", width = 11, height = 4.25)

AgentsData <- data.frame(
  mother_health_b = get_ci(ModelBoysHealthMother, 1),
  mother_health_g = get_ci(ModelGirlsHealthMother, 1),
  mother_foreign_b = get_ci(ModelBoysForeignMother, 1),
  mother_foreign_g = get_ci(ModelGirlsForeignMother, 1),
  mother_law_b = get_ci(ModelBoysLawMother, 1),
  mother_law_g = get_ci(ModelGirlsLawMother, 1),
  mother_education_b = get_ci(ModelBoysEducationMother, 1),
  mother_education_g = get_ci(ModelGirlsEducationMother, 1),
  mother_partisan_b = get_ci(ModelBoysPartisanMother, 1),
  mother_partisan_g = get_ci(ModelGirlsPartisanMother, 1),
  mother_all_b = get_ci(ModelBoysAllMother, 1),
  mother_all_g = get_ci(ModelGirlsAllMother, 1),
  father_health_b = get_ci(ModelBoysHealthFather, 1),
  father_health_g = get_ci(ModelGirlsHealthFather, 1),
  father_foreign_b = get_ci(ModelBoysForeignFather, 1),
  father_foreign_g = get_ci(ModelGirlsForeignFather, 1),
  father_law_b = get_ci(ModelBoysLawFather, 1),
  father_law_g = get_ci(ModelGirlsLawFather, 1),
  father_education_b = get_ci(ModelBoysEducationFather, 1),
  father_education_g = get_ci(ModelGirlsEducationFather, 1),
  father_partisan_b = get_ci(ModelBoysPartisanFather, 1),
  father_partisan_g = get_ci(ModelGirlsPartisanFather, 1),
  father_all_b = get_ci(ModelBoysAllFather, 1),
  father_all_g = get_ci(ModelGirlsAllFather, 1),
  femalefriends_health_b = get_ci(ModelBoysHealthFemaleFriends, 1),
  femalefriends_health_g = get_ci(ModelGirlsHealthFemaleFriends, 1),
  femalefriends_foreign_b = get_ci(ModelBoysForeignFemaleFriends, 1),
  femalefriends_foreign_g = get_ci(ModelGirlsForeignFemaleFriends, 1),
  femalefriends_law_b = get_ci(ModelBoysLawFemaleFriends, 1),
  femalefriends_law_g = get_ci(ModelGirlsLawFemaleFriends, 1),
  femalefriends_education_b = get_ci(ModelBoysEducationFemaleFriends, 1),
  femalefriends_education_g = get_ci(ModelGirlsEducationFemaleFriends, 1),
  femalefriends_partisan_b = get_ci(ModelBoysPartisanFemaleFriends, 1),
  femalefriends_partisan_g = get_ci(ModelGirlsPartisanFemaleFriends, 1),
  femalefriends_all_b = get_ci(ModelBoysAllFemaleFriends, 1),
  femalefriends_all_g = get_ci(ModelGirlsAllFemaleFriends, 1),
  malefriends_health_b = get_ci(ModelBoysHealthMaleFriends, 1),
  malefriends_health_g = get_ci(ModelGirlsHealthMaleFriends, 1),
  malefriends_foreign_b = get_ci(ModelBoysForeignMaleFriends, 1),
  malefriends_foreign_g = get_ci(ModelGirlsForeignMaleFriends, 1),
  malefriends_law_b = get_ci(ModelBoysLawMaleFriends, 1),
  malefriends_law_g = get_ci(ModelGirlsLawMaleFriends, 1),
  malefriends_education_b = get_ci(ModelBoysEducationMaleFriends, 1),
  malefriends_education_g = get_ci(ModelGirlsEducationMaleFriends, 1),
  malefriends_partisan_b = get_ci(ModelBoysPartisanMaleFriends, 1),
  malefriends_partisan_g = get_ci(ModelGirlsPartisanMaleFriends, 1),
  malefriends_all_b = get_ci(ModelBoysAllMaleFriends, 1),
  malefriends_all_g = get_ci(ModelGirlsAllMaleFriends, 1))
rownames(AgentsData) <- c("ci_l", "pred", "ci_u")
AgentsData <- as.data.frame(t(AgentsData))
AgentsData$gender <- rep(c("Boys", "Girls"), nrow(AgentsData) / 2)
AgentsData$topic <- c(rep(c(
  rep("Health care", 2), rep("International affairs", 2),
  rep("Law and crime", 2), rep("Education", 2), rep("Partisan politics", 2),
  rep("All topics", 2)), 4))
AgentsData$agents <- c(rep("Mother", 12), rep("Father", 12),
                       rep("Female friends", 12), rep("Male friends", 12))
AgentsData$controls <- "Without Controls"
AgentsSESData <- data.frame(
  mother_health_b = get_ci(ModelBoysHealthMotherSES, 1),
  mother_health_g = get_ci(ModelGirlsHealthMotherSES, 1),
  mother_foreign_b = get_ci(ModelBoysForeignMotherSES, 1),
  mother_foreign_g = get_ci(ModelGirlsForeignMotherSES, 1),
  mother_law_b = get_ci(ModelBoysLawMotherSES, 1),
  mother_law_g = get_ci(ModelGirlsLawMotherSES, 1),
  mother_education_b = get_ci(ModelBoysEducationMotherSES, 1),
  mother_education_g = get_ci(ModelGirlsEducationMotherSES, 1),
  mother_partisan_b = get_ci(ModelBoysPartisanMotherSES, 1),
  mother_partisan_g = get_ci(ModelGirlsPartisanMotherSES, 1),
  mother_all_b = get_ci(ModelBoysAllMotherSES, 1),
  mother_all_g = get_ci(ModelGirlsAllMotherSES, 1),
  father_health_b = get_ci(ModelBoysHealthFatherSES, 1),
  father_health_g = get_ci(ModelGirlsHealthFatherSES, 1),
  father_foreign_b = get_ci(ModelBoysForeignFatherSES, 1),
  father_foreign_g = get_ci(ModelGirlsForeignFatherSES, 1),
  father_law_b = get_ci(ModelBoysLawFatherSES, 1),
  father_law_g = get_ci(ModelGirlsLawFatherSES, 1),
  father_education_b = get_ci(ModelBoysEducationFatherSES, 1),
  father_education_g = get_ci(ModelGirlsEducationFatherSES, 1),
  father_partisan_b = get_ci(ModelBoysPartisanFatherSES, 1),
  father_partisan_g = get_ci(ModelGirlsPartisanFatherSES, 1),
  father_all_b = get_ci(ModelBoysAllFatherSES, 1),
  father_all_g = get_ci(ModelGirlsAllFatherSES, 1),
  femalefriends_health_b = get_ci(ModelBoysHealthFemaleFriendsSES, 1),
  femalefriends_health_g = get_ci(ModelGirlsHealthFemaleFriendsSES, 1),
  femalefriends_foreign_b = get_ci(ModelBoysForeignFemaleFriendsSES, 1),
  femalefriends_foreign_g = get_ci(ModelGirlsForeignFemaleFriendsSES, 1),
  femalefriends_law_b = get_ci(ModelBoysLawFemaleFriendsSES, 1),
  femalefriends_law_g = get_ci(ModelGirlsLawFemaleFriendsSES, 1),
  femalefriends_education_b = get_ci(ModelBoysEducationFemaleFriendsSES, 1),
  femalefriends_education_g = get_ci(ModelGirlsEducationFemaleFriendsSES, 1),
  femalefriends_partisan_b = get_ci(ModelBoysPartisanFemaleFriendsSES, 1),
  femalefriends_partisan_g = get_ci(ModelGirlsPartisanFemaleFriendsSES, 1),
  femalefriends_all_b = get_ci(ModelBoysAllFemaleFriendsSES, 1),
  femalefriends_all_g = get_ci(ModelGirlsAllFemaleFriendsSES, 1),
  malefriends_health_b = get_ci(ModelBoysHealthMaleFriendsSES, 1),
  malefriends_health_g = get_ci(ModelGirlsHealthMaleFriendsSES, 1),
  malefriends_foreign_b = get_ci(ModelBoysForeignMaleFriendsSES, 1),
  malefriends_foreign_g = get_ci(ModelGirlsForeignMaleFriendsSES, 1),
  malefriends_law_b = get_ci(ModelBoysLawMaleFriendsSES, 1),
  malefriends_law_g = get_ci(ModelGirlsLawMaleFriendsSES, 1),
  malefriends_education_b = get_ci(ModelBoysEducationMaleFriendsSES, 1),
  malefriends_education_g = get_ci(ModelGirlsEducationMaleFriendsSES, 1),
  malefriends_partisan_b = get_ci(ModelBoysPartisanMaleFriendsSES, 1),
  malefriends_partisan_g = get_ci(ModelGirlsPartisanMaleFriendsSES, 1),
  malefriends_all_b = get_ci(ModelBoysAllMaleFriendsSES, 1),
  malefriends_all_g = get_ci(ModelGirlsAllMaleFriendsSES, 1))
rownames(AgentsSESData) <- c("ci_l", "pred", "ci_u")
AgentsSESData <- as.data.frame(t(AgentsSESData))
AgentsSESData$gender <- rep(c("Boys", "Girls"), nrow(AgentsSESData) / 2)
AgentsSESData$topic <- c(rep(c(
  rep("Health care", 2), rep("International affairs", 2),
  rep("Law and crime", 2), rep("Education", 2), rep("Partisan politics", 2),
  rep("All topics", 2)), 4))
AgentsSESData$agents <- c(rep("Mother", 12), rep("Father", 12),
                          rep("Female friends", 12), rep("Male friends", 12))
AgentsSESData$controls <- "With Controls for SES"
confint2 <- confint(ModelGirlsPartisanAgentsSES)
AgentsSESInteracData <- data.frame(
  mother_health_b = get_ci(ModelBoysHealthAgentsSES, 1),
  mother_health_g = get_ci(ModelGirlsHealthAgentsSES, 1),
  mother_foreign_b = get_ci(ModelBoysForeignAgentsSES, 1),
  mother_foreign_g = get_ci(ModelGirlsForeignAgentsSES, 1),
  mother_law_b = get_ci(ModelBoysLawAgentsSES, 1),
  mother_law_g = get_ci(ModelGirlsLawAgentsSES, 1),
  mother_education_b = get_ci(ModelBoysEducationAgentsSES, 1),
  mother_education_g = get_ci(ModelGirlsEducationAgentsSES, 1),
  mother_partisan_b = get_ci(ModelBoysPartisanAgentsSES, 1),
  mother_partisan_g = NA,
  mother_all_b = get_ci(ModelBoysAllAgentsSES, 1),
  mother_all_g = get_ci(ModelGirlsAllAgentsSES, 1),
  father_health_b = get_ci(ModelBoysHealthAgentsSES, 2),
  father_health_g = get_ci(ModelGirlsHealthAgentsSES, 2),
  father_foreign_b = get_ci(ModelBoysForeignAgentsSES, 2),
  father_foreign_g = get_ci(ModelGirlsForeignAgentsSES, 2),
  father_law_b = get_ci(ModelBoysLawAgentsSES, 2),
  father_law_g = get_ci(ModelGirlsLawAgentsSES, 2),
  father_education_b = get_ci(ModelBoysEducationAgentsSES, 2),
  father_education_g = get_ci(ModelGirlsEducationAgentsSES, 2),
  father_partisan_b = get_ci(ModelBoysPartisanAgentsSES, 2),
  father_partisan_g = c(confint2[4, 1], coef(
    ModelGirlsPartisanAgentsSES)$Class[1, 2], confint2[4, 2]),
  father_all_b = get_ci(ModelBoysAllAgentsSES, 2),
  father_all_g = get_ci(ModelGirlsAllAgentsSES, 2),
  femalefriends_health_b = get_ci(ModelBoysHealthAgentsSES, 3),
  femalefriends_health_g = get_ci(ModelGirlsHealthAgentsSES, 3),
  femalefriends_foreign_b = get_ci(ModelBoysForeignAgentsSES, 3),
  femalefriends_foreign_g = get_ci(ModelGirlsForeignAgentsSES, 3),
  femalefriends_law_b = get_ci(ModelBoysLawAgentsSES, 3),
  femalefriends_law_g = get_ci(ModelGirlsLawAgentsSES, 3),
  femalefriends_education_b = get_ci(ModelBoysEducationAgentsSES, 3),
  femalefriends_education_g = get_ci(ModelGirlsEducationAgentsSES, 3),
  femalefriends_partisan_b = get_ci(ModelBoysPartisanAgentsSES, 3),
  femalefriends_partisan_g = c(confint2[5, 1], coef(
    ModelGirlsPartisanAgentsSES)$Class[1, 3], confint2[5, 2]),
  femalefriends_all_b = get_ci(ModelBoysAllAgentsSES, 3),
  femalefriends_all_g = get_ci(ModelGirlsAllAgentsSES, 3),
  malefriends_health_b = get_ci(ModelBoysHealthAgentsSES, 4),
  malefriends_health_g = get_ci(ModelGirlsHealthAgentsSES, 4),
  malefriends_foreign_b = get_ci(ModelBoysForeignAgentsSES, 4),
  malefriends_foreign_g = get_ci(ModelGirlsForeignAgentsSES, 4),
  malefriends_law_b = get_ci(ModelBoysLawAgentsSES, 4),
  malefriends_law_g = get_ci(ModelGirlsLawAgentsSES, 4),
  malefriends_education_b = get_ci(ModelBoysEducationAgentsSES, 4),
  malefriends_education_g = get_ci(ModelGirlsEducationAgentsSES, 4),
  malefriends_partisan_b = get_ci(ModelBoysPartisanAgentsSES, 4),
  malefriends_partisan_g = c(confint2[6, 1], coef(
    ModelGirlsPartisanAgentsSES)$Class[1, 4], confint2[6, 2]),
  malefriends_all_b = get_ci(ModelBoysAllAgentsSES, 4),
  malefriends_all_g = get_ci(ModelGirlsAllAgentsSES, 4))
rownames(AgentsSESInteracData) <- c("ci_l", "pred", "ci_u")
AgentsSESInteracData <- as.data.frame(t(AgentsSESInteracData))
AgentsSESInteracData$gender <- rep(c("Boys", "Girls"), nrow(AgentsSESInteracData) / 2)
AgentsSESInteracData$topic <- c(rep(c(
  rep("Health care", 2), rep("International affairs", 2),
  rep("Law and crime", 2), rep("Education", 2), rep("Partisan politics", 2),
  rep("All topics", 2)), 4))
AgentsSESInteracData$agents <- c(rep("Mother", 12), rep("Father", 12),
                          rep("Female friends", 12), rep("Male friends", 12))
AgentsSESInteracData$controls <-
 "With Controls for SES and\nDiscussions with\nOther Socialization Agents"
AgentsSESPersonalityData <- data.frame(
  mother_health_b = get_ci(ModelBoysHealthMotherSESPersonality, 1),
  mother_health_g = get_ci(ModelGirlsHealthMotherSESPersonality, 1),
  mother_foreign_b = get_ci(ModelBoysForeignMotherSESPersonality, 1),
  mother_foreign_g = get_ci(ModelGirlsForeignMotherSESPersonality, 1),
  mother_law_b = get_ci(ModelBoysLawMotherSESPersonality, 1),
  mother_law_g = get_ci(ModelGirlsLawMotherSESPersonality, 1),
  mother_education_b = get_ci(ModelBoysEducationMotherSESPersonality, 1),
  mother_education_g = get_ci(ModelGirlsEducationMotherSESPersonality, 1),
  mother_partisan_b = get_ci(ModelBoysPartisanMotherSESPersonality, 1),
  mother_partisan_g = get_ci(ModelGirlsPartisanMotherSESPersonality, 1),
  mother_all_b = get_ci(ModelBoysAllMotherSESPersonality, 1),
  mother_all_g = get_ci(ModelGirlsAllMotherSESPersonality, 1),
  father_health_b = get_ci(ModelBoysHealthFatherSESPersonality, 1),
  father_health_g = get_ci(ModelGirlsHealthFatherSESPersonality, 1),
  father_foreign_b = get_ci(ModelBoysForeignFatherSESPersonality, 1),
  father_foreign_g = get_ci(ModelGirlsForeignFatherSESPersonality, 1),
  father_law_b = get_ci(ModelBoysLawFatherSESPersonality, 1),
  father_law_g = get_ci(ModelGirlsLawFatherSESPersonality, 1),
  father_education_b = get_ci(ModelBoysEducationFatherSESPersonality, 1),
  father_education_g = get_ci(ModelGirlsEducationFatherSESPersonality, 1),
  father_partisan_b = get_ci(ModelBoysPartisanFatherSESPersonality, 1),
  father_partisan_g = get_ci(ModelGirlsPartisanFatherSESPersonality, 1),
  father_all_b = get_ci(ModelBoysAllFatherSESPersonality, 1),
  father_all_g = get_ci(ModelGirlsAllFatherSESPersonality, 1),
  femalefriends_health_b = get_ci(ModelBoysHealthFemaleFriendsSESPersonality, 1),
  femalefriends_health_g = get_ci(ModelGirlsHealthFemaleFriendsSESPersonality, 1),
  femalefriends_foreign_b = get_ci(ModelBoysForeignFemaleFriendsSESPersonality, 1),
  femalefriends_foreign_g = get_ci(ModelGirlsForeignFemaleFriendsSESPersonality, 1),
  femalefriends_law_b = get_ci(ModelBoysLawFemaleFriendsSESPersonality, 1),
  femalefriends_law_g = get_ci(ModelGirlsLawFemaleFriendsSESPersonality, 1),
  femalefriends_education_b = get_ci(ModelBoysEducationFemaleFriendsSESPersonality, 1),
  femalefriends_education_g = get_ci(ModelGirlsEducationFemaleFriendsSESPersonality, 1),
  femalefriends_partisan_b = get_ci(ModelBoysPartisanFemaleFriendsSESPersonality, 1),
  femalefriends_partisan_g = get_ci(ModelGirlsPartisanFemaleFriendsSESPersonality, 1),
  femalefriends_all_b = get_ci(ModelBoysAllFemaleFriendsSESPersonality, 1),
  femalefriends_all_g = get_ci(ModelGirlsAllFemaleFriendsSESPersonality, 1),
  malefriends_health_b = get_ci(ModelBoysHealthMaleFriendsSESPersonality, 1),
  malefriends_health_g = get_ci(ModelGirlsHealthMaleFriendsSESPersonality, 1),
  malefriends_foreign_b = get_ci(ModelBoysForeignMaleFriendsSESPersonality, 1),
  malefriends_foreign_g = get_ci(ModelGirlsForeignMaleFriendsSESPersonality, 1),
  malefriends_law_b = get_ci(ModelBoysLawMaleFriendsSESPersonality, 1),
  malefriends_law_g = get_ci(ModelGirlsLawMaleFriendsSESPersonality, 1),
  malefriends_education_b = get_ci(ModelBoysEducationMaleFriendsSESPersonality, 1),
  malefriends_education_g = get_ci(ModelGirlsEducationMaleFriendsSESPersonality, 1),
  malefriends_partisan_b = get_ci(ModelBoysPartisanMaleFriendsSESPersonality, 1),
  malefriends_partisan_g = get_ci(ModelGirlsPartisanMaleFriendsSESPersonality, 1),
  malefriends_all_b = get_ci(ModelBoysAllMaleFriendsSESPersonality, 1),
  malefriends_all_g = get_ci(ModelGirlsAllMaleFriendsSESPersonality, 1))
rownames(AgentsSESPersonalityData) <- c("ci_l", "pred", "ci_u")
AgentsSESPersonalityData <- as.data.frame(t(AgentsSESPersonalityData))
AgentsSESPersonalityData$gender <- rep(c("Boys", "Girls"), nrow(AgentsSESPersonalityData) / 2)
AgentsSESPersonalityData$topic <- c(rep(c(
  rep("Health care", 2), rep("International affairs", 2),
  rep("Law and crime", 2), rep("Education", 2), rep("Partisan politics", 2),
  rep("All topics", 2)), 4))
AgentsSESPersonalityData$agents <- c(rep("Mother", 12), rep("Father", 12),
                                     rep("Female friends", 12), rep("Male friends", 12))
AgentsSESPersonalityData$controls <- "With Controls for SES\nand Personality Traits"
get_ci_lmer <- function(model, var_order, level = 0.95) {
  mod_int <- confint(model, level = level)
  c(mod_int[var_order + 3, 1], fixef(model)[var_order + 1],
    mod_int[var_order + 3, 2])
# extract est. and ci for 1st IV only
}
AgentsCtrlData <- data.frame(
  mother_health_b = get_ci(ModelBoysHealthAgentsCtrl, 1),
  mother_health_g = get_ci(ModelGirlsHealthAgentsCtrl, 1),
  mother_foreign_b = get_ci(ModelBoysForeignAgentsCtrl, 1),
  mother_foreign_g = get_ci(ModelGirlsForeignAgentsCtrl, 1),
  mother_law_b = get_ci(ModelBoysLawAgentsCtrl, 1),
  mother_law_g = get_ci(ModelGirlsLawAgentsCtrl, 1),
  mother_education_b = get_ci(ModelBoysEducationAgentsCtrl, 1),
  mother_education_g = get_ci(ModelGirlsEducationAgentsCtrl, 1),
  mother_partisan_b = get_ci(ModelBoysPartisanAgentsCtrl, 1),
  mother_partisan_g = c(NA, NA, NA),
  mother_all_b = get_ci(ModelBoysAllAgentsCtrl, 1),
  mother_all_g = get_ci(ModelGirlsAllAgentsCtrl, 1),
  father_health_b = get_ci(ModelBoysHealthAgentsCtrl, 2),
  father_health_g = get_ci(ModelGirlsHealthAgentsCtrl, 2),
  father_foreign_b = get_ci(ModelBoysForeignAgentsCtrl, 2),
  father_foreign_g = get_ci(ModelGirlsForeignAgentsCtrl, 2),
  father_law_b = get_ci(ModelBoysLawAgentsCtrl, 2),
  father_law_g = get_ci(ModelGirlsLawAgentsCtrl, 2),
  father_education_b = get_ci(ModelBoysEducationAgentsCtrl, 2),
  father_education_g = get_ci(ModelGirlsEducationAgentsCtrl, 2),
  father_partisan_b = get_ci(ModelBoysPartisanAgentsCtrl, 2),
  father_partisan_g = get_ci_lmer(ModelGirlsPartisanAgentsCtrl, 1),
  father_all_b = get_ci(ModelBoysAllAgentsCtrl, 2),
  father_all_g = get_ci(ModelGirlsAllAgentsCtrl, 2),
  femalefriends_health_b = get_ci(ModelBoysHealthAgentsCtrl, 3),
  femalefriends_health_g = get_ci(ModelGirlsHealthAgentsCtrl, 3),
  femalefriends_foreign_b = get_ci(ModelBoysForeignAgentsCtrl, 3),
  femalefriends_foreign_g = get_ci(ModelGirlsForeignAgentsCtrl, 3),
  femalefriends_law_b = get_ci(ModelBoysLawAgentsCtrl, 3),
  femalefriends_law_g = get_ci(ModelGirlsLawAgentsCtrl, 3),
  femalefriends_education_b = get_ci(ModelBoysEducationAgentsCtrl, 3),
  femalefriends_education_g = get_ci(ModelGirlsEducationAgentsCtrl, 3),
  femalefriends_partisan_b = get_ci(ModelBoysPartisanAgentsCtrl, 3),
  femalefriends_partisan_g = get_ci_lmer(ModelGirlsPartisanAgentsCtrl, 2),
  femalefriends_all_b = get_ci(ModelBoysAllAgentsCtrl, 3),
  femalefriends_all_g = get_ci(ModelGirlsAllAgentsCtrl, 3),
  malefriends_health_b = get_ci(ModelBoysHealthAgentsCtrl, 4),
  malefriends_health_g = get_ci(ModelGirlsHealthAgentsCtrl, 4),
  malefriends_foreign_b = get_ci(ModelBoysForeignAgentsCtrl, 4),
  malefriends_foreign_g = get_ci(ModelGirlsForeignAgentsCtrl, 4),
  malefriends_law_b = get_ci(ModelBoysLawAgentsCtrl, 4),
  malefriends_law_g = get_ci(ModelGirlsLawAgentsCtrl, 4),
  malefriends_education_b = get_ci(ModelBoysEducationAgentsCtrl, 4),
  malefriends_education_g = get_ci(ModelGirlsEducationAgentsCtrl, 4),
  malefriends_partisan_b = get_ci(ModelBoysPartisanAgentsCtrl, 4),
  malefriends_partisan_g = get_ci_lmer(ModelGirlsPartisanAgentsCtrl, 3),
  malefriends_all_b = get_ci(ModelBoysAllAgentsCtrl, 4),
  malefriends_all_g = get_ci(ModelGirlsAllAgentsCtrl, 4))
rownames(AgentsCtrlData) <- c("ci_l", "pred", "ci_u")
AgentsCtrlData <- as.data.frame(t(AgentsCtrlData))
AgentsCtrlData$gender <- rep(c("Boys", "Girls"), nrow(AgentsCtrlData) / 2)
AgentsCtrlData$topic <- c(rep(c(
  rep("Health care", 2), rep("International affairs", 2),
  rep("Law and crime", 2), rep("Education", 2), rep("Partisan politics", 2),
  rep("All topics", 2)), 4))
AgentsCtrlData$agents <- c(rep("Mother", 12), rep("Father", 12),
                           rep("Female friends", 12), rep("Male friends", 12))
AgentsCtrlData$controls <- paste0("With Controls for SES,\nPersonality Traits ",
                                  "and\nDiscussions with\nOther Socialization Agents")
AgentsDataAll <- rbind(AgentsData, AgentsSESData, AgentsSESInteracData)
AgentsDataAll$controls <- factor(AgentsDataAll$controls, levels = c(
  "Without Controls", "With Controls for SES",
  "With Controls for SES and\nDiscussions with\nOther Socialization Agents"))
AgentsDataAll |>
  filter(agents == "Mother") |>
  ggplot(aes(x = pred, y = topic)) +
  geom_point() +
  facet_grid(rows = vars(controls), cols = vars(gender)) +
  geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = 0.5) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_x_continuous(
    "\nTopic most often discussed with mother\n(vs. other topics)") +
  scale_y_discrete("Topics", limits = rev) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text = element_text(size = 17.5),
        axis.text.y = ggtext::element_markdown(
          color = c(rep("black", 5), "red")),
          text = element_text(family = "CM Roman"))
ggsave("_graphs/MotherDiscuss.pdf", width = 11, height = 12.75)
AgentsDataAll |>
  filter(agents == "Father") |>
  ggplot(aes(x = pred, y = topic)) +
  geom_point() +
  facet_grid(rows = vars(controls), cols = vars(gender)) +
  geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = 0.5) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_x_continuous(
    "\nTopic most often discussed with father\n(vs. other topics)") +
  scale_y_discrete("Topics", limits = rev) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text = element_text(size = 17.5),
        axis.text.y = ggtext::element_markdown(
          color = c(rep("black", 5), "red")),
          text = element_text(family = "CM Roman"))
ggsave("_graphs/FatherDiscuss.pdf", width = 11, height = 12.75)
AgentsDataAll |>
  filter(agents == "Female friends") |>
  ggplot(aes(x = pred, y = topic)) +
  geom_point() +
  facet_grid(rows = vars(controls), cols = vars(gender)) +
  geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = 0.5) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_x_continuous(
    "\nTopic most often discussed with female friends\n(vs. other topics)") +
  scale_y_discrete("Topics", limits = rev) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text = element_text(size = 17.5),
        axis.text.y = ggtext::element_markdown(
          color = c(rep("black", 5), "red")),
          text = element_text(family = "CM Roman"))
ggsave("_graphs/FemaleFriendsDiscuss.pdf", width = 11, height = 12.75)
AgentsDataAll |>
  filter(agents == "Male friends") |>
  ggplot(aes(x = pred, y = topic)) +
  geom_point() +
  facet_grid(rows = vars(controls), cols = vars(gender)) +
  geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = 0.5) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_x_continuous(
    "\nTopic most often discussed with male friends\n(vs. other topics)") +
  scale_y_discrete("Topics", limits = rev) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text = element_text(size = 17.5),
        axis.text.y = ggtext::element_markdown(
          color = c(rep("black", 5), "red")),
          text = element_text(family = "CM Roman"))
ggsave("_graphs/MaleFriendsDiscuss.pdf", width = 11, height = 12.75)
GenderParentYOData <- data.frame(
  pred_yb = get_ci(ModelYoungBoysGenderParent, 1),
  pred_yg = get_ci(ModelYoungGirlsGenderParent, 1),
  pred_ob = get_ci(ModelOldBoysGenderParent, 1),
  pred_og = get_ci(ModelOldGirlsGenderParent, 1))
rownames(GenderParentYOData) <- c("ci_l", "pred", "ci_u")
GenderParentYOData <- as.data.frame(t(GenderParentYOData))
GenderParentYOData$gender <- rep(c("Boys", "Girls"), 2)
GenderParentYOData$agents <- "Parent"
GenderParentYOData$age <- c(rep("10-15", 2), rep("16-18", 2))
GenderParentYOData$age <- factor(GenderParentYOData$age, levels = c(
  "10-15", "16-18"))
GenderParentYOSESData <- data.frame(
  pred_yb = get_ci(ModelYoungBoysGenderParentSESInterac, 1),
  pred_yg = get_ci(ModelYoungGirlsGenderParentSESInterac, 1),
  pred_ob = get_ci(ModelOldBoysGenderParentSESInterac, 1),
  pred_og = get_ci(ModelOldGirlsGenderParentSESInterac, 1))
rownames(GenderParentYOSESData) <- c("ci_l", "pred", "ci_u")
GenderParentYOSESData <- as.data.frame(t(GenderParentYOSESData))
GenderParentYOSESData$gender <- rep(c("Boys", "Girls"), 2)
GenderParentYOSESData$agents <- "Parent"
GenderParentYOSESData$age <- c(rep("10-15", 2), rep("16-18", 2))
GenderParentYOSESData$age <- factor(GenderParentYOSESData$age, levels = c(
  "10-15", "16-18"))
GenderParentYOCtrlData <- data.frame(
  pred_yb = get_ci(ModelYoungBoysGenderParentCtrl, 1),
  pred_yg = get_ci(ModelYoungGirlsGenderParentCtrl, 1),
  pred_ob = get_ci(ModelOldBoysGenderParentCtrl, 1),
  pred_og = get_ci(ModelOldGirlsGenderParentCtrl, 1))
rownames(GenderParentYOCtrlData) <- c("ci_l", "pred", "ci_u")
GenderParentYOCtrlData <- as.data.frame(t(GenderParentYOCtrlData))
GenderParentYOCtrlData$gender <- rep(c("Boys", "Girls"), 2)
GenderParentYOCtrlData$agents <- "Parent"
GenderParentYOCtrlData$age <- c(rep("10-15", 2), rep("16-18", 2))
GenderParentYOCtrlData$age <- factor(GenderParentYOCtrlData$age, levels = c(
  "10-15", "16-18"))

AgentsYOData <- data.frame(
  mother_yb = get_ci(ModelYoungBoysMother, 1),
  mother_yg = get_ci(ModelYoungGirlsMother, 1),
  mother_ob = get_ci(ModelOldBoysMother, 1),
  mother_og = get_ci(ModelOldGirlsMother, 1),
  father_yb = get_ci(ModelYoungBoysFather, 1),
  father_yg = get_ci(ModelYoungGirlsFather, 1),
  father_ob = get_ci(ModelOldBoysFather, 1),
  father_og = get_ci(ModelOldGirlsFather, 1),
  femalefriends_yb = get_ci(ModelYoungBoysFemaleFriends, 1),
  femalefriends_yg = get_ci(ModelYoungGirlsFemaleFriends, 1),
  femalefriends_ob = get_ci(ModelOldBoysFemaleFriends, 1),
  femalefriends_og = get_ci(ModelOldGirlsFemaleFriends, 1),
  malefriends_yb = get_ci(ModelYoungBoysMaleFriends, 1),
  malefriends_yg = get_ci(ModelYoungGirlsMaleFriends, 1),
  malefriends_ob = get_ci(ModelOldBoysMaleFriends, 1),
  malefriends_og = get_ci(ModelOldGirlsMaleFriends, 1))
rownames(AgentsYOData) <- c("ci_l", "pred", "ci_u")
AgentsYOData <- as.data.frame(t(AgentsYOData))
AgentsYOData$gender <- rep(c("Boys", "Girls"), nrow(AgentsYOData) / 2)
AgentsYOData$agents <- c(rep("Mother", 4), rep("Father", 4),
                         rep("Female friends", 4), rep("Male friends", 4))
AgentsYOData$age <- rep(c(rep("10-15", 2), rep("16-18", 2)), 4)
AgentsYOData$age <- factor(AgentsYOData$age, levels = c(
  "10-15", "16-18"))
AgentsYOSESData <- data.frame(
  mother_yb = get_ci(ModelYoungBoysAgentsSES, 1),
  mother_yg = get_ci(ModelYoungGirlsAgentsSES, 1),
  mother_ob = get_ci(ModelOldBoysAgentsSES, 1),
  mother_og = get_ci(ModelOldGirlsAgentsSES, 1),
  father_yb = get_ci(ModelYoungBoysAgentsSES, 2),
  father_yg = get_ci(ModelYoungGirlsAgentsSES, 2),
  father_ob = get_ci(ModelOldBoysAgentsSES, 2),
  father_og = get_ci(ModelOldGirlsAgentsSES, 2),
  femalefriends_yb = get_ci(ModelYoungBoysAgentsSES, 3),
  femalefriends_yg = get_ci(ModelYoungGirlsAgentsSES, 3),
  femalefriends_ob = get_ci(ModelOldBoysAgentsSES, 3),
  femalefriends_og = get_ci(ModelOldGirlsAgentsSES, 3),
  malefriends_yb = get_ci(ModelYoungBoysAgentsSES, 4),
  malefriends_yg = get_ci(ModelYoungGirlsAgentsSES, 4),
  malefriends_ob = get_ci(ModelOldBoysAgentsSES, 4),
  malefriends_og = get_ci(ModelOldGirlsAgentsSES, 4))
rownames(AgentsYOSESData) <- c("ci_l", "pred", "ci_u")
AgentsYOSESData <- as.data.frame(t(AgentsYOSESData))
AgentsYOSESData$gender <- rep(c("Boys", "Girls"), nrow(AgentsYOSESData) / 2)
AgentsYOSESData$agents <- c(rep("Mother", 4), rep("Father", 4),
                             rep("Female friends", 4), rep("Male friends", 4))
AgentsYOSESData$age <- rep(c(rep("10-15", 2), rep("16-18", 2)), 4)
AgentsYOSESData$age <- factor(AgentsYOSESData$age, levels = c(
  "10-15", "16-18"))
confint <- confint(ModelOldBoysAgentsCtrl)
AgentsYOCtrlData <- data.frame(
  mother_yb = get_ci(ModelYoungBoysAgentsCtrl, 1),
  mother_yg = get_ci(ModelYoungGirlsAgentsCtrl, 1),
  mother_ob = c(confint[4,1], coef(ModelOldBoysAgentsCtrl)$Class[1,2],
               confint[4,2]),
  mother_og = get_ci(ModelOldGirlsAgentsCtrl, 1),
  father_yb = get_ci(ModelYoungBoysAgentsCtrl, 2),
  father_yg = get_ci(ModelYoungGirlsAgentsCtrl, 2),
  father_ob = c(confint[5,1], coef(ModelOldBoysAgentsCtrl)$Class[1,3],
               confint[5,2]),
  father_og = get_ci(ModelOldGirlsAgentsCtrl, 2),
  femalefriends_yb = get_ci(ModelYoungBoysAgentsCtrl, 3),
  femalefriends_yg = get_ci(ModelYoungGirlsAgentsCtrl, 3),
  femalefriends_ob = c(confint[6,1], coef(ModelOldBoysAgentsCtrl)$Class[1,4],
                     confint[6,2]),
  femalefriends_og = get_ci(ModelOldGirlsAgentsCtrl, 3),
  malefriends_yb = get_ci(ModelYoungBoysAgentsCtrl, 4),
  malefriends_yg = get_ci(ModelYoungGirlsAgentsCtrl, 4),
  malefriends_ob = c(confint[7,1], coef(ModelOldBoysAgentsCtrl)$Class[1,5],
                   confint[7,2]),
  malefriends_og = get_ci(ModelOldGirlsAgentsCtrl, 4))
rownames(AgentsYOCtrlData) <- c("ci_l", "pred", "ci_u")
AgentsYOCtrlData <- as.data.frame(t(AgentsYOCtrlData))
AgentsYOCtrlData$gender <- rep(c("Boys", "Girls"), nrow(AgentsYOCtrlData) / 2)
AgentsYOCtrlData$agents <- c(rep("Mother", 4), rep("Father", 4),
                             rep("Female friends", 4), rep("Male friends", 4))
AgentsYOCtrlData$age <- rep(c(rep("10-15", 2), rep("16-18", 2)), 4)
AgentsYOCtrlData$age <- factor(AgentsYOCtrlData$age, levels = c(
  "10-15", "16-18"))

DiscussParentYOData <- rbind(GenderParentYOData, AgentsYOData)
DiscussParentYOData$agents <- factor(DiscussParentYOData$agents, levels = c(
  "Parent", "Mother", "Father", "Female friends", "Male friends"))
DiscussParentYOSESData <- rbind(GenderParentYOSESData, AgentsYOSESData)
DiscussParentYOSESData$agents <- factor(DiscussParentYOSESData$agents, levels = c(
  "Parent", "Mother", "Father", "Female friends", "Male friends"))
DiscussParentYOCtrlData <- rbind(GenderParentYOCtrlData, AgentsYOCtrlData)
DiscussParentYOCtrlData$agents <- factor(DiscussParentYOCtrlData$agents, levels = c(
  "Parent", "Mother", "Father", "Female friends", "Male friends"))

DiscussParentYOData |>
  filter(agents %in% c("Parent", "Mother", "Father")) |>
  ggplot(aes(x = pred, y = age)) +
  geom_point() +
  facet_wrap(~ gender + agents, ncol = 3) +
  geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = 0.5) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_x_continuous(paste0(
    "\nTopic most often discussed with mother or father\n",
    "(vs. other topics)\n",
    "For Parent: Gender of parent who discusses that topic the most\n",
    "Left: Father, Right: Mother")) +
  scale_y_discrete("Age", limits = rev) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.y = ggtext::element_markdown(color = c(rep("black", 5), "red")),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/DiscussParentYO.pdf", width = 11, height = 4.25)
DiscussParentYOSESData |>
  filter(agents %in% c("Parent", "Mother", "Father")) |>
  ggplot(aes(x = pred, y = age)) +
  geom_point() +
  facet_wrap(~ gender + agents, ncol = 3) +
  geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = 0.5) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_x_continuous(paste0(
    "\nTopic most often discussed with mother or father\n",
    "(vs. other topics)\n",
    "For Parent: Gender of parent who discusses that topic the most\n",
    "Left: Father, Right: Mother")) +
  scale_y_discrete("Age", limits = rev) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.y = ggtext::element_markdown(color = c(rep("black", 5), "red")),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/DiscussParentYOSES.pdf", width = 11, height = 4.25)
DiscussParentYOCtrlData |>
  filter(agents %in% c("Parent", "Mother", "Father")) |>
  ggplot(aes(x = pred, y = age)) +
  geom_point() +
  facet_wrap(~ gender + agents, ncol = 3) +
  geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = 0.5) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_x_continuous(paste0(
    "\nTopic most often discussed with mother or father\n",
    "(vs. other topics)\n",
    "For Parent: Gender of parent who discusses that topic the most\n",
    "Left: Father, Right: Mother")) +
  scale_y_discrete("Age", limits = rev) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.y = ggtext::element_markdown(color = c(rep("black", 5), "red")),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/DiscussParentYOCtrl.pdf", width = 11, height = 4.25)
DiscussParentYOData |>
  filter(agents %in% c("Female friends", "Male friends")) |>
  ggplot(aes(x = pred, y = age)) +
  geom_point() +
  facet_wrap(~ gender + agents, ncol = 2) +
  geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = 0.5) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_x_continuous(paste0(
    "\nTopic most often discussed with female friends or\n",
    "male friends (vs. other topics)")) +
  scale_y_discrete("Age", limits = rev) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.y = ggtext::element_markdown(color = c(rep("black", 5), "red")),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/DiscussPeersYO.pdf", width = 11, height = 4.25)
DiscussParentYOSESData |>
  filter(agents %in% c("Female friends", "Male friends")) |>
  ggplot(aes(x = pred, y = age)) +
  geom_point() +
  facet_wrap(~ gender + agents, ncol = 2) +
  geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = 0.5) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_x_continuous(paste0(
    "\nTopic most often discussed with female friends or\n",
    "male friends (vs. other topics)")) +
  scale_y_discrete("Age", limits = rev) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.y = ggtext::element_markdown(color = c(rep("black", 5), "red")),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/DiscussPeersYOSES.pdf", width = 11, height = 4.25)
DiscussParentYOCtrlData |>
  filter(agents %in% c("Female friends", "Male friends")) |>
  ggplot(aes(x = pred, y = age)) +
  geom_point() +
  facet_wrap(~ gender + agents, ncol = 2) +
  geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = 0.5) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  scale_x_continuous(paste0(
    "\nTopic most often discussed with female friends or\n",
    "male friends (vs. other topics)")) +
  scale_y_discrete("Age", limits = rev) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.y = ggtext::element_markdown(color = c(rep("black", 5), "red")),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/DiscussPeersYOCtrl.pdf", width = 11, height = 4.25)

### 5. T-tests ####
t.test(CPIS$femalefriends_discuss_health,
 CPIS$malefriends_discuss_health)
t.test(CPIS$femalefriends_discuss_foreign,
 CPIS$malefriends_discuss_foreign)
t.test(CPIS$femalefriends_discuss_law,
 CPIS$malefriends_discuss_law)
t.test(CPIS$femalefriends_discuss_education,
 CPIS$malefriends_discuss_education)
t.test(CPISBoys$femalefriends_discuss_partisan,
 CPISGirls$malefriends_discuss_partisan)
t.test(CPISYoung$interest, CPISOld$interest) # interest higher for 16-18-year-olds
t.test(CPISYoung$interest_health, CPISOld$interest_health) # interest higher for 16-18-year-olds
t.test(CPISYoung$interest_foreign, CPISOld$interest_foreign) # interest higher for 16-18-year-olds
t.test(CPISYoung$interest_law, CPISOld$interest_law) # interest higher for 16-18-year-olds
t.test(CPISYoung$interest_education, CPISOld$interest_education) # interest higher for 16-18-year-olds
t.test(CPISYoung$interest_partisan, CPISOld$interest_partisan) # N.S.
CPISYoung$female <- as.numeric(CPISYoung$female)
CPISOld$female <- as.numeric(CPISOld$female)
t.test(CPISYoung$female, CPISOld$female) # 53% vs. 45% girls, p<0.05
t.test(CPISYoung$white, CPISOld$white) # 53% vs. 64% whites, p<0.01
t.test(CPISYoung$immig, CPISOld$immig) # 9% vs. 20% immigrnats, p<0.001

### 6. Correlations ####
CorrCPIS <- CPIS[c(21, 32:36)]
names(CorrCPIS) <- c("General", "Health care", "International affairs",
                      "Law and crime", "Education", "Partisan politics")
CorMatrixCPIS <- cor(CorrCPIS, use = "pairwise.complete.obs") # imputation
diag(CorMatrixCPIS) <- NA
ggcorrplot::ggcorrplot(CorMatrixCPIS, method = "circle",
                       legend.title = "Correlation") +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/CorMatrixCPIS.pdf", width = 11, height = 8.5)
CorrDG <- DG[191:196]
names(CorrDG) <- c("General", "Health care", "International affairs",
                   "Law and crime", "Education", "Partisan politics")
CorMatrixDG <- cor(CorrDG, use = "pairwise.complete.obs") # imputation
diag(CorMatrixDG) <- NA
ggcorrplot::ggcorrplot(CorMatrixDG, method = "circle",
                       legend.title = "Correlation") +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 17.5),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 17.5),
        strip.text.x = element_text(size = 17.5),
        axis.text.x = element_text(angle = 90),
        text = element_text(family = "CM Roman"))
ggsave("_graphs/CorMatrixDG.pdf", width = 11, height = 8.5)
