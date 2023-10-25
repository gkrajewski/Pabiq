library(stringr)
library(lubridate)
library(dplyr)
library(Multilada)
library(data.table)

#CHOOSE DATA
data_calc <- read.csv("data_ready_no.csv")

#AGE CALCULATION
data_calc$age_months <- age_months(data_calc$date_birth_old, data_calc$sb_date)

#RISK CALCULATION - GENERAL DEVELOPMENT AND FAMILY HISTORY DATA, UNIVERSAL FOR ANY LANGUAGE
data_calc[data_calc$first_word < 16 & !is.na(data_calc$first_word), "first_word"] <- 6
data_calc[data_calc$first_word >= 16 & data_calc$first_word < 24 & !is.na(data_calc$first_word), "first_word"] <- 4
data_calc[data_calc$first_word >= 24 & !is.na(data_calc$first_word), "first_word"] <- 0
data_calc$first_word <- as.integer(data_calc$first_word)

data_calc[data_calc$first_phrase < 25 & !is.na(data_calc$first_phrase), "first_phrase"] <- 6
data_calc[data_calc$first_phrase >= 25 & data_calc$first_phrase < 30 & !is.na(data_calc$first_phrase), "first_phrase"] <- 4
data_calc[data_calc$first_phrase >= 30 & !is.na(data_calc$first_phrase), "first_phrase"] <- 0
data_calc$first_phrase <- as.integer(data_calc$first_phrase)

data_calc[data_calc$worried == "no", "worried"] <- 2
data_calc[data_calc$worried == "yes", "worried"] <- 0
data_calc$worried <- as.integer(data_calc$worried)

data_calc[data_calc$hearing_loss== "no", "hearing_loss"] <- 1
data_calc[data_calc$hearing_loss== "yes", "hearing_loss"] <- 0
data_calc$hearing_loss<- as.integer(data_calc$hearing_loss)

data_calc[data_calc == ""] <- NA

#IMPORTANT!
#to count the number of child's relatives with specific problems, I count the number of commas in the proper data cell and add 1
#(after making sure to exclude the "not that I know of" option)
#REMEMBER to check if that's okay with your data and apply proper corrections if necessary

data_calc$problem_read_write <-ifelse(data_calc$problem_read_write == "", NA, ifelse(data_calc$problem_read_write == 0, 0, str_count(data_calc$problem_read_write,",")+1))
data_calc$problem_understand <-ifelse(data_calc$problem_understand == "", NA, ifelse(data_calc$problem_understand == 0, 0, str_count(data_calc$problem_understand,",")+1))
data_calc$problem_speak <-ifelse(data_calc$problem_speak == "", NA, ifelse(data_calc$problem_speak == 0, 0, str_count(data_calc$problem_speak,",")+1))

data_calc$family_difficulties <- (15 - data_calc$problem_read_write - data_calc$problem_understand - data_calc$problem_speak)*2/3

#calculation of exposure component that is universal for any language
data_calc$Exp_Risk <- data_calc$first_word + data_calc$first_phrase + data_calc$worried + data_calc$hearing_loss + data_calc$family_difficulties


#CONVERSIONS WORDS -> NUMBERS

#IMPORTANT!
#to count the number of child's activities in the language, I count the number of commas in the proper data cell and add 1
#REMEMBER to check if that's okay with your data and apply proper corrections if necessary (example below)

#FOR POLISH ONLY - corrections for native_q1, other1_q1, other2_q1, other3_q1
data_calc$native_q1 <- gsub("książki, czasopisma, komiksy, gazety", "książki/czasopisma/komiksy/gazety", data_calc$native_q1)
if("other1_q1" %in% names(data_calc)){
  data_calc$other1_q1 <- gsub("książki, czasopisma, komiksy, gazety", "książki/czasopisma/komiksy/gazety", data_calc$other1_q1)
}
if("other2_q1" %in% names(data_calc)){
  data_calc$other2_q1 <- gsub("książki, czasopisma, komiksy, gazety", "książki/czasopisma/komiksy/gazety", data_calc$other2_q1)
}
if("other3_q1" %in% names(data_calc)){
  data_calc$other3_q1 <- gsub("książki, czasopisma, komiksy, gazety", "książki/czasopisma/komiksy/gazety", data_calc$other3_q1)
}

#FOR NORWEGIAN ONLY - corrections for native_q1, other1_q1, other2_q1, other3_q1
data_calc$native_q1 <- gsub("bøker, blader, tegneserier, aviser", "bøker/blader/tegneserier/aviser", data_calc$native_q1)
if("other1_q1" %in% names(data_calc)){
  data_calc$other1_q1 <- gsub("bøker, blader, tegneserier, aviser", "bøker/blader/tegneserier/aviser", data_calc$other1_q1)
}
if("other2_q1" %in% names(data_calc)){
  data_calc$other2_q1 <- gsub("bøker, blader, tegneserier, aviser", "bøker/blader/tegneserier/aviser", data_calc$other2_q1)
}
if("other3_q1" %in% names(data_calc)){
  data_calc$other3_q1 <- gsub("bøker, blader, tegneserier, aviser", "bøker/blader/tegneserier/aviser", data_calc$other3_q1)
}

#BACK TO CONVERSIONS AND CALCULATIONS
data_calc$native_q1 <-ifelse(data_calc$native_q1 == "", NA, str_count(data_calc$native_q1,",")+1)
if("other1_q1" %in% names(data_calc)){
  data_calc$other1_q1 <-ifelse(data_calc$other1_q1 == "", NA, str_count(data_calc$other1_q1,",")+1)
}
if("other2_q1" %in% names(data_calc)){
  data_calc$other2_q1 <-ifelse(data_calc$other2_q1 == "", NA, str_count(data_calc$other2_q1,",")+1)
}
if("other3_q1" %in% names(data_calc)){
  data_calc$other3_q1 <-ifelse(data_calc$other3_q1 == "", NA, str_count(data_calc$other3_q1,",")+1)
}


#TOTAL EXPOSURE CALCULATION - FUNCTION
exp_sum_calculation <- function(data_sum, lang_cols, additional_cols){
  
  #adjustment of column names
  cols_pom <- c("x_first_contact", "x_q1", "x_q2", "x_q3", "x_q4", "x_q5", "x_q6", "x_how_often", "x_guardian1", "x_guardian2", "x_guardian3", "x_guardian4",
                "x_friends", "x_guests", "x_other_children", "x_books", "x_computer", "x_films", "x_stories", "x_songs")
  data_sum2 <- setnames(data_sum, lang_cols, cols_pom, skip_absent = T)
  
  #number of months of contact withe the language
  data_sum2$x_lang_timeframe <- data_sum2$age_months - data_sum2$x_first_contact
  
  #early exposition
  data_sum2$x_early_exp <- data_sum2$x_lang_timeframe*data_sum2$x_how_often
  
  data_sum2$x_current_skills <- data_sum2$x_q2 + data_sum2$x_q3 + data_sum2$x_q4 + data_sum2$x_q5 + data_sum2$x_q6  
  
  data_sum2$x_other_people_contact <- data_sum2$x_guardian1 *2 + data_sum2$x_guests + data_sum2$x_friends *2
  data_sum2$x_other_people_contact <- ifelse(data_sum2$is_guardian2 == "yes", data_sum2$x_other_people_contact + 2* data_sum2$x_guardian2, data_sum2$x_other_people_contact)
  data_sum2$x_other_people_contact <- ifelse(data_sum2$other_guardians >0, data_sum2$x_other_people_contact + data_sum2$x_guardian3, data_sum2$x_other_people_contact)
  data_sum2$x_other_people_contact <- ifelse(data_sum2$other_guardians >1, data_sum2$x_other_people_contact + data_sum2$x_guardian4, data_sum2$x_other_people_contact)
  data_sum2$x_other_people_contact <- ifelse(data_sum2$child_number_house >1, data_sum2$x_other_people_contact + 2* data_sum2$x_other_children, data_sum2$x_other_people_contact)
  
  data_sum2$x_situations <- data_sum2$x_books + data_sum2$x_computer + data_sum2$x_films + data_sum2$x_stories + data_sum2$x_songs
  data_sum2$x_Exposure <- data_sum2$Exp_Risk + data_sum2$x_lang_timeframe + data_sum2$x_early_exp + data_sum2$x_current_skills + data_sum2$x_q1 + data_sum2$x_situations + data_sum2$x_other_people_contact
  
  #readjustment of column names and setting proper names of the new columns
  cols_pom2 <- c("x_lang_timeframe", "x_early_exp", "x_current_skills", "x_other_people_contact", "x_situations", "x_Exposure")
  data_sum2 <- setnames(data_sum2, cols_pom2, additional_cols, skip_absent = T)
  data_sum2 <- setnames(data_sum2, cols_pom, lang_cols, skip_absent = T)
}

#TOTAL EXPOSURE CALCULATION
lang_cols_native <- c("first_contact_native", "native_q1", "native_q2", "native_q3", "native_q4", "native_q5", "native_q6", "how_often_native", "guardian1_native", "guardian2_native", "guardian3_native", "guardian4_native",
                      "friends_native", "guests_native", "other_children_native", "books_native", "computer_native", "films_native", "stories_native", "songs_native")
additional_cols_native <- c("lang_timeframe_native", "early_exp_native", "current_skills_native", "other_people_contact_native", "situations_native", "Exposure_native")
data_calc_exp <- exp_sum_calculation(data_calc, lang_cols_native, additional_cols_native)

lang_cols_other1 <- c("first_contact_other1", "other1_q1", "other1_q2", "other1_q3", "other1_q4", "other1_q5", "other1_q6", "how_often_other1", "guardian1_other1", "guardian2_other1", "guardian3_other1", "guardian4_other1",
                      "friends_other1", "guests_other1", "other_children_other1", "books_other1", "computer_other1", "films_other1", "stories_other1", "songs_other1")
additional_cols_other1 <- c("lang_timeframe_other1", "early_exp_other1", "current_skills_other1", "other_people_contact_other1", "situations_other1", "Exposure_other1")
data_calc_exp <- exp_sum_calculation(data_calc_exp, lang_cols_other1, additional_cols_other1)

lang_cols_other2 <- c("first_contact_other2", "other2_q1", "other2_q2", "other2_q3", "other2_q4", "other2_q5", "other2_q6", "how_often_other2", "guardian1_other2", "guardian2_other2", "guardian3_other2", "guardian4_other2",
                      "friends_other2", "guests_other2", "other_children_other2", "books_other2", "computer_other2", "films_other2", "stories_other2", "songs_other2")
additional_cols_other2 <- c("lang_timeframe_other2", "early_exp_other2", "current_skills_other2", "other_people_contact_other2", "situations_other2", "Exposure_other2")
data_calc_exp <- exp_sum_calculation(data_calc_exp, lang_cols_other2, additional_cols_other2)

lang_cols_other3 <- c("first_contact_other3", "other3_q1", "other3_q2", "other3_q3", "other3_q4", "other3_q5", "other3_q6", "how_often_other3", "guardian1_other3", "guardian2_other3", "guardian3_other3", "guardian4_other3",
                      "friends_other3", "guests_other3", "other_children_other3", "books_other3", "computer_other3", "films_other3", "stories_other3", "songs_other3")
additional_cols_other3 <- c("lang_timeframe_other3", "early_exp_other3", "current_skills_other3", "other_people_contact_other3", "situations_other3", "Exposure_other3")
data_calc_exp <- exp_sum_calculation(data_calc_exp, lang_cols_other3, additional_cols_other3)

write.csv2(data_calc_exp,"data_calc_no.csv",fileEncoding = "UTF-8")

#END

#data_calc_selected <- data_calc_exp %>% select("id", "Exposure_native")
#data_calc_selected1 <- data_calc_selected[!is.na(data_calc_selected$Exposure_native),]

#data_native <- data_calc_exp %>% select("id", "Exp_Risk", lang_cols_native, additional_cols_native)
#data_native <- data_native[!is.na(data_native$Exposure_native),]
#write.csv2(data_native,"data_calc_native1.csv",fileEncoding = "UTF-8")
