library(stringr)
library(lubridate)
library(dplyr)
library(Multilada)
library(data.table)

#CHOOSE DATA
data_calc <- read.csv("data_ready_it_pn_pl.csv")

#AGE CALCULATION
data_calc$age_months <- age_months(data_calc$date_birth_all, data_calc$sb_date)

#RISK CALCULATION - GENERAL DEVELOPMENT AND FAMILY HISTORY DATA, UNIVERSAL FOR ANY LANGUAGE
data_calc[data_calc$first_word_all < 16 & !is.na(data_calc$first_word_all), "first_word_all"] <- 6
data_calc[data_calc$first_word_all >= 16 & data_calc$first_word_all < 24 & !is.na(data_calc$first_word_all), "first_word_all"] <- 4
data_calc[data_calc$first_word_all >= 24 & !is.na(data_calc$first_word_all), "first_word_all"] <- 0
data_calc$first_word_all <- as.integer(data_calc$first_word_all)

data_calc[data_calc$worried == "no", "worried"] <- 2
data_calc[data_calc$worried == "yes", "worried"] <- 0
data_calc$worried <- as.integer(data_calc$worried)

data_calc[data_calc$hearing_loss== "no", "hearing_loss"] <- 0.5
data_calc[data_calc$hearing_loss== "yes", "hearing_loss"] <- 0
data_calc$hearing_loss<- as.numeric(data_calc$hearing_loss)

data_calc[data_calc$drain== "no", "drain"] <- 0.5
data_calc[data_calc$drain== "yes", "drain"] <- 0
data_calc$drain<- as.numeric(data_calc$drain)

data_calc[data_calc$ear_infections== "no", "ear_infections"] <- 0.5
data_calc[data_calc$ear_infections== "yes", "ear_infections"] <- 0
data_calc$ear_infections<- as.numeric(data_calc$ear_infections)

#data_calc[data_calc$other_illnesses== "no", "other_illnesses"] <- 0.5
#data_calc[data_calc$other_illnesses== "yes", "other_illnesses"] <- 0
#data_calc$other_illnesses<- as.numeric(data_calc$other_illnesses)

data_calc[data_calc == ""] <- NA

data_calc$hearing_problems <- data_calc$hearing_loss + data_calc$drain + data_calc$ear_infections #+ data_calc$other_illnesses

#IMPORTANT!
#to count the number of child's relatives with specific problems, I count the number of commas in the proper data cell and add 1
#REMEMBER to check if that's okay with your data and apply proper corrections if necessary

data_calc$problem_read_write <-ifelse(data_calc$problem_read_write == "", NA, ifelse(data_calc$problem_read_write == 0, 0, str_count(data_calc$problem_read_write,",")+1))
data_calc$problem_understand <-ifelse(data_calc$problem_understand == "", NA, ifelse(data_calc$problem_understand == 0, 0, str_count(data_calc$problem_understand,",")+1))
data_calc$problem_speak <-ifelse(data_calc$problem_speak == "", NA, ifelse(data_calc$problem_speak == 0, 0, str_count(data_calc$problem_speak,",")+1))

data_calc$family_difficulties <- (15 - data_calc$problem_read_write - data_calc$problem_understand - data_calc$problem_speak)*2/3

#calculation of exposure component that is universal for any language
data_calc$Exp_Risk <- data_calc$first_word_all + data_calc$worried + data_calc$hearing_problems + data_calc$family_difficulties


#TOTAL EXPOSURE CALCULATION - FUNCTION
exp_sum_calculation <- function(data_sum, lang_cols, additional_cols){
  
  #adjustment of column names
  cols_pom <- c("x_first_contact", "x_how_often", "x_guardian1_speak", "x_guardian1_child", "x_guardian2_speak", "x_guardian2_child",
                "x_guardian3_speak", "x_guardian3_child", "x_guardian4_speak", "x_guardian4_child", "x_other_children_speak", "x_other_children_child")
  
  data_sum2 <- setnames(data_sum, lang_cols, cols_pom, skip_absent = T)
  
  #number of months of contact withe the language
  data_sum2$x_lang_timeframe <- data_sum2$age_months - data_sum2$x_first_contact
  
  #early exposition
  data_sum2$x_early_exp <- data_sum2$x_lang_timeframe*data_sum2$x_how_often
  
  data_sum2$x_input <- data_sum2$x_guardian1_speak *2 + data_sum2$family_lang
  data_sum2$x_input <- ifelse(data_sum2$is_guardian2 == "yes", data_sum2$x_input + 2* data_sum2$x_guardian2_speak, data_sum2$x_input)
  data_sum2$x_input <- ifelse(data_sum2$other_guardians >0, data_sum2$x_input + data_sum2$x_guardian3_speak, data_sum2$x_input)
  data_sum2$x_input <- ifelse(data_sum2$other_guardians >1, data_sum2$x_input + data_sum2$x_guardian4_speak, data_sum2$x_input)
  data_sum2$x_input <- ifelse(data_sum2$siblings >0, data_sum2$x_input + 2* data_sum2$x_other_children_speak, data_sum2$x_input)
  #for native we can add family language frequency
  if(lang_cols[1] == "first_contact_native"){data_sum2$x_input <- data_sum2$x_input + data_sum2$family_lang}
  
  data_sum2$x_output <- data_sum2$x_guardian1_child *2
  data_sum2$x_output <- ifelse(data_sum2$is_guardian2 == "yes", data_sum2$x_output + 2* data_sum2$x_guardian2_child, data_sum2$x_output)
  data_sum2$x_output <- ifelse(data_sum2$other_guardians >0, data_sum2$x_output + data_sum2$x_guardian3_child, data_sum2$x_output)
  data_sum2$x_output <- ifelse(data_sum2$other_guardians >1, data_sum2$x_output + data_sum2$x_guardian4_child, data_sum2$x_output)
  data_sum2$x_output <- ifelse(data_sum2$siblings >0, data_sum2$x_output + 2* data_sum2$x_other_children_child, data_sum2$x_output)
  
  data_sum2$x_Exposure <- data_sum2$Exp_Risk + data_sum2$x_early_exp + data_sum2$x_lang_timeframe + data_sum2$x_input + data_sum2$x_output
  
  #readjustment of column names and setting proper names of the new columns
  cols_pom2 <- c("x_lang_timeframe", "x_early_exp", "x_input", "x_output", "x_Exposure")
  data_sum2 <- setnames(data_sum2, cols_pom2, additional_cols, skip_absent = T)
  data_sum2 <- setnames(data_sum2, cols_pom, lang_cols, skip_absent = T)
}

#TOTAL EXPOSURE CALCULATION
lang_cols_native <- c("first_contact_native", "how_often_native", "guardian1_speak_native", "guardian1_child_native", "guardian2_speak_native", "guardian2_child_native",
                      "guardian3_speak_native", "guardian3_child_native", "guardian4_speak_native", "guardian4_child_native", "other_children_speak_native", "other_children_child_native")
additional_cols_native <- c("lang_timeframe_native", "early_exp_native", "input_native", "output_native", "Exposure_native")
data_calc_exp <- exp_sum_calculation(data_calc, lang_cols_native, additional_cols_native)

lang_cols_other1 <- c("first_contact_other1", "how_often_other1", "guardian1_speak_other1", "guardian1_child_other1", "guardian2_speak_other1", "guardian2_child_other1",
                      "guardian3_speak_other1", "guardian3_child_other1", "guardian4_speak_other1", "guardian4_child_other1", "other_children_speak_other1", "other_children_child_other1")
additional_cols_other1 <- c("lang_timeframe_other1", "early_exp_other1", "input_other1", "output_other1", "Exposure_other1")
data_calc_exp <- exp_sum_calculation(data_calc_exp, lang_cols_other1, additional_cols_other1)

lang_cols_other2 <- c("first_contact_other2", "how_often_other2", "guardian1_speak_other2", "guardian1_child_other2", "guardian2_speak_other2", "guardian2_child_other2",
                      "guardian3_speak_other2", "guardian3_child_other2", "guardian4_speak_other2", "guardian4_child_other2", "other_children_speak_other2", "other_children_child_other2")
additional_cols_other2 <- c("lang_timeframe_other2", "early_exp_other2", "input_other2", "output_other2", "Exposure_other2")
data_calc_exp <- exp_sum_calculation(data_calc_exp, lang_cols_other2, additional_cols_other2)

lang_cols_other3 <- c("first_contact_other3", "how_often_other3", "guardian1_speak_other3", "guardian1_child_other3", "guardian2_speak_other3", "guardian2_child_other3",
                      "guardian3_speak_other3", "guardian3_child_other3", "guardian4_speak_other3", "guardian4_child_other3", "other_children_speak_other3", "other_children_child_other3")
additional_cols_other3 <- c("lang_timeframe_other3", "early_exp_other3", "input_other3", "output_other3", "Exposure_other3")
data_calc_exp <- exp_sum_calculation(data_calc_exp, lang_cols_other3, additional_cols_other3)

write.csv2(data_calc_exp,"data_calc_it_pn_pl.csv",fileEncoding = "UTF-8")

#END

#data_calc_selected <- data_calc_exp %>% select("id", "Exposure_native")
#data_calc_selected1 <- data_calc_selected[!is.na(data_calc_selected$Exposure_native),]
