library(dplyr)
library(tidyr)
library(data.table)

#Dla PABIQ-IT-PN nie ma automatycznej konwersji typu date ze względu na pomieszane sposoby zapisu daty w danych
#W kodzie uwzględniona jest konwersja daty urodzenia, sb_date jest w prawidłowym formacie

#Wybierz plik z danymi:
#data <- read.csv("PABIQITPNNO_20231025.csv")
data <- read.csv("PABIQITPNPL_20231025.csv")

#Wybierz plik z kluczami:
keys <- read.csv("PABIQ-labels-IT-PN.csv")
keys <- keys %>% filter(keys$Label != "")
keys <- keys %>% filter(keys$If.needed != "")

#Wybierz plik z tłumaczeniami
trans <- read.csv("PABIQ-translations-IT-PN.csv")

#Wybierz język (na końcu KAŻDEJ linijki!):
trans <- trans %>% select(Translation, pl)
trans <- trans %>% rename(x = pl)
old <- keys$pl

#Koniec części z wyborami
#(Wers 96: konwersje wybranych kolumn częstotliwości używania języka - należy ustawić ręcznie,
#które mają się przekonwertować)
#Na koniec: zmiana nazwy pliku, w którym zapiszą się wyniki
################################################################################

new <- keys$Label

#Zmiana nazw kolumn
data2 <- setnames(data, old, new, skip_absent = T)
data2 <- data2 %>% select(any_of(new))

#Zmiana słów wewnątrz tabeli na tłumaczenia
trans <- trans %>% filter(trans$x != "")
for (i in 1:nrow(trans)){
  data2[data2 == trans$x[i]] <- trans$Translation[i]
}

#Połaczenie kolumn z datą urodzenia
#Ze względu na pojawiającą się różną konwencję zapisu w kolumnie date_birth_1 (czasem dd/mm/yyyy, czasem mm/dd/yyyy)
#uwzględnione są tylko zapisy, w których jednoznacznie udało się ustalić datę
data2["date_birth"] <- lapply(data2["date_birth"], function(x) as.Date(x, format="%d-%m-%Y"))
data2["date_birth_1a"] <- lapply(data2["date_birth_1"], function(x) as.Date(as.character(x), format = "%d/%m/%Y"))
data2["date_birth_1b"] <- lapply(data2["date_birth_1"], function(x) as.Date(as.character(x), format = "%m/%d/%Y"))
data2$date_birth_all <- data2$date_birth
dat <- c("date_birth_1a", "date_birth_1b", "date_birth_all")
data2[dat] <- lapply(data2[dat], as.character)
data2$date_birth_all <- ifelse(is.na(data2$date_birth_all) & is.na(data2$date_birth_1b), data2$date_birth_1a, data2$date_birth_all)
data2$date_birth_all <- ifelse(is.na(data2$date_birth_all) & is.na(data2$date_birth_1a), data2$date_birth_1b, data2$date_birth_all)
data2$date_birth_all <- ifelse(is.na(data2$date_birth_all) & data2$date_birth_1a == data2$date_birth_1b, data2$date_birth_1a, data2$date_birth_all)
data2["date_birth_all"] <- lapply(data2["date_birth_all"], function(x) as.Date(x, format="%Y-%m-%d"))
data2 <- data2 %>% select(-c("date_birth_1a", "date_birth_1b"))

#Konwersja kolumny first_contact_lang
fcl <- c("first_contact_native", "first_contact_other1", "first_contact_other2", "first_contact_other3")
data3 <- separate(data = data2, col = first_contact_lang, into = fcl, sep = "\\,")
data3[fcl] <- lapply(data3[fcl], gsub, pattern = "^.*: ", replacement = "")
data3[fcl] <- lapply(data3[fcl], as.integer)

#Konwersja kolumn first_word opcje "n miesięcy lub wcześniej/później" zamieniane są na "n"
fwp <- c("first_word")
data3[fwp] <- lapply(data3[fwp], gsub, pattern = " .*$", replacement = "")
data3[fwp] <- lapply(data3[fwp], as.integer)

#Konwersja kolumny first_word_1
fwp <- c("first_word_1")
data3 <- separate(data = data3, col = first_word_1, into = fwp, sep = "\\,")
data3[fwp] <- lapply(data3[fwp], gsub, pattern = "^.*: ", replacement = "")
data3[fwp] <- lapply(data3[fwp], as.integer)

#Połączenie kolumn first_word i first_word_1
data3$first_word_all <- data3$first_word
data3$first_word_all <- ifelse(is.na(data3$first_word_all), data3$first_word_1, data3$first_word_all)

#Konwersja kolumny weight_birth_1
wb1 <- c("weight_birth_1")
data3 <- separate(data = data3, col = weight_birth_1, into = wb1, sep = "\\,")
data3[wb1] <- lapply(data3[wb1], gsub, pattern = "^.*: ", replacement = "")
data3[wb1] <- lapply(data3[wb1], as.integer)

#Połączenie kolumn weight_birth i weight_birth_1
data3$weight_birth_all <- data3$weight_birth
data3$weight_birth_all <- ifelse(is.na(data3$weight_birth_all), data3$weight_birth_1, data3$weight_birth_all)

#Funkcje konwertujące kolumny częstości języka - należy podać aktualną tabelę, tytuł kolumny do zmiany
#i pożądane tytuły po rozdzieleniu (najlepiej: pierwotny_tytuł_oznaczeniejęzyka)
#UWAGA! Funkcje bazują na schemacie "test=wartość;" i wyciągają "wartość" spomiędzy "=" i ";".
#Jeśli w danym formularzu wyniki zapisują się inaczej - może być konieczne napisanie nowej funkcji.

#funkcja dla kolumn bez opcji "nigdy"
how_often_lang_function_1 <- function(data3, to_change, hol){
  pom <- c("x_how_often_lang_0_rarely","x_how_often_lang_0_sometimes","x_how_often_lang_0_often", "x_how_often_lang_0_always",
           "x_how_often_lang_1_rarely","x_how_often_lang_1_sometimes","x_how_often_lang_1_often", "x_how_often_lang_1_always",
           "x_how_often_lang_2_rarely","x_how_often_lang_2_sometimes","x_how_often_lang_2_often", "x_how_often_lang_2_always",
           "x_how_often_lang_3_rarely","x_how_often_lang_3_sometimes","x_how_often_lang_3_often", "x_how_often_lang_3_always")
  data4 <- separate(data = data3, col = to_change, into = pom, sep = "\\;")
  data4[pom] <- lapply(data4[pom], gsub, pattern = "^.*=", replacement = "")
  data4[pom] <- lapply(data4[pom], as.integer)
  
  hol_pom <- c("hol0", "hol1", "hol2", "hol3")
  data4$hol0 <- data4$x_how_often_lang_0_rarely*1+data4$x_how_often_lang_0_sometimes*2+data4$x_how_often_lang_0_often*3+data4$x_how_often_lang_0_always*4
  data4$hol1 <- data4$x_how_often_lang_1_rarely*1+data4$x_how_often_lang_1_sometimes*2+data4$x_how_often_lang_1_often*3+data4$x_how_often_lang_1_always*4
  data4$hol2 <- data4$x_how_often_lang_2_rarely*1+data4$x_how_often_lang_2_sometimes*2+data4$x_how_often_lang_2_often*3+data4$x_how_often_lang_2_always*4
  data4$hol3 <- data4$x_how_often_lang_3_rarely*1+data4$x_how_often_lang_3_sometimes*2+data4$x_how_often_lang_3_often*3+data4$x_how_often_lang_3_always*4
  
  data5 <- data4 %>% select(-pom)
  data5 <- setnames(data5, hol_pom, hol, skip_absent = T)
}

#funkcja dla kolumn z opcją "nigdy"
how_often_lang_function_2 <- function(data3, to_change, hol){
  pom <- c("x_how_often_lang_0_never", "x_how_often_lang_0_rarely","x_how_often_lang_0_sometimes","x_how_often_lang_0_often", "x_how_often_lang_0_always",
           "x_how_often_lang_1_never", "x_how_often_lang_1_rarely","x_how_often_lang_1_sometimes","x_how_often_lang_1_often", "x_how_often_lang_1_always",
           "x_how_often_lang_2_never", "x_how_often_lang_2_rarely","x_how_often_lang_2_sometimes","x_how_often_lang_2_often", "x_how_often_lang_2_always",
           "x_how_often_lang_3_never", "x_how_often_lang_3_rarely","x_how_often_lang_3_sometimes","x_how_often_lang_3_often", "x_how_often_lang_3_always")
  data4 <- separate(data = data3, col = to_change, into = pom, sep = "\\;")
  data4[pom] <- lapply(data4[pom], gsub, pattern = "^.*=", replacement = "")
  data4[pom] <- lapply(data4[pom], as.integer)
  
  hol_pom <- c("hol0", "hol1", "hol2", "hol3")
  data4$hol0 <- data4$x_how_often_lang_0_never*0+data4$x_how_often_lang_0_rarely*1+data4$x_how_often_lang_0_sometimes*2+data4$x_how_often_lang_0_often*3+data4$x_how_often_lang_0_always*4
  data4$hol1 <- data4$x_how_often_lang_1_never*0+data4$x_how_often_lang_1_rarely*1+data4$x_how_often_lang_1_sometimes*2+data4$x_how_often_lang_1_often*3+data4$x_how_often_lang_1_always*4
  data4$hol2 <- data4$x_how_often_lang_2_never*0+data4$x_how_often_lang_2_rarely*1+data4$x_how_often_lang_2_sometimes*2+data4$x_how_often_lang_2_often*3+data4$x_how_often_lang_2_always*4
  data4$hol3 <- data4$x_how_often_lang_3_never*0+data4$x_how_often_lang_3_rarely*1+data4$x_how_often_lang_3_sometimes*2+data4$x_how_often_lang_3_often*3+data4$x_how_often_lang_3_always*4
  
  data5 <- data4 %>% select(-pom)
  data5 <- setnames(data5, hol_pom, hol, skip_absent = T)
}

#Konwersja wybranych kolumn częstości języka

#which language frequency column we choose
to_change <- "how_often_lang"
#what labels we want to have at the end
hol <- c("how_often_native", "how_often_other1", "how_often_other2", "how_often_other3")
#we call our function
data_fin <- how_often_lang_function_1(data3, to_change, hol)

to_change <- "guardian1_lang_speak"
hol <- c("guardian1_speak_native", "guardian1_speak_other1", "guardian1_speak_other2", "guardian1_speak_other3")  
data_fin <- how_often_lang_function_2(data_fin, to_change, hol)

to_change <- "guardian1_lang_child"
hol <- c("guardian1_child_native", "guardian1_child_other1", "guardian1_child_other2", "guardian1_child_other3")  
data_fin <- how_often_lang_function_2(data_fin, to_change, hol)

to_change <- "guardian2_lang_speak"
hol <- c("guardian2_speak_native", "guardian2_speak_other1", "guardian2_speak_other2", "guardian2_speak_other3")  
data_fin <- how_often_lang_function_2(data_fin, to_change, hol)

to_change <- "guardian2_lang_child"
hol <- c("guardian2_child_native", "guardian2_child_other1", "guardian2_child_other2", "guardian2_child_other3")  
data_fin <- how_often_lang_function_2(data_fin, to_change, hol)

to_change <- "guardian3_lang_speak"
hol <- c("guardian3_speak_native", "guardian3_speak_other1", "guardian3_speak_other2", "guardian3_speak_other3")  
data_fin <- how_often_lang_function_2(data_fin, to_change, hol)

to_change <- "guardian3_lang_child"
hol <- c("guardian3_child_native", "guardian3_child_other1", "guardian3_child_other2", "guardian3_child_other3")  
data_fin <- how_often_lang_function_2(data_fin, to_change, hol)

to_change <- "guardian4_lang_speak"
hol <- c("guardian4_speak_native", "guardian4_speak_other1", "guardian4_speak_other2", "guardian4_speak_other3")  
data_fin <- how_often_lang_function_2(data_fin, to_change, hol)

to_change <- "guardian4_lang_child"
hol <- c("guardian4_child_native", "guardian4_child_other1", "guardian4_child_other2", "guardian4_child_other3")  
data_fin <- how_often_lang_function_2(data_fin, to_change, hol)

to_change <- "other_children_lang_speak"
hol <- c("other_children_speak_native", "other_children_speak_other1", "other_children_speak_other2", "other_children_speak_other3")  
data_fin <- how_often_lang_function_2(data_fin, to_change, hol)

to_change <- "other_children_lang_child"
hol <- c("other_children_child_native", "other_children_child_other1", "other_children_child_other2", "other_children_child_other3")  
data_fin <- how_often_lang_function_2(data_fin, to_change, hol)


#Type conversion
intg <- c() #integer
dat <- c()  #date
ch <- c()   #character

for (i in 1:nrow(keys)){
  if(keys$Type.import[i] == "date"){
    dat <- append(dat, keys$Label[i])
  }
  if(keys$Type.import[i] == "integer"){
    intg <- append(intg, keys$Label[i])
  }
  if(keys$Type.import[i] == "character"){
    ch <- append(ch, keys$Label[i])
  }
}

data_fin[ch] <- lapply(data_fin[ch], as.character)
data_fin[intg] <- lapply(data_fin[intg], as.integer)
#data_fin[dat] <- lapply(data_fin[dat], function(x) as.Date(x, format="%d-%m-%Y"))

write.csv(data_fin,"data_ready_it_pn_pl.csv",fileEncoding = "UTF-8")
