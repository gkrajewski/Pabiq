library(dplyr)
library(tidyr)
library(data.table)

#Wybierz plik z danymi:
#data <- read.csv("PABIQCDINO_20231018.csv")
#data <- read.csv("PABIQWP3PL_20231018.csv")
data <- read.csv("PABIQSCRPL_20231124.csv")

#Wybierz plik z kluczami:
keys <- read.csv("labels_pabiq.csv")
keys <- keys %>% filter(keys$Label != "")
keys <- keys %>% filter(keys$If.needed != "")

#Wybierz plik z tłumaczeniami
trans <- read.csv("translations_pabiq.csv")

#Wybierz język (na końcu KAŻDEJ linijki!):
trans <- trans %>% select(Translation, pl)
trans <- trans %>% rename(x = pl)
old <- keys$pl

#Koniec części z wyborami
#(Wers 97: konwersje wybranych kolumn częstotliwości używania języka - należy ustawić ręcznie,
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

#Konwersja kolumny first_contact_lang
fcl <- c("first_contact_native", "first_contact_other1", "first_contact_other2", "first_contact_other3")
data3 <- separate(data = data2, col = first_contact_lang, into = fcl, sep = "\\,")
data3[fcl] <- lapply(data3[fcl], gsub, pattern = "^.*: ", replacement = "")
data3[fcl] <- lapply(data3[fcl], as.integer)

#Konwersja kolumn first_word, first_phrase - opcje "n miesięcy lub wcześniej/później" zamieniane są na "n"
fwp <- c("first_word", "first_phrase")
data3[fwp] <- lapply(data3[fwp], gsub, pattern = " .*$", replacement = "")
data3[fwp] <- lapply(data3[fwp], as.integer)

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

to_change <- "friends_lang"
hol <- c("friends_native", "friends_other1", "friends_other2", "friends_other3")  
data_fin <- how_often_lang_function_2(data_fin, to_change, hol)

to_change <- "guests_lang"
hol <- c("guests_native", "guests_other1", "guests_other2", "guests_other3")
data_fin <- how_often_lang_function_2(data_fin, to_change, hol)

to_change <- "other_children_lang"
hol <- c("other_children_native", "other_children_other1", "other_children_other2", "other_children_other3")  
data_fin <- how_often_lang_function_2(data_fin, to_change, hol)

to_change <- "guardian1_lang"
hol <- c("guardian1_native", "guardian1_other1", "guardian1_other2", "guardian1_other3")  
data_fin <- how_often_lang_function_2(data_fin, to_change, hol)

to_change <- "guardian2_lang"
hol <- c("guardian2_native", "guardian2_other1", "guardian2_other2", "guardian2_other3")  
data_fin <- how_often_lang_function_2(data_fin, to_change, hol)

to_change <- "guardian3_lang"
hol <- c("guardian3_native", "guardian3_other1", "guardian3_other2", "guardian3_other3")  
data_fin <- how_often_lang_function_2(data_fin, to_change, hol)

to_change <- "guardian4_lang"
hol <- c("guardian4_native", "guardian4_other1", "guardian4_other2", "guardian4_other3")  
data_fin <- how_often_lang_function_2(data_fin, to_change, hol)

#Funkcja konwertująca kolumny częstości aktywności w danym języku - podać aktualną tabelę, tytuł kolumny do zmiany
#i pożądane tytuły po rozdzieleniu

how_often_activities_function <- function(data3, to_change, hoa){
  pom <- c("x_books_never","x_books_sometimes","x_books_everyday", "x_computer_never","x_computer_sometimes","x_computer_everyday",
           "x_films_never","x_films_sometimes","x_films_everyday", "x_stories_never","x_stories_sometimes","x_stories_everyday",
           "x_songs_never","x_songs_sometimes","x_songs_everyday")
  data4 <- separate(data = data3, col = to_change, into = pom, sep = "\\;")
  data4[pom] <- lapply(data4[pom], gsub, pattern = "^.*=", replacement = "")
  data4[pom] <- lapply(data4[pom], as.integer)
  
  hoa_pom <- c("x_books", "x_computer", "x_films", "x_stories", "x_songs")
  data4$x_books <- data4$x_books_never*0 + data4$x_books_sometimes*1 + data4$x_books_everyday*2
  data4$x_books <- ifelse(data4$x_books == 0 & data4$x_books_never == 0, NA, data4$x_books)
  data4$x_computer <- data4$x_computer_never*0 + data4$x_computer_sometimes*1 + data4$x_computer_everyday*2
  data4$x_computer <- ifelse(data4$x_computer == 0 & data4$x_computer_never == 0, NA, data4$x_computer)
  data4$x_films <- data4$x_films_never*0 + data4$x_films_sometimes*1 + data4$x_films_everyday*2
  data4$x_films <- ifelse(data4$x_films == 0 & data4$x_films_never == 0, NA, data4$x_films)
  data4$x_stories <- data4$x_stories_never*0 + data4$x_stories_sometimes*1 + data4$x_stories_everyday*2
  data4$x_stories <- ifelse(data4$x_stories == 0 & data4$x_stories_never == 0, NA, data4$x_stories)
  data4$x_songs <-data4$x_songs_never*0 + data4$x_songs_sometimes*1 + data4$x_songs_everyday*2
  data4$x_songs <- ifelse(data4$x_songs == 0 & data4$x_songs_never == 0, NA, data4$x_songs)
  
  data5 <- data4 %>% select(-pom)
  data5 <- setnames(data5, hoa_pom, hoa, skip_absent = T)
}

#Konwersja kolumn częstości aktywności w danych językach

to_change <- "activity_freq_native"
hoa <- c("books_native", "computer_native", "films_native", "stories_native", "songs_native")  
data_fin <- how_often_activities_function(data_fin, to_change, hoa)

to_change <- "activity_freq_other1"
hoa <- c("books_other1", "computer_other1", "films_other1", "stories_other1", "songs_other1")  
data_fin <- how_often_activities_function(data_fin, to_change, hoa)

if("activity_freq_other2" %in% names(data_fin)){
  to_change <- "activity_freq_other2"
  hoa <- c("books_other2", "computer_other2", "films_other2", "stories_other2", "songs_other2")  
  data_fin <- how_often_activities_function(data_fin, to_change, hoa)
}

if("activity_freq_other3" %in% names(data_fin)){
  to_change <- "activity_freq_other3"
  hoa <- c("books_other3", "computer_other3", "films_other3", "stories_other3", "songs_other3")  
  data_fin <- how_often_activities_function(data_fin, to_change, hoa)
}

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
data_fin[dat] <- lapply(data_fin[dat], function(x) as.Date(x, format="%d-%m-%Y"))

write.csv(data_fin,"data_ready_uk.csv",fileEncoding = "UTF-8")
