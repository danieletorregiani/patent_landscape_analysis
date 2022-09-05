## EXAM PROJECT ##

rm(list=ls())
library(rstudioapi)
current_path<-getActiveDocumentContext()$path
setwd(dirname(current_path))
library(readxl)
library(tidyverse)

# importo dataset
data1 = read_excel("data.xlsx")
View(data1)


# 1) Publication_number: Id of the patent
# 2) Publication_year: Publication Year
# 3) Currentowners: Applicant's name
# 4) id_Currentowners: ID of the applicant
# 5) Currentownerscountrycode: Applicant's country
# 6) Inventors: Inventor's name
# 7) Inventorscountrycode: Inventor's country
# 8) CooperativeClassification: CPC code
# 9) CooperativeClassificationlabel: CPC label


## CPC - COOPERATIVE PATENT CLASSIFICATION
## Y02W - CLIMATE CHANGE MITIGATION TECHNOLOGIES RELATED TO WASTEWATER TREATMENT OR WASTE MANAGEMENT

# focus su brevetti con tecnologie CPC: Y02W 
data2 = data1 %>% filter(grepl("Y02W", CooperativeClassification)) %>% select(Publication_number)
data = inner_join(data1, data2, by = "Publication_number") %>% distinct() # elimino duplicati con distinct


## DATA ELABORATION: MACRO LEVEL

# 1° - NUMBER OF PATENTS BY PERIOD - TABLE
# seleziono solo brevetti univoci
one1 = data %>% distinct(Publication_number, Publication_year)

# aggiungo colonna per il decennio relativo alla data di pubblicazione del brevetto
for (i in 1:nrow(one1)){
  if (between(one1$Publication_year[i], 1900, 1909)){
    one1$Period[i] = "1900-1909"
  }
  else if (between(one1$Publication_year[i], 1910, 1919)){
    one1$Period[i] = "1910-1919"
  }
  else if (between(one1$Publication_year[i], 1920, 1929)){
    one1$Period[i] = "1920-1929"
  }
  else if (between(one1$Publication_year[i], 1930, 1939)){
    one1$Period[i] = "1930-1939"
  }
  else if (between(one1$Publication_year[i], 1940, 1949)){
    one1$Period[i] = "1940-1949"
  }
  else if (between(one1$Publication_year[i], 1950, 1959)){
    one1$Period[i] = "1950-1959"
  }
  else if (between(one1$Publication_year[i], 1960, 1969)){
    one1$Period[i] = "1960-1969"
  }
  else if (between(one1$Publication_year[i], 1970, 1979)){
    one1$Period[i] = "1970-1979"
  }
  else if (between(one1$Publication_year[i], 1980, 1989)){
    one1$Period[i] = "1980-1989"
  }
  else if (between(one1$Publication_year[i], 1990, 1999)){
    one1$Period[i] = "1990-1999"
  }
  else if (between(one1$Publication_year[i], 2000, 2009)){
    one1$Period[i] = "2000-2009"
  }
  else if (between(one1$Publication_year[i], 2010, 2019)){
    one1$Period[i] = "2010-2019"
  }
  else {
    one1$Period[i] = "2020-2029"
  }
}

# conteggio numero di brevetti per decennio e aggiungo riga per il totale
one = one1 %>% count(Period, name = "Number of Patents")
one$`Number of Patents` = as.numeric(one$`Number of Patents`)
total = c("Total", sum(one$`Number of Patents`))
one = rbind(one, total)
View(one)


# 2° - NUMBER OF PATENTS - TIME SERIES

two = one1 %>% count(Publication_year, name = "Number of Patents")
colnames(two)[1] = c("Publication Year")
two %>%
  ggplot(aes(`Publication Year`, `Number of Patents`)) +
  geom_line() +
  scale_x_continuous(breaks = c(1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010,2020)) +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  labs(title = "Number of Patents", y = element_blank()) #+
 # ggsave(filename = "Num_patents2.png")


# 3° - NUMBER OF PATENTS BY APPLICANT COUNTRY (TOP 10)

# dataframe solo con i 10 Paesi con più brevetti
three1 = data %>% 
  drop_na(Currentownerscountrycode) %>% 
  count(Currentownerscountrycode, name = "Patents", sort = TRUE)
colnames(three1)[1] = "Applicant"
three1 = three1 %>%
  mutate(Applicant = fct_reorder(Applicant, Patents))
three = three1[1:10,]

three %>% 
  mutate(Applicant = fct_reorder(Applicant, Patents)) %>%
  ggplot(aes(Applicant, Patents)) +
  geom_bar(fill = "#009933", stat = "identity") +
  labs(title = "Number of Patents", y = element_blank()) #+
 # ggsave("Patents_by_Applicant3.png")


# 4° - NUMBER OF PATENTS BY INVENTOR COUNTRY (TOP 10)

# dataframe solo con i 10 Paesi con più brevetti
four1 = data %>% 
  drop_na(Inventorscountrycode) %>% 
  count(Inventorscountrycode, name = "Patents", sort = TRUE)
colnames(four1)[1] = "Inventor"
four1 = four1 %>%
  mutate(Inventor = fct_reorder(Inventor, Patents))
four = four1[1:10,]

four %>% 
  mutate(Inventor = fct_reorder(Inventor, Patents)) %>%
  ggplot(aes(Inventor, Patents)) +
  geom_bar(fill = "#CC0000", stat = "identity") +
labs(title = "Number of Patents", y = element_blank()) #+
  #ggsave("Patents_by_Inventor4.png")

# confronto Paesi Applicants - Inventors
colnames(four1)[1] = "Country"
colnames(three1)[1] = "Country"
full = full_join(three1, four1,  by = "Country")
full = full %>% pivot_longer(cols = c("Patents.x", "Patents.y"))
full %>%
  mutate(Country = fct_reorder(Country, value)) %>%
  drop_na(value) %>% 
  head(20) %>% 
  ggplot(aes(Country, value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Patents", y = element_blank(), fill = element_blank()) +
  scale_fill_manual(values = c("#009933", "#CC0000"),
    labels = c('Applicants', 'Inventors')) #+
  #ggsave("Applicant_vs_Inventor.png")


# 5° - THE NUMBER OF CPC CLASSES OF PATENTS RELATED TO Y02 TECHNOLOGY

# estraggo solo le prime 4 lettere del codice CPC
five = data %>% distinct() %>% select(c(1,2,8))
five$CPC = substr(five$CooperativeClassification, 1, 4)
# costruisco una tabella con il conteggio delle altre tecnologie contenute nei brevetti con Y02W
five %>% drop_na() %>% filter(CPC != "Y02W") %>%
  count(CPC, name = "Frequency", sort = TRUE) %>%  head(20) %>%
  View()


# 6° - NUMBER OF PATENT COLLABORATION

# no NA per Stato e Id dell'applicant
six_1 = data %>% drop_na(id_Currentowners, Currentownerscountrycode) 
six = six_1 %>% count(Publication_number, name = "CPC_for_Patent") # numero totale brevetti = 17364 
six = six %>% filter(CPC_for_Patent > 1) # brevetti con più di una tecnologia al suo interno

# collaborazione se brevetto con più di una tecnologia prodotte da applicant diversi
coll = inner_join(six_1, six, by = "Publication_number")

# funzione per contare collaborazioni nei brevetti
x = 0

collaboration = function(dataframe, x){

  for (i in 1:(nrow(dataframe) - 1)){
    if (dataframe$Publication_number[i] == dataframe$Publication_number[i+1]) {
      if (dataframe$id_Currentowners[i] != dataframe$id_Currentowners[i+1]) {
        x = x + 1
      }  
    }
  }
  
return(x)
  
}

# Numero di collaborazioni 
n_coll = collaboration(coll, x)
n_coll

# Numero totale di brevetti senza NA
tot = 17364

tab = data.frame(rbind(c("Number of patent with multiple applicants", n_coll, round(n_coll/tot,2)),
                       c("Number of total patent", tot, tot/tot)))
colnames(tab) = c("Variable", "Number", "%")

tab

# ma la collaborazione è un fenomeno recente? 
# filtro i dati dal 2010 in poi e costruisco la stessa tabella
six_2 = six_1 %>% filter(Publication_year > 2009)
six_b = six_2 %>% count(Publication_number, name = "CPC_for_Patent") # numero totale brevetti = 7014 
six_b = six_b %>% filter(CPC_for_Patent > 1) # brevetti con più di una tecnologia al suo interno

# collaborazione se brevetto con più di una tecnologia prodotte da applicant diversi
coll2 = inner_join(six_2, six_b, by = "Publication_number")

# numero di collaborazioni dal 2010 in poi
n_coll2 = collaboration(coll2, x)
n_coll2

# Numero totale di brevetti senza NA dal 2010 in poi
tot1 = 7014

tab1 = data.frame(rbind(c("Number of patent with multiple applicants", n_coll2, round(n_coll2/tot1,2)),
                       c("Number of total patent", tot1, tot1/tot1)))
colnames(tab1) = c("Variable (Year > 2010)", "Number", "%")

tab1 # le collaborazioni sono un fenomeno più recente

# 7° - NUMBER OF INTERNATIONAL AND NATIONAL PATENT COLLABORATION

# funzione per contare collaborazioni nazionali ed internazionali nei brevetti
y = 0
z = 0

int_nat = function(dataframe, y, z){
  
  for (i in 1:(nrow(dataframe) - 1)){
    if (dataframe$Publication_number[i] == dataframe$Publication_number[i+1]) {
      if (dataframe$id_Currentowners[i] != dataframe$id_Currentowners[i+1]) {
        if (dataframe$Currentownerscountrycode[i] == dataframe$Currentownerscountrycode[i+1]) {
          y = y + 1
        } else {z = z + 1}
      }  
    }
  }
  
  return(a = c(y,z))
  
}

# Numero di collaborazioni nazionali e internazionali
n_int_nat = int_nat(coll, y, z )
n_int_nat

tab2 = data.frame(rbind(c("National collaborations", n_int_nat[1], round(n_int_nat[1]/n_coll, 2)),
                       c("International collaborations", n_int_nat[2], round(n_int_nat[2]/n_coll, 2)),
                       c("Total number of collaborations", n_coll, n_coll/n_coll)))
colnames(tab2) = c("Variable", "Number", "%")

tab2


# 8° - DISTRIBUTION OF FIRMS BY NUMBER OF PATENT
# creo grafico a barre in cui: 
# ascisse = numero di brevetti
# ordinate = numero di imprese con quei brevetti

eight1 = data %>% filter(!is.na(id_Currentowners)) %>% 
  distinct(Publication_number, id_Currentowners, CooperativeClassification, .keep_all = TRUE) %>%
  count(id_Currentowners, name = "Patents", sort = TRUE)

eight = data.frame(table(eight1$Patents))
eight %>%
  ggplot(aes(Var1, Freq)) +
  geom_col(fill = "red") +
  labs(title = "Distribution of Firms by number of Patents",
       x = "Number of Patents",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) #+
  #ggsave("Firms_npatents8.png")


## DATA ELABORATION: FIRM LEVEL


# 9 ° - DISTRIBUTION OF PATENT BY APPLICANT

nine = data %>% drop_na(Currentowners) %>%
  distinct(Publication_number, Currentowners, CooperativeClassification, .keep_all = TRUE) %>%
  count(Currentowners, name = "Number_of_Patents", sort = TRUE)

# funzione per aggiungere una colonna con il numero totale di brevetti ed una con le percentuali 
add <- function(dataframe) {
  
  dataframe = dataframe %>%
    mutate(Tot = sum(Number_of_Patents)) %>%
    mutate(Perc = Number_of_Patents/Tot)
  return(dataframe)
  
}

# funzione per calcolare il Concentration Ratio Index
CR <- function(dataframe) {
  
  dataframe <- head(dataframe, n=4)
  cr <- sum(dataframe$Perc)
  return(cr)
  
}

# 20 imprese con maggior numero di brevetti Y02W
nine = add(nine)
head(nine, 20)

# Concentration Ratio Index
Conc_ind = CR(nine)
Conc_ind


# 10° - DISTRIBUTION OF PATENT BY APPLICANT (DIFFERENT PERIODS)

# patents before 2000
ten_1 = data %>% filter(Publication_year < 2000) %>% 
  drop_na(Currentowners) %>%
  distinct(Publication_number, Currentowners, CooperativeClassification, .keep_all = TRUE) %>%
  count(Currentowners, name = "Number_of_Patents", sort = TRUE)
ten_1 = add(ten_1)

# patents between 2000 and 2009
ten_2 = data %>% filter(between(Publication_year, 2000, 2009)) %>% 
  drop_na(Currentowners) %>%
  distinct(Publication_number, Currentowners, CooperativeClassification, .keep_all = TRUE) %>%
  count(Currentowners, name = "Number_of_Patents", sort = TRUE)
ten_2 = add(ten_2)

# patents after 2009
ten_3 = data %>% filter(Publication_year > 2009) %>% 
  drop_na(Currentowners) %>%
  distinct(Publication_number, Currentowners, CooperativeClassification, .keep_all = TRUE) %>%
  count(Currentowners, name = "Number_of_Patents", sort = TRUE)
ten_3 = add(ten_3)

# confronto leadership nel tempo 
ten = cbind(head(ten_1, 10), head(ten_2, 10), head(ten_3, 10))
View(ten)

# CR Index nel tempo
CR_before = CR(ten_1)
CR_between = CR(ten_2)
CR_after = CR(ten_3)
print(c(CR_before, CR_between, CR_after))
# cambia anche l'indice di concentrazione nel tempo

