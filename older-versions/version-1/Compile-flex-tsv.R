#Welcome to Compile-flex-tsv.R
#This R-Code takes multiple tsv files made by FlexText-to-tsv.R and combines
  #them into one dataframe for whatever your needs may be!

#Edit Import Location: From where do you want to import your tsv files?
ImportLocation = "FilesExport/"
#Load tidyverse
library(tidyverse)
#Make list of file names for Parse and Trans
Files = list.files(ImportLocation)
ParseFiles = Files[str_detect(Files,"Parse")]
TransFiles = Files[str_detect(Files,"Trans")]
#Make empty tibbles to be filled in!
ParseTable = tibble(Type=character(),
                    Language=character(),
                    Content=character(),
                    WordID=numeric(),
                    IL=numeric(),
                    PP=numeric(),
                    LN=numeric())
TransTable = tibble(Free.Trans=character(),
                    Lit.Trans=character(),
                    IL=numeric(),
                    PP=numeric(),
                    LN=numeric())
#Fill in the tibbles with each file
for (i in ParseFiles){
  ParseTable = bind_rows(ParseTable,read_tsv(paste0(ImportLocation,i)))
}
for (i in TransFiles){
  TransTable = bind_rows(TransTable,read_tsv(paste0(ImportLocation,i)))
}
#make a dataframe where each Word is a row rather than each Item,
  #Curly brackets allow for empty Items to be recognized as individual
  #items rather than double spaces for LaTeX use with gb4e
df = ParseTable %>% 
  select(-Language) %>% 
  pivot_wider(names_from=Type,
              values_from=Content) %>% 
  mutate(Line1 = ifelse(!is.na(txt),txt,punct),
         Line2 = ifelse(!is.na(gls),gls,punct),
         Line1 = ifelse(is.na(Line1),"{}",paste0("{",Line1,"}")),
         Line2 = ifelse(is.na(Line2),"{}",paste0("{",Line2,"}")))
#Import titles for each Interlinear Text
Titles = read_tsv(paste0(ImportLocation,"dfil.txt")) %>% 
  mutate(IL = IL.NUM,
         Title = IL.TITLE) %>% 
  select(IL, Title)
#Phrases is now a dataframe which contains each phrase in your corpus!
phrases = df %>% 
  group_by(IL,PP,LN) %>% 
  summarise(Text = paste(Line1,collapse=" "),
            Gloss = paste(Line2,collapse=" ")) %>% 
  left_join(TransTable) %>% 
  left_join(Titles)
