#Welcome to FlexText-to-tsv.R
#This R-Code takes FlexText files and converts them into tsv files.
  #FlexText is a XML like format which divides a corpus into several layers,
  #the Phrase, consisting of sentence level information, and series of words:
    #Interlinear Text: a given text in a corpus
      #Paragraph: a section of an interlinear text
        #Phrase: a sentence or equivalent within a paragraph
          #Translations: information about the meaning of a sentence
          #Word: an individual word item in the phrase
            #Item: an aspect of the word, such as its transcription, gloss, or POS
#The code makes two kinds of tables (tsv files),
  #one for information on the word content of given phrases
  #one for the information on the translations of given phrases
#By applying these to ranges of paragraphs, large files can be made into tables piecemeal
  #Compile-flex-tsv.R will take several tsv files and combine them to make a single tibble

#Edit Export Location: Where do you want to export your tsv files?
ExportLocation = "FilesExport/"
#Edit FlexText FileName: What is the FlexText file you want to read?
FlexTextFileName = "Files/Import/allfiles.flextext"
#Edit the last bit of code at the bottom of this file to determine
  #what range of paragraphs you want to export for large files


#Load tidyverse
library(tidyverse)
#Read FlexText File
Files=read_file(FlexTextFileName)
#Convert FlexText to an Array of Interlinear Texts
Interlinear = Files %>% 
  str_remove(regex("^.*?(?=<interlinear-text)",dotall = TRUE)) %>% 
  str_replace_all(regex("(?<=</interlinear-text>).*?(?=<interlinear-text)",
                        dotall = TRUE),
                  "!!!!!") %>% 
  str_split("!!!!!") %>% 
  unlist()
#For each Interlinear Text, find:
  #the GUID (provided by Flex),
  #the title of the Interlinear Text
  #the paragraph content 
  #an ID number for the given text (in order of appearance)
#Then, make a Dataframe which contains each value for each Interlinear Text
#Write this dataframe to save for later
Interlinear.guid = c()
Interlinear.title = c()
Interlinear.paragraphs = c()
Interlinear.number = c()
for (i in seq(length(Interlinear))){
  Interlinear.guid = c(Interlinear.guid,
                       str_extract(Interlinear[i],"(?<=interlinear-text guid=\")[^\"]*(?=\")"))
  Interlinear.title = c(Interlinear.title,
                        str_extract(Interlinear[i],"<item type=\"title\".*?</item>") %>% 
                          str_remove("<item type=\"title\"[^>]*>") %>% 
                          str_remove("</item>"))
  Interlinear.paragraphs = c(Interlinear.paragraphs,
                             str_extract(Interlinear[i],regex("(?<=<paragraphs>).*(?=</paragraphs>)",dotall = TRUE)))
  Interlinear.number=c(Interlinear.number,i)
}
DF.IL = tibble(IL.GUID = Interlinear.guid,
               IL.TITLE = Interlinear.title,
               IL.NUM = Interlinear.number,
               IL.PPS = Interlinear.paragraphs)
dfil = DF.IL %>% select(IL.NUM,IL.TITLE)
write_tsv(dfil,paste0(ExportLocation,"dfil.txt"))
#Split each paragraph content Interlinear Text into individual paragraphs,
#For each paragraph, find:
  #the GUID (provided by Flex),
  #an ID number for the given paragraph (in order of appearance within an Interlinear Text),
  #the ID number of the Interlinear Text which the paragraph belongs to
  #the phrase content 
#Then, make a Dataframe which contains each value for each Paragraph
Paragraph.guid=c()
Paragraph.number=c()
Paragraph.interlinearid=c()
Paragraph.phrases=c()
for (i in seq(length(Interlinear))){
  InterlinearID = i
  Paragraphs = Interlinear.paragraphs[i] %>% 
    str_replace_all(regex("</paragraph>.*?<paragraph",
                          dotall = TRUE),
                    "</paragraph>!!!!!<paragraph") %>% 
    str_split("!!!!!") %>% 
    unlist()
  
  for (j in seq(length(Paragraphs))){
    Paragraph.guid=c(Paragraph.guid,
                     str_extract(Paragraphs[j],"(?<=guid=\")[^\"]*(?=\")"))
    Paragraph.number=c(Paragraph.number,j)
    Paragraph.interlinearid=c(Paragraph.interlinearid,i)
    Paragraph.phrases = c(Paragraph.phrases,
                          str_extract(Paragraphs[j],regex("(?<=<phrases>).*(?=</phrases>)",dotall = TRUE)))
  }
}
DF.PP = tibble(PP.GUID = Paragraph.guid,
               PP.NUM = Paragraph.number,
               IL.NUM = Paragraph.interlinearid,
               LN.CONTENT = Paragraph.phrases)
#Make a dataframe with relevant information of each paragraph:
  #Title of the Interlinear Text
  #Number ID of the Interlinear Text
  #Number ID of the Paragraph Text
  #Phrase content
DF = DF.PP %>% 
  left_join(DF.IL) %>% 
  select(IL.TITLE,IL.NUM,PP.NUM,LN.CONTENT)

#A function which finds the Item Type for each Item
Item.Type = function(str){str %>% str_extract("(?<=type=\").*?(?=\")")}
#A function which finds the Language for each Item
Item.Lang = function(str){str %>% str_extract("(?<=lang=\").*?(?=\")")}
#A function which finds the Content for each Item
Item.Cont = function(str){str %>% str_extract("(?<=>).*(?=</item>)")}
#A function which makes a tibble from the Type, Language, and Content of an Item
ParseItem = function(vector){
  tibble(Type = Item.Type(vector),
         Language = Item.Lang(vector),
         Content = Item.Cont(vector))
}
#A function which parses a given word in a phrase,
  #the function finds all items in the word, and parses them, forming a tibble of
  #all items. The resulting tibble is given a WordID, based off of a number which
  #is input into the system.
ParseWord = function(str,n=0){
  str %>% 
    str_replace_all(regex("</item>.*?<item",dotall = TRUE),
                    "</item>!!!!!<item") %>% 
    str_split("!!!!!") %>% 
    unlist() %>% 
    ParseItem() %>% 
    mutate(WordID = n)
}
#A function which performs ParseWord on all words in a phrase,
  #giving each word a unique WordID (based on order of appearance)
ParseWords = function(str){
  wordlist = str %>% 
    str_replace_all(regex("</word>.*?<word",dotall = TRUE),
                    "</word>!!!!!<word") %>% 
    str_split("!!!!!") %>% 
    unlist()
  results = tibble(Type=character(),
                   Language=character(),
                   Content=character(),
                   WordID=numeric())
  for (i in seq(length(wordlist))){
    results = bind_rows(results,ParseWord(wordlist[i],i))
  }
  return(results)
}
#A function which performs ParseWords, adding information on:
  #Interlinear Text Number (IL)
  #Paragraph Number (PP)
  #Line Number (phrase number, LN)
PhraseParseMaker = function(str,il=0,pp=0,ln=0){
  str %>% 
    str_extract(regex("<words>.*</words>",dotall = TRUE)) %>% 
    ParseWords() %>% 
    mutate(IL = il, PP = pp, LN = ln)
}
#A function which, given a phrase, finds:
  #The phrase's Free Translation (Free.Trans)
  #The phrase's Literal Translation (Lit.Trans)
  #adding information on:
    #Interlinear Text Number (IL)
    #Paragraph Number (PP)
    #Line Number (phrase number, LN)
PhraseTransMaker = function(str,il=0,pp=0,ln=0){
  string = str %>% 
    str_remove(regex("^.*</words>",dotall=TRUE))
  tibble(Free.Trans= string %>% 
           str_extract(regex("(?<=<item type=\"gls\" lang=\"en\">).*?(?=</item>)",dotall=TRUE)),
         Lit.Trans=string %>% 
           str_extract(regex("(?<=<item type=\"lit\" lang=\"en\">).*?(?=</item>)",dotall=TRUE)),
         IL = il, PP = pp, LN = ln) %>% return()
}
#A function which applies PhraseTransMaker and PhraseParseMaker to all phrases in a paragraph,
#Then binds all PhraseTransMaker tibbles together and all PhraseParseMaker tibbles together,
#Then returns a list of these two tibbles.
ParagraphMaker = function(str,il=0,pp=0){
  phraselist = str %>% 
    str_remove(regex(".*<phrases>",dotall=TRUE)) %>% 
    str_remove(regex("</phrases>.*",dotall=TRUE)) %>% 
    str_replace_all(regex("</phrase>.*?<phrase",dotall = TRUE),
                    "</phrase>!!!!!<phrase") %>% 
    str_split("!!!!!") %>% 
    unlist()
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
  for (i in seq(length(phraselist))){
    ParseTable = bind_rows(ParseTable,PhraseParseMaker(phraselist[i],il,pp,i))
    TransTable = bind_rows(TransTable,PhraseTransMaker(phraselist[i],il,pp,i))
  }
  return(list(Parse = ParseTable,
              Trans = TransTable))
}
#A function which applies ParagraphMaker to a range of paragraphs, supplied by the DF
  #and creates a larger combined list of the Parse and Trans tibbles
ParagraphMakerSectional = function(range){
  str=DF$LN.CONTENT[range]
  il=DF$IL.NUM[range]
  pp=DF$PP.NUM[range]
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
  for (i in seq(length(il))){
    Paragraph = ParagraphMaker(str[i],il[i],pp[i])
    ParseTable = bind_rows(ParseTable,Paragraph$Parse)
    TransTable = bind_rows(TransTable,Paragraph$Trans)
  }
  return(list(Parse=ParseTable,
              Trans=TransTable))
}
#A function which takes a range of paragraphs and performs
  #ParagraphMakerSectional to said range, then exports the results as a tsv
  #under FilesExport/Parse and FilesExport/Trans respectively
WritePartial = function(range){
  x = ParagraphMakerSectional(range)
  Parse.Title = paste0(ExportLocation,"Parse-",min(range),"-",max(range),".txt")
  Trans.Title = paste0(ExportLocation,"Trans-",min(range),"-",max(range),".txt")
  write_tsv(x$Parse,Parse.Title)
  write_tsv(x$Trans,Trans.Title)
}

#This last bit of code is a loop which allows for multiple ranges to be run in sequence,
  #allowing one to run the program continuously without worry of losing progress if
  #they need to close the computer or restart it.
#The ranges in xbegin and xend now will make the code write tsvs for:
  #paragraphs 1-10, then 11-20, then 21-30, then 31-40
#Replace the numbers in xbegin and xend to your choice of range and run!
xbegin = c(1,11,21,31)
xend = c(10,20,30,40)
for (i in seq(length(xbegin))){
  WritePartial(xbegin[i]:xend[i])
}

