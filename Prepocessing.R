library(tidyverse)
library(XML)
library("methods")
library(stopwords)
library(textstem)

# Setting paths for where tp load and save data
load_path = "data/raw/finance"
save_path = "data/processed/finance"

#### Make idList ####
files = list.files(paste0(load_path,"/metadata"))
idList = c()

#Extract id's from the filenames in the metadata folder
for (x in 1:length(files)){
  id = gsub("journal-article-", "",files[x])
  id = gsub(".xml", "",id)
  idList[x] = id
}

##### Make data frame with meta data ####
meta_data_list = list()
i = 1
for (id in idList){
  
  # load xml data from one meta data file
  meta = xmlParse(file = paste0(load_path,"/metadata/journal-article-",id,".xml"))
  
  # convert xml data to list element
  meta = xmlToList(meta)
  
  #extract language tag
  lang = meta[1]$front$`article-meta`$`custom-meta-group`$`custom-meta`$`meta-value`
  
  #Only look at english articles
  try(if (lang %in% c("en", "eng", "EN")) { 
    
    # Extract article type tag
    type = meta$.attrs[[1]]
    if (is.null(type)) type = ""
    if (type == "1.0") {
      type = meta$.attrs[[2]]
    }
    
    # Only look at articles of type: research-article
    if (type == "research-article"){
      
      #Extract the title of the article
      title = meta[1]$front$`article-meta`$`title-group`$`article-title`[[1]]
      if (is.null(title)) title = ""
      
      # Extract the number of contributing authors
      ncon = sum(names(meta[1]$front$`article-meta`$`contrib-group`) == "contrib")
      if (is.null(ncon)) ncon = ""
      
      #Extract the names of the authors
      names = c() #Emty list for putting names in
      for (aNum in 1:ncon){  #loop through authors
        # Extract the first name
        first = try(meta[1]$front$`article-meta`$`contrib-group`[aNum]$contrib$`string-name`$`given-names`, silent = T)
        if (is.null(first)) {
          names = "ERROR" # If NULL is returned there is an error
          break # if error then break the loop
        }
        # first name wasnøt in the other place try this
        if (class(first) == "try-error") first = meta[1]$front$`article-meta`$`contrib-group`[aNum]$contrib$`string-name`
        first = tolower(first) # make lower case
        first = gsub("\\b[a-z][[:punct:]]\\s*","",first) # remove puntuation and single lettes
        first = gsub(" ","",first) # remove spaces
        
        #Extract last name (and same as above)
        last = try(meta[1]$front$`article-meta`$`contrib-group`[aNum]$contrib$`string-name`$surname, silent = T)
        if (is.null(last)) last = ""
        if (class(last) == "try-error") last = ""
        last = tolower(last)
        last = gsub('\\b[a-z][[:punct:]]\\s*','',last)
        last = gsub(" ","",last)
        
        # Put the author names together whith "," in between and no spaces
        if (aNum != ncon) names = paste0(names,first,last,",") else names = paste0(names,first,last)
      }
      
      # Extract year and moth of publication
      year = as.numeric(meta[1]$front$`article-meta`$`pub-date`$year)
      month = meta[1]$front$`article-meta`$`pub-date`$month
      if (is.null(month)){
        month = 0
        timeMonth = 0
      } else {
        month = as.numeric(month)
        timeMonth = (month/12 + (month-1)/12)/2 # convert months into format of time variable
        # The above equation puts the time of publication in the middle of the month (not the start or the ending as the would create abiguity)
      }
      # create time variable
      time = year - 1993 + timeMonth
      
      # Create the article row of the dataframe
      row = data.frame(
        ID =id,
        title = title,
        ncon = ncon,
        names = names,
        time = time,
        year = year,
        month = month)
      
      # list of rows
      meta_data_list[[i]] = row
      i = i + 1

    }
  } , silent = T)
}

# join all the rows into a data frame
meta_data = do.call(rbind, meta_data_list)

# Remove rows with name error
meta_data2 = subset(meta_data, names != "ERROR")

# time variables that produced NA's are fixed
meta_data2$time[is.na(meta_data2$time)] = meta_data2$year[is.na(meta_data2$time)] - 1993

# Articles with the title "Book Reviews" is removed
meta_data2 = subset(meta_data2, title != "Book Reviews")



##### Process text #####

#stop words
stopwords = stopwords(source = "smart")
wordList = c("al", "pp", "cf", "ii")
stopwords = c(stopwords, wordList)

#list for putting missing id's in
missing = c()

# loop through articles 
for (id in meta_data2$ID){
  
  #Load unigram data
  doc = try(read.csv(paste(load_path,"/ngram1/journal-article-",id,"-ngram1.txt", sep=""), sep = "", stringsAsFactors = F, header = F), silent = T)
  
  # if the file wasn't there put the id in missing ELSE process the data
  if (class(doc) == "try-error"){
    missing = c(missing,id)
  } else { 
    # convert column names
    colnames(doc) = c("word", "freq")
    
    # remove single letter tokens
    doc = doc[!(grepl("^.$", doc$word)),]
    
    #remove digits
    doc = doc[!(grepl("[[:digit:]]", doc$word)),]
    
    #lematize all tokens
    doc$word = lemmatize_words(as.character(doc$word))
    
    # remove stopwords
    doc = subset(doc, !(word %in% stopwords))
    
    # create a new doc for putting alle the lemmatized words togehter (the word social is the most frequent word in cogntion corpus)
    new_doc = data.frame(word = "social", freq = 0)
    
    # Loop through words 
    for (r in 1:length(doc[,1])){
      word = doc[r,1]
      freq = doc[r,2]
      
      # if the word is in the new doc, then add to the frequency ELSE create a new row with this word
      if (word %in% new_doc$word){
        new_doc$freq[new_doc$word == word] = new_doc$freq[new_doc$word == word] + freq
      } else {
        row = data.frame(word = word, freq = freq)
        new_doc = rbind(new_doc,row)
      }
    }
    
    # Save the new doc as processed file
    write.csv(new_doc, paste0(save_path,"/1gram/",id,".csv"))
  }
}

# remove missing IDs from the meta data
meta_data2 = subset(meta_data2, !(ID %in% missing))

#Save the meta data.
write.csv(meta_data2, paste0(save_path,"/metadata.csv"))


#####