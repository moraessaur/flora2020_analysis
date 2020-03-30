#### Flora 2020 Analysis Functions ####


# All functions, those written inline and also those sourced, will be listed in this file #


# clean_dates:
  # converts string to NA if it is not exactly four characters in length
  
clean_dates <- function(x){ # where x is a string e.g. "1984"
  if(is.na(x)){return(NA)  # keeps the NA value if the entry is already a NA
    break
  } 
  if (nchar(x) == 4){return(x)}# Check if the entry has four characters 
  else{return(NA)}} # if it does not, return NA

#--------------------------------------------------------------

# year_fix
  # converts the value of x (an integer) to NA if it is not between 1500 and 2020

year_fix <- function(x){
  if (is.na(x)){return(NA)} #if the year is NA, return NA 
  if (x<1500 || x>2020){return(NA)}else{return(x)} #values outside the boundary as NA
}

#--------------------------------------------------------------

# collapse_column
  # uses a species id (x), filters a df using this id
  # then collapses all results from a column of this data frame to a single string
  # all entries contained in this string will be separated by the split character

collapse_column <- function(x,column,df,split_character=";"){ 
  df_temp <- df %>% filter(id == x) # filter df by species id
  # if this information does NOT exist for the species, return NA
  if (length(rownames(df_temp)) == 0){return(NA)} else { 
    # if it exists, paste all information in a single string
    output <- paste(df_temp[[column]],collapse = split_character) 
    return(output) # return the string in which each value is separated by a ";"
  }
}

#--------------------------------------------------------------

# alter_empty
  # this function works exclusively from within the json_parser function
  # the data stored as json in the files has different "missing information" formats
  # in some, missing data is stored as "" in others it is stored as "{}"
  # this function converts all empty cases to "{}"


alter_empty <- function(x){if (x == "" || x == "{}" || is.na(x)){
  x <- "{}"
}
  return(x)}


#--------------------------------------------------------------

# json_parser
  # this functions parses the entries that are stored as json
  # the input is a string which contains a json entry as formated in the files 
  # makes use of the jsonlite package

json_parser <- function(vetor){ # string containing a json entry
  vetor <- as.character(vetor)
  vetor_clean <- map(vetor,alter_empty) # use alter_empty to format empty values
  k <- map(vetor_clean,fromJSON)
  t <- map(k,names)
  nomes <- unique(unlist(t))
  temp_df <- as_tibble(data.frame((matrix(ncol=length(names),nrow = length(k)))))
  temp <- rep("",length(k))
  
  for (t in 1:length(nomes)){ 
    for (i in 1:length(k)){
      query <- grep(nomes[t], names(k[[i]]))
      if(is.null(names(k[[i]])) || length(names(k[[i]])) == 0 ||length(query)==0){
        temp[i] <- NA
        next
      }
      temp[i] <- paste(k[[i]][[query]],collapse = ";")
    }
    temp_df[,t] <- temp
  }
  colnames(temp_df) <- nomes
  return(temp_df)
}

#--------------------------------------------------------------

# function_split_pluck
  # splits a string to a vector and picks a given position of it

function_split_pluck <- function(string,split=";",pluck=2){
  string %>% # string that will be splitted
    strsplit(split = split) %>% # split argument
    unlist() %>% # convert to vector
    pluck(pluck) # pluck position value
}

#--------------------------------------------------------------

# century_freq
  # counts the frequency of years (desc_year) that appear within a given century (query)
  # needs a column called desc_century which contains the century of a given description

century_freq <- function(df,query){ # where df is the data frame I am working on
  # and query is the century being filtered
  a <- df %>% filter(desc_century == query) 
  b <- table(a$desc_year) %>% as.data.frame() %>% as_tibble() 
  b['century'] <- query
  return(b)
}

#--------------------------------------------------------------

# clean_author
  # this function extracts the largest word of each entry
  # all entries with the same largest words are grouped together
  # this cleans a bit issues with duplicated author information

clean_author <- function(x){
  if (is.na(x)){
    return("no_author")
    break
  }
  y <- unlist(strsplit(x," ")) %>% 
    map(str_replace_all,"[[:punct:]]"," ") %>% 
    unlist()
  w <- gsub(" ","",y)
  k <- sapply(w, nchar) %>% bind_rows()
  k <- k[,colSums(k != 0) > 0] 
  a <- unname(unlist(slice(k,1)))
  for (i in 1:length(a)){ 
    if (a[[i]] > 3){
      q <- names(k)[i]
      break
    }
  }
  return(q)
}

#--------------------------------------------------------------

# df_stack_parse

 # this functions arranges the data to be able to be used in a stacked bar chart
 # it works between two columns of a given data frame
 # if any of these columns has values which must be parsed, the may be using the parse_char
 # need to specify if each column needs to be parsed (parse = TRUE) or not (parse = FALSE)
 # the output is a list containing two data frames
  # one with all values being compared
  # one with only the top x values in each category, defined by the slice_value argument


df_stack_parse <- function(df,column1,column2,slice_value=5,parse_char=";", parse1=TRUE,parse2=TRUE){
  if (parse1==TRUE){ 
    k <-  na.omit(unique(unlist(strsplit(df[column1][[1]],split=parse_char))))
  } else {k <- unique(na.omit(df[column1][[1]]))}
  if (parse2==TRUE){ 
    l <-  na.omit(unique(unlist(strsplit(df[column2][[1]],split=parse_char))))
  } else {l <- unique(na.omit(df[column2][[1]]))}
  y = list()
  i <- 1
  for (a in k){
    temp_df <- filter(df, str_detect(df[column1][[1]],a)) 
    for (q in l){
      temp <- str_detect(temp_df[column2][[1]],q)
      mesa <- table(temp)["TRUE"]
      tabela <- tibble(x=a, y=q,value=mesa)
      y[[i]] <- tabela
      i <- i + 1
    }
  }
  tabela_stacked <- do.call("rbind",y)
  o = list()
  i <- 1
  for (f in k){
    temp2 <-   filter(tabela_stacked, tabela_stacked["x"] == f) %>%  arrange(desc(value)) %>% slice(1:slice_value)
    o[[i]] <- temp2
    i <- i + 1
  }
  tabela_stacked_slice <- do.call("rbind",o)
  return(list(tabela_stacked,tabela_stacked_slice))
}















