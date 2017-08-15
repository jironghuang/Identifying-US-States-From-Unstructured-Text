#Writing a function to identify states mentioned in the tweets
library(rvest)

#First scrape states data from wikipedia
#Scrape html table from wikipedia
#Extract all the tables

# url <- "https://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations"
# 
# webpage = read_html(url)
# tbls <- html_nodes(webpage, "table")
# tbls_ls <- webpage %>%
#   html_nodes("table") %>%
#   .[1:5] %>%
#   html_table(fill = TRUE)
# 
# states_wiki = tbls_ls[[1]]
# write.csv(states_wiki,"C:/Users/HUANGJ/Desktop/US_state_indicator_function/us_states.csv",row.names = FALSE)

# url <- "https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population"
# 
# webpage = read_html(url)
# tbls <- html_nodes(webpage, "table")
# tbls_ls <- webpage %>%
#   html_nodes("table") %>%
#   .[1:5] %>%
#   html_table(fill = TRUE)
# 
# city_state = tbls_ls[[4]]


#Function that create columns and assign an indicator 1 if a US state is mentioned in the body of unstructured text
#Data frame must have at 
identify_states = function(name_dat,col_name,wd){
  
#Assign name_dat to pay_dat  
pay_dat = get(name_dat)

#Change column name to cleaned_col
pay_dat$find_textin_col = ""
pay_dat$find_textin_col = pay_dat[,which(names(pay_dat) == col_name)] 
pay_dat$find_textin_col = tolower(pay_dat$find_textin_col)

#Change the location  


#Scrape html table from wikipedia
#Extract all the tables

# url <- "https://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations"
# 
# webpage = read_html(url)
# tbls <- html_nodes(webpage, "table")
# tbls_ls <- webpage %>%
#   html_nodes("table") %>%
#   .[1:5] %>%
#   html_table(fill = TRUE)
# 
# states_wiki = tbls_ls[[1]]
# write.csv(states_wiki,"C:/Users/HUANGJ/Desktop/US_state_indicator_function/us_states.csv",row.names = FALSE)
s = paste(wd,"/us_states.csv",sep = "")
states_wiki = read.csv(s,stringsAsFactors = FALSE)

states_wiki = states_wiki[-1:-11,]
states_wiki = states_wiki[,1:10]
names(states_wiki) = states_wiki[1,] 
states_wiki = states_wiki[-1,]

states_wiki = states_wiki[,-2]
names(states_wiki) = c("States","Ctry_State","abb1","num","abb2","abb3","abb4","abb5","abb6")

states_wiki = subset(states_wiki,states_wiki$abb2 != "")

for(i in 1:ncol(states_wiki)){
  states_wiki[,i] = tolower(states_wiki[,i])
}

states_wiki = subset(states_wiki,states_wiki$abb5!="")

states_wiki$abb7 = ""
states_wiki$abb8 = ""
states_wiki$abb9 = ""

#use while loop
for(i in 1:nrow(states_wiki)){
  pos = which(names(states_wiki) == "abb7")
  
  if(length(unlist(strsplit(states_wiki$abb6[i], ",", fixed = TRUE))) == 0){
    states_wiki[i,pos] = states_wiki[i,pos-1]
  }else if((states_wiki$abb6[i] != "")){
    abl = unlist(strsplit(states_wiki$abb6[i], ",", fixed = TRUE))
    l = length(abl)
    while(l>0){
      states_wiki[i,pos] = abl[l] 
      pos = pos + 1
      l = l-1
    }
  }
}

#Remove all []
for(i in 1:ncol(states_wiki)){
  states_wiki[,i] = gsub("\\[.*\\]","",states_wiki[,i])
}


states_wiki = states_wiki[,-which(names(states_wiki) == c("Ctry_State"))]
states_wiki = states_wiki[,-which(names(states_wiki) == c("num"))]
states_wiki = states_wiki[,-which(names(states_wiki) == c("abb6"))]
states_wiki = states_wiki[,-which(names(states_wiki) == c("abb1"))]
states_wiki = states_wiki[,-which(names(states_wiki) == c("abb3"))]

#Store list of abbbrievation 
abb = states_wiki$abb2

#Take out the pure abbrievation
#exclude what state ca, ny, il, ma, nj
#exclude miss, pwc
#exclude word on own-->co, hi,in,me, oh, ok, or

for(i in 1:length(abb)){
  if((abb[i] == "co")|(abb[i] == "hi")|(abb[i] == "in")|(abb[i] == "me")|(abb[i] == "oh")|(abb[i] == "ok")|(abb[i] == "or")){
    s = abb[i]
    s = paste("(?:[[:punct:]]+|( )+)",s,"(?:[[:punct:]]+|( )+)",sep = "")
    pay_dat$find_textin_col = gsub(s," ",pay_dat$find_textin_col)    
  }
}

# grepl("(?:[[:punct:]]+|( )+)al(?:c|())(?:[[:punct:]]+|( )+)"," al ")

#Build regular expressions
# grepl("district( )*of( )*columbia","district of columbia")
for(i in 1:ncol(states_wiki)){
  # states_wiki$regexp = gsub(" +","( )*",states_wiki$States) 
  # states_wiki$regexp = gsub("[.]","[.]*",states_wiki$regexp) 
  states_wiki[,i]= gsub(" +","( )*",states_wiki[,i]) 
  states_wiki[,i]= gsub("[.]","( )*[.]*( )*",states_wiki[,i]) 
}

#Include regular expression with c as optional-->grepl("wash( )*( )*[.]*( )*dc?c","wash .dcc",perl = TRUE)
for(i in 2:4){
  states_wiki[,i]= ifelse(states_wiki[,i] != "",paste(states_wiki[,i],"(?:c|())",sep = ""),"")
}

tok1 = "(?:[[:punct:]]*|( )*)"
states_wiki[,1] = paste("(",tok1,states_wiki[,1],tok1,")",sep = "")
tok = "(?:[[:punct:]]+|( )+)"
for(i in 2:ncol(states_wiki)){
  states_wiki[,i] = paste("(",tok,states_wiki[,i],tok,")",sep = "")
}


states_wiki$re_ep = ""
empty_tok = "((?:[[:punct:]]+|( )+)(?:[[:punct:]]+|( )+))"

#Regular expression with a space before and behind ( )+(| | |)( )+ -->(?:[:punct:]|( ))+(| | |)(?:[:punct:]|( ))+
for(i in 1:nrow(states_wiki)){
  a = as.data.frame(t(as.data.frame(states_wiki[i,])))
  a = subset(a,a[,1] != empty_tok )
  a = as.data.frame(a[!duplicated(a[names(a)[1]]),])
  
  re = ""
  
  for(j in 1:(nrow(a)-1)){
    if(j != (nrow(a) - 1)){
      re = paste(re,a[j,1],"|",sep = "")        
    }else{
      re = paste(re,a[j,1],sep = "") 
    }
  } 
  
  # end = ")(?:[[:punct:]]|( ))+"
  # re = paste(re,end,sep = "")
  
  states_wiki$re_ep[i] = re
}


for(i in 1:length(abb)){
  # print(i)
  pay_dat$new = ifelse(grepl(states_wiki$re_ep[i],pay_dat$find_textin_col),1,0)
  names(pay_dat)[ncol(pay_dat)] = abb[i]
}

#Using 2000 cities may be far too long
####################Scrape form wikipedia####################
# url <- "https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population"
# 
# webpage = read_html(url)
# tbls <- html_nodes(webpage, "table")
# tbls_ls <- webpage %>%
#   html_nodes("table") %>%
#   .[1:5] %>%
#   html_table(fill = TRUE)
# 
# city_state = tbls_ls[[4]]
# write.csv(city_state,"C:/Users/HUANGJ/Desktop/city_state.csv",row.names = FALSE)

s = paste(wd,"/city_state.csv",sep = "")
city_state = read.csv(s,stringsAsFactors = FALSE)

#Remove all []
for(i in 1:ncol(city_state)){
  city_state[,i] = gsub("\\[.*\\]","",city_state[,i])
}

city_state = city_state[,2:3]; names(city_state) = c("city","state")

############################################################
s = paste(wd,"/us_states_abb.csv",sep = "")
states = read.csv(s,stringsAsFactors = FALSE)

city_state = merge(city_state,states,by.x = "state",by.y="US.State",all.x = TRUE)

for(i in 1:ncol(city_state)){
  city_state[,i] = tolower(city_state[,i]) 
}

city_state = subset(city_state,city_state$city!= "mobile" & city_state$city!= "surprise" & city_state$city!= "clinton" & city_state$city!= "independence" & city_state$city!= "aurora")
city_state = subset(city_state,!is.na(city_state$Abbreviation)) #exclude dc



city = city_state$city
city_state$orig_city = city_state$city 
city_state$orig_city = gsub(" ","_",city_state$orig_city) #take note of.

city_state$city= gsub(" +","( )*",city_state$city) 
city_state$city= gsub("[.]","( )*[.]*( )*",city_state$city) 
city_state$city = paste("(?:[[:punct:]]*|( )*)",city_state$city,"(?:[[:punct:]]*|( )*)",sep = "")

# pay_dat$holding = ""
for(i in 1:nrow(city_state)){
  # print(i); print(city_state$orig_city[i])
  pay_dat$holding = ifelse(grepl(city_state$city[i],pay_dat$find_textin_col),1,0)
  names(pay_dat)[ncol(pay_dat)] = city_state$orig_city[i]
}

#Collapse city to state level
abb_list = unique(city_state$Abbreviation)

for(i in 1:length(abb_list)){
  # print(i);print(abb_list[i])
  s_list = subset(city_state,city_state$Abbreviation == abb_list[i])
  
  l = s_list$orig_city
  # a = c("")
  # for(j in 1:length(l)){
  #   a = c(a,l[j])
  # }
  # a = a[-1]
  c_s = subset(pay_dat,select = l) 
  pay_dat[,which(names(pay_dat) == abb_list[i])] =  apply(c_s,1,sum) + pay_dat[,which(names(pay_dat) == abb_list[i])]
  pay_dat[,which(names(pay_dat) == abb_list[i])] = ifelse(pay_dat[,which(names(pay_dat) == abb_list[i])] > 0, 1,0)
}


return(pay_dat)  
}

test = read.csv("C:/Users/HUANGJ/Desktop/items/Rewards research/Twitter analysis/170814_verizon.csv",stringsAsFactors = FALSE)
name_dat = "test"
col_name = "location"
wd = "C:/Users/HUANGJ/Desktop/US_state_indicator_function"   
test = identify_states(name_dat,col_name,wd)





