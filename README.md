# Identifying-US-States-From-Unstructured-Text

An example of how this function could be used. I didn't really seek to optimise the code as I was rushing through this. But feel free to use apply functions or Rcpp to speed up the loops

In this function, I also identified the top 300 cities and collapse it at the state level. 

test = read.csv("C:/Users/HUANGJ/Desktop/items/Rewards research/Twitter analysis/170814_verizon.csv",stringsAsFactors = FALSE)

name_dat = "test"

col_name = "location"

wd = "C:/Users/HUANGJ/Desktop/US_state_indicator_function"   

test = identify_states(name_dat,col_name,wd)
