# Identifying-US-States-From-Unstructured-Text

As part of my daily exercise to write a function - partly to document what I've done but also to increase my future productivity - I wrote a function to identify States (Cities) mentioned in unstrucutred text using regular expression.

You would have to dl the csvs used in this repo to use the functions. Alternatively, you can scrape the tables which contains state info by uncommenting out sections in the code.

Here's an example of how this function could be used. I didn't really seek to optimise the code as I was rushing through this. But feel free to use apply functions or Rcpp to speed up the loops

In this function, I also identified the top 300 cities and collapse it at the state level. 

test = read.csv("C:/Users/HUANGJ/Desktop/items/Rewards research/Twitter analysis/170814_verizon.csv",stringsAsFactors = FALSE)

name_dat = "test"      #name of data frame

col_name = "location"   #Column of data which you want to use to identify the unstructured text

wd = "C:/Users/HUANGJ/Desktop/US_state_indicator_function"    #wd where function is stored.

test = identify_states(name_dat,col_name,wd)
