# I don't know what I'm doing :)

if( FALSE )
{
  install.packages("XML")
  install.packages("xlsx")
  install.packages("dplyr")
  install.packages("stringr")
  install.packages("RCurl")
  install.packages("gdata")
  install.packages("sqldf")
  install.packages("rJava")
  install.packages("rvest")
}
if( FALSE )
{
  library(XML)
  # library(xlsx)
  library(dplyr)
  library(stringr)
  library(RCurl)
  library(gdata)
  library(sqldf)
  # library(rJava)
  library(rvest)
}

# rm(list=ls())

# Website to be captured
abs_url <- c("https://www.healthgrades.com")
url_sc <- c("https://www.healthgrades.com/specialty-directory")
# html_sp <- getURL(url_sp, followlocation = TRUE)
html_sc <- getURL(url_sc)

# Set Working Directory
# setwd("D://NHL")
# getwd()

#parse source code, keep html tags intact
parse_sc <- htmlParse(html_sc, asText = TRUE)
parse_sc

# 
content_sp <- xpathSApply(parse_sc, "//a[@href]", xmlGetAttr, "href")
content_sp <- content_sp[21:302]
content_sp
len_sp <- length(content_sp)

url_sp <- list()
url_state <- list()
url_city <- list()
url_doc <- list()
city_name <- list()
ctr <- 1

state_codes <- c("al,ak,as,az,ar,ca,co,ct,de,dc,fl,ga,gu,hi,id,il,in,ia,ks,ky,la,me,md,mh,ma,mi,fm,mn,ms,mo,mt,ne,nv,nh,nj,nm,ny,nc,nd,mp,oh,ok,or,pw,pa,pr,ri,sc,sd,tn,tx,ut,vt,va,vi,wa,wv,wi,wy")

for( i in 1:len_sp )
{
  i <- 1
  temp <- paste(abs_url,content_sp[i],sep="")
  url_sp[i] <- temp
  url_sp[i]
  
  html_sp <- getURL(url_sp[i])
  parse_sp <- htmlParse(html_sp, asText = TRUE)
  parse_sp
  
  content_state <- xpathSApply(parse_sp, "//h4/a", xmlGetAttr, "href")
  content_state
  len_state <- length(content_state)
  
  for( j in 1:len_state )
  {
    # j <- 1
    content_state[j]
    # z <- c("al")
    
    for( k1 in 1:59 )
    {
      # k <- 1
      z <- state_codes[1]
      t1 <- regexpr(z, content_state[j], perl = TRUE)
      if( t1 > 0 )
      {
        len <- nchar(content_state[j])
        len
        name <- substring(content_state[j],t1,len)
        city_name[ctr] <- name
        ctr <- ctr + 1
        ctr
      }
    }
    
    temp <- paste(abs_url,content_state[j],sep="")
    
    url_state[i] <- temp
    url_state[i]
    
    html_state <- getURL(url_state[i])
    parse_state <- htmlParse(html_state, asText = TRUE)
    parse_state
    
    content_city <- xpathSApply(parse_state, "//span/a[@href]", xmlGetAttr, "href")
    content_city
    len_city <- length(content_city)
  }
}

