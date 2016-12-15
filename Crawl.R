# R is dope, I lost all hope, damn the pope

# R based Crawler for healthgrades.com

# Package Shenanigans

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

# Library Shenanigans

if( FALSE )
{
  library(XML)
  library(xlsx)
  library(dplyr)
  library(stringr)
  library(RCurl)
  library(gdata)
  library(sqldf)
  library(rJava)
  library(rvest)
}

# rm(list=ls())

# Hierarchy
# sd: specialty directory
# st: state
# ct: city
# doc: doctor

# Compute Specialty-Directory List
compute_sd <- function{

  # Home Page URL
  url <- c("https://www.healthgrades.com")
  
  # Specialty-Directory URL
  url_sd <- c("https://www.healthgrades.com/specialty-directory")
  html_sd <- getURL(url_sd)  
  parse_sd <- htmlParse(html_sd, asText = TRUE)
  parse_sd
  
  # Specialty-Directory Page: Links of Specialties
  content_sd <- xpathSApply(parse_sd, "//a[@href]", xmlGetAttr, "href")
  content_sd <- content_sd[21:301]
  content_sd
  len_sd <- length(content_sd)
  
  # Call compute_sd_link() 
  compute_sd_link()
}

# Compute Specialty-Directory Links
compute_sd_link <- function{
  
  # Iterate through all specialties
  for( i in 1:len_sd )
  {
    i <- 1
    # Specialty-Directory URL
    url_sd_link <- paste(url,content_sd[i],sep="")
    url_sd_link
    html_sd_link <- getURL(url_sd_link)
    html_sd_link
    parse_sd_link <- htmlParse(html_sd_link, asText = TRUE)
    parse_sd_link
    
    # Specialty-Directory Page: List of States for each Specialty-Directory
    content_sd_link <- xpathSApply(parse_sd_link, "//h4/a", xmlGetAttr, "href")
    content_sd_link
    len_sd_link <- length(content_sd_link)
    
    # call compute_st()
    compute_st()
  }
}

# Compute Specialty Directory/State Links
compute_st <- function{
  
  # Iterate through all specialty/states
  for( j in 1:len_st )
  {
    j <- 1
    # Specialty-Directory/State/City URL
    url_st <- paste(url,content_sd_link[j],sep="")
    url_st
    html_st <- getURL(url_st)
    html_st
    parse_st <- htmlParse(html_st, asText = TRUE)
    parse_st
    
    # [@data-hgtopcity='city.IsTopCity']
    # Specialty-Directory/State/City Page: List of Cities for each Specialty-Directory/State
    content_st <- xpathSApply(parse_st, "//a", xmlGetAttr, "href")
    content_st
    len_st <- length(content_st)
    len_st
    
    # call compute_st_name
    compute_st_name
    
    # call compute_ct()
    compute_ct()
  }
}

# Compute State Name from url_st
compute_st_name <- function{
  
  st_name <- substring(url_st,regexpr("y/",url_st,perl = TRUE)+2,nchar(url_st))
  st_name
}

# Compute Specialty-Directory/State/City Links
compute_ct <- function{
  
  for( k in len_ct )
  {
    k <- 50
    # Filter through content_st for Specialty-Directory/State/City Links
    url_st
    content_st[k]
    pos <- regexpr(st_name,content_st[k],perl = TRUE)
    if( pos > 0 )
    {
      url_ct <- paste(url_st,"/",substring(content_st[k],pos+nchar(st_name)+1,nchar(content_st[k])),sep="")
      url_ct
      html_ct <- getURL(url_ct)
      html_ct
      parse_ct <- htmlParse(html_ct, asText = TRUE)
      parse_ct
      
      # [@data-hgtopcity='city.IsTopCity']
      # Specialty-Directory/State/City Page: List of Doctors for each Specialty-Directory/State
      content_ct <- xpathSApply(parse_ct, "//h3/a", xmlGetAttr, "href")
      content_ct
      len_ct <- length(content_ct)
      len_ct
      
      # call compute_ct_name
      compute_ct_name()
      
      # call compute_doc()
      compute_doc()
    }
  }
}

# Compute City Name from content_st
compute_ct_name <- function{
  
  ct_name <- substring(content_st[k],pos+nchar(st_name)+1,nchar(content_st[k]))
  ct_name
}

# Compute Specialty-Directory/State/City/Doctor Links
compute_doc <- function{
  
  for( l in 1:len_ct )
  {
    l <- 1
    # Specialty-Directory/State/City/Doctor URL
    url_doc <- paste(url,content_ct[l],sep="")
    url_doc 
    # <- c("https://www.healthgrades.com/provider/li-chun-huang-xs8qf")
    html_doc <- getURL(url_doc)
    parse_doc <- htmlParse(html_doc, asText = TRUE)
    
    # Specialty-Directory/State/City/Doctor Page: Doctor Data for each Specialty-Directory/State/City/Doctor
    compute_doc_data()
  }
}

compute_ratings <- function(){
  
  # Parsing JavaScript
  js <<- xpathSApply(parse_doc, "//script[@type='text/javascript']", xmlValue)
  js <<- js[8]
  
  while( TRUE )
  {
    pos2 <<- regexpr(ls[1], js, perl = TRUE)
    pos2
    if( pos2 < 0 )
      break
    else
    {
      js <<- substring(js, pos2+nchar(ls[1])+2, nchar(js))
      # js
      # ls[2]
      pos3 <<- regexpr(ls[2], js, perl = TRUE)
      # pos3
      temp1 <<- substring(js, pos3+nchar(ls[2])+2, pos3+nchar(ls[2])+4)
      # temp1
    }
    break
  }
}

compute_doc_data <- function{
  
  # General Details
  add1 <- xpathSApply(parse_doc, "//div[@class='hg3-profile-content'][@id='hospital-profiles']/div[@class='hidden-container']/address[@class='hg3-address']/a", xmlValue)
  add2 <- xpathSApply(parse_doc, "//div[@class='hg3-profile-content'][@id='hospital-profiles']/div[@class='hidden-container']/address[@class='hg3-address']/div", xmlValue)
  address <- c(add1, add2)
  address
  
  specialties <- xpathSApply(parse_doc, "//div[@class='tab-container inactive'][@id='tab-specialties']/div[@class='hg3-striped-list']/ul/li", xmlValue)
  specialties
  
  conditions_treated <- xpathSApply(parse_doc, "//div[@class='tab-container inactive'][@id='tab-conditions']/div[@class='hg3-striped-list']/ul/li", xmlValue)
  conditions_treated
  
  procedures_performed <- xpathSApply(parse_doc, "//div[@class='tab-container inactive'][@id='tab-procedures']/div[@class='hg3-striped-list']/ul/li", xmlValue)
  procedures_performed
  
  malpractice <- xpathSApply(parse_doc, "//h5[@class='summary-title disabled-text']", xmlValue)
  malpractice
  
  doc_data <- xpathSApply(parse_doc, "//div[@class='hg3-overlay-summary nohover']/div[@class='summary-content']/p", xmlValue)
  
  sanctions <- doc_data[2]
  sanctions
  
  board_actions <- doc_data[3]
  board_actions
  
  education <- xpathSApply(parse_doc, "//div[@class='tab-container inactive'][@id='tab-education']/div[@class='hg3-striped-list']/ul/li/div/h5", xmlValue)
  education
  
  languages_spoken <- xpathSApply(parse_doc, "//div[@class='tab-container inactive'][@id='tab-languages']/div[@class='hg3-striped-list']/ul/li", xmlValue)
  languages_spoken
  
  memberships <- xpathSApply(parse_doc, "//div[@class='hg3-striped-list']/p[@class='no-results']", xmlValue)
  memberships
  
  # Payor Data
  payor <- list()
  ctr2 <- 1
  
  # Parsing JavaScript
  js <- xpathSApply(parse_doc, "//script[@type='text/javascript']", xmlValue)
  js <- js[12]
  
  while( TRUE )
  {
    pos2 <- regexpr("\"payor\":", js, perl = TRUE)
    pos2
    if( pos2 < 0 )
      break
    else
    {
      js <- substring(js,pos2+9,nchar(js))
      pos3 <- regexpr("\"}", js, perl = TRUE)
      temp <- substring(js, 1, pos3-1)
      payor[ctr2] <- temp
      ctr2 <- ctr2 + 1
    }
  }
  payor
  
  # Rating Data
  rating <- list()
  ctr2 <- 1
  
  # Key Survey Insights
  i1 <- c("Trustworthiness","actualScore")
  i2 <- c("Helpfulness","actualScore")
  i3 <- c("Staff","actualScore")
  i4 <- c("Scheduling","actualScore")
  
  # Experience with Doctor
  q1 <- c("Level of trust in provider's decisions","actualScore")
  q2 <- c("How well provider explains medical condition(s)","actualScore")
  q3 <- c("How well provider listens and answers questions","actualScore")
  q4 <- c("Spends appropriate amount of time with patients","actualScore")
  
  # Office & Staff Rating
  q5 <- c("Ease of scheduling urgent appointments","actualScore")
  q6 <- c("Office environment, cleanliness, comfort, etc.","actualScore")
  q7 <- c("Staff friendliness and courteousness","actualScore")
  q8 <- c("Total wait time (waiting & exam rooms)","actualScore")
  
  # Compute Ratings
  ls <- i1
  compute_ratings()
  rating[ctr2] <- temp1
  ctr2 <- ctr2 + 1
  ls <- i2
  compute_ratings()
  rating[ctr2] <- temp1
  ctr2 <- ctr2 + 1
  ls <- i3
  compute_ratings()
  rating[ctr2] <- temp1
  ctr2 <- ctr2 + 1
  ls <- i4
  compute_ratings()
  rating[ctr2] <- temp1
  ctr2 <- ctr2 + 1
  
  ls <- q1
  compute_ratings()
  rating[ctr2] <- temp1
  ctr2 <- ctr2 + 1
  ls <- q2
  compute_ratings()
  rating[ctr2] <- temp1
  ctr2 <- ctr2 + 1
  ls <- q3
  compute_ratings()
  rating[ctr2] <- temp1
  ctr2 <- ctr2 + 1
  ls <- q4
  compute_ratings()
  rating[ctr2] <- temp1
  ctr2 <- ctr2 + 1
  
  ls <- q5
  compute_ratings()
  rating[ctr2] <- temp1
  ctr2 <- ctr2 + 1
  ls <- q6
  compute_ratings()
  rating[ctr2] <- temp1
  ctr2 <- ctr2 + 1
  ls <- q7
  compute_ratings()
  rating[ctr2] <- temp1
  ctr2 <- ctr2 + 1
  ls <- q8
  ls
  compute_ratings()
  rating[ctr2] <- temp1
  ctr2 <- ctr2 + 1
  
  rating
}

# Call Main Function
compute_sd()
