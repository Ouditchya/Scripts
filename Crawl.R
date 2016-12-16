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

compile_data <- function(){
  
  p1 <- unlist(payor)
  for( idx in 1:length(p1) )
  {
    if( idx == 1 )
      p2 <- p1[idx]
    else
      p2 <- paste(p2, p1[idx], sep=", ")
  }
  
  # central_index
  central_index <- data.frame(
    Specialty = sp_name, 
    StateName = substring(st_name,4,nchar(st_name)), 
    CityName = ct_name, 
    DoctorName = doc_name,
    Payors = p2
  )
  # central_index
  
  for( idx in 1:length(specialties) )
  {
    if( idx == 1 )
      p1 <- specialties[idx]
    else
      p1 <- paste(p1, specialties[idx], sep=", " )
  }
  
  for( idx in 1:length(conditions_treated) )
  {
    if( idx == 1 )
      p2 <- conditions_treated[idx]
    else
      p2 <- paste(p2, conditions_treated[idx], sep=", " )
  }
  
  for( idx in 1:length(procedures_performed) )
  {
    if( idx == 1 )
      p3 <- procedures_performed[idx]
    else
      p3 <- paste(p3, procedures_performed[idx], sep=", " )
  }
  
  # medical_data
  medical_data <- data.frame(
    DoctorName = doc_name,
    CityName = ct_name,
    Specialties = p1,
    ConditionsTreated = p2,
    ProceduresPerformed = p3
  )
  medical_data
  
  for( idx in 1:length(education) )
  {
    if( idx ==  1)
      p1 <- education[idx]
    else
      p1 <- paste(p1, education[idx], sep=", " )
  }
  
  for( idx in 1:length(languages_spoken) )
  {
    if( idx ==  1)
      p2 <- languages_spoken[idx]
    else
      p2 <- paste(p2, languages_spoken[idx], sep=", " )
  }
  
  # qualification_data
  qualification_data <- data.frame(
    DoctorName = doc_name,
    CityName = ct_name,
    Schools = p1,
    LanguagesSpoken = p2
  )
  # qualification_data
  
  # legal_data
  legal_data <- data.frame(
    DoctorName = doc_name,
    CityName = ct_name,
    Malpractice = malpractice,
    Sanctions = sanctions,
    BoardActions = board_actions
  )
  # legal_data
  
  ratings <- unlist(rating)
  # performance_data
  performance_data <- data.frame(
    DoctorName = doc_name,
    CityName = ct_name,
    Trustworthiness = ratings[1],
    Helpfulness = ratings[2],
    Staff = ratings[3],
    Scheduling = ratings[4]
  )
  # performance_data
  
  if( length(review) > 0 )
  {
    # reviews_data
    reviews_data <- data.frame(
      DoctorName = doc_name,
      CityName = ct_name,
      Reviews = review
    )
    # reviews_data
  }
}

compute_ratings <- function(){
  
  # Parsing JavaScript
  js <- xpathSApply(parse_doc, "//script[@type='text/javascript']", xmlValue)
  js <- js[8]
  
  while( TRUE )
  {
    pos2 <- regexpr(ls[1], js, perl = TRUE)
    # pos2
    if( pos2 < 0 )
      break
    else
    {
      js <- substring(js, pos2+nchar(ls[1])+2, nchar(js))
      # js
      # ls[2]
      pos3 <- regexpr(ls[2], js, perl = TRUE)
      # pos3
      temp1 <<- substring(js, pos3+nchar(ls[2])+2, pos3+nchar(ls[2])+4)
      # temp1
    }
    break
  }
}

compute_doc_data <- function(){
  
  # General Details
  add1 <- xpathSApply(parse_doc, "//div[@class='hg3-profile-content'][@id='hospital-profiles']/div[@class='hidden-container']/address[@class='hg3-address']/a", xmlValue)
  add2 <- xpathSApply(parse_doc, "//div[@class='hg3-profile-content'][@id='hospital-profiles']/div[@class='hidden-container']/address[@class='hg3-address']/div", xmlValue)
  address <<- c(add1, add2)
  address
  
  specialties <<- xpathSApply(parse_doc, "//div[@class='tab-container inactive'][@id='tab-specialties']/div[@class='hg3-striped-list']/ul/li", xmlValue)
  specialties
  
  conditions_treated <<- xpathSApply(parse_doc, "//div[@class='tab-container inactive'][@id='tab-conditions']/div[@class='hg3-striped-list']/ul/li", xmlValue)
  # conditions_treated
  
  procedures_performed <<- xpathSApply(parse_doc, "//div[@class='tab-container inactive'][@id='tab-procedures']/div[@class='hg3-striped-list']/ul/li", xmlValue)
  # procedures_performed
  
  malpractice <<- xpathSApply(parse_doc, "//h5[@class='summary-title disabled-text']", xmlValue)
  # malpractice
  
  doc_data <- xpathSApply(parse_doc, "//div[@class='hg3-overlay-summary nohover']/div[@class='summary-content']/p", xmlValue)
  
  sanctions <<- doc_data[2]
  # sanctions
  
  board_actions <<- doc_data[3]
  # board_actions
  
  education <<- xpathSApply(parse_doc, "//div[@class='tab-container inactive'][@id='tab-education']/div[@class='hg3-striped-list']/ul/li/div/h5", xmlValue)
  # education

  languages_spoken <<- xpathSApply(parse_doc, "//div[@class='tab-container inactive'][@id='tab-languages']/div[@class='hg3-striped-list']/ul/li", xmlValue)
  # languages_spoken
  
  memberships <<- xpathSApply(parse_doc, "//div[@class='hg3-striped-list']/p[@class='no-results']", xmlValue)
  # memberships
  
  review <<- xpathSApply(parse_doc, "//div[@itemprop='itemReviewed']/div[@itemprop='description'][@class='comment-text']/span", xmlValue)
  # review
  
  # Payor Data
  payor <<- list()
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
  # payor

  # Rating Data
  rating <<- list()
  ctr2 <- 1
  
  # Key Survey Insights
  i1 <- c("Trustworthiness","actualScore")
  i2 <- c("Helpfulness","actualScore")
  i3 <- c("Staff","actualScore")
  i4 <- c("Scheduling","actualScore")
  
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
  # rating
  
  # call compile_data
  compile_data()
}

# Compute Specialty-Directory/State/City/Doctor Links
compute_doc <- function(){
  
  for( l in 1:len_ct )
  {
    # l <- 1
    # Specialty-Directory/State/City/Doctor URL
    url_doc <- paste(url,content_ct[l],sep="")
    # url_doc 
    # <- c("https://www.healthgrades.com/provider/li-chun-huang-xs8qf")
    html_doc <- getURL(url_doc)
    parse_doc <- htmlParse(html_doc, asText = TRUE)
    
    doc_name <<- xpathSApply(parse_doc, "//div[@class='hg3-intercept-summary-header']/div[@class='provider-name-container']/h1[@class='provider-name'][@itemprop='name']", xmlValue)
    # doc_name
    
    # Specialty-Directory/State/City/Doctor Page: Doctor Data for each Specialty-Directory/State/City/Doctor
    compute_doc_data()
  }
}

# Compute City Name from content_st
compute_ct_name <- function(){
  
  ct_name <<- substring(content_st[k],pos+nchar(st_name)+1,nchar(content_st[k]))
  # ct_name
}

# Compute Specialty-Directory/State/City Links
compute_ct <- function(){
  
  for( k in len_st )
  {
    # k <- 50
    # Filter through content_st for Specialty-Directory/State/City Links
    # url_st
    # content_st[k]
    # content_sd[i]
    
    pos <- regexpr("-", content_sd[i], perl = TRUE)
    sp_name <<- substring(content_sd[i], 2, pos-1)
    # sp_name
    
    pos <- regexpr(st_name,content_st[k],perl = TRUE)
    if( pos > 0 )
    {
      url_ct <- paste(url_st,"/",substring(content_st[k],pos+nchar(st_name)+1,nchar(content_st[k])),sep="")
      # url_ct
      html_ct <- getURL(url_ct)
      # html_ct
      parse_ct <- htmlParse(html_ct, asText = TRUE)
      # parse_ct
      
      # Specialty-Directory/State/City Page: List of Doctors for each Specialty-Directory/State
      # /provider/alex-casey-3t7rl
      content_ct <<- xpathSApply(parse_ct, "//h3/a", xmlGetAttr, "href")
      # content_ct
      len_ct <<- length(content_ct)
      # len_ct
      
      # call compute_ct_name
      compute_ct_name()
      
      # call compute_doc()
      compute_doc()
    }
  }
}
j
# Compute State Name from url_st
compute_st_name <- function(){
  
  st_name <<- substring(url_st,regexpr("y/",url_st,perl = TRUE)+2,nchar(url_st))
  # st_name
}

# Compute Specialty Directory/State Links
compute_st <- function(){
  
  # Iterate through all specialty/states
  for( j in 1:len_sd_link )
  {
    # j <- 1
    # Specialty-Directory/State/City URL
    url_st <<- paste(url,content_sd_link[j],sep="")
    # url_st
    html_st <- getURL(url_st)
    # html_st
    parse_st <- htmlParse(html_st, asText = TRUE)
    # parse_st
    
    # Specialty-Directory/State/City Page: List of Cities for each Specialty-Directory/State
    content_st <<- xpathSApply(parse_st, "//a", xmlGetAttr, "href")
    # content_st
    len_st <<- length(content_st)
    # len_st
    
    # call compute_st_name
    compute_st_name()
    
    # call compute_ct()
    compute_ct()
  }
}

# Compute Specialty-Directory Links
compute_sd_link <- function(){
  
  # idx1 <- 1
  # Iterate through all specialties
  for( i in 1:len_sd )
  {
    # i <- 1
    # Specialty-Directory URL
    url_sd_link <- paste(url,content_sd[i],sep="")
    # url_sd_link
    html_sd_link <- getURL(url_sd_link)
    # html_sd_link
    parse_sd_link <- htmlParse(html_sd_link, asText = TRUE)
    # parse_sd_link
    
    # Specialty-Directory Page: List of States for each Specialty-Directory
    # /acupuncture-directory/al-alabama
    content_sd_link <<- xpathSApply(parse_sd_link, "//h4/a", xmlGetAttr, "href")
    # content_sd_link
    len_sd_link <<- length(content_sd_link)
    
    # call compute_st()
    compute_st()
  }
}

# Compute Specialty-Directory List
compute_sd <- function(){
  
  # Specialty-Directory URL
  url_sd <- c("https://www.healthgrades.com/specialty-directory")
  html_sd <- getURL(url_sd)  
  parse_sd <- htmlParse(html_sd, asText = TRUE)
  # parse_sd
  
  # Specialty-Directory Page: Links of Specialties
  # /acupuncture-directory
  content_sd <<- xpathSApply(parse_sd, "//a[@href]", xmlGetAttr, "href")
  content_sd <<- content_sd[21:301]
  # content_sd
  len_sd <<- length(content_sd)
  
  # Call compute_sd_link() 
  compute_sd_link()
}

export_data <- function(){
  
  write.xlsx(central_index, "c:/central_index.xlsx")
  write.xlsx(medical_data, "c:/medical_data.xlsx")
  write.xlsx(qualification_data, "c:/qualification_data.xlsx")
  write.xlsx(performance_data, "c:/performance_data.xlsx")
  write.xlsx(legal_data, "c:/legal_data.xlsx")
  write.xlsx(reviews_data, "c:/reviews_data.xlsx")
}

# Home Page URL
url <<- c("https://www.healthgrades.com")

# Data Frames for Export
central_index <<- data.frame()
medical_data <<- data.frame()
qualification_data <<- data.frame()
performance_data <<- data.frame()
legal_data <<- data.frame()
reviews_data <<- data.frame()

# Global Variables
i <<- 1
j <<- 1
k <<- 1

# Call Main Function
compute_sd()

# Call export_data
# export_data()
