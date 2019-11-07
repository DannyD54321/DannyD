train <- read.csv("D:/UCLA STUDY/2018 SPRING/STATS 101C/final project/train.csv")
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)


train$SubjectArmed[is.na(train$SubjectArmed)] <- "U"

#Date

train$Date <- ymd(train$Date)
train$Year <- NA
train$Year <- year(train$Date)
train$Year[is.na(train$Year)] <- "U"
train$Year <- as.factor(train$Year)

# SubjectAge

levels(train$SubjectAge)
levels(train$SubjectAge)[1] <- 10

levels(train$SubjectAge)[c(1:9)] <- "0-19"
levels(train$SubjectAge)[c(2:13)] <- "20-29"
levels(train$SubjectAge)[c(3:13)] <- "30-39"
levels(train$SubjectAge)[4] <- "0-19"
levels(train$SubjectAge)[c(4:14)]  <- "40-49"
levels(train$SubjectAge)[c(5:15)]  <- "50-59"

levels(train$SubjectAge)[c(6:16)]  <- "Above 60"
levels(train$SubjectAge)[7] <- "0-19"
levels(train$SubjectAge)[c(7:9)]  <- "U"    

train$SubjectAge[is.na(train$SubjectAge)] <- "U"



# Number of Shots

train$shots <- as.character(train$NumberOfShots)

train$shots <- sub(">/=|>|\\*| total", "", train$shots)


train$shots[grep("([0-9]+;)+([0-9]+)", train$shots, value = FALSE)] <- sapply(grep("([0-9]+;)+([0-9]+)", train$shots, value = TRUE), function(x){
  sum(scan(text = x, sep = ";", what = numeric(), quiet = TRUE))})

is.character(train$shots)


train$shots[which(train$shots == "Multiple")] <- median(as.numeric(train$shots), na.rm = TRUE)

train$shots <- as.factor(train$shots)
levels(train$shots)[66:69] <- NA

train$shots <- as.character(train$shots)
train$shots <- as.numeric(train$shots)





# Keyword Search
Fword.notes <- grep("Police Shooting Death|Shooting, fatal|fatally|demise|died|dead|Fatal|killed", train$Notes)
NFword.notes <- grep("No hit|no hits|No Hits|No Hit|NO HITS|NO HIT|Shooting, non-fatal", train$Notes)

Fword.narrative <- grep("Fatal%|fatally|dead|DOA|killed|Fatality| fatal|demise|died|killing|deceased", train$FullNarrative)
NFword.narrative <- grep("Non-Fatal|Non-fatal|apprehended|No hits|NO HITS|non-fatal", train$FullNarrative)


#Fword.notes <- grep("Police Shooting Death|Shooting, fatal|fatally|demise|died", train$Notes)
#NFword.notes <- grep("No hit|no hits|No Hits|No Hit|NO HITS|NO HIT|Shooting, non-fatal|accidental|Accidental", train$Notes)

#Fword.narrative <- grep("Fatal%|fatally|dead|DOA|killed|Fatality| fatal|demise|died|killing|knife|deseased", train$FullNarrative)
#NFword.narrative <- grep("Non-Fatal|Non-fatal|apprehended|No hits|NO HITS|non-fatal|accidental discharge|survive|Miss|miss", train$FullNarrative)

#|Victim|victim|knife|deseased|mental adding these did not score better

train$NoteFword <- NA
train$NoteNFword <- NA
train$NarFword <- NA
train$NarNFword <- NA

train$NoteFword[Fword.notes] <- "Y"
train$NoteNFword[NFword.notes] <- "Y"
train$NarFword[Fword.narrative] <- "Y"
train$NarNFword[NFword.narrative] <- "Y"

train$NarNFword[is.na(train$NarNFword)] <- "N"
train$NarFword[is.na(train$NarFword)] <- "N"
train$NoteNFword[is.na(train$NoteNFword)] <- "N"
train$NoteFword[is.na(train$NoteFword)] <- "N"

train$cert_f <- rep(NA,nrow(train))


for(i in 1:nrow(train)){
  
  if(train$NoteFword[i]=="Y"){
    if(train$NoteFword[i]==train$NoteNFword[i]) train$cert_f[i] <- FALSE
    else if(train$NoteFword[i]==train$NarNFword[i]) train$cert_f[i] <- FALSE
    else train$cert_f[i] <- TRUE
    
  }
  if(train$NarFword[i] == "Y"){
    
    if(train$NarFword[i]==train$NoteNFword[i]) train$cert_f[i] <- FALSE
    if(train$NarFword[i]==train$NarNFword[i]) train$cert_f[i] <- FALSE
    else train$cert_f[i] <- TRUE
  } 
  if(train$NarNFword[i] == "Y") train$cert_f[i] <- FALSE
  if(train$NoteNFword[i]== "Y") train$cert_f[i] <-  FALSE
}

for(i in 1:nrow(train)){
  if(train$NoteFword[i]=="N"){
    if(train$NoteNFword[i]=="N"){
      if(train$NarFword[i]=="N"){
        if(train$NarNFword[i]=="N") {
          train$cert_f[i] <- "Unknown"
        }
      }
    } 
  }}

train$cert_f <- as.factor(train$cert_f)

View(train)



# Output Algorithm


train$Fatalpredpred <- "Unknown"

for(i in 1:length(train$id)){
  if(train$cert_f[i]=="TRUE") {train$Fatalpred[i] <- "Yes"}
  if(train$cert_f[i]=="FALSE") {train$Fatalpred[i] <- "No"}
}

for (i in 1:length(train$id)) {
  if(train$Fatalpred[i]=="Unknown"){
    if(train$SubjectArmed[i]=="U") train$Fatalpred[i] <- "No"
  }
}

for (i in 1:length(train$id)) {
  if(train$Fatalpred[i]=="Unknown"){
    if(train$Year[i]=="1905") train$Fatalpred[i] <- "No"
  }
}

train$shots[which(is.na(train$shots))] <- 0
for (i in 1:length(train$id)) {
  if(train$Fatalpred[i]=="Unknown"){
    if(train$shots[i]>=7) train$Fatalpred[i] <- "Yes"
  }
}

for (i in 1:length(train$id)) {
  if(train$Fatalpred[i]=="Unknown"){
    if(train$SubjectAge[i]=="40-49") train$Fatalpred[i] <- "Yes"
    if(train$SubjectAge[i]=="50-59") train$Fatalpred[i] <- "Yes"
    if(train$SubjectAge[i]=="Above 60") train$Fatalpred[i] <- "Yes"
  }
}


Fword.nature <- grep("home invasion|Home invasion|Mental|mental|Radio|shots fired|Shots fired|Shooting|shooting", train$NatureOfStop)

train$NatureFword <- NA

train$NatureFword[Fword.nature] <- "Y"
train$NatureFword[is.na(train$NatureFword)] <- "N"

for (i in 1:length(train$id)) {
  if(train$Fatalpred[i]=="Unknown"){
    if(train$NatureFword[i] == "Y") train$Fatalpred[i] <- "Yes"
  }
}

train$Fatalpred[train$Fatalpred=="Unknown"] <- "No"


# adding cert_city Column

train$cert_city <- "Unknown"

for (i in 1:nrow(train)) {
  if(train$City[i]=="Detroit"){train$cert_city[i] <- FALSE}
  if(train$City[i]=="Tampa"){train$cert_city[i] <- FALSE}
  if(train$City[i]=="St. Louis"){train$cert_city[i] <- FALSE}
  if(train$City[i]=="Chicago"){train$cert_city[i] <- FALSE}
  if(train$City[i]=="Philadelphia"){train$cert_city[i] <- FALSE}
  if(train$City[i]=="Houston"){train$cert_city[i] <- FALSE}
  if(train$City[i]=="Boston"){train$cert_city[i] <- TRUE}
  if(train$City[i]=="Portland"){train$cert_city[i] <- TRUE}
  if(train$City[i]=="Tucson"){train$cert_city[i] <- TRUE}
  if(train$City[i]=="Austin"){train$cert_city[i] <- TRUE}
  if(train$City[i]=="Albuquerque"){train$cert_city[i] <- TRUE}
  if(train$City[i]=="Phoenix"){train$cert_city[i] <- TRUE}
}

train$cert_city <- as.factor(train$cert_city)



################################train validation###############################################
levels(train$Fatal)
levels(train$Fatal)[1] <- "Yes"
levels(train$Fatal)[2] <- "No"
levels(train$Fatal)[3] <- "No"

mean(train$Fatalpred != train$Fatal)



###############################################################################################

library(tree)
tree1 <- tree(Fatal~SubjectArmed+SubjectAge+Year+shots+cert_f+cert_city, data = train, mindev=0, minsize=2)

prune1 <- prune.misclass(tree1, best = 13)

plot(prune1)
text(prune1, pretty = 0, cex=0.7)

summary(prune1)







###################################################################################

test <- read.csv("D:/UCLA STUDY/2018 SPRING/STATS 101C/final project/test.csv")

test$SubjectArmed[is.na(test$SubjectArmed)] <- "U"


# Date
test$Date <- mdy(test$Date)

test$Year <- NA
test$Year <- year(test$Date)
test$Year[is.na(test$Year)] <- "U"
test$Year <- as.factor(test$Year)

# Subject Age

levels(test$SubjectAge)
levels(test$SubjectAge)[1] <- 10

levels(test$SubjectAge)[c(1:8)] <- "0-19"
levels(test$SubjectAge)[c(2:12)] <- "20-29"
levels(test$SubjectAge)[c(3:13)] <- "30-39"
levels(test$SubjectAge)[c(4:14)]  <- "40-49"
levels(test$SubjectAge)[c(5:15)]  <- "50-59"

levels(test$SubjectAge)[c(6:15)]  <- "Above 60"
levels(test$SubjectAge)[c(7:9)]  <- "U"    

test$SubjectAge[is.na(test$SubjectAge)] <- "U"



# Number of Shots

test$shots <- as.character(test$NumberOfShots)

test$shots <- sub(">/=|>|\\*| total", "", test$shots)


test$shots[grep("([0-9]+;)+([0-9]+)", test$shots, value = FALSE)] <- sapply(grep("([0-9]+;)+([0-9]+)", test$shots, value = TRUE), function(x){
  sum(scan(text = x, sep = ";", what = numeric(), quiet = TRUE))})

is.character(test$shots)


test$shots[which(test$shots == "Multiple")] <- median(as.numeric(test$shots), na.rm = TRUE)

test$shots <- as.factor(test$shots)
levels(test$shots)[41:43] <- NA

test$shots <- as.character(test$shots)
test$shots <- as.numeric(test$shots)





# Keyword Search
Fword.notes <- grep("Police Shooting Death|Shooting, fatal|fatally|demise|died|dead|Fatal|killed", test$Notes)
NFword.notes <- grep("No hit|no hits|No Hits|No Hit|NO HITS|NO HIT|Shooting, non-fatal", test$Notes)

Fword.narrative <- grep("Fatal%|fatally|dead|DOA|killed|Fatality| fatal|demise|died|killing|deceased", test$FullNarrative)
NFword.narrative <- grep("Non-Fatal|Non-fatal|apprehended|No hits|NO HITS|non-fatal", test$FullNarrative)


#Fword.notes <- grep("Police Shooting Death|Shooting, fatal|fatally|demise|died", test$Notes)
#NFword.notes <- grep("No hit|no hits|No Hits|No Hit|NO HITS|NO HIT|Shooting, non-fatal|accidental|Accidental", test$Notes)

#Fword.narrative <- grep("Fatal%|fatally|dead|DOA|killed|Fatality| fatal|demise|died|killing|knife|deseased", test$FullNarrative)
#NFword.narrative <- grep("Non-Fatal|Non-fatal|apprehended|No hits|NO HITS|non-fatal|accidental discharge|survive|Miss|miss", test$FullNarrative)

#|Victim|victim|knife|deseased|mental adding these did not score better

test$NoteFword <- NA
test$NoteNFword <- NA
test$NarFword <- NA
test$NarNFword <- NA

test$NoteFword[Fword.notes] <- "Y"
test$NoteNFword[NFword.notes] <- "Y"
test$NarFword[Fword.narrative] <- "Y"
test$NarNFword[NFword.narrative] <- "Y"

test$NarNFword[is.na(test$NarNFword)] <- "N"
test$NarFword[is.na(test$NarFword)] <- "N"
test$NoteNFword[is.na(test$NoteNFword)] <- "N"
test$NoteFword[is.na(test$NoteFword)] <- "N"

test$cert_f <- rep(NA,nrow(test))


for(i in 1:nrow(test)){
  
  if(test$NoteFword[i]=="Y"){
    if(test$NoteFword[i]==test$NoteNFword[i]) test$cert_f[i] <- FALSE
    else if(test$NoteFword[i]==test$NarNFword[i]) test$cert_f[i] <- FALSE
    else test$cert_f[i] <- TRUE
    
  }
  if(test$NarFword == "Y"){
    
    if(test$NarFword[i]==test$NoteNFword[i]) test$cert_f[i] <- FALSE
    if(test$NarFword[i]==test$NarNFword[i]) test$cert_f[i] <- FALSE
    else test$cert_f[i] <- TRUE
  } 
  if(test$NarNFword[i] == "Y") test$cert_f[i] <- FALSE
  if(test$NoteNFword[i]== "Y") test$cert_f[i] <-  FALSE
}

for(i in 1:nrow(test)){
  if(test$NoteFword[i]=="N"){
    if(test$NoteNFword[i]=="N"){
      if(test$NarFword[i]=="N"){
        if(test$NarNFword[i]=="N") {
          test$cert_f[i] <- "Unknown"
        }
      }
    } 
  }}

test$cert_f <- as.factor(test$cert_f)

test$cert_f[which(is.na(test$cert_f))] <- TRUE


# adding cert_city Column

test$cert_city <- "Unknown"

for (i in 1:nrow(test)) {
  if(test$City[i]=="Detroit"){test$cert_city[i] <- FALSE}
  if(test$City[i]=="Tampa"){test$cert_city[i] <- FALSE}
  if(test$City[i]=="St. Louis"){test$cert_city[i] <- FALSE}
  if(test$City[i]=="Chicago"){test$cert_city[i] <- FALSE}
  if(test$City[i]=="Philadelphia"){test$cert_city[i] <- FALSE}
  if(test$City[i]=="Houston"){test$cert_city[i] <- FALSE}
  if(test$City[i]=="Boston"){test$cert_city[i] <- TRUE}
  if(test$City[i]=="Portland"){test$cert_city[i] <- TRUE}
  if(test$City[i]=="Tucson"){test$cert_city[i] <- TRUE}
  if(test$City[i]=="Austin"){test$cert_city[i] <- TRUE}
  if(test$City[i]=="Albuquerque"){test$cert_city[i] <- TRUE}
  if(test$City[i]=="Phoenix"){test$cert_city[i] <- TRUE}
}

test$cert_city <- as.factor(test$cert_city)

View(test)



#pruned Tree

test$Fatal <- predict(prune1, newdata = test, type = "class")
table(test$Fatal)

write.csv(test, file = "1.csv")

