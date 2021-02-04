setwd("C:/Users/colin/Documents/WorkHobby/AdventOfCode/Day1/")

num <- read.csv("Day1Data.csv")

num <- num$ï..num

# Part 1 & 2

library("gtools")

num_perm <- data.frame(unique(permute(num)),row.names = NULL)

num_perm <- data.frame(permutations(v = num,n=200,r = 3,repeats.allowed = TRUE))

num_perm$k <- num_perm$X1 + num_perm$X2
num_perm$k2 <- num_perm$X1 + num_perm$X2 + num_perm$X3

num_perm[num_perm$k == 2020,]
num_perm[num_perm$k2 == 2020,]

# Day 2
rm(list=ls())

pass <- read.csv("../Day2/Day2Data.csv",stringsAsFactors = FALSE)
names(pass)[1] <- "min"

library(stringr)

pass$count <- str_count(pass$pass,pass$char)
pass$good <- 0
pass$good[pass$count <= pass$max & pass$count >= pass$min] <- 1
sum(pass$good)

pass$good2 <- FALSE
for(i in c(1:nrow(pass))){
a <- unique(unlist(stringr::str_locate_all(pass$pass[i],pattern = pass$char[i])))
print(paste(a,pass$char[i]))
pass$good2[i] <- (pass$min[i] %in% a & !(pass$max[i] %in% a)) | (!(pass$min[i] %in% a) & pass$max[i] %in% a)
}
sum(pass$good2)


# Day 3
rm(list= ls())

tree <- read.csv("../Day3/Day3Data.csv")

names(tree)[1] <- "c1"
tree <- tree[,1:31]

tree <- cbind(tree,tree)
tree <- cbind(tree,tree)
tree <- cbind(tree,tree)
tree <- cbind(tree,tree)
tree <- cbind(tree,tree)
tree <- cbind(tree,tree)
tree <- cbind(tree,tree)

tree[c(1:10),c(1:10)]

path <- c()
for(i in c(1:355)){
  path <- c(path,paste(tree[1+i,3*i+1]))
}
path
path13 <- sum(stringr::str_count(path,"#"))

path <- c()
for(i in c(1:355)){
  path <- c(path,paste(tree[1+i,i+1]))
}
path11 <- sum(stringr::str_count(path,"#"))

path <- c()
for(i in c(1:355)){
  path <- c(path,paste(tree[1+i,5*i+1]))
}
path15 <- sum(stringr::str_count(path,"#"))

path <- c()
for(i in c(1:355)){
  path <- c(path,paste(tree[1+i,7*i+1]))
}
path17 <- sum(stringr::str_count(path,"#"))

path <- c()
for(i in c(1:355)){
  path <- c(path,paste(tree[2*i+1,i+1]))
}
path21 <- sum(stringr::str_count(path,"#"))

as.numeric(path11) * as.numeric(path13) * as.numeric(path15) * as.numeric(path17) * as.numeric(path21)

## Day 4
rm(list=ls())

pass <- unlist(str_split(readChar("../Day4/passports.txt",nchars = 100000),pattern="\r\n\r\n"))
pass <- stringr::str_replace_all(pass,'\r',replacement = " ")
pass <- stringr::str_replace_all(pass,'\n',replacement = " ")

pass2 <- c()
pass2$byr <- as.numeric(str_remove(str_remove_all(str_extract(pass,regex("byr:.+")),pattern = regex("\\s.+")),pattern = regex(".+:")))
pass2$iyr <- as.numeric(str_remove(str_remove_all(str_extract(pass,regex("iyr:.+")),pattern = regex("\\s.+")) ,pattern = regex(".+:")))
pass2$eyr <- as.numeric(str_remove(str_remove_all(str_extract(pass,regex("eyr:.+")),pattern = regex("\\s.+")) ,pattern = regex(".+:")))
pass2$hgt <- str_remove(str_remove_all(str_extract(pass,regex("hgt:.+")),pattern = regex("\\s.+")) ,pattern = regex(".+:"))
pass2$hcl <- str_remove(str_remove_all(str_extract(pass,regex("hcl:.+")),pattern = regex("\\s.+")) ,pattern = regex(".+:"))
pass2$ecl <- str_remove(str_remove_all(str_extract(pass,regex("ecl:.+")),pattern = regex("\\s.+")) ,pattern = regex(".+:"))
pass2$pid <- str_remove(str_remove_all(str_extract(pass,regex("pid:.+")),pattern = regex("\\s.+")) ,pattern = regex(".+:"))
pass2$cid <- str_remove(str_remove_all(str_extract(pass,regex("cid:.+")),pattern = regex("\\s.+")) ,pattern = regex(".+:"))

pass2 <- data.frame(pass2)
pass2

pass2$c1 <- 0
pass2$c1[pass2$byr >= 1920 & pass2$byr <= 2002] <- 1

pass2$c2 <- 0
pass2$c2[pass2$iyr >= 2010 & pass2$iyr <= 2020] <- 1

pass2$c3 <- 0
pass2$c3[pass2$eyr >= 2020 & pass2$eyr <= 2030] <- 1

pass2$c4 <- 0
pass2$c4[str_detect(pass2$hgt,regex("in")) & as.numeric(str_remove(pass2$hgt,pattern = regex("in"))) %in% c(59:76)] <- 1
pass2$c4[str_detect(pass2$hgt,regex("cm")) & as.numeric(str_remove(pass2$hgt,pattern = regex("cm"))) %in% c(150:193)] <- 1

pass2$c5 <- 0
pass2$c5[str_match(pass2$hcl,pattern = regex("#[A-Z|a-z|0-9]{6}")) == pass2$hcl] <- 1 

pass2$c6 <- 0
colorval <- c("amb","blu","brn","gry","grn","hzl","oth")
pass2$c6[pass2$ecl %in% colorval] <- 1

pass2$c7 <- 0
pass2$c7[str_match(pass2$pid,regex("[0-9]{9}")) == pass2$pid] <- 1

pass2$c8 <- pass2$c1 + pass2$c2 + pass2$c3 + pass2$c4 + pass2$c5 + pass2$c6 + pass2$c7

sum(pass2$c8 == 7)

## Day 5
rm(list=ls())
library(tidyverse)
flight <- read.csv("../Day5/seating.csv")


names(flight)[1] <- "seat"
flight
flight <- data.frame(matrix(unlist(str_split(flight$seat,"")),byrow = T,nrow = 805),stringsAsFactors = FALSE)



for(i in c(1:7)){
  flight[,i] <- recode(flight[,i],"F" = 0, "B" = 1)
}
for(i in c(8:10)){
  flight[,i] <- recode(flight[,i],"L" = 0, "R" = 1)
}

myvect <- c(64,32,16,8,4,2,1,4,2,1)

flight <- sweep(flight,MARGIN=2,myvect,`*`)

flight$row <- flight$X1 + flight$X2 + flight$X3 + flight$X4 + flight$X5 + flight$X6 + flight$X7
flight$column <- flight$X8 + flight$X9 + flight$X10
flight$id <- flight$row * 8 + flight$column
hist(flight$id,breaks = 20)

k <- 0
for(i in c(1:805)){
  k <- sort(flight$id)[i+1] - sort(flight$id)[i]
  if(k != 1){
    print(paste(i,i-1))
  }
}
sort(flight$id)[567]

## Day 6
rm(list=ls())

pass <- unlist(str_split(readChar("../Day6/Day6.txt",nchars = 100000),pattern="\r\n\r\n"))
pass <- stringr::str_replace_all(pass,'\r',replacement = " ")
pass <- stringr::str_replace_all(pass,'\n',replacement = " ")
pass <- stringr::str_replace_all(pass,' ',replacement = "")
pass <- data.frame(pass)

i <- 1
k <- length(unique(unlist(str_split(pass[i,1],pattern = ""))))

for(i in c(1:454)){
k <- length(unique(unlist(str_split(pass[i,1],pattern = ""))))
pass[i,2] <- k
}
sum(pass$unq)

## Part 2

pass2 <- separate(data = data.frame(pass),sep="\r\n",col = 1,into = c(as.character(c(1:4))))
str(pass2)


for(i in c(1:454)){
    list1 <- unlist(str_split(pass2$`1`[i],pattern = ""))
    list2 <- unlist(str_split(pass2$`2`[i],pattern = ""))
    list3 <- unlist(str_split(pass2$`3`[i],pattern = ""))
    list4 <- unlist(str_split(pass2$`4`[i],pattern = ""))
    
    if(!is.na(list4) & !is.na(list3) & !is.na(list2)){
      mylist <- intersect(list4,list1)
      mylist <- intersect(list3,mylist)
      mylist <- intersect(list2,mylist)
      pass2[i,5] <- paste(mylist,collapse = "")
      pass2[i,6] <- length(mylist)
    } else if(!is.na(list3) & !is.na(list2)){
      mylist <- intersect(list3,list1)
      mylist <- intersect(list2,mylist)
      pass2[i,5] <- paste(mylist,collapse = "")
      pass2[i,6] <- length(mylist)
    } else if(!is.na(list2)) {
      mylist <- intersect(list2,list1)
      pass2[i,5] <- paste(mylist,collapse = "")
      pass2[i,6] <- length(mylist)
    } else {
      mylist <- list1
      pass2[i,5] <- paste(mylist,collapse = "")
      pass2[i,6] <- length(mylist)
    }

}
sum(pass2$V6)
pass2


## Day 7
rm(list=ls())

bags <- read_lines(file = "../Day7/bags.txt")
separate(data.frame(bags),into = c("container","inside"),sep = " contain ",col=1)
