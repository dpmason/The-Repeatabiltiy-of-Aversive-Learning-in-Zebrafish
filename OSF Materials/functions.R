#install.packages("pacman") # run this line if you don't have pacman installed
# loading packages
pacman::p_load(readxl, tidyr, dplyr, magrittr, lubridate, stringr)

import_csv <- function(filepath){
  
  have <- read.csv(filepath, header = F)

# extracting fish_ID for each arena, the date, and time
fish <- strsplit(as.character(have[2,4]), " ") %>% unlist
date <- as.Date(have[1,3])
time <- substr(have[1,3], 12, 20)

# keeping only rows that have data
data <- have[have$V12 != "", ]

# extracting the four arenas
arena <- list()
pos <- c(8, 10, 12, 14) # position of arena columns
for(i in 1:4){
  arena[[i]] <- data[, c(1, 5:7, pos[i]:(pos[i]+1))]
  names(arena[[i]]) <- paste0("C", 1:ncol(arena[[i]]))
  names(arena)[[i]] <- fish[i]
}

# writing function for dealing with one arena
#a = arena[[1]]
arena.func <- function(a){
  # filling in blank rows
  blank <- (1:length(a$C2))[a$C2 == ""]
  chunks <- seq(1, length(blank), by = 3)
  for(i in 1:length(chunks)){
    a[blank[chunks[i]]:(blank[chunks[i]]+2),2:3] <- a[(blank[chunks[i]]-1),2:3]
  }
  # putting data in long form
  a <- rbind(a[,-6], a[,-5] %>% rename(C5 = C6))
  
  # making a column for the arena and type of test 
  arena_test <- a$C5
  # finding position of numbers
  char.num <- as.numeric(arena_test %>% as.character)
  pos.char <- which(is.na(char.num)==T)
  for(i in 1:length(pos.char)){
    arena_test[(pos.char[i]+1):(pos.char[i]+3)] <- arena_test[pos.char[i]]
  }
  a$arena_test <- arena_test
  
  # removing unnecessary rows
  a <- a[!is.na(char.num),] %>% rename(time = C1,
                                       type = C2,
                                       unit = C3,
                                       variable = C4,
                                       number = C5)
  
  # making column for arena, zone, and check/grey
  splt <- strsplit(as.character(a$arena), "_")
  a$arena <- lapply(splt, function(x) x[1]) %>% unlist 
  a$zone <- lapply(splt, function(x) x[2]) %>% unlist 
  a$test <- lapply(splt, function(x) x[3]) %>% unlist
  
  a <- dplyr::select(a, -arena_test)
  
  return(a)
  
}

# applying function to all arenas
df <- lapply(arena, function(x) arena.func(x)) %>% bind_rows()

# adding columns for fish and date and the file number
df %<>% mutate(marking = case_when(arena == "ARENA1" ~ fish[1],
                                  arena == "ARENA2" ~ fish[2],
                                  arena == "ARENA3" ~ fish[3],
                                  arena == "ARENA4" ~ fish[4]),
               date = date,
               filepath = filepath)

df <- df[!str_detect(as.character(df$unit), "TOTAL"),]

# fixing numeric column
df$number <- as.numeric(df$number)

return(df)
}

add_day <- function(x) { #adding day column
  x %>% mutate(day = case_when (grepl("2019-06-24", date) ~ "1",
                                grepl("0024-06-19", date) ~ "1",
                                grepl("2019-06-25", date) ~ "1",
                                grepl("2019-06-26", date) ~ "3",
                                grepl("2019-06-27", date) ~ "3",
                                grepl("2019-07-01", date) ~ "8",
                                grepl("2019-07-02", date) ~ "8",
                                grepl("2019-07-03", date) ~ "10",
                                grepl("2019-07-04", date) ~ "10",
                                grepl("2019-07-08", date) ~ "15",
                                grepl("2019-07-09", date) ~ "15",
                                grepl("2019-07-10", date) ~ "17",
                                grepl("2019-07-11", date) ~ "17",
                                grepl("2019-07-15", date) ~ "22",
                                grepl("2019-07-16", date) ~ "22",
                                grepl("2019-07-17", date) ~ "24",
                                grepl("2019-07-18", date) ~ "24"))
}

add_day2 <- function(x) { #adding day column for experiment 2
  x %>% mutate(day = case_when (grepl("D1", filepath) ~ "93",
                                grepl("D2", filepath) ~ "93",
                                grepl("D3", filepath) ~ "95",
                                grepl("D4", filepath) ~ "95",
                                grepl("D5", filepath) ~ "100",
                                grepl("D6", filepath) ~ "100",
                                grepl("D7", filepath) ~ "102",
                                grepl("D8", filepath) ~ "102"))
}  

add_exp <- function(x) { #adding experiment type column
  x %>% mutate(exp = case_when (grepl("REDYEL", filepath) ~ "REDYEL",
                                grepl("YELRED", filepath) ~ "YELRED",
                                grepl("GREBLU", filepath) ~ "GREBLU",
                                grepl("BLUGRE", filepath) ~ "BLUGRE",
                                grepl("CHKGRY", filepath) ~ "CHKGRY",
                                grepl("GRYCHK", filepath) ~ "GRYCHK",
                                grepl("MAGORA", filepath) ~ "MAGORA",
                                grepl("ORAMAG", filepath) ~ "ORAMAG",
                                grepl("MOBASELINE", filepath) ~ "MAGORA"))
}

add_learning <- function(x) { #adding intial or reverse learning column
  x %>% mutate(learning = case_when (grepl("2019-06-24", date) ~ "initial",
                                     grepl("0024-06-19", date) ~ "initial",
                                     grepl("2019-06-25", date) ~ "initial",
                                     grepl("2019-06-26", date) ~ "initial",
                                     grepl("2019-06-27", date) ~ "initial",
                                     grepl("2019-07-01", date) ~ "initial",
                                     grepl("2019-07-02", date) ~ "initial",
                                     grepl("2019-07-03", date) ~ "initial",
                                     grepl("2019-07-04", date) ~ "initial",
                                     grepl("2019-07-08", date) ~ "reverse",
                                     grepl("2019-07-09", date) ~ "reverse",
                                     grepl("2019-07-10", date) ~ "reverse",
                                     grepl("2019-07-11", date) ~ "reverse",
                                     grepl("2019-07-15", date) ~ "reverse",
                                     grepl("2019-07-16", date) ~ "reverse",
                                     grepl("2019-07-17", date) ~ "reverse",
                                     grepl("2019-07-18", date) ~ "reverse",
                                     grepl("2019-08-06", date) ~ "reverse",
                                     grepl("2019-08-07", date) ~ "reverse"))
}

add_learning2 <- function(x) { #adding intial or reverse learning column for experiment 2
  x %>% mutate(learning = case_when (grepl("D1", filepath) ~ "second",
                                     grepl("D2", filepath) ~ "second",
                                     grepl("D3", filepath) ~ "secondreverse",
                                     grepl("D4", filepath) ~ "secondreverse",
                                     grepl("D5", filepath) ~ "third",
                                     grepl("D6", filepath) ~ "third",
                                     grepl("D7", filepath) ~ "thirdreverse",
                                     grepl("D8", filepath) ~ "thirdreverse"))
}  

add_learning3 <-function(x) {
  x %>% mutate(learning = case_when (grepl("D1-", filepath) ~ "initial",
                                     grepl("D2-", filepath) ~ "initial",
                                     grepl("D3-", filepath) ~ "initial",
                                     grepl("D4-", filepath) ~ "initial",
                                     grepl("D5-", filepath) ~ "initial",
                                     grepl("D6-", filepath) ~ "initial",
                                     grepl("D7-", filepath) ~ "initial",
                                     grepl("D8-", filepath) ~ "initial",
                                     grepl("D9-", filepath) ~ "reverse",
                                     grepl("D10-", filepath) ~ "reverse",
                                     grepl("D11-", filepath) ~ "reverse",
                                     grepl("D12-", filepath) ~ "reverse",
                                     grepl("D13-", filepath) ~ "reverse",
                                     grepl("D14-", filepath) ~ "reverse",
                                     grepl("D15-", filepath) ~ "reverse",
                                     grepl("D16-", filepath) ~ "reverse",
                                     grepl("MOBASELINE", filepath) ~ "reverse"))
}

add_set <- function(x) { #adds 1st measurement for green and blue
  x %>% mutate(set = case_when (grepl("GREBLU", exp) ~ "firstg",
                                grepl("BLUGRE", exp) ~ "firstb"))
}

add_set2 <- function(x) { #dds 2nd, 3rd measurement for green and blue
  x %>% mutate(set = case_when (grepl("D1", filepath) ~ "secondg",
                                grepl("D2", filepath) ~ "secondg",
                                grepl("D3", filepath) ~ "secondb",
                                grepl("D4", filepath) ~ "secondb",
                                grepl("D5", filepath) ~ "thirdg",
                                grepl("D6", filepath) ~ "thirdg",
                                grepl("D7", filepath) ~ "thirdb",
                                grepl("D8", filepath) ~ "thirdb"))
}

add_zone <- function(x) { #adds whether the time spent is conditioned stimulus or not
  x %>%   mutate(zone = case_when (grepl("CS", test) ~ "CS",
                                   grepl("NON", test) ~ "NON"))
}

add_tod <- function(x) { #adds time of day of observation
  x %>% mutate(tod = case_when (grepl("AM", filepath) ~ "AM",
                                grepl("NOON", filepath) ~ "NOON",
                                grepl("PM", filepath) ~ "PM")) 
}

add_expgroup <- function(x) { #groups colours
  x %>% mutate(exp_group = case_when (grepl("REDYEL", exp) ~ "1",
                                      grepl("YELRED", exp) ~ "1",
                                      grepl("GREBLU", exp) ~ "2",
                                      grepl("BLUGRE", exp) ~ "2",
                                      grepl("CHKGRY", exp) ~ "3",
                                      grepl("GRYCHK", exp) ~ "3",
                                      grepl("ORAMAG", exp) ~ "4",
                                      grepl("MAGORA", exp) ~ "4"))
}

add_diff <- function(x) { #spreads data and adds the response variable 'difference'
  
  x[,7] = NULL
  
  x %>% spread(type, time) %>% mutate(difference = (BASELINE - PROBE))
}

fp <- "MOBASELINE-t1d-B4-PM.csv" #missing baseline data

##CS filter function
cs_only <- function (x) {
  x %>% filter(zone == "CS")
}
