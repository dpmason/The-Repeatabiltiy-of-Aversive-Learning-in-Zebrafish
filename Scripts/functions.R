#install.packages("pacman") # run this line if you don't have pacman installed
# loading packages
pacman::p_load(readxl, tidyr, dplyr, magrittr, lubridate, stringr)

#here is the excel sheet i enter data into, to give you an idea of what data i want from the files produced by the automated experimental (Zantiks) units

#Aversive Pilots.xlsx

#want <- read_excel("Aversive Pilots.xlsx", sheet = 1)

#and here are some examples of the csv files that the units produce

#trial-20190221T041305.csv 
#trial-20190221T041317.csv 

#have <- read.csv("trial-20190221T041305.csv", header = F) 

#i want to add up all the baseline times and distances & and the probe times and distances, but instead of zone specific, colour specific. so instead of collating the data straight down the columns. for zone distances and zone timers, the colours alternate so i need to add them up diagonally down in a zig zag direction the outcome should be zone timers and zone distances for each arena (which is each fish) for both the baseline and the probe phase please let me know if this doesn’t make any sense thanks! also, each .csv file is from one of the 4 Zantiks Units after an experiment. so i have to extract data from 4 of these files per experiment.

# Function for importing csv and exporting as long format

# testing function
#filepath = "trial-20190221T041317.csv"
#filepath = "./raw data/trial-20190221T041317.csv"
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

# same as this
# df <- df %>% mutate(marking = case_when(arena == "ARENA1" ~ fish[1],
#                                    arena == "ARENA2" ~ fish[2],
#                                    arena == "ARENA3" ~ fish[3]),
#                date = date,
#                filepath = filepath)

# removing "total" columns
df <- df[!str_detect(as.character(df$unit), "TOTAL"),]

# fixing numeric column
df$number <- as.numeric(df$number)

return(df)
}

