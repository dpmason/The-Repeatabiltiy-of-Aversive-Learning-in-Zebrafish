

dom_draw <- function(dat, what = "") {
  dat <- filter(dat, exp == what) %>% 
    group_by(type) %>% summarise(mean=mean(time_min), sd=sd(time_min), se=sd(time_min)/sqrt(n())) 
  
  gplot <- ggplot(dat, aes(type, mean)) +
  geom_bar(position=position_dodge(), stat= "identity", width = 0.7, colour="black") +
  geom_errorbar(aes(ymin=mean - se, ymax = mean +se), width = 0.4, colour = "black", position=position_dodge(0.9)) +
  xlab("Test Period") +
  ylab("Time spent in unsafe zone (sec)") +
  ylim(0,60) + ggtitle(paste("Zebrafish preference for CS") )
  
  print(gplot)
}





#csv2df <- function(folder = "") {
  
#path <- paste0("./Day", 1:8, "/")

#allpath <- map(path,  ~list.files(path = .)[str_detect(list.files(path = .), ".csv")] ) %>% map2(.,path,~ paste0(.y, .x))

##pathdat <- map(flatten(allpath), import_csv) %>% bind_rows()
#pathdat <- map(unlist(allpath), import_csv) %>% bind_rows() %>% as_tibble()

#}


dom_sum <- function(x) {
  x %<>% group_by(filepath, date, marking, type, test, unit, variable, arena) %>% 
  summarise(total = sum(number)) %>% filter(variable == "ZONE_TIMERS") %>% 
  group_by(type, marking, test, filepath, date) %>% summarise(sum_time = sum(total), arena = unique(arena))
}







