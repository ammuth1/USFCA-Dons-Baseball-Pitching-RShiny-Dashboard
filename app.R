library(dplyr)
library(readxl)
library(data.table)
library(rlang)
library(ggplot2)
library(DT)
library(shinythemes)

#READ IN EXCEL DATA FILE
annaliese_path <- "rapsodo_master_fall_2020.xlsx"
file <- read_excel(annaliese_path)

#CLEAN FILE FUNCTION

cleanFile <- function(afile){
  
  afile %>% 
    mutate(across(where(is.character), ~na_if(., " ")))
  
  col_names <- names(afile)
  col_names
  
  afile <- replace(afile, ':', '.')
  afile$strike <- as.numeric(as.factor(afile$strike))
  
  afile$pitch_type[afile$pitch_type == 'FB'] <- 1
  afile$pitch_type[afile$pitch_type == '2FB'] <- 2
  afile$pitch_type[afile$pitch_type == 'CH'] <- 3
  afile$pitch_type[afile$pitch_type == 'CV'] <- 4
  afile$pitch_type[afile$pitch_type == 'SL'] <- 5
  afile$pitch_type[afile$pitch_type == 'CT'] <- 6
  afile$pitch_type[afile$pitch_type == 'SP'] <- 7
  afile$pitch_type[afile$pitch_type == 'KN'] <- 8
  
  sub <- afile[, 5:length(col_names)]
  
  sub <- type.convert(sub)
  
  sub <- data.frame('name'=afile$name, sub)
  
  sub$strike <- ifelse(sub$strike == 1,'Y','N')
  
  sub <- transform(sub, break_ratio = vertical_breaks/ horizontal_breaks)
  
  library(tibble)
  sub <- na.omit(sub)
  
  sub <- add_column(sub, outliers=0, .after = "break_ratio")
  
  pitch_nums <- c(1,2,3,4,5,6,7,8)
  colnames <- names(sub)
  names <- unique(file$name)  
  outliers <- c()
  
  for (i in 1:length(unique(sub$name))) {
    for (j in 1:length(pitch_nums)){
      
      df <- subset(sub, name== names[i] & pitch_type== pitch_nums[j])
      
      Q <- quantile(df$break_ratio, probs=c(.25, .75), na.rm = FALSE)
      iqr <- IQR(df$break_ratio)
      up <-  Q[2]+1.5*iqr # Upper Range  
      low<- Q[1]-1.5*iqr # Lower Range
      
      for (r in 1:dim(sub)[1]){
        player = names[i] 
        pitch = pitch_nums[j]
        if (is.na(sub[r,17])){
          sub[r,18]=NA
        }
        else if (((sub[r, 17]< low)|(sub[r, 17]> up))&((sub[r, 1]==player)&(sub[r, 15]==pitch))){
          sub[r, 18] = 1
        }
      }
    }
  }  
  file = sub
  return(file)
}

cleaned_file <- cleanFile(file)

#COMPARE PITCH DISTANCE RANK FUNCTION

comparePitchDistanceRank <- function(p1, p2, sortby, last_name) {
  
  
  df <- cleaned_file
  
  df$pitch_type[df$pitch_type == 1] <- 'FB'
  df$pitch_type[df$pitch_type == 2] <- '2FB'
  df$pitch_type[df$pitch_type == 3] <- 'CH'
  df$pitch_type[df$pitch_type == 4] <- 'CV'
  df$pitch_type[df$pitch_type == 5] <- 'SL'
  df$pitch_type[df$pitch_type == 6] <- 'CT'
  df$pitch_type[df$pitch_type == 7] <- 'SP'
  df$pitch_type[df$pitch_type == 8] <- 'KN'
  
  ptypes <- unique(df$pitch_type)
  grouped <- setDT(df)[,list(h_max=max(horizontal_breaks), h_min=min(horizontal_breaks), h_median=as.numeric(median(horizontal_breaks)), v_max=max(vertical_breaks), v_min=min(vertical_breaks), v_median=as.numeric(median(vertical_breaks))), by=list(name, pitch_type)]
  grouped <- as.data.frame(grouped)
  
  col_order <- c("name", "pitch_type", "h_median", "h_min", "h_max", "v_median", "v_min", "v_max")
  grouped <- grouped[, col_order]
  
  o <- subset(grouped, grouped$pitch_type==p1 | grouped$pitch_type==p2)
  
  for (i in o$name) {
    
    pitcher_rows_in_df = sum(o[, 'name'] == i)
    
    if (pitcher_rows_in_df != 2) {
      
      o <- subset(o, o$name != i)
      if (i==last_name) {
        rank<- NA
        return(rank)
      }
    }
    
    
    
    distances <- c('med_dist_h', 'med_dist_v', 'min_dist_h', 'min_dist_v', 'max_dist_h', 'max_dist_v')
    for (j in seq(1,length(distances))) {
      o[distances[j]]<-0
    }
  }
  
  for (i in seq(1,dim(o)[1],2)){
    o[i, 9]=abs(o[i, 3]-o[i+1,3])
    o[i, 10]=abs(o[i, 6]-o[i+1, 6])
    o[i, 11]=abs(o[i, 4]-o[i+1,4])
    o[i, 12]=abs(o[i, 7]-o[i+1,7])
    o[i, 13]=abs(o[i, 5]-o[i+1,5])
    o[i, 14]=abs(o[i, 8]-o[i+1,8])
  }
  
  o <- subset(o, med_dist_h != 0.0)
  
  l <- paste(p1,p2)
  
  o$pitch_type[o$pitch_type == p1] <- 1
  o$pitch_type[o$pitch_type == p2] <- 1
  
  o=o[, c(1,2,9,10,11,12,13,14)]
  
  
  pitch_dist <- o
  
  pitch_dist <- pitch_dist[order(-pitch_dist[, sortby]), ]
  pitch_dist <- pitch_dist[ , c('name', sortby)]
  
  
  return(pitch_dist)
  
}

#TEST COMPARE PITCH DISTANCE RANK FUNCTION
#t1 <- comparePitchDistanceRank('SL', 'FB', 'med_dist_v', 'tolsma')


#COMPARE PITCH RANK FUNCTION

comparePitchRank <- function(p1, variable, sortby, last_name, tableorrank) {
  
  df <- cleaned_file
  df$pitch_type[df$pitch_type == 1] <- 'FB'
  df$pitch_type[df$pitch_type == 2] <- '2FB'
  df$pitch_type[df$pitch_type == 3] <- 'CH'
  df$pitch_type[df$pitch_type == 4] <- 'CV'
  df$pitch_type[df$pitch_type == 5] <- 'SL'
  df$pitch_type[df$pitch_type == 6] <- 'CT'
  df$pitch_type[df$pitch_type == 7] <- 'SP'
  df$pitch_type[df$pitch_type == 8] <- 'KN'
  
  ptypes <- unique(df$pitch_type)
  
  if (is.element(p1, ptypes) == FALSE) {
    return("Cannot calculate distance; one of the inputs is not a pitch type. Try entering a pitch abreviation in all caps")
  } else {
    
    grouped <- subset(df, pitch_type == p1, )
    
  }
  if ((last_name %in% unique(grouped$name)) == TRUE) {
    var <- match(variable, names(df))
    if (variable == 'velocity') {
      grouped <- grouped %>% group_by(name) %>% summarise(median = median(velocity), min = min(velocity), max = max(velocity), .groups='drop')
    } else if (variable == 'total_spin') {
      grouped <- grouped %>% group_by(name) %>% summarise(median = median(total_spin), min = min(total_spin), max = max(total_spin), .groups='drop')
    } else if (variable == 'true_spin') {
      grouped <- grouped %>% group_by(name) %>% summarise(median = median(true_spin), min = min(true_spin), max = max(true_spin), .groups='drop')
    } else if (variable == 'spin_efficiencies') {
      grouped <- grouped %>% group_by(name) %>% summarise(median = median(spin_efficiencies), min = min(spin_efficiencies), max = max(spin_efficiencies), .groups='drop')
    } else if (variable == 'spin_directions') {
      grouped <- grouped %>% group_by(name) %>% summarise(median = median(spin_directions), min = min(spin_directions), max = max(spin_directions), .groups='drop')
    } else if (variable == 'horizontal_breaks') {
      grouped <- grouped %>% group_by(name) %>% summarise(median = median(horizontal_breaks), min = min(horizontal_breaks), max = max(horizontal_breaks), .groups='drop')
    } else if (variable == 'vertical_breaks') {
      grouped <- grouped %>% group_by(name) %>% summarise(median = median(vertical_breaks), min = min(vertical_breaks), max = max(vertical_breaks), .groups='drop')
    } else if (variable == 'relative_height') {
      grouped <- grouped %>% group_by(name) %>% summarise(median = median(relative_height), min = min(relative_height), max = max(relative_height), .groups='drop')
    } else if (variable == 'relative_side') {
      grouped <- grouped %>% group_by(name) %>% summarise(median = median(relative_side), min = min(relative_side), max = max(relative_side), .groups='drop')
    } else if (variable == 'relative_angle') {
      grouped <- grouped %>% group_by(name) %>% summarise(median = median(relative_angle), min(relative_agle), max = max(relative_angle), .groups='drop')
    } else if (variable == 'horizontal_angle') {
      grouped <- grouped %>% group_by(name) %>% summarise(median = median(horizontal_angle), min = min(horizontal_angle), max = max(horizontal_angle), .groups='drop')
    } else if (variable == 'gyro') {
      grouped <- grouped %>% group_by(name) %>% summarise(median = median(gyro), min = min(gyro), max = max(gyro), .groups='drop')
    } else if (variable == 'spin_degrees') {
      grouped <- grouped %>% group_by(name) %>% summarise(median = median(spin_degrees), min = min(spin_degrees), max = max(spin_degrees), .groups='drop')
    } else if (variable == 'break_ratio') {
      grouped <- grouped %>% group_by(name) %>% summarise(median = median(break_ratio), min = min(break_ratio), max = max(break_ratio), .groups='drop')
    }
    
    grouped <- as.data.frame(grouped)
    
    grouped <- grouped[order(-grouped[, sortby]), ]
    
    grouped <- grouped[ , c('name', sortby)]
    grouped[[sortby]] <- round(grouped[[sortby]], 1) 
    
    
    if (tableorrank == '') {
      return(grouped)
    } else if (tableorrank == 'y') {
      rank <- match(last_name, grouped$name)
      return(rank)
      
    }
    
  } else {
    grouped = 'NA'
    rank = 'NA'
  }
  
  
}
#generate tables; doesn't matter which player we choose, so long as they have data on the given pitch type.
t1 <- comparePitchRank('FB', 'velocity', 'median','pham','')
t2 <- comparePitchRank('FB', 'total_spin', 'median','pham','')
t3 <- comparePitchRank('FB', 'spin_efficiencies', 'median','pham','')
t4 <- comparePitchRank('FB', 'vertical_breaks', 'median','pham','')
t5 <- comparePitchRank('FB', 'horizontal_breaks', 'median','pham','')
t6 <- comparePitchRank('CV', 'spin_efficiencies', 'median','pham','')
t7 <- comparePitchRank('CV', 'vertical_breaks', 'median','pham','')
t8 <- comparePitchRank('CV', 'horizontal_breaks', 'median','pham','')
t9 <- comparePitchRank('SL', 'total_spin', 'median','washburn','')
t10 <- comparePitchRank('SL', 'spin_efficiencies', 'median','washburn','')
t11 <- comparePitchRank('SL', 'vertical_breaks', 'median','washburn','')
t12 <- comparePitchRank('SL', 'horizontal_breaks', 'median','washburn','')
t13 <- comparePitchRank('CH', 'spin_efficiencies', 'median','washburn','')
t14 <- comparePitchRank('CH', 'vertical_breaks', 'median','washburn','')
t15 <- comparePitchRank('CH', 'horizontal_breaks', 'median','washburn','')

#BOXPLOTS FUNCTION
boxplots <- function(ptype, variable) {
  
  
  s <- paste(ptype, variable)
  df <- cleaned_file
  df$spin_degrees <- as.numeric(df$spin_degrees)
  df$spin_degrees <- 360-df$spin_degrees
  cols <- names(df)
  df$pitch_type[df$pitch_type == 1] <- 'FB'
  df$pitch_type[df$pitch_type == 2] <- '2FB'
  df$pitch_type[df$pitch_type == 3] <- 'CH'
  df$pitch_type[df$pitch_type == 4] <- 'CV'
  df$pitch_type[df$pitch_type == 5] <- 'SL'
  df$pitch_type[df$pitch_type == 6] <- 'CT'
  df$pitch_type[df$pitch_type == 7] <- 'SP'
  df$pitch_type[df$pitch_type == 8] <- 'KN'
  if ((ptype %in% unique(df$pitch_type)) & (variable %in% cols)) {
    df <- df[(df$pitch_type == ptype), ][c('name', variable)]
    ggplot(data=df, aes(x=velocity, y=name, fill=name))+geom_boxplot()+xlab(s)
    if (variable == 'velocity') {
      ggplot(data=df, aes(x=velocity, y=name, fill=name))+geom_boxplot()+xlab(s)
    } else if (variable == 'total_spin') {
      ggplot(data=df, aes(x=total_spin, y=name, fill=name))+geom_boxplot()+xlab(s)
    } else if (variable == 'true_spin') {
      ggplot(data=df, aes(x=true_spin, y=name, fill=name))+geom_boxplot()+xlab(s)
    } else if (variable == 'spin_efficiencies') {
      ggplot(data=df, aes(x=spin_efficiencies, y=name, fill=name))+geom_boxplot()+xlab(s)
    } else if (variable == 'spin_directions') {
      ggplot(data=df, aes(x=spin_directions, y=name, fill=name))+geom_boxplot()+xlab(s)
    } else if (variable == 'horizontal_breaks') {
      ggplot(data=df, aes(x=horizontal_breaks, y=name, fill=name))+geom_boxplot()+xlab(s)
    } else if (variable == 'vertical_breaks') {
      ggplot(data=df, aes(x=vertical_breaks, y=name, fill=name))+geom_boxplot()+xlab(s)
    } else if (variable == 'relative_height') {
      ggplot(data=df, aes(x=relative_height, y=name, fill=name))+geom_boxplot()+xlab(s)
    } else if (variable == 'relative_side') {
      ggplot(data=df, aes(x=relative_side, y=name, fill=name))+geom_boxplot()+xlab(s)
    } else if (variable == 'relative_angle') {
      ggplot(data=df, aes(x=relative_angle, y=name, fill=name))+geom_boxplot()+xlab(s)
    } else if (variable == 'horizontal_angle') {
      ggplot(data=df, aes(x=horizontal_angle, y=name, fill=name))+geom_boxplot()+xlab(s)
    } else if (variable == 'gyro') {
      ggplot(data=df, aes(x=gyro, y=name, fill=name))+geom_boxplot()+xlab(s)
    } else if (variable == 'spin_degrees') {
      ggplot(data=df, aes(x=spin_degrees, y=name, fill=name))+geom_boxplot()+xlab(s)
    } else if (variable == 'break_ratio') {
      ggplot(data=df, aes(x=break_ratio, y=name, fill=name))+geom_boxplot()+xlab(s)
    }
  } 
}


plt1 <- boxplots('FB', 'velocity')

#SCATTERPLOT FUNCTION
scatterplot <- function(aname) {
  df <- subset(cleaned_file, name==aname)
  
  df$pitch_type[df$pitch_type == 1] <- 'FB'
  df$pitch_type[df$pitch_type == 2] <- '2FB'
  df$pitch_type[df$pitch_type == 3] <- 'CH'
  df$pitch_type[df$pitch_type == 4] <- 'CV'
  df$pitch_type[df$pitch_type == 5] <- 'SL'
  df$pitch_type[df$pitch_type == 6] <- 'CT'
  df$pitch_type[df$pitch_type == 7] <- 'SP'
  df$pitch_type[df$pitch_type == 8] <- 'KN'
  
  ggplot(df, aes(x=horizontal_breaks, y=vertical_breaks, fill=pitch_type, color=pitch_type))+geom_point()+xlim(-20, 20)+ylim(-20, 20)+geom_hline(yintercept = 0) +geom_vline(xintercept = 0)
  
}

#USER AUTHENTICATION if applicable

#credentials <- data.frame(
#  user = c("zach", "annaliese", "tim", "keplinger"), # mandatory
#  password = c("godonsz", "godonsa", "godonst", "godonsk"), # mandatory
#  start = c("2019-04-15"), # optinal (all others)
#  expire = c(NA, "2060-12-31"),
#  admin = c(FALSE, TRUE),
#  comment = "Simple and secure authentification mechanism 
#  for single ‘Shiny’ applications.",
#  stringsAsFactors = FALSE
#)



#TEST ANALYSIS FUNCTION

analysis <- function(aname, ptype) {
  raw <- read_excel("rapsodo_master_fall_2020.xlsx")
  
  zach_path_mac <- "/Volumes/NO NAME/USF/usf_rapsodo/Rapsodo_master4.xlsx"
  zach_path_pc <- "D:\\USF\\usf_rapsodo\\rapsodo_master_fall_2020.xlsx"
  annaliese_path <- "rapsodo_master_fall_2020.xlsx"
  
  file <- read_excel(annaliese_path)
  file <- cleanFile(file)
  file <- cleaned_file
  file$pitch_type[file$pitch_type == 1] <- 'FB'
  file$pitch_type[file$pitch_type == 2] <- '2FB'
  file$pitch_type[file$pitch_type == 3] <- 'CH'
  file$pitch_type[file$pitch_type == 4] <- 'CV'
  file$pitch_type[file$pitch_type == 5] <- 'SL'
  file$pitch_type[file$pitch_type == 6] <- 'CT'
  file$pitch_type[file$pitch_type == 7] <- 'SP'
  file$pitch_type[file$pitch_type == 8] <- 'KN'
  data <- file
  names <- unique(file$name)
  playersubset <- subset(data, subset = name==aname)
  if (nrow(subset(playersubset, pitch_type==ptype))==0) {
    return(paste("No ", ptype, " found."))
  } else {
    data <- raw
    
    
    uniquepitch <- unique(raw$pitch_type)
    if (is.element(ptype, unique(data$pitch_type)) == FALSE) {
      return(paste("No data on ", aname, "'s ", ptype))
    } else {
      data1 <- data
      for (pitchs in uniquepitch) {
        
        data <- subset(data, pitch_type==pitchs)
        
        data[data== "\\s+"] <- NA
        data[data==''] <- NA
        data$strike[data$strike=="Y"] <- 1
        data$strike[data$strike=="N"] <- 0
        data$pitch_type[data$pitch_type == 'FB'] <- 1
        data$pitch_type[data$pitch_type == '2FB'] <- 2
        data$pitch_type[data$pitch_type == 'CH'] <- 3
        data$pitch_type[data$pitch_type == 'CV'] <- 4
        data$pitch_type[data$pitch_type == 'SL'] <- 5
        data$pitch_type[data$pitch_type == 'CT'] <- 6
        data$pitch_type[data$pitch_type == 'SP'] <- 7
        data$pitch_type[data$pitch_type == 'KN'] <- 8
        
        sub <- data[ , c(2,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
        sub <- na.omit(sub)
        subnames <- sub$name
        subspindeg <- sub$spin_degrees
        sub <- subset(sub, select = -c(name))
        
        sub <- type.convert(sub)
        
        sub <- cbind(name = subnames, sub)
        
        
        sub$strike[sub$strike == 1] <- 'Y'
        sub$strike[sub$strike == 0] <- 'N'
        
        sub <- transform(sub, break_ratio = vertical_breaks/ horizontal_breaks)
        row.has.na <- apply(sub, 1, function(x){any(is.na(x))})
        sub <- sub[!row.has.na,]
        
        
        outliers = c()
        pitch_nums <- c(1,2,3,4,5,6,7,8)
        
        names <- unique(sub$name)
        
        
        sub['outlier'] <- 0
        
        
        for (i in range(length(names))) {
          for (j in range(length(pitch_nums))) {
            
            
            df <- subset(sub, name==names[i] & pitch_type == pitch_nums[j])
            Q <- quantile(df$break_ratio, probs=c(.25, .75), na.rm = FALSE)
            iqr <- IQR(df$break_ratio)
            up <-  Q[2]+1.5*iqr # Upper Range  
            low<- Q[1]-1.5*iqr # Lower Range
            rng <- subset(df, break_ratio > up | break_ratio < low)
            append(outliers, c(low, up))
            
            for (r in range(nrow(sub))) {
              player <- names[i] 
              pitch <- pitch_nums[j]
              if (is.na(sub[r,17])){
                sub[r,18]<-NA
              } else if (((sub[r, 17]< low)|(sub[r, 17]> up))&((sub[r, 1]==player)&(sub[r, 15]==pitch))){
                sub[r, 18] <- 1
              }
            }
          }
        }
        
        sub$pitch_type[sub$pitch_type == 1] <- 'FB'
        sub$pitch_type[sub$pitch_type == 2] <- '2FB'
        sub$pitch_type[sub$pitch_type == 3] <- 'CH'
        sub$pitch_type[sub$pitch_type == 4] <- 'CV'
        sub$pitch_type[sub$pitch_type == 5] <- 'SL'
        sub$pitch_type[sub$pitch_type == 6] <- 'CT'
        sub$pitch_type[sub$pitch_type == 7] <- 'SP'
        sub$pitch_type[sub$pitch_type == 8] <- 'KN'
        
        sub$spin_degrees <- subspindeg
        data <- sub
        
        data <- subset(data, outlier != 1)
        belowzero <- subset(data, spin_degrees >= 0)
        
        
        lefties <- c("young", "cowdrey")
        
        
        hand = c()
        for (i in data$name) {
          if (i %in% lefties) {
            hand <- c(hand, 'LH')
            
          } else {
            hand <- c(hand, 'RH')
            
          }
        }
        
        data <- cbind(data, hand)
        
        
        data$spin_degrees <- as.numeric(data$spin_degrees)
        data$spin_degrees <- 360-data$spin_degrees
        
        data <- na.omit(data)
        data <- subset(data, name == aname)
        if (pitchs == "FB"){
          fastball4_df <- data
        } else if (pitchs == "2FB") {
          fastball2_df <- data
        } else if (pitchs == "SL") {
          slider_df <- data
        } else if (pitchs == "CV") {
          curve_df <- data
        } else if (pitchs == "CH") {
          change_df <- data
        }
        data <- data1
      }
      names <- unique(file$name)
      
      
      if (ptype == 'FB') {
        #4 seam FB
        #return(fb4txt)
        velo_mean_fb <- mean(fastball4_df$velocity)
        velo_median_fb <- median(fastball4_df$velocity)
        rank_velo_median_fb <- comparePitchRank('FB', 'velocity', 'median', aname, 'y')
        
        fb4txt <- paste("4sFastball velo Mean: ", velo_mean_fb)
        
        RPM_median_fb <- median(fastball4_df$total_spin)
        RPM_mean_fb <- mean(fastball4_df$total_spin)
        rank_RPM_median_fb <- comparePitchRank('FB', 'total_spin', 'median', aname, 'y')
        fb4txt <- paste(fb4txt,'\n4sFastball rpm Median: ', RPM_median_fb)
        
        
        spin_deg_mean_fb <- mean(fastball4_df$spin_degrees)
        spin_deg_median_fb  <- median(fastball4_df$spin_degrees)
        
        fb4txt <- paste(fb4txt,'\n4sFastball Spin Degree Mean: ', spin_deg_median_fb)
        
        spin_efficiency_mean_fb <- mean(fastball4_df$spin_efficiencies)
        spin_efficiency_median_fb <- median(fastball4_df$spin_efficiencies)
        
        rank_spin_efficiency_median_fb <- comparePitchRank('FB', 'spin_efficiencies', 'median', aname, 'y')
        fb4txt <- paste(fb4txt,'\n4sFastball Spin Efficiency Mean: ', spin_efficiency_mean_fb)
        
        rank_horizontal_break_median_fb <- comparePitchRank('FB', 'horizontal_breaks', 'median', aname, 'y')
        rank_vertical_break_median_fb <- comparePitchRank('FB', 'vertical_breaks', 'median', aname, 'y')
        
        bauer4 <- (RPM_median_fb/velo_mean_fb)
        bauer4 <- ceiling(bauer4)
        
        
        fb4txt <- paste("<h2/>4 Seam Fastball:</h2>", "<br/>")
        fb4txt  <- paste(fb4txt,"<b>Velo & Rank:</b> ", velo_median_fb, '&', rank_velo_median_fb, '<br/>')
        fb4txt <- paste(fb4txt, "<b>Spin Efficiency & Rank:</b> ", spin_efficiency_median_fb, '&', rank_spin_efficiency_median_fb, '<br/>')
        fb4txt <- paste(fb4txt, "<b>RPM & Rank:</b> ", RPM_median_fb, '&', rank_RPM_median_fb, '<br/>')
        fb4txt <- paste(fb4txt, "<b>Spin Direction:</b> ", spin_deg_median_fb, '<br/>')
        fb4txt <- paste(fb4txt, "<b>Horizontal Break Rank:</b> ", rank_horizontal_break_median_fb, '<br/>')
        fb4txt <- paste(fb4txt, "<b>Vertical Break (positive for FB) Rank:</b> ", rank_vertical_break_median_fb, '<br/>')
        
        #Fastball- 4 seam analysis
        if (bauer4 < 24) {
          fb4txt <- paste(fb4txt,"<br/><li>Below Average Bauer Units. Gravity acting more aggressively on FB's. Heavy/AS run/Sinking FB. Pair with east-west breaking balls/pitches ideally.</li>")
          
        } else {
          fb4txt <- paste(fb4txt,"<br/><li>Above Average Bauer Units. Gravity having less of effect on FB's. Up in zone = 'Rising'. Pair with Up/Down CB ideally.</li>")
          
        }
        if (RPM_median_fb > 2200) {
          fb4txt <- paste(fb4txt,'<br/><li>MLB equivalent spin.</li>')
        }
        
        if (spin_efficiency_median_fb < 87){
          fb4txt <- paste(fb4txt, "<br/><li>Inadvertant Cut on 4SFB:<br/>Potential Grip Issues: Fingers on the Outside of the ball, Thumb not underneath the middle and index fingers.<br/>Potential Grip Solutions: Adjust Finger position to more central location.<br/>Have the middle and index finger lined up on the middle/biggest part of the ball. Line up thumb and middle finger directly.<br/>Have fingertips at the index and middle finger perpendicular to the seam.</li>")
          
          
        } else if (spin_efficiency_median_fb <= 95) {
          fb4txt <- paste(fb4txt, "<br/><li>4SFB Spin Efficiency below desired efficiency, look at coordinates to indicate inadvertant cut.<br/>If efficiency can be improved, movement gains remain...</li>")
          
          
        } else if (spin_efficiency_median_fb > 95) {
          fb4txt <- paste(fb4txt, "<br/><li>Likely no inadvertent cut. Pure Spin dominated 4sFB. Movement gains likely already realized.</li>")
          
        }
        
        if ((spin_deg_median_fb > 180) && (spin_deg_median_fb < 210)) {
          fb4txt <- paste(fb4txt, "<br/><li>Over the Top Fastball. Expect max vertical break, minimum horizontal break.<br/>If movement gains remain, expect them to be vertical.<br/>Look to match with Up/Down CB.</li><br/><br/>")
          
        } else if (spin_deg_median_fb > 210 & spin_deg_median_fb <= 240) {
          fb4txt <- paste(fb4txt, '<br/><li>Classic three fourths Fastball: Expect equal vertical and horizontal break.vIf movement gains remain, expect them to be equal horizontal and vertical.<br/>Look to match with Slurve CB/SL.</li><br/><br/>')
        } else if (spin_deg_median_fb > 240 & spin_deg_median_fb <= 270) {
          fb4txt <- paste(fb4txt, "<br/><li>Lower three fourths Fastball: Expect maximum horizontal break, minimal vertical break.<br/>If movement gains remain, expect them to be hoizontal.<br/>Look to match with Sweeping / True Slider.</li><br/><br/>")
        }
        return(fb4txt)
        
      } else if (ptype == '2FB') {
        #2 Seam FB Analysis
        velo_mean_2fb <- mean(fastball2_df$velocity)
        velo_median_2fb <- median(fastball2_df$velocity)
        txt2fb <- paste("2sFastball velo Mean: ", velo_mean_2fb, '<br/>')
        
        
        RPM_mean_2fb <- mean(fastball2_df$total_spin)
        RPM_median_2fb <- median(fastball2_df$total_spin)
        rank_RPM_median_2fb <- comparePitchRank('2FB', 'total_spin', 'median', 'washburn', 'y')
        txt2fb <- paste(txt2fb, "2sFastball rpm Mean: ", RPM_mean_2fb, '<br/>')
        
        
        spin_deg_mean_2fb <- mean(fastball2_df$spin_degrees)
        spin_deg_median_2fb <- median(fastball2_df$spin_degrees)
        txt2fb <-paste(txt2fb,"2sFastball Spin Degree Mean: ", spin_deg_mean_2fb, '<br/>')
        
        
        spin_efficiency_mean_2fb <- mean(fastball2_df$spin_efficiencies)
        spin_efficiency_median_2fb <- median(fastball2_df$spin_efficiencies)
        txt2fb <- paste(txt2fb,"2sFastball Spin Efficiency Mean: ", spin_efficiency_mean_2fb, '<br/>')
        
        rank_horizontal_break_median_2fb <- comparePitchRank('2FB', 'horizontal_breaks', 'median', aname, 'y')
        rank_vertical_break_median_2fb <- comparePitchRank("2FB", 'vertical_breaks', 'median', aname, 'y')
        rank_spin_efficiency_median_2fb <- comparePitchRank('2FB', 'spin_efficiencies', 'median', aname, 'y')
        rank_vertical_break_off_fb_median_2fb <- comparePitchDistanceRank('2FB', 'FB', 'med_dist_v', aname)
        rank_vertical_break_off_fb_median_2fb <- match(aname, rank_vertical_break_off_fb_median_2fb$name)
        rank_horizontal_break_off_fb_median_2fb <- comparePitchDistanceRank('2FB', 'FB', 'med_dist_h', aname)
        rank_horizontal_break_off_fb_median_2fb <- match(aname, rank_horizontal_break_off_fb_median_2fb$name)
        
        bauer2 <- (RPM_median_2fb/velo_median_2fb)
        nan <- is.na(bauer2)
        if (nan==TRUE) {
          print("No 2FB Found")
        } else {
          bauer2 <- ceiling(bauer2)
        }
        
        
        names <- unique(file$name) 
        txt2fb <- paste("<h2/>2 Seam Fastball:</h2>", "<br/>")
        txt2fb <- paste(txt2fb, "<b>Velo:</b> ", velo_median_2fb, '<br/>')
        txt2fb <- paste(txt2fb, "<b>Spin Efficiency & Rank:</b> ", spin_efficiency_median_2fb, "& ", rank_spin_efficiency_median_2fb, '<br/>')
        txt2fb <- paste(txt2fb, "<b>RPM & Rank:</b> ", RPM_median_2fb, '&', rank_RPM_median_2fb, '<br/>')
        txt2fb <- paste(txt2fb, "<b>Spin Direction:</b> ", spin_deg_median_2fb, '<br/>')
        txt2fb <- paste(txt2fb, "<b>Horizontal Break Rank:</b> ", rank_horizontal_break_median_2fb)
        txt2fb <- paste(txt2fb, "<b>Vertical break (positive for FB) Rank:</b> ", rank_vertical_break_median_2fb, '<br/>')
        txt2fb <- paste(txt2fb, "<b>Horizontal Break off 4FB Rank:</b> ", rank_horizontal_break_off_fb_median_2fb, '<br/>')
        txt2fb <- paste(txt2fb, "<b>Vertical Break off 4FB Rank:</b> ", rank_vertical_break_off_fb_median_2fb, '<br/>')
        
        
        if (bauer2 < 24) {
          txt2fb <- paste(txt2fb,"<br/><li>Below Average Bauer Units on 2FB. Gravity acting more aggressively on 2SFB's. Heavy/AS Run/Sinking. Check coordinates to confirm seperation from 4fb.</li>")
          
        } else {
          txt2fb <- paste(txt2fb, "<br/><li>Above Average Bauer Units on 2FB. Gravity having less of effect on 2SFB's. Up in zone = 'Rising'. Examine XY Plots, unlikely optimal for 2SFB.</li>")
          
        }
        
        
        if(spin_efficiency_median_2fb < 87) {
          txt2fb <- paste(txt2fb, "<br/><li>Inadvertant Cut on 2SFB's': Potential Grip Issues: Fingers on the Outside of the ball, Thumb not underneath the middle and index fingers.<br/>Potential Grip Solutions: Adjust Finger position to more central location.<br/>Have the middle and index finger lined up on the middle/biggest part of the ball. Line up thumb and middle finger directly.<br/>Have fingertips at the index and middle finger perpendicular to the seam.</li>")
          
        } else if (spin_efficiency_median_2fb <= 95) {
          txt2fb <- paste(txt2fb, "<br/><li>2SFB Spin Efficiency below desired efficiency, look at coordinates to indicate inadvertant cut. Movement gains likely remain.</li>")
          
        } else if (spin_efficiency_median_2fb > 95) {
          txt2fb <- paste(txt2fb, "<br/><li>Likely no inadvertent cut on 2SFB.</li>")
          
        }
        
        if (spin_deg_median_2fb >= 180 & spin_deg_median_2fb <= 210) {
          txt2fb <- paste(txt2fb, "<br/><li>Over the Top 2FB. Look at coordinates, movement rank, and speak with pitcher about use.Pitch might not be a fit for the arm slot.</li>")
          
        } else if (spin_deg_median_2fb > 210 & spin_deg_median_2fb <= 240) {
          txt2fb <- paste(txt2fb, "<br/><li>Three fourths 2FB: Expect equal vertical and horizontal break. If movement gains remain, expect them to be equal horizontal and vertical. Need to examine how well pitcher backs off spin.</li>")
        }else if (spin_deg_median_2fb > 240 & spin_deg_median_2fb <= 270) {
          txt2fb <- paste(txt2fb, "<br/><li>Lower three fourths 2FB Fastball: Expect maximum horizontal break, minimal vertical break. If movement gains remain, expect them to be hoizontal. Examine how well pitcher backs off spin to generate run.</li>")
        }
        return(txt2fb)
      } else if (ptype == 'CH') {
        # #Changeup analysis 
        velo_median_fb <- median(fastball4_df$velocity)
        
        velo_mean_changeup <- mean(change_df$velocity)
        velo_median_changeup <- median(change_df$velocity)
        
        
        spin_deg_mean_changeup <- mean(change_df$spin_degrees)
        spin_deg_median_changeup <- median(change_df$spin_degrees)
        
        
        RPM_median_fb <- median(fastball4_df$total_spin)
        RPM_median_changeup <- median(change_df$total_spin)
        RPM_mean_changeup <- mean(change_df$total_spin)
        
        
        spin_efficiency_mean_changeup <- mean(change_df$spin_efficiencies)
        spin_efficiency_median_changeup <- median(change_df$spin_efficiencies)
        rank_spin_efficiency_median_changeup <- comparePitchRank('CH', 'spin_efficiencies', 'median', aname, 'y')
        
        changeup_velo_off_fb <- velo_median_fb - velo_median_changeup
        
        changeup_spin_off_fb <- RPM_median_fb - RPM_median_changeup
        
        spincnan <- is.na(spin_deg_mean_changeup)
        
        rank_RPM_changeup <- comparePitchRank('CH', 'total_spin', 'median', aname, 'y')
        rank_horizontal_break_median_changeup <- comparePitchRank('CH', 'horizontal_breaks', 'median', aname, 'y')
        rank_vertical_break_median_changeup <- comparePitchRank('CH', 'vertical_breaks', 'median', aname, 'y')
        rank_vertical_break_off_fb_median_changeup <- comparePitchDistanceRank('CH', 'FB', 'med_dist_v', aname)
        rank_vertical_break_off_fb_median_changeup <- match(aname, rank_vertical_break_off_fb_median_changeup$name)
        rank_horizontal_break_off_fb_median_changeup <- comparePitchDistanceRank('CH', 'FB', 'med_dist_h', aname)
        rank_horizontal_break_off_fb_median_changeup <- match(aname, rank_horizontal_break_off_fb_median_changeup$name)
        
        if (spincnan  == TRUE) {
          return("No Changeup Found")
        } else {
          RPM_median_changeup <- median(change_df$total_spin)
          RPM_mean_changeup <- mean(change_df$total_spin)
          spin_efficiency_median_changeup <- median(change_df$spin_efficiencies)
        }
        
        
        # #Changeup
        handcheck <- any(change_df$hand == 'RH')
        txtch <- paste("<h2>Changeup:</h2>", "<br/>")
        
        txtch <- paste(txtch, "<b>Velo:</b> ", velo_median_changeup, "<br/>")
        txtch <- paste(txtch, "<b>RPM & Rank:</b> ", RPM_median_changeup, '& ', rank_RPM_changeup,"<br/>")
        txtch <- paste(txtch, "<b>Spin Direction:</b> ", spin_deg_median_changeup, "<br/>")
        txtch <- paste(txtch, "<b>Spin Efficiency & Rank:</b> ", spin_efficiency_median_changeup, "<br/>")
        txtch <- paste(txtch, "<b>Velo Difference off FB:</b> ", changeup_velo_off_fb, "<br/>")
        txtch <- paste(txtch, "<b>RPM Difference off FB:</b> ", changeup_spin_off_fb, "<br/>")
        txtch <- paste(txtch, "<b>Horizontal Break Rank:</b> ", rank_horizontal_break_median_changeup, "<br/>")
        txtch <- paste(txtch, "<b>Vertical Break Rank:</b> ", rank_vertical_break_median_changeup, "<br/>")
        txtch <- paste(txtch, "<b>Horizontal Break off 4FB Rank:</b> ", rank_horizontal_break_off_fb_median_changeup, "<br/>")
        
        
        
        
        if (handcheck) {
          if (spin_deg_median_changeup >= 180 & spin_deg_median_changeup <= 210) {
            txtch <- paste(txtch, "<li>Firm CH: Expect movement profile to closely mimic FB. Acts like 'BP Fastball'.<br/>Monitor change in velo of 4sFB/CH to be sure getting enough seperation.<br/>May pair well with CB/SL that move aggressive downward.</li><br/><br/>")
          }
          if(spin_deg_median_changeup >= 225 & spin_deg_median_changeup <= 240) {
            txtch <- paste(txtch, "<li>Classic CH: Near equal rise/run. Should plot 15 degrees off of FB.<br/>Pairs well with slurve SL/CB. Examine for match.<br/><br/>")
          }
          if(spin_deg_median_changeup >= 240 & spin_efficiency_median_changeup > .85) {
            txtch <- paste(txtch, "<li>Fader CH: Significant AS run. Should plot 30 degrees off of FB.<br/>Pairs well with true/sweeping slider, but downward CB works too. Examine for match.<br/>Minimal movement (if any) gains from increased efficiency remain.</li><br/><br/>")
          }
          if(spin_deg_median_changeup >= 240 & spin_efficiency_median_changeup < .85) {
            txtch <- paste(txtch, "<li>Fader CH: Significant AS run. Should plot 30 degrees off of FB.<br/>Pairs well with true/sweeping slider, but aggressive up/down CB works too. Examine for match.<br/>Spin Efficiency not yet on target (target 85). Movement gains from increased efficiency remain.</li><br/><br/>")
          }
        }
        
        
        return(txtch)
        
      } else if (ptype == 'SL') {
        
        #Slider analysis
        
        velo_mean_slider <- mean(slider_df$velocity)
        velo_median_slider <- median(slider_df$velocity)
        velo_median_fb <- median(fastball4_df$velocity)
        txtsl <- paste("Slider velo Median: ", velo_median_slider, '<br/>')
        
        
        spin_deg_mean_slider <- mean(slider_df$spin_degrees)
        spin_deg_median_slider <- median(slider_df$spin_degrees)
        txtsl <- paste(txtsl, "Slider Spin Degree Median: ", spin_deg_median_slider, '<br/>')
        
        
        RPM_median_slider <- median(slider_df$total_spin)
        RPM_mean_slider <- mean(slider_df$total_spin)
        rank_RPM_median_slider <- comparePitchRank('SL', 'total_spin', 'median', aname, 'y')
        txtsl  <- paste(txtsl, "Slider rpm Median: ", RPM_median_slider, '<br/>')
        
        
        spin_efficiency_mean_slider <- mean(slider_df$spin_efficiencies)
        spin_efficiency_median_slider <- median(slider_df$spin_efficiencies)
        rank_spin_efficiency_median_slider <- comparePitchRank('SL', 'spin_efficiencies', 'median', aname, 'y')
        txtsl <- paste(txtsl, "Slider Spin Efficiency Median: ", spin_efficiency_median_slider, '<br/>')
        
        
        rank_horizontal_break_median_slider <- comparePitchRank('SL', 'horizontal_breaks', 'median', aname, 'y')
        rank_vertical_break_median_slider <- comparePitchRank('SL', 'vertical_breaks', 'median', aname, 'y')
        rank_horizontal_break_off_fb_median_slider <- comparePitchDistanceRank('SL', 'FB', 'med_dist_h', aname)
        rank_horizontal_break_off_fb_median_slider <- match(aname, rank_horizontal_break_off_fb_median_slider$name)
        rank_vertical_break_off_fb_median_slider <- comparePitchDistanceRank('SL', 'FB', 'med_dist_v', aname)
        rank_vertical_break_off_fb_median_slider <- match(aname, rank_vertical_break_off_fb_median_slider$name)
        
        
        
        slider_velo_off_fb <- velo_median_fb - velo_median_slider
        
        nanspin <- is.na(spin_deg_median_slider)
        if (nanspin == TRUE) {
          print("No Slider Found")
        } else {
          spin_deg_median_slider <- ceiling(spin_deg_median_slider)
        }
        nanspin <- is.nan(spin_deg_median_slider)
        if (nanspin == TRUE) {
          return("No Slider Found.")
        } else {
          spin_deg_median_slider <- ceiling(spin_deg_median_slider)
        }
        txtsl <- paste("<h2/>Slider:</h2>", "<br/>")
        txtsl <- paste(txtsl, "Velo: ", velo_median_slider, '<br/>')
        txtsl <- paste(txtsl, "<b>RPM & Rank:</b> ", RPM_median_slider, '&', rank_RPM_median_slider, '<br/>')
        txtsl <- paste(txtsl, "<b>Spin Direction:</b> ", spin_deg_median_slider)
        txtsl <- paste(txtsl, "<b>Spin Efficiency & Rank:</b> ", spin_efficiency_median_slider,'&', rank_spin_efficiency_median_slider, '<br/>')
        txtsl <- paste(txtsl, "<b>Velo Difference off FB:</b> ", slider_velo_off_fb, '<br/>')
        txtsl <- paste(txtsl, "<b>Horizontal Break Rank:</b> ", rank_horizontal_break_median_slider, '<br/>')
        txtsl <- paste(txtsl, "<b>Vertical Break Rank:</b> ", rank_vertical_break_median_slider, '<br/>')
        txtsl <- paste(txtsl, "<b>Horizontal Break off 4FB Rank:</b> ", rank_horizontal_break_off_fb_median_slider, '<br/>')
        txtsl <- paste(txtsl, "<b>Vertical Break off 4FB Rank:</b> ", rank_vertical_break_off_fb_median_slider, '<br/>')
        
        #SLIDER
        
        if (spin_deg_median_slider >= 160 & spin_deg_median_slider <= 180) {
          txtsl <- "<br/><li>Inefficient Spin on the Slider.<br/>Possible Grip Issues: Fingers too “Behind the Ball” causing backspin, Lack of pressure / tension in fingers, ball not deep in hand.<br/>Possible Grip Solutions: Cheat fingers further up on the ball to help leverage a seam. Increase pressure/tension of fingers especially the middle finger, “spike” the grip with the middle finger.<br/>Cue the pitcher to: “Make it move”, “Think Slurve / Curve”.</li>"
          
        }
        poors <- subset(slider_df, spin_degrees >= 160 & spin_degrees <= 180)
        
        if (length(poors)>5) {
          txtsl <- paste(txtsl, "<br/><li>Monitor for Flat Slider. Poor spin efficiencies are present in data. Could be danger pitch.</li>")
        }
        
        
        if (spin_deg_median_slider >= 120 & spin_deg_median_slider <= 160) {
          txtsl <- paste("<br/><li>Hybrid Slider: little / no glove side break, majority gyro spin, some back & side spin, mistaken for bad cutters if thrown poorly.<br/>Check for pair with Up/Down CB to create seperation in breaking ball repertoire.</li>")
        } else if (spin_deg_median_slider >= 85 & spin_deg_median_slider <= 100) {
          txtsl <- paste(txtsl, "<br/><li>True Slider: Most Glove / Side Break, little to no rise, majority gyrospin, some side spin.<br/>Check for pair with Fader CH and significant AS run on FB's.</li>")
        } else if (spin_deg_median_slider >= 50 & spin_deg_median_slider <= 80) {
          txtsl <- paste(txtsl, "<br/><li>Slurve Slider: Some sweep / some depth, Majority gyro spin with near equal side spin and tail spin.<br/>Check for pair with classic changeup, 3/4FB and 2SFB.<br/>Beware of 'Slurve CBs' coupled with this pitch.</li>")
        } else if (spin_deg_median_slider <= 50) {
          txtsl <- paste(txtsl, "<br/><li>Slider mimics Curveball: Operational Errors could be present, or slider could have aggressive downward action.<br/>Speak with pitcher on cues about getting around the ball instead of on top.</li>")
        }
        return(txtsl)
      } else if (ptype == 'CV') {
        # #Curveball analysis
        velo_median_fb <- median(fastball4_df$velocity)
        velo_mean_curveball <- mean(curve_df$velocity)
        velo_median_curveball <- median(curve_df$velocity)
        
        
        spin_deg_mean_curveball <- mean(curve_df$spin_degrees)
        spin_deg_median_curveball <- median(curve_df$spin_degrees)
        
        
        RPM_median_curveball <- median(curve_df$total_spin)
        RPM_mean_curveball <- mean(curve_df$total_spin)
        rank_RPM_median_curveball <- comparePitchRank('CV', 'total_spin', 'median', aname, 'y')
        
        
        spin_efficiency_mean_curveball  <- mean(curve_df$spin_efficiencies)
        spin_efficiency_median_curveball <- median(curve_df$spin_efficiencies)
        rank_spin_efficiency_median_curveball <- comparePitchRank('CV', 'spin_efficiencies', 'median', aname, 'y')
        
        
        curveball_velo_off_fb <- velo_median_fb - velo_median_curveball
        
        rank_RPM_changeup <- comparePitchRank('CH', 'total_spin', 'median', aname, 'y')
        rank_horizontal_break_median_curveball <- comparePitchRank('CV', 'horizontal_breaks', 'median', aname, 'y')
        rank_vertical_break_median_curveball <- comparePitchRank('CV', 'vertical_breaks', 'median', aname, 'y')
        rank_vertical_break_off_fb_median_curveball <- comparePitchDistanceRank('CV', 'FB', 'med_dist_v', aname)
        rank_vertical_break_off_fb_median_curveball <- match(aname, rank_vertical_break_off_fb_median_curveball$name)
        rank_horizontal_break_off_fb_median_curveball <- comparePitchDistanceRank('CV', 'FB', 'med_dist_h', aname)
        rank_horizontal_break_off_fb_median_curveball <- match(aname, rank_horizontal_break_off_fb_median_curveball$name)
        
        
        spincbnan <- is.na(RPM_median_curveball)
        if (spincbnan == TRUE) {
          return("No Curve Found.")
        } else {
          RPM_median_curveball <- median(curve_df$total_spin)
          RPM_mean_curveball <- mean(curve_df$total_spin)
          spin_efficiency_median_curveball <- median(curve_df$spin_efficiencies)
        }
        handcheck <- any(curve_df$hand == 'RH')
        
        txtcv <- paste("<h2/>Curveball:</h2>", "<br/>")
        txtcv <- paste(txtcv, "<b>Velo:</b> ", velo_median_curveball, '<br/>')
        txtcv <- paste(txtcv, "<b>RPM & Rank:</b> ", RPM_median_curveball, '&', rank_RPM_median_curveball, '<br/>')
        txtcv <- paste(txtcv, "<b>Spin Efficiency & Rank:</b> ", spin_efficiency_median_curveball, '&', rank_spin_efficiency_median_curveball, '<br/>')
        txtcv <- paste(txtcv, "<b>Spin Direction:</b> ", spin_deg_median_curveball, '<br/>')
        txtcv <- paste(txtcv, "<b>Velo Difference off FB:</b> ", curveball_velo_off_fb, '<br/>')
        txtcv <- paste(txtcv, "<b>Horizontal Break Rank:</b> ", rank_horizontal_break_median_curveball, '<br/>')
        txtcv <- paste(txtcv, "<b>Vertical Break Rank:</b> ", rank_vertical_break_median_curveball, '<br/>')
        txtcv <- paste(txtcv, "<b>Horizontal Break off 4FB Rank:</b> ", rank_horizontal_break_off_fb_median_curveball, '<br/>')
        txtcv <- paste(txtcv, "<b>Vertical Break off 4FB Rank:</b> ", rank_vertical_break_off_fb_median_curveball, '<br/>')
        
        
        if (RPM_median_curveball > 2500) {
          txtcv <- paste(txtcv, "MLB Equivalent Spin.<br/><br/>")
        }
        handcheck <- ('RH' %in% unique(curve_df$hand))
        
        if (handcheck==TRUE) {
          if(spin_deg_median_curveball >= 60 & spin_deg_median_curveball <= 90) {
            txtcv <- paste(txtcv, "CB not picking up enough vertical break.<br/>Spin Direction mimics a sweeping slider or more likely a slurve.<br/>Possible grip issues could be: Ball Orientation in Hand not leveraging the seam.<br/>Or lack of finger pressure or tension is causing the ball to prematurely ‘pop’ out of the hand.<br/>Pitcher thinks he is on top of baseball but in fact on the side of it or under it.<br/>")
            txtcv <- paste(txtcv, "Potential Grip Solutions: Cheat Fingers up the laces. Increase finger pressure with the middle finger (maybe add a spike).<br/>Grip the ball between the seams vs across.<br/><br/>")
          } else if (spin_deg_median_curveball <= 60 & spin_deg_median_curveball >= 30 & spin_efficiency_median_curveball > 60) {
            txtcv <- paste(txtcv, "CB likely plays like a Slurve/Hybrid.<br/>Check for pair with classic Changeup, Classic 3/4 FB 2SFB.<br/>Spin Efficiency not yet on target (60 target). Movement gains from increased efficiency remain. All in the release, get fingers in front of the ball.<br/><br/>")
          } else if (spin_deg_median_curveball <= 60 & spin_deg_median_curveball >= 30 & spin_efficiency_median_curveball > 60) {
            txtcv <- paste(txtcv, "CB likely plays like a Slurve/Hybrid.<br/>Check for pair with classic Changeup, Classic 3/4 FB 2SFB.<br/>Minimal movement (if any) gains from increased efficiency remain.<br/><br/>")
          } else if (spin_deg_median_curveball <= 30 & spin_efficiency_median_curveball > 78) {
            txtcv <- paste(txtcv, "CB plays Up/Down. Pair with FB up in zone if FB plays up, examine FB Bauer Units for match.\nSpin efficiency on target. Minimal movement (if any) gains from increased efficiency remain.<br/><br/>")
          } else if (spin_deg_median_curveball <= 30 & spin_efficiency_median_curveball < 78) {
            txtcv <- paste(txtcv, "CB plays Up/Down. Pair with FB up in zone if FB plays up, examine FB Bauer Units for match.<br/>Spin efficiency not yet on target (78 target). Movement gains from increased efficiency remain.<br/>All in the release, too much gyro/bullet spin. Look for spin efficiency problems with other pitches.<br/><br/>")
          }
        } else { 
          if(spin_deg_median_curveball >= 90 & spin_deg_median_curveball <= 270) {
            txtcv <- paste(txtcv, "CB not picking up enough vertical break. Spin Direction mimics a slider or more likely a slurve.<br/>Possible grip issues could be: Ball Orientation in Hand not leveraging the seam.<br/>Or lack of finger pressure or tension is causing the ball to prematurely ‘pop’ out of the hand.<br/>Pitcher thinks he is on top of baseball but in fact on the side of it or under it.<br/>")
            txtcv <- paste(txtcv, "Potential Grip Solutions: Cheat Fingers up the laces. Increase finger pressure with the middle finger (maybe add a spike).<br/>Grip the ball between the seams vs across.<br/><br/>")
          }
        }
        return(txtcv)
        
      }
      
    }
  }
}
#TEST ANALYSIS FUNCTION
#textanalysis <- analysis()









######################################################################
######################################################################
######################################################################
##############BEGIN SHINY APP SERVERY AND UI##########################


library(shiny)
library(shinymanager)

ui <- fluidPage(theme = shinytheme("darkly"),
                
                tags$h2("Welcome to USFCA Dons Baseball 2020 Pitching Analysis"),
                tags$small(em("Created, designed, and edited by Annaliese Muth.")),
                sidebarPanel(
                  selectInput("playername", "Please Select a Player",
                              choices = unique(file$name)
                  ),
                  selectInput("ptype", "Select a pitch type for Boxplot",
                              choices = c("FB"= "FB",
                                          "2FB"="2FB",
                                          "CH"="CH",
                                          "CV"="CV",
                                          "SL"="SL")),
                  selectInput("var", "Select a Variable for Boxplot",
                              choices = names(cleaned_file[ , -c(1,9,15,18)])),
                  tags$p(
                    style = "margin-top: 15px;", # Adds a little clean spacing below the dropdowns
                    tags$a(
                      href = "https://github.com/ammuth1/USFCA-Dons-Baseball-Pitching-RShiny-Dashboard/tree/main", 
                      "Check out the code for this dashboard on Github here.", 
                      target = "_blank" # Opens the link in a new browser tab
                    )
                  )
                ),
                mainPanel(
                  tabsetPanel(type = "tabs",
                              tabPanel("Data Visulizations",
                                       fluidRow(
                                         column(width = 6,
                                                h4("Boxplot"),
                                                plotOutput("plot1")),
                                         column(width = 6,
                                                h4("H/V Break Scatterplot"),
                                                plotOutput("plot2")))),
                              
                              tabPanel(
                                "Leaderboard",
                                fluidRow(
                                  column(width = 4,
                                         h4("FB Velocity"),
                                         div(DT::dataTableOutput("table1"))),
                                  column(width = 4,
                                         h4("FB Total Spin"),
                                         div(DT::dataTableOutput("table2"))), 
                                  column(width = 4,
                                         h4("FB Spin Efficiencies"),
                                         div(DT::dataTableOutput("table3")))),
                                fluidRow(
                                  column(width = 4,
                                         h4("FB Vertical Breaks"),
                                         div(DT::dataTableOutput("table4"))),
                                  column(width = 4,
                                         h4("FB Horizontal Breaks"),
                                         div(DT::dataTableOutput("table5"))),
                                  column(width = 4,
                                         h4("CV Spin Efficiencies"),
                                         DT::dataTableOutput('table6')),
                                  
                                ),
                                
                                fluidRow(
                                  column(width = 4,
                                         h4("CV Vertical Breaks"),
                                         DT::dataTableOutput('table7')), 
                                  column(width = 4,
                                         h4("CV Horizontal Breaks"),
                                         DT::dataTableOutput('table8')),
                                  column(width = 4,
                                         h4("SL Total Spin"),
                                         DT::dataTableOutput('table9')),
                                  
                                ),
                                fluidRow(
                                  column(width = 4,
                                         h4("SL Spin Efficiencies"),
                                         DT::dataTableOutput('table10')), 
                                  column(width = 4,
                                         h4("SL Vertical Breaks"),
                                         DT::dataTableOutput('table11')),
                                  column(width = 4,
                                         h4("SL Horizontal Breaks"),
                                         DT::dataTableOutput('table12'))
                                  
                                ),
                                fluidRow(
                                  column(width = 4,
                                         h4("CH Spin Efficiencies"),
                                         DT::dataTableOutput('table13')),
                                  column(width = 4,
                                         h4("CH Vertical Breaks"),
                                         DT::dataTableOutput('table14')), 
                                  column(width = 4,
                                         h4("CH Horizontal Breaks"),
                                         DT::dataTableOutput('table15'))
                                  
                                )
                              ),
                              tabPanel('Summary', htmlOutput('summary')))
                  
                ),
                
)



# Wrap your UI with secure_app for user authentication if applicable
#ui <- secure_app(ui)


server <- function(input, output, session) {
  
  # connect user authentication data if applicable
  #  res_auth <- secure_server(
  #    check_credentials = check_credentials(credentials)
  
  #  )
  output$table1 <- DT::renderDataTable({
    #table 1
    datatable(t1, selection="multiple", escape=FALSE, 
              options = list(dom = 't'))
  })
  output$table2 <- DT::renderDataTable({
    #table 2
    datatable(t2, selection="multiple", escape=FALSE, 
              options = list(dom = 't'))
  })
  output$table3 <- DT::renderDataTable({
    #table 3
    datatable(t3, selection="multiple", escape=FALSE, 
              options = list(dom = 't'))
  })
  output$table4 <- DT::renderDataTable({
    #table 4
    datatable(t4, selection="multiple", escape=FALSE, 
              options = list(dom = 't'))
  })
  output$table5 <- DT::renderDataTable({
    #table 5
    datatable(t5,  selection="multiple", escape=FALSE, 
              options = list(dom = 't'))
  })
  output$table6 <- DT::renderDataTable({
    datatable(t6,  selection="multiple", escape=FALSE, 
              options = list(dom = 't'))
    #table 6
  })
  output$table7 <- DT::renderDataTable({
    datatable(t7,  selection="multiple", escape=FALSE, 
              options = list(dom = 't'))
    #table 7
  })
  output$table8 <- DT::renderDataTable({
    datatable(t8,  selection="multiple", escape=FALSE, 
              options = list(dom = 't'))
    #table 8
  })
  output$table9 <- DT::renderDataTable({
    datatable(t9,  selection="multiple", escape=FALSE, 
              options = list(dom = 't'))
    #table 9
  })
  output$table10 <- DT::renderDataTable({
    datatable(t10,  selection="multiple", escape=FALSE, 
              options = list(dom = 't'))
    #table 10
  })
  output$table11 <- DT::renderDataTable({
    datatable(t11,  selection="multiple", escape=FALSE, 
              options = list(dom = 't'))
    #tale 11
  })
  output$table12 <- DT::renderDataTable({
    datatable(t12,  selection="multiple", escape=FALSE, 
              options = list(dom = 't'))
    #table 12
  })
  output$table13 <- DT::renderDataTable({
    datatable(t13,  selection="multiple", escape=FALSE, 
              options = list(dom = 't'))
    #table 13
  })
  output$table14 <- DT::renderDataTable({
    datatable(t14,  selection="multiple", escape=FALSE, 
              options = list(dom = 't'))
    #table 14
  })
  output$table15 <- DT::renderDataTable({
    datatable(t15,  selection="multiple", escape=FALSE, 
              options = list(dom = 't'))
    #table 15
  })
  
  
  output$plot1 <- renderPlot(
    #plt1
    boxplots(input$ptype, input$var)
  )
  output$plot2 <- renderPlot(
    scatterplot(input$playername)
    
    
  )
  output$summary <- renderText({
    #textanalysis
    textanalysis <- analysis(input$playername, input$ptype)
    HTML(
      textanalysis
      #paste0('This is a test.<br/>', "We don't know if this is working or not.<br/>", "But let's hope.")
    )
    #testing <- c('this is a test.', 'want to know if this is working.', 'is it??')
  })
  
  output$auth_output <- renderText({
    
  })
  
  
  # your classic server logic
  
}

shinyApp(ui, server)
