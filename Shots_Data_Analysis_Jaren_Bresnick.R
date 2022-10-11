# Shot Data Analysis 

# Read in file
library(readr)
stats <- read.csv("shots_data.csv")

# Team A Stats
TeamA_total = 0


TeamA_NC3_Makes = 0
TeamA_C3_Makes= 0
TeamA_2_Makes = 0

TeamA_NC3_Misses = 0
TeamA_C3_Misses = 0
TeamA_2_Misses = 0


# Team B Stats
TeamB_total=0


TeamB_NC3_Makes = 0
TeamB_C3_Makes= 0
TeamB_2_Makes = 0

TeamB_NC3_Misses = 0
TeamB_C3_Misses = 0
TeamB_2_Misses = 0


# Loop for identifying Team, Shot Coordinates and Shot Outcome
for (i in 1:nrow(stats)){
  
  # Identifying Team A
  if (stats[i,"team"]=="Team A"){
    TeamA_total=TeamA_total+1
    
    # Identifying Corner 3 Pointer
    if (abs(stats[i,"x"]) > 22 && stats[i,"y"]<=7.8){
      if (stats[i,"fgmade"]==1){
        TeamA_C3_Makes = TeamA_C3_Makes +1
      }
      else{
        TeamA_C3_Misses = TeamA_C3_Misses+1
      }
    }
    # Identifying 3 Pointer (Excluding Corner)
    else if ((sqrt(stats[i,"x"]^2 +stats[i,"y"]^2) > 23.75) && stats[i,"y"] >7.8){
      if (stats[i,"fgmade"]==1){
        TeamA_NC3_Makes = TeamA_NC3_Makes +1
      }
      else{
        TeamA_NC3_Misses = TeamA_NC3_Misses+1
      }
    }
    
    # Identifying 2 Pointer
   else{
      if (stats[i,"fgmade"]==1){
        
        TeamA_2_Makes = TeamA_2_Makes +1
      }
      else{
        TeamA_2_Misses = TeamA_2_Misses+1
      }
    }
  }
  
  # Identifying Team B
  if (stats[i,"team"]=="Team B"){
    TeamB_total=TeamB_total+1
    
    # Identifying Corner 3 Pointer
    if (abs(stats[i,"x"]) > 22 && stats[i,"y"]<=7.8){
      if (stats[i,"fgmade"]==1){
        TeamB_C3_Makes = TeamB_C3_Makes +1
      }
      else{
        TeamB_C3_Misses = TeamB_C3_Misses+1
      }
    }
    # Identifying 3 Pointer (Excluding Corner)
    else if ((sqrt(stats[i,"x"]^2 +stats[i,"y"]^2) > 23.75) && stats[i,"y"] >7.8){
      if (stats[i,"fgmade"]==1){
        TeamB_NC3_Makes = TeamB_NC3_Makes +1
      }
      else{
        TeamB_NC3_Misses = TeamB_NC3_Misses+1
      }
    }
    
    # Identifying 2 Pointer
    else{
      if (stats[i,"fgmade"]==1){
        
        TeamB_2_Makes = TeamB_2_Makes +1
      }
      else{
        TeamB_2_Misses = TeamB_2_Misses+1
      }
    }
  }
  
}


#eFG Calculations
TeamA_eFG_2 = (TeamA_2_Makes)/(TeamA_2_Makes+TeamA_2_Misses)
TeamA_eFG_NC3 = (TeamA_NC3_Makes+(0.5*TeamA_NC3_Makes))/(TeamA_NC3_Makes+TeamA_NC3_Misses)
TeamA_eFG_C3 = (TeamA_C3_Makes+(0.5*TeamA_C3_Makes))/(TeamA_C3_Makes+TeamA_C3_Misses)

TeamB_eFG_2 = (TeamB_2_Makes)/(TeamB_2_Makes+TeamB_2_Misses)
TeamB_eFG_NC3 = (TeamB_NC3_Makes+(0.5*TeamB_NC3_Makes))/(TeamB_NC3_Makes+TeamB_NC3_Misses)
TeamB_eFG_C3 = (TeamB_C3_Makes+(0.5*TeamB_C3_Makes))/(TeamB_C3_Makes+TeamB_C3_Misses)

#Shot Distribution Calculations
TeamA_SD_2=(TeamA_2_Makes+TeamA_2_Misses)/TeamA_total
TeamA_SD_NC3=(TeamA_NC3_Makes+TeamA_NC3_Misses)/TeamA_total
TeamA_SD_C3=(TeamA_C3_Makes+TeamA_C3_Misses)/TeamA_total

TeamB_SD_2=(TeamB_2_Makes+TeamB_2_Misses)/TeamB_total
TeamB_SD_NC3=(TeamB_NC3_Makes+TeamB_NC3_Misses)/TeamB_total
TeamB_SD_C3=(TeamB_C3_Makes+TeamB_C3_Misses)/TeamB_total


