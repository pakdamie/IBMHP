###Have 80 Red Blood cell
###Have 20 parasite

library(gganimate)
library(ggplot2)

###Let's say this is a data.frame of what it would look like
Pop<- 
  
  data.frame(
    individuals = seq(1,100),
    type = c(rep("R",80), rep("P",20)),
    x = sample.int(100, 100),
    y = sample.int(100, 100),
    time_alive = rep(0,100),
    time_infected = rep(0,100),
    infected = rep("F",100),
    dead = rep("F",100),
    infect = rep("F",100)
  )



###Let's try getting them to move?
Movement<- function(Pop){

  move_x <- sample(x = c(-1, 0, 1), size = 100,replace=TRUE)
  move_y <- sample(x = c(-1, 0, 1), size = 100,replace=TRUE)
  
  Pop[,'x'] <- Pop[,'x'] + move_x
  Pop[,'y'] <- Pop[,'y'] + move_y
  
  ###
  for (p in seq(1,100)){
    
    #If going out of the right boundary
    if (Pop[p,'x'] < 100){
      Pop[p,'x'] <- 99
    }
    #If going out of the left boundary
    if (Pop[p,'x'] < 1){
      Pop[p,'x'] <- 2
    }
    #If going out of the top boundary
    if (Pop[p,'y'] < 100){
      Pop[p,'y'] <- 99
    }
    #If going out of the bottom boundary
    if (Pop[p,'y'] < 1){
      Pop[p,'y'] <- 2
    }
  return (Pop)
  }
}

Infection <- function(Pop){
  
  ###RBC
  RBC <-     subset(Pop, Pop$type=='R')
  ###Parasite
  Parasite <- subset(Pop,Pop$type=='P')
  ###Infected
  Infected_RBC <- subset(Pop,Pop$type=='I')
  
  
  ###Look for the red blood cells that are in the same
  ###square as the parasite
  id_RBC <- which(RBC[,'x'] %in%  Parasite[,'x'] &
          RBC[,'y'] %in%  Parasite[,'y'] )
  
  ###Look for the parasites that are in the same
  ###square as the red blood cells
  id_Parasite<- which(Parasite[,'x'] %in%  RBC[,'x'] &
                      Parasite[,'y'] %in%  RBC[,'y'] )

  ###
 if(length(id_RBC) > 0 | length(id_Parasite) > 0 ){
   RBC[id_RBC,]$type<- "I"
   RBC[id_RBC,]$infected<- T
   RBC[id_RBC,]$time_infected<- RBC[id_RBC,]$time_infected + 1
  
   Parasite[id_Parasite,]$infect <-T
   Parasite[id_Parasite,]$x <-NA
   Parasite[id_Parasite,]$y <-NA
   
   Pop <- rbind(RBC,Parasite,Infected_RBC)
   Pop  <- Pop[order(  Pop $individuals),]
   
 }
  else{
    Pop <- rbind(RBC,Parasite,Infected_RBC)
    Pop  <- Pop[order(  Pop $individuals),]
    
  }
  return(Pop)
  
}


full_list <- NULL

for (k in seq(1,10)){
  Infection_DF <- Infection(Pop)
  Pop$type<-  Infection_DF$type
  Pop$x <-   Infection_DF$x
  Pop$y <-   Infection_DF$y
  Pop$time_alive <- Pop$time_alive+1
  Pop$time_infected <-  Infection_DF$time_infected
  Pop$infected <-  Infection_DF$infected
  Pop$infect <-   Infection_DF$infect 
  Pop$time <- k
  Pop <- Movement(Pop)

  full_list[[k]] <-Pop

}

Pop_All <- do.call(rbind,full_list )

b<- ggplot(Pop_All, aes(x= x, y= y,color=type))+
  geom_point(size=1)+
  scale_color_manual(values=c('I'='green',
                              'R'='red',
                              'P'='blue'))+
                              transition_time(time)+ theme_void()+ labs(title = 'Year: {frame_time}')
anim_save('b.gif', animation = last_animation())
