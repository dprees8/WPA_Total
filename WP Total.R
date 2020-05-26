library(dplyr)

seasons <- 2019
pbp2019 <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

#clean up some of the NA WP values - run this process twice through
count=0

pbp2019fix<-pbp2019

for(i in pbp2019$play_id) {
  count=count+1
  count2=count+1
  if(pbp2019$game_id[count] == pbp2019$game_id[count2]) {
    if(is.na(pbp2019$home_wp[count])) {
      pbp2019fix$home_wp[count]=pbp2019$home_wp[count2]
    }
    if(is.na(pbp2019$home_wp_post[count])) {
      pbp2019fix$home_wp_post[count]=pbp2019fix$home_wp[count]
    }
    if(is.na(pbp2019$away_wp[count])) {
      pbp2019fix$away_wp[count]=pbp2019$away_wp[count2]
    }
    if(is.na(pbp2019$away_wp_post[count])) {
      pbp2019fix$away_wp_post[count]=pbp2019fix$away_wp[count]
    }
  }
}

#round 2 of clean up
count=0

pbp2019fix2<-pbp2019fix

for(i in pbp2019fix$play_id) {
  count=count+1
  count2=count+1
  if(pbp2019fix$game_id[count] == pbp2019fix$game_id[count2]) {
    if(is.na(pbp2019fix$home_wp[count])) {
      pbp2019fix2$home_wp[count]=pbp2019fix$home_wp[count2]
    }
    if(is.na(pbp2019fix$away_wp[count])) {
      pbp2019fix2$away_wp[count]=pbp2019fix$away_wp[count2]
    }
    if(is.na(pbp2019fix$away_wp[count])) {
      pbp2019fix2$away_wp[count]=pbp2019fix$away_wp[count2]
    }
    if(is.na(pbp2019fix$away_wp_post[count])) {
      pbp2019fix2$away_wp_post[count]=pbp2019fix2$away_wp[count]
    }
  }
}

## Fix data for Play_ID 4504
pbp2019fix2$home_wp[pbp2019fix2$play_id=="4504" & pbp2019fix2$game_id =="2019120801"]=0
pbp2019fix2$away_wp[pbp2019fix2$play_id=="4504" & pbp2019fix2$game_id =="2019120801"]=1

BAL_gameIDs_Reg<-c("2019090803","2019091500","2019092204","2019092901","2019100607","2019101301","2019102010","2019110311","2019111001","2019111700","2019112500","2019120100","2019120801","2019121200","2019122206","2019122900")

pbp2019_BAL<-subset(pbp2019fix2, pbp2019fix2$game_id %in% BAL_gameIDs_Reg)
pbp2019_BAL$wpa_BAL=0
count=0

#  for(i in pbp2019_game$play_id) {
for(i in pbp2019_BAL$play_id) {
      count=count+1
      count2=count+1
      count3=count2+1
      count4=count3+1
      count0=count-1
  if(pbp2019_BAL$play_id[count]==36) {
      if(pbp2019_BAL$home_team[count]=="BAL") {
          pbp2019_BAL$wpa_BAL[count]=pbp2019_BAL$home_wp[count2]-0.5
      } else {
          pbp2019_BAL$wpa_BAL[count]=pbp2019_BAL$away_wp[count2]-0.5
      }
  } else if(pbp2019_BAL$game_id[count]!=pbp2019_BAL$game_id[count0]) {
      if(pbp2019_BAL$home_team[count]=="BAL") {
        pbp2019_BAL$wpa_BAL[count]=pbp2019_BAL$home_wp_post[count]-0.5
      } else {
        pbp2019_BAL$wpa_BAL[count]=pbp2019_BAL$away_wp_post[count]-0.5
      }   
  } else if(is.na(pbp2019_BAL$game_id[count2])) {
      if(pbp2019_BAL$home_team[count]=="BAL") {
          pbp2019_BAL$wpa_BAL[count]=pbp2019_BAL$home_wp_post[count]-pbp2019_BAL$home_wp[count]
      } else {
          pbp2019_BAL$wpa_BAL[count]=pbp2019_BAL$away_wp_post[count]-pbp2019_BAL$away_wp[count]
      }    
  } else if(pbp2019_BAL$game_id[count]==pbp2019_BAL$game_id[count2]) {
      if(is.na(pbp2019_BAL$home_team[count])) {
          pbp2019_BAL$wpa_BAL[count]=0
      } else if(pbp2019_BAL$home_team[count]=="BAL") {
          if(is.na(pbp2019_BAL$home_wp[count])) {
              pbp2019_BAL$wpa_BAL[count]=0
          } else if(is.na(pbp2019_BAL$home_wp[count2])) {
              if(is.na(pbp2019_BAL$home_wp[count3])) {
                  pbp2019_BAL$wpa_BAL[count]=pbp2019_BAL$home_wp[count4]-pbp2019_BAL$home_wp[count]
              } else {
                  pbp2019_BAL$wpa_BAL[count]=pbp2019_BAL$home_wp[count3]-pbp2019_BAL$home_wp[count]
              }
          } else {
              pbp2019_BAL$wpa_BAL[count]=pbp2019_BAL$home_wp[count2]-pbp2019_BAL$home_wp[count]
          }
      } else {
          if(is.na(pbp2019_BAL$away_wp[count])) {
              pbp2019_BAL$wpa_BAL[count]=0
          } else if(is.na(pbp2019_BAL$away_wp[count2])) {
              if(is.na(pbp2019_BAL$away_wp[count3])) {
                  pbp2019_BAL$wpa_BAL[count]=pbp2019_BAL$away_wp[count4]-pbp2019_BAL$away_wp[count]
              } else {
                  pbp2019_BAL$wpa_BAL[count]=pbp2019_BAL$away_wp[count3]-pbp2019_BAL$away_wp[count]
              }
          } else {
              pbp2019_BAL$wpa_BAL[count]=pbp2019_BAL$away_wp[count2]-pbp2019_BAL$away_wp[count]
          }
      }
  } else {
      if(pbp2019_BAL$home_team[count]=="BAL") {
          pbp2019_BAL$wpa_BAL[count]=pbp2019_BAL$home_wp_post[count]-pbp2019_BAL$home_wp[count]
      } else {
          pbp2019_BAL$wpa_BAL[count]=pbp2019_BAL$away_wp_post[count]-pbp2019_BAL$away_wp[count]
    }    
  } 
}

total<-pbp2019_BAL %>% group_by(game_id) %>% summarize(sum=sum(wpa_BAL, na.rm=TRUE))
offense<-pbp2019_BAL %>% filter(posteam=="BAL") %>% group_by(game_id) %>% summarize(sum=sum(wpa_BAL, na.rm=TRUE))
defense<-pbp2019_BAL %>% filter(posteam!="BAL" | is.na(posteam)) %>% group_by(game_id) %>% summarize(sum=sum(wpa_BAL, na.rm=TRUE))

x<-merge(total, offense, by="game_id")
x<-merge(x, defense, by="game_id")
colnames(x)=c("Game_ID", "Total_WPA", "Offense_WPA", "Defense_WPA")

x.long<-gather(x, Variable, Value, -Game_ID)
x.long<-transform(x.long, Game_ID=factor(Game_ID, labels=c("W1","W2","W3","W4","W5","W6","W7","W9","W10","W11","W12","W13","W14","W15","W16","W17")))
ggplot(data=x.long, aes(x=Game_ID, y=Value, fill=Variable)) + geom_col(position="dodge") + ggtitle("Ravens Total WPA by Game") +labs(y="Total WPA",x="",fill="")