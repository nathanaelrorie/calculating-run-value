#Install dplyr
install.packages("dplyr")
library(dplyr)

#Set Working Director to folder where game files exist
temp = list.files(pattern="\\.csv$")
myfiles = lapply(temp, read.csv)
myfiles[[3]]
summary(myfiles)
#Read in CSVs
df = bind_rows(myfiles[[1]], myfiles[[2]], myfiles[[3]], myfiles[[4]]
               , myfiles[[5]], myfiles[[6]], myfiles[[7]], myfiles[[8]]
               , myfiles[[9]], myfiles[[10]], myfiles[[11]], myfiles[[12]]
               , myfiles[[13]], myfiles[[14]], myfiles[[15]], myfiles[[16]]
               , myfiles[[17]], myfiles[[18]], myfiles[[19]], myfiles[[20]]
               , myfiles[[21]], myfiles[[22]], myfiles[[23]], myfiles[[24]]
               , myfiles[[25]], myfiles[[26]], myfiles[[27]], myfiles[[28]]
               , myfiles[[29]], myfiles[[30]], myfiles[[31]], myfiles[[32]]
               , myfiles[[33]], myfiles[[34]], myfiles[[35]], myfiles[[36]]
               , myfiles[[37]], myfiles[[38]], myfiles[[39]], myfiles[[40]]
               , myfiles[[41]], myfiles[[42]], myfiles[[43]], myfiles[[44]]
               , myfiles[[45]])

summary(df)
#df <- read.csv("/Users/nathanaelrorie/Desktop/Belmont Baseball/Trackman Data/2022-2023/Game CSV's/040223 Evansville GM 2.csv")
#df
#runvalues <- read.csv("RunValues.csv")
#runvalues

# Calculating Run Value Constants
values <- data.frame(
  three0B = .051,
  three0S = -.117,
  three0GB = -.314,
  three0LD = .045,
  three0FB = -.212,
  three1B = .168,
  three1S = -.066,
  three1GB = -.197,
  three1LD = .162,
  three1FB = -.095,
  two0B = .234,
  two0S = -.062,
  two0GB = -.171,
  two0LD = .188,
  two0FB = -.069,
  three2B = .234,
  three2S = -.294,
  three2GB = -.131,
  three2LD = .228,
  three2FB = -.029,
  one0B = .088,
  one0S = .035,
  one0GB = -.109,
  one0LD = .250,
  one0FB = -.007,
  two1B = .064,
  two1S = -.069,
  two1GB = -.107,
  two1LD = .252,
  two1FB = -.005,
  o0B = .032,
  o0S = -.037,
  o0GB = -.074,
  o0LD = .285,
  o0FB = .028,
  one1B = .048,
  one1S = -.054,
  one1GB = -.061,
  one1LD = .298,
  one1FB = .041,
  two2B = .085,
  two2S = -.209,
  two2GB = -.046,
  two2LD = .313,
  two2FB = .056,
  o1B = .024,
  o1S = -.051,
  o1GB = -.038,
  o1LD = .321,
  o1FB = .064,
  one2B = .038,
  one2S = -.171,
  one2GB = -.008,
  one2LD = .351,
  one2FB = .094,
  o2B = .021,
  o2S = -.150,
  o2GB = .013,
  o2LD = .372,
  o2FB = .115
)


#Create count column
#df["Count"] <- paste(c(df["Balls"],df["Strikes"]),collapse="")
#df

#for(PitcherId in df) {
#  if(df["Balls"] = 0 & df["Strikes"] = 0 & df["PitchCall"] = "BallCalled"){
#    rv = .032
#  }
#}

#Function assigns the run value for each event in the dataframe
rv <- function(data) {
  RunValue <- NULL
  for (i in 1: dim(data)[1]) {
    if(data$Balls[i] == 0 & data$Strikes[i] == 0 & data$PitchCall[i] == "BallCalled") {
      RunValue[i] = values$o0B
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 0 & data$PitchCall[i] == "StrikeCalled"){
      RunValue[i] = values$o0S
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 0 & data$PitchCall[i] == "StrikeSwinging"){
      RunValue[i] = values$o0S
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 0 & data$PitchCall[i] == "FoulBall"){
      RunValue[i] = values$o0S
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 0 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "GroundBall"){
      RunValue[i] = values$o0GB
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 0 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "LineDrive"){
      RunValue[i] = values$o0LD
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 0 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "FlyBall"){
      RunValue[i] = values$o0FB
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 1 & data$PitchCall[i] == "BallCalled"){
      RunValue[i] = values$o1B
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 1 & data$PitchCall[i] == "StrikeCalled"){
      RunValue[i] = values$o1S
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 1 & data$PitchCall[i] == "StrikeSwinging"){
      RunValue[i] = values$o1S
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 1 & data$PitchCall[i] == "FoulBall"){
      RunValue[i] = values$o1S
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 1 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "GroundBall"){
      RunValue[i] = values$o1GB
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 1 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "LineDrive"){
      RunValue[i] = values$o1LD
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 1 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "FlyBall"){
      RunValue[i] = values$o1FB
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 2 & data$PitchCall[i] == "BallCalled"){
      RunValue[i] = values$o2B
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 2 & data$PitchCall[i] == "StrikeCalled"){
      RunValue[i] = values$o2S
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 2 & data$PitchCall[i] == "StrikeSwinging"){
      RunValue[i] = values$o2S
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 2 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "GroundBall"){
      RunValue[i] = values$o2GB
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 2 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "LineDrive"){
      RunValue[i] = values$o2LD
    } else if (data$Balls[i] == 0 & data$Strikes[i] == 2 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "FlyBall"){
      RunValue[i] = values$o2FB
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 1 & data$PitchCall[i] == "BallCalled"){
      RunValue[i] = values$one1B
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 1 & data$PitchCall[i] == "StrikeCalled"){
      RunValue[i] = values$one1S
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 1 & data$PitchCall[i] == "StrikeSwinging"){
      RunValue[i] = values$one1S
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 1 & data$PitchCall[i] == "FoulBall"){
      RunValue[i] = values$one1S
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 1 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "GroundBall"){
      RunValue[i] = values$one1GB
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 1 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "LineDrive"){
      RunValue[i] = values$one1LD
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 1 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "FlyBall"){
      RunValue[i] = values$one1FB
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 2 & data$PitchCall[i] == "BallCalled"){
      RunValue[i] = values$one2B
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 2 & data$PitchCall[i] == "StrikeCalled"){
      RunValue[i] = values$one2S
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 2 & data$PitchCall[i] == "StrikeSwinging"){
      RunValue[i] = values$one2S
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 2 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "GroundBall"){
      RunValue[i] = values$one2GB
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 2 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "LineDrive"){
      RunValue[i] = values$one2LD
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 2 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "FlyBall"){
      RunValue[i] = values$one2FB
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 2 & data$PitchCall[i] == "BallCalled"){
      RunValue[i] = values$two2B
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 2 & data$PitchCall[i] == "StrikeCalled"){
      RunValue[i] = values$two2S
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 2 & data$PitchCall[i] == "StrikeSwinging"){
      RunValue[i] = values$two2S
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 2 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "GroundBall"){
      RunValue[i] = values$two2GB
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 2 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "LineDrive"){
      RunValue[i] = values$two2LD
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 2 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "FlyBall"){
      RunValue[i] = values$two2FB
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 0 & data$PitchCall[i] == "BallCalled"){
      RunValue[i] = values$one0B
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 0 & data$PitchCall[i] == "StrikeCalled"){
      RunValue[i] = values$one0S
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 0 & data$PitchCall[i] == "StrikeSwinging"){
      RunValue[i] = values$one0S
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 0 & data$PitchCall[i] == "FoulBall"){
      RunValue[i] = values$one0S
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 0 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "GroundBall"){
      RunValue[i] = values$one0GB
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 0 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "LineDrive"){
      RunValue[i] = values$one0LD
    } else if (data$Balls[i] == 1 & data$Strikes[i] == 0 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "FlyBall"){
      RunValue[i] = values$one0FB
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 0 & data$PitchCall[i] == "BallCalled"){
      RunValue[i] = values$two0B
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 0 & data$PitchCall[i] == "StrikeCalled"){
      RunValue[i] = values$two0S
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 0 & data$PitchCall[i] == "StrikeSwinging"){
      RunValue[i] = values$two0S
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 0 & data$PitchCall[i] == "FoulBall"){
      RunValue[i] = values$two0S
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 0 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "GroundBall"){
      RunValue[i] = values$two0GB
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 0 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "LineDrive"){
      RunValue[i] = values$two0LD
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 0 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "FlyBall"){
      RunValue[i] = values$two0FB
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 0 & data$PitchCall[i] == "BallCalled"){
      RunValue[i] = values$three0B
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 0 & data$PitchCall[i] == "StrikeCalled"){
      RunValue[i] = values$three0S
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 0 & data$PitchCall[i] == "StrikeSwinging"){
      RunValue[i] = values$three0S
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 0 & data$PitchCall[i] == "FoulBall"){
      RunValue[i] = values$three0S
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 0 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "GroundBall"){
      RunValue[i] = values$three0GB
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 0 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "LineDrive"){
      RunValue[i] = values$three0LD
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 0 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "FlyBall"){
      RunValue[i] = values$three0FB
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 1 & data$PitchCall[i] == "BallCalled"){
      RunValue[i] = values$two1B
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 1 & data$PitchCall[i] == "StrikeCalled"){
      RunValue[i] = values$two1S
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 1 & data$PitchCall[i] == "StrikeSwinging"){
      RunValue[i] = values$two1S
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 1 & data$PitchCall[i] == "FoulBall"){
      RunValue[i] = values$two1S
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 1 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "GroundBall"){
      RunValue[i] = values$two1GB
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 1 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "LineDrive"){
      RunValue[i] = values$two1LD
    } else if (data$Balls[i] == 2 & data$Strikes[i] == 1 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "FlyBall"){
      RunValue[i] = values$two1FB
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 1 & data$PitchCall[i] == "BallCalled"){
      RunValue[i] = values$three1B
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 1 & data$PitchCall[i] == "StrikeCalled"){
      RunValue[i] = values$three1S
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 1 & data$PitchCall[i] == "StrikeSwinging"){
      RunValue[i] = values$three1S
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 1 & data$PitchCall[i] == "FoulBall"){
      RunValue[i] = values$three1S
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 1 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "GroundBall"){
      RunValue[i] = values$three1GB
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 1 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "LineDrive"){
      RunValue[i] = values$three1LD
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 1 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "FlyBall"){
      RunValue[i] = values$three1FB
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 2 & data$PitchCall[i] == "BallCalled"){
      RunValue[i] = values$three2B
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 2 & data$PitchCall[i] == "StrikeCalled"){
      RunValue[i] = values$three2S
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 2 & data$PitchCall[i] == "StrikeSwinging"){
      RunValue[i] = values$three2S
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 2 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "GroundBall"){
      RunValue[i] = values$three2GB
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 2 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "LineDrive"){
      RunValue[i] = values$three2LD
    } else if (data$Balls[i] == 3 & data$Strikes[i] == 2 & data$PitchCall[i] == "InPlay" & data$TaggedHitType[i] == "FlyBall"){
      RunValue[i] = values$three2FB
    } else {
      RunValue[i] = NA
    }
    
  }
  cbind(data, RunValue)
}

#Calculate Run Value for each pitch
new_df <- rv(df)
new_df
new_df$RunValue
new_df

#Sum the Run Values for each pitcher's pitches
aggregate(new_df$RunValue, list(new_df$Pitcher, new_df$TaggedPitchType), FUN=sum, na.rm = TRUE)

#Create df with pitchers from select team
newest_df <- new_df[new_df$PitcherTeam == "BEL_BRU",]
newest_df
aggregate(newest_df$RunValue, list(newest_df$Pitcher, newest_df$TaggedPitchType), FUN=sum, na.rm = TRUE)
output <- data.frame(
  
)
