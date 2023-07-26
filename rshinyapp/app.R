#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



############################################################################################################
################################ Term Project Modeling Code ################################################
############################################################################################################

# clean env
rm(list = ls())

# install packages
library(dplyr)
library(ggplot2)
library(stringr)
library(class)

data_matches <- read.csv("cleaned_data/data_matches.csv")
data_points <- read.csv("cleaned_data/data_points.csv")
data_serves <- read.csv("cleaned_data/data_serves.csv")
data_shots <- read.csv("cleaned_data/data_shots.csv")
data_matches <- data_matches %>% mutate(date = as.Date(date))

# players list
player1 <- data_matches %>% 
  select("player1") %>%
  rename(player = "player1")

player2 <- data_matches %>%
  select("player2") %>%
  rename(player = "player2")

players <- unique(rbind(player1, player2))
for(i in 1:nrow(players)) {
  players[i,] <- str_trim(players[i,])
}
players <- unique(players)

row.names(players) <- NULL
players

NAs <- sample(c(NA), nrow(players), replace = TRUE)
NULLs <- sample(c(""), nrow(players), replace = TRUE)

data_players <- players %>%
  mutate(wide_t_ratio = NAs) %>%
  mutate(snv_ratio = NAs) %>%
  mutate(avg_rally_count = NAs) %>%
  mutate(winner_base_net_ratio = NAs) %>%
  mutate(playing_type = NULLs)

for(i in 1:nrow(players)) {
  p <- players[i,]
  m1 <- data_matches %>%
    filter(player1 == p) %>%
    select(match_id)
  
  m2 <- data_matches %>%
    filter(player2 == p) %>%
    select(match_id)
  
  # wide & t
  # snv
  srv1 <- m1 %>%
    inner_join(data_serves, by = "match_id") %>%
    filter(player == 1)
  
  srv2 <- m2 %>%
    inner_join(data_serves, by = "match_id") %>%
    filter(player == 2)
  
  srv <- rbind(srv1, srv2)
  wide_t_ratio <- (sum(srv$t) + sum(srv$wide)) / (sum(srv$t) + sum(srv$wide) + sum(srv$body)) * 100
  snv_ratio <- sum(srv$snvPts) / sum(srv$pts) * 100
  
  data_players[i, 2] <- wide_t_ratio
  data_players[i, 3] <- snv_ratio
  
  # rally count
  m <- rbind(m1, m2)
  pts <- m %>%
    inner_join(data_points, by = "match_id")
  
  avg_rally_count <- mean(pts$rallyCount)
  
  data_players[i, 4] <- avg_rally_count
  
  # winner ratio
  shots1 <- m1 %>%
    inner_join(data_shots, by = "match_id") %>%
    filter(player == 1)
  
  shots2 <- m2 %>%
    inner_join(data_shots, by = "match_id") %>%
    filter(player == 2)
  
  shots <- rbind(shots1, shots2)
  
  base_shots <- shots %>%
    filter(type == "Base")
  
  net_shots <- shots %>%
    filter(type == "Net")
  
  winner_base_net_ratio <- sum(base_shots$winners) / max(1,sum(net_shots$winners))   
  
  data_players[i, 5] <- winner_base_net_ratio
}

# remove observations with NAs
data_players <- data_players %>%
  filter(!is.na(wide_t_ratio) & !is.na(snv_ratio) & !is.na(avg_rally_count) & !is.na(winner_base_net_ratio))

# filter only valid players
valid_players <- players %>%
  inner_join(data_players, by = "player") %>%
  select(player)

############### Modeling 1 ###################
# K-NN Clusturing
agr_baseliner <- c("Kei Nishikori", "Alexander Zverev", "Rafael Nadal", "Novak Djokovic")
snvolleyer <- c("Patrick Rafter", "Pete Sampras", "Brian Teacher", "Kim Warwick", "Chris Lewis", "Andrea Vavassori", "Steve Denton")
cnt_pnchr <- c("Michael Chang", "Lleyton Hewitt", "Andy Murray", "Daniil Medvedev")
all_rounder <- c("Roger Federer", "Grigor Dimitrov", "Stefanos Tsitsipas", "Grigor Dimitrov")

data_players$playing_type <- if_else(data_players$player %in% agr_baseliner, "aggressive baseliner", data_players$playing_type)
data_players$playing_type <- if_else(data_players$player %in% snvolleyer, "serve & volleyer", data_players$playing_type)
data_players$playing_type <- if_else(data_players$player %in% cnt_pnchr, "counter puncher", data_players$playing_type)
data_players$playing_type <- if_else(data_players$player %in% all_rounder, "all-round player", data_players$playing_type)

train_set <- data_players %>%
  filter(playing_type != "")

predict_set <- data_players %>%
  filter(playing_type == "")

# normalization function
nor <- function(x) { (x - min(x)) / (max(x) - min(x)) }

# run nomalization on dataset
# because they are the predictors
data_players_norm <- as.data.frame(lapply(data_players[,c(2,3,4,5)], nor))
data_players_norm <- cbind(player = data_players$player, data_players_norm, playing_type = data_players$playing_type)

train_set_norm <- data_players_norm %>%
  filter(playing_type != "")

predict_set_norm <- data_players_norm %>%
  filter(playing_type == "")

set.seed(400)
pr <- knn(train_set_norm[,c(2:5)], predict_set_norm[,c(2:5)], cl = train_set[,6], k = 10)
predict_set <- cbind(predict_set[,c(1:5)], playing_type = pr)

clustered_data_players <- rbind(train_set, predict_set)
clustered_data_players$playing_type <- factor(clustered_data_players$playing_type)

data_players <- clustered_data_players

# add winner value to data_matches
matches <- data_matches %>%
  mutate(winner = sample(NA, nrow(data_matches), replace = TRUE))

for (i in c(1:nrow(matches))) {
  match <- matches[i,2]
  
  pts <- data_points %>%
    filter(match_id == match)
  
  matches[i, 9] <- pts[nrow(pts),]$winner
}

############################################################################################################

getNetRatio <- function(mid, p) {
  total <- data_shots %>%
    filter(match_id == mid & player == p & type == "Total")
  
  net <- data_shots %>%
    filter(match_id == mid & player == p & type == "Net")
  
  r <- net[1,]$shots / total[1,]$shots * 100
  
  if(identical(r, numeric(0))) {
    return(NA)
  }
  
  return(r)
}

getWinnerShotRatio <- function(mid, p) {
  total <- data_shots %>%
    filter(match_id == mid & player == p & type == "Total")
  
  r <- total[1,]$winner / total[1,]$shots * 100
  
  if(identical(r, numeric(0))) {
    return(NA)
  }
  
  return(r)
}

get1stSrvSuccessRatio <- function(mid, p) {
  pts <- data_points %>%
    filter(match_id == mid & svr == p)
  
  return(sum(if_else(pts$x2nd == "", 1, 0)) / nrow(pts) * 100)
}


getLlmPlot <- function(p1) {
  p1_type <- data_players %>%
    filter(player == p1) %>%
    select(playing_type)
  p1_type <- p1_type$playing_type
  
  # aggressive baseliner
  if(p1_type == "aggressive baseliner") {
    AB <- data_players %>%
      filter(player == p1) %>%
      select(player)
    
    # AG = player1
    matches_AB1 <- matches %>%
      filter(player1 %in% AB$player) %>%
      select(match_id, player1, player2, winner)
    
    matches_AB1 <- matches_AB1 %>%
      mutate(
        is_AB_winner = if_else(winner == 1, 1, 0),
      )
    matches_AB1$net_ratio <- NA
    
    for (i in c(1:nrow(matches_AB1))) {
      matches_AB1[i,6] <- (getNetRatio(matches_AB1[i, ]$match_id, 1))
    }
    
    # AG = player2
    matches_AB2 <- matches %>%
      filter(player2 %in% AB$player) %>%
      select(match_id, player1, player2, winner)
    
    matches_AB2 <- matches_AB2 %>%
      mutate(
        is_AB_winner = if_else(winner == 2, 1, 0),
      )
    matches_AB2$net_ratio <- NA
    
    for (i in c(1:nrow(matches_AB2))) {
      matches_AB2[i,6] <- (getNetRatio(matches_AB2[i, ]$match_id, 2))
    }
    
    # total
    matches_AB <- rbind(matches_AB1, matches_AB2) %>%
      filter(!is.na(net_ratio))
    
    model <- glm(is_AB_winner ~ net_ratio, family = "binomial", data = matches_AB)
    
    llmPlot <- matches_AB %>%
      ggplot(aes(net_ratio, is_AB_winner)) +
      geom_point(alpha = .15) +
      scale_x_reverse() +
      geom_smooth(method = "glm", method.args = list(family = "binomial")) +
      ggtitle("Aggressive Baseliner Logistic Regression Model") +
      xlab("Net shot ratio") +
      ylab("Probability of Winning")
  }
  
  # counter puncher
  if(p1_type == "counter puncher") {
    CP <- data_players %>%
      filter(player == p1) %>%
      select(player)
    
    # CP = player1
    matches_CP1 <- matches %>%
      filter(player1 %in% CP$player) %>%
      select(match_id, player1, player2, winner)
    
    matches_CP1 <- matches_CP1 %>%
      mutate(
        is_CP_winner = if_else(winner == 1, 1, 0),
      )
    matches_CP1$winner_shot_ratio <- NA
    
    for (i in c(1:nrow(matches_CP1))) {
      matches_CP1[i,6] <- (getWinnerShotRatio(matches_CP1[i, ]$match_id, 2))
    }
    
    # CP = player2
    matches_CP2 <- matches %>%
      filter(player2 %in% CP$player) %>%
      select(match_id, player1, player2, winner)
    
    matches_CP2 <- matches_CP2 %>%
      mutate(
        is_CP_winner = if_else(winner == 1, 1, 0),
      )
    matches_CP2$winner_shot_ratio <- NA
    
    for (i in c(1:nrow(matches_CP2))) {
      matches_CP2[i,6] <- (getWinnerShotRatio(matches_CP2[i, ]$match_id, 1))
    }
    
    # total
    matches_CP <- rbind(matches_CP1, matches_CP2) %>%
      filter(!is.na(winner_shot_ratio))
    
    model <- glm(is_CP_winner ~ winner_shot_ratio, family = "binomial", data = matches_CP)
    
    llmPlot <- matches_CP %>%
      ggplot(aes(winner_shot_ratio, is_CP_winner)) +
      geom_point(alpha = .15) +
      geom_smooth(method = "glm", method.args = list(family = "binomial")) +
      ggtitle("Counter Puncher Logistic Regression Model") +
      xlab("Opponent's winner shot ratio") +
      ylab("Probability of Winning")
  }
  
  # serve & volleyer
  if(p1_type == "serve & volleyer") {
    SV <- data_players %>%
      filter(player == p1) %>%
      select(player)
    
    # SV = player1
    matches_SV1 <- matches %>%
      filter(player1 %in% SV$player) %>%
      select(match_id, player1, player2, winner)
    
    matches_SV1 <- matches_SV1 %>%
      mutate(
        is_SV_winner = if_else(winner == 1, 1, 0),
      )
    matches_SV1$fst_srv_suc_ratio <- NA
    
    for (i in c(1:nrow(matches_SV1))) {
      matches_SV1[i,6] <- (get1stSrvSuccessRatio(matches_SV1[i, ]$match_id, 2))
    }
    
    # SV = player2
    matches_SV2 <- matches %>%
      filter(player2 %in% SV$player) %>%
      select(match_id, player1, player2, winner)
    
    matches_SV2 <- matches_SV2 %>%
      mutate(
        is_SV_winner = if_else(winner == 1, 1, 0),
      )
    matches_SV2$fst_srv_suc_ratio <- NA
    
    for (i in c(1:nrow(matches_SV2))) {
      matches_SV2[i,6] <- (get1stSrvSuccessRatio(matches_SV2[i, ]$match_id, 1))
    }
    
    # total
    matches_SV <- rbind(matches_SV1, matches_SV2)
    model <- glm(is_SV_winner ~ fst_srv_suc_ratio, family = "binomial", data = matches_SV)
    
    llmPlot <- matches_SV %>%
      ggplot(aes(fst_srv_suc_ratio, is_SV_winner)) +
      geom_point(alpha = .15) +
      scale_x_reverse() +
      geom_smooth(method = "glm", method.args = list(family = "binomial")) +
      ggtitle("Serve & Volleyer Logistic Regression Model") +
      xlab("Opponent's 1st serve success ratio") +
      ylab("Probability of Winning")
  }
  
  if(p1_type == "all-round player") {
    llmPlot <- ggplot() + ggtitle("Sorry! All-round players are not included in this analysis")
  }
  
  return(llmPlot)
}


############################################################################################################
############################################################################################################
############################################################################################################

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  ################## Panel 1 #####################
  titlePanel("Tennis Player Clustering"),
  
  sidebarLayout(
    sidebarPanel(
      p("In this project players were categorized into 4 types based on 4 criteria."),
      p("Select one criteria that you want and see what the characteristics are based on the playing type."),        
      
      selectInput(
        inputId = "clustering_criteria",
        label = "Select One Criteria:",
        choices = colnames(clustered_data_players)[2:5],
        selected = "wide_t_ratio"
      ),
      
      checkboxGroupInput(
        inputId = "clustering_type",
        label = "Check types you want to see",
        choices = c("aggressive baseliner", "counter puncher", "serve & volleyer", "all-round player"),
        selected = c("aggressive baseliner", "counter puncher", "serve & volleyer", "all-round player"),
      )
    ),
    
    mainPanel(
      plotOutput("clusterPlot")
    ),
  ),
  
  ################### Panel 2 ###################
  titlePanel("Match Analysis of Each Player"),
  
  sidebarLayout(
    sidebarPanel(
      p("When looking at all the players' data comprehensively, some conventional winning strategies did not fit well with the data."),
      p("Then what about for each player? Choose one player you want and see if the conventional strategy fits for each player well or not."),
      p("It would be interesting to check the analysis results of the most representative players of each type."),
      p("- Aggressive Baseliner: Rafael Nadal, Novak Djokovic, Kei Nishikori, Alexander Zverev"),
      p("- Counter Puncher: Andy Murray, Michael Chang, Lleyton Hewitt"),
      p("- Serve & Volleyer: Patrick Rafter, Pete Sampras"),
      
      selectInput(
        inputId = "player",
        label = "Select Player:",
        choices = valid_players,
        selected = "Andy Murray"
      ),
    ),
    mainPanel(
      plotOutput("llmPlot")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$clusterPlot <- renderPlot({
    plot_data <- data_players %>%
      filter(playing_type %in% input$clustering_type)
    
    plot(plot_data[input$clustering_criteria][,1],
         col = plot_data$playing_type,
         pch = 16,
         main = "K-NN Clustering",
         ylab = input$clustering_criteria)
    
    legend("bottomright",
           legend = c("aggressive baseliner", "counter puncher", "serve & volleyer", "all-round player"),
           col= c(1, 3, 4, 2),
           pch=16)
  })
  
  output$llmPlot <- renderPlot({
    getLlmPlot(input$player)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

