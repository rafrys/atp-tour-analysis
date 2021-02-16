setwd('C:/Users/salac/Desktop/Advanced Visualisation in R/Project')

#### libraries ####

library(magrittr)
library(tidyverse)

#### import the data ####

## data - the main data set for the ATP matches (retrieved on January 1 2021, 21:05 CET)
## As for January 1, 2021, the data on the reposotory was provided through December 28, 2020,
## so it encompassed the whole season 2020.

data <- read_csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_1968.csv')

for (i in 1969:2020) {
  data <- rbind(data,
                read_csv(paste('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_',
                               i, '.csv', sep = '')))
}


## rankings - the data set for the players' rankings (retrieved 3.01.2020 at 21:40 CET)

rankings <- read_csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_rankings_70s.csv')
rankings_80s <- read_csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_rankings_80s.csv')
rankings_90s <- read_csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_rankings_90s.csv')
rankings_00s <- read_csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_rankings_00s.csv')
rankings_10s <- read_csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_rankings_10s.csv')
rankings_20s <- read_csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_rankings_current.csv',
                         col_names = c("ranking_date","rank", "player","points"))

rankings <- rbind(rankings, rankings_80s, rankings_90s, rankings_00s, rankings_10s, rankings_20s)
rm(rankings_80s, rankings_90s, rankings_00s, rankings_10s, rankings_20s)


## players - needed mostly for exacting the players' date of birth (retrieved 3.01.2020 at 21:54 CET)

players <- read_csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_players.csv',
                    col_names = c("player_id", "first_name", "last_name", "hand", "birth_date", "country_code"))


#### data preprocessing ####

## We will need to change the date format (in all of the data frames). 
## We will also unify the naming conventions, making `player_id` stand for player ID, and `player` 
## for his name and surname.

players %<>% 
  mutate(birth_date = as.Date(as.character(birth_date), '%Y%m%d')) %>%
  unite(player, c("first_name", "last_name"), sep = " ")

## In the players datset there are many minor players, who 

## In the rankings data frame we will restrict it to only top 100 players for each ranking week, 
## which helps significantly (~15x) reduce the size of the object.
## We will also change date format and naming convention. 
## It will be convenient to add players names (from the players df) also in the rankings data frame.

rankings %<>% 
  filter(rank < 101) %>% 
  mutate(ranking_date = as.Date(as.character(ranking_date), '%Y%m%d')) %>% 
  arrange(ranking_date, rank) %>% 
  rename(player_id = player) %>% 
  left_join(players %>%  select(player_id, player), by = "player_id")

## We can observe that the rankings were provided only since 1973-08-27, and the points not until
## 1990-01-01.


## In the data df, we will transfom the names in the `tourney_level` column for better 
## informativeness. Additionally, we will add exact date of birth for both winner and loser,
## and change date format as above.

data %<>% 
  mutate(tourney_level = recode(tourney_level, 'G' = 'Grand Slam',
                                               'M' = 'Masters 1000',
                                               'A' = 'other tour-level event',
                                               'C' = 'Challenger',
                                               'S' = 'Satellite/ITF event',
                                               'F' = 'Tour finals and other season-ending events',
                                               'D' = 'Davis Cup'),
         tourney_date = as.Date(as.character(tourney_date), '%Y%m%d')) %>% 
  left_join(players[, c("player_id", "birth_date")], by = c("winner_id" = "player_id")) %>%
  rename(winner_birth = birth_date) %>%
  left_join(players[, c("player_id", "birth_date")], by = c("loser_id" = "player_id")) %>%
  rename(loser_birth = birth_date)

## Let us control the missings in the data
colSums(is.na(data))
colSums(is.na(rankings))

## We can also see that we have a lot of missing data for the old matches. E.g. `minutes` (match
## duration) is not given for metches before 1990-12-31, same for `w_bpSaved` (number od break points
## saved by a winner), `w_bpFaced` (no. bp faced by winner), `l_ace` (no. of loser's aces), etc.
 

## Before we proceed futher, we will review our players df. Let us check for how many players we
## have data.

length(unique(c(data$winner_name, data$loser_name)))  ## 6405
length(unique(c(data$winner_id, data$loser_id)))  ## 6433

## We have 28 cases in which 1 name is linked to (at least) 2 ids. The most probable hypothesis 
## is that these are different players bearing the same name. Let us check it.

winners <- data %>% 
  select(winner_name, winner_id) %>% 
  rename(player = winner_name, player_id = winner_id)

losers <- data %>% 
  select(loser_name, loser_id) %>% 
  rename(player = loser_name, player_id = loser_id)

players_all <- bind_rows(winners, losers) %>% 
  distinct(player_id, .keep_all = T) %>% 
  arrange(player_id) %>% 
  as.data.frame()

players_all %>% 
  group_by(player) %>% 
  filter(n()>1) %>% 
  print(n = Inf)

## We can clearly see that we indeed have 28 cases of players bearing the same names. 
## As we can see only one pair of these players entered top 100, because in the rankings df
## the difference between the number of unique names and the number of unique ids is 1:

length(unique(rankings$player)) ## 1103
length(unique(rankings$player_id)) ## 1104

# Which name is linked to 2 ids:
rankings %>% 
  distinct(player_id, .keep_all = T) %>% 
  group_by(player) %>% 
  filter(n()>1)  ## John Austin 100492 i 130938

# Let us check John Austin in players df:
players %>% 
  filter(player == "John Austin")

# Indeed, we have 2 John Austins that entered into top 100: player_id==100492 and player_id==130938.
# They have even different nationality, so they clearly are not the same person.

## Let us also check, whether there are players in the rankings df that are not in the
## data df.

rankings %>% 
  anti_join(players_all, by = "player_id")  ## Olivier Cayla (107888), Rassel Mayers (108050), 
                                            ## Danny Granot (101201)

## Interestingly, we have found 3 players that featured in the top 100 in 1976-1978 but there are no
## matches recorded in which they played. It is impossible that they still featured there back then 
## despite ending carrers before 1968. All 3 can be found in the players df.
## It can be checked that they feature only once in the (top 100) rankings df - there is no record
## of them reaching these positions. Moreover, it can be checked tht they feature in the
## non-decreased (full) rankings df at the higher (worse) ranking positions multiple times, so
## their presence in the top 100 is a typo.
## We also manually checked at the www.atptour.com site for these players - they are in the database, there
## is no record of their matches, but their career highest rankings not top 100: Danny Granot - 160,
## Olivier Cayla - 393, Rassel Mayers - 793.

## We will therefore drop them from the rankings df:
rankings %<>%
  filter(!(player %in% c("Olivier Cayla", "Rassel Mayers", "Danny Granot")))

length(unique(rankings$player_id))  ## 1101

## In fact we are not interested in all players in the players df, because there are much more of
## them there (54979 unique ids) then the players that feature in our data df (6433), not to mention
## those, whoe ever entered top 100 (1101). Therefore we may decrease our players df and keep only
## those player, for whom we have any record in the data df.

players %<>% 
  filter(player_id %in% players_all$player_id)

## Just for a checkup, let us see if we have full information on our players
colSums(is.na(players))

## We have full information on names and ids, minor gap in country_code and hand and significant one
## in birth_date.

## These can be removed:
rm(winners, losers, players_all)

## As we worked with the data, we found an important mistake - John McEnroe (id 100581) is named
## "John Mcenroe" in the rankings data set, and "John McEnroe" in the data (data set for matches).
## He is an important player, who reached top 1 at a time, so this should be fixed. Possibly, the issue
## will be resolved in the source data set in the future, as we plan to report it to the database
## maintainer.
rankings %<>% 
  mutate(player = if_else(player=="John Mcenroe", "John McEnroe", player))

#### data extra ####

# data_extra will extend data by additional features.

## data_extra: 
## Legend:
# - w_SetsWon/l_SetsWon - number of sets won in match
# - w_GmsWon/l_GmsWon - number of games won in match
# - w_SvGmsLost/l_SvGmsLost - number of service games lost in match
# - w_SvGmsWon/l_SvGmsWon - number of service games won in match
# (NOTE: when using 'SvGmsWon', filter our positive values. There may be a few that are 
# wrong - negative - probably becaue w_SvGms/l_SvGms was wrong).
# - w_tbWon/l_tbWon - number of tie-breaks won in match
# - w_set1/l_set1 - winner's/loser's score in the 1st set (same for sets 2-5)
# - tbSet1 - loser's score in the 1st set tie-break (winner's score: max(7,tbSet1+2) (same for sets 2-5)
# - w_tbSet1Won/l_tbSet1Won - whether winner/loser won 1st set tie-break (0/1) (same for sets 2-5)

data %>%
  # some scores (in particular old 3rd sets, are written in '[...]' - we will get rid of it)
  mutate(score = gsub('\\[', '', gsub('\\]', '', score))) %>% 
  # we will spilit the scores into sets. In a few cases there will be text information that
  # the matches were finished early, that will be partly drop in the process. We do not care
  # about it, as in this cases winners and losers are NAs, so these records would be dropped 
  # in subsetting.
  separate(score, into = c("set1", "set2", "set3", "set4", "set5"), sep = " ", fill = "right",
           extra = "drop",  remove = F) %>%
  # We leave only numeric information (info such as "RET", "Unfinished" etc. will be dropped)
  mutate(set1 = gsub('^[a-zA-Z]', NA, set1),
         set2 = gsub('^[a-zA-Z]', NA, set2),
         set3 = gsub('^[a-zA-Z]', NA, set3),
         set4 = gsub('^[a-zA-Z]', NA, set4),
         set5 = gsub('^[a-zA-Z]', NA, set5)) %>% 
  # We split sets scores into winner and loser
  separate(set1, into = c("w_set1", "l_set1"), sep = '-', fill = 'right') %>% 
  separate(set2, into = c("w_set2", "l_set2"), sep = '-', fill = 'right') %>% 
  separate(set3, into = c("w_set3", "l_set3"), sep = '-', fill = 'right') %>% 
  separate(set4, into = c("w_set4", "l_set4"), sep = '-', fill = 'right') %>% 
  separate(set5, into = c("w_set5", "l_set5"), sep = '-', fill = 'right') %>%
  # We need to clear some textual information on the loser side
  mutate(l_set1 = gsub('^[a-zA-Z]', NA, l_set1)) %>% 
  # We need to retrieve information on the tie-breaks score from the brackets
  separate(l_set1, into = c("l_set1", "tbSet1"), sep = '\\(', fill = 'right') %>% 
  separate(l_set2, into = c("l_set2", "tbSet2"), sep = '\\(', fill = 'right') %>% 
  separate(l_set3, into = c("l_set3", "tbSet3"), sep = '\\(', fill = 'right') %>% 
  separate(l_set4, into = c("l_set4", "tbSet4"), sep = '\\(', fill = 'right') %>% 
  separate(l_set5, into = c("l_set5", "tbSet5"), sep = '\\(', fill = 'right') %>% 
  # Clearing the brackets in tie-breaks scores
  mutate(tbSet1 = gsub(')', '', tbSet1),
         tbSet2 = gsub(')', '', tbSet2),
         tbSet3 = gsub(')', '', tbSet3),
         tbSet4 = gsub(')', '', tbSet4),
         tbSet5 = gsub(')', '', tbSet5)) %>% 
  # Turn the cleaned values into numeric
  mutate(w_set1 = as.numeric(w_set1), w_set2 = as.numeric(w_set2), 
         w_set3 = as.numeric(w_set3), w_set4 = as.numeric(w_set4), 
         w_set5 = as.numeric(w_set5), l_set1 = as.numeric(l_set1), 
         l_set2 = as.numeric(l_set2), l_set3 = as.numeric(l_set3), 
         l_set4 = as.numeric(l_set4), l_set5 = as.numeric(l_set5),
         tbSet1 = as.numeric(tbSet1), tbSet2 = as.numeric(tbSet2),
         tbSet3 = as.numeric(tbSet3), tbSet4 = as.numeric(tbSet4),
         tbSet5 = as.numeric(tbSet5)) %>% 
  # Add number of games won
  rowwise() %>% 
  mutate(w_GmsWon = sum(w_set1, w_set2, w_set3, w_set4, w_set5, na.rm = T),
         l_GmsWon = sum(l_set1, l_set2, l_set3, l_set4, l_set5, na.rm = T)) %>% 
  # For each set and player add whether a player won the tie-break
  mutate(w_tbSet1Won = if_else(!is.na(tbSet1), if_else(w_set1>l_set1, 1, 0), 0),
         l_tbSet1Won = if_else(!is.na(tbSet1), if_else(w_set1<l_set1, 1, 0), 0),
         w_tbSet2Won = if_else(!is.na(tbSet2), if_else(w_set2>l_set2, 1, 0), 0),
         l_tbSet2Won = if_else(!is.na(tbSet2), if_else(w_set2<l_set2, 1, 0), 0),
         w_tbSet3Won = if_else(!is.na(tbSet3), if_else(w_set3>l_set3, 1, 0), 0),
         l_tbSet3Won = if_else(!is.na(tbSet3), if_else(w_set3<l_set3, 1, 0), 0),
         w_tbSet4Won = if_else(!is.na(tbSet4), if_else(w_set4>l_set4, 1, 0), 0),
         l_tbSet4Won = if_else(!is.na(tbSet4), if_else(w_set4<l_set4, 1, 0), 0),
         w_tbSet5Won = if_else(!is.na(tbSet5), if_else(w_set5>l_set5, 1, 0), 0),
         l_tbSet5Won = if_else(!is.na(tbSet5), if_else(w_set5<l_set5, 1, 0), 0)) %>% 
  # Add number of tie-brekas won in match
  mutate(w_tbWon = sum(w_tbSet1Won, w_tbSet2Won, w_tbSet3Won, w_tbSet4Won, w_tbSet5Won),
         l_tbWon = sum(l_tbSet1Won, l_tbSet2Won, l_tbSet3Won, l_tbSet4Won, l_tbSet5Won)) %>% 
  # Add number of sets won in match
  mutate(w_SetsWon = sum(w_set1>l_set1,w_set2>l_set2,w_set3>l_set3,w_set4>l_set4,w_set5>l_set5,
                         na.rm = T),
         l_SetsWon = sum(w_set1<l_set1,w_set2<l_set2,w_set3<l_set3,w_set4<l_set4,w_set5<l_set5,
                         na.rm = T)) %>% 
  # Add number of lost service games in match
  mutate(w_SvGmsLost =  w_bpFaced-w_bpSaved,
         l_SvGmsLost =  l_bpFaced-l_bpSaved) %>% 
  # Add number of won service games in match
  mutate(w_SvGmsWon = w_SvGms - w_SvGmsLost,
         l_SvGmsWon = l_SvGms - l_SvGmsLost) -> data_extra


#### save the data ####

# saveRDS(data, file = "../Data/data_atp.rds")
# saveRDS(data_extra, file = "../Data/data_extra.rds")
# saveRDS(players, file = "../Data/players.rds")  
# saveRDS(rankings, file = "../Data/rankings.rds") 


#### load the data ####

# data <- readRDS("../Data/data_atp.rds") 
# data_extra <- readRDS("../Data/data_extra.rds")
# players <- readRDS("../Data/players.rds") 
# rankings <- readRDS("../Data/rankings.rds") 