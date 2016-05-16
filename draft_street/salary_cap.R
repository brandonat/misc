# Create team for salary cap draft

# Data combines prices of players and average ranking from fantasypros.com
nfl <- read.csv("nfl_wk2.csv", stringsAsFactors=FALSE)

# Select random player by position  
random_player <- function(players, slot) {
  sub <- subset(players, Position == slot)
  rnum <- round(runif(n=1,min=1,max=nrow(sub)), digits=0)
  rplayer  <- sub$Name[rnum]
  rplayer
}

# Build Team
build_team <- function(nfl) {
  players <- nfl
  #QB
  qb1 <- random_player(players, "QB")
  players <- subset(players, Name != qb1)
  qb2 <- random_player(players, "QB") 
  #RB
  rb1 <- random_player(players, "RB")
  players <- subset(players, Name != rb1)
  rb2 <- random_player(players, "RB")
  players <- subset(players, Name != rb2)
  #WR
  wr1 <- random_player(players, "WR")
  players <- subset(players, Name != wr1)
  wr2 <- random_player(players, "WR")
  players <- subset(players, Name != wr2)
  #TE
  te1 <- random_player(players, "TE")
  players <- subset(players, Name != te1)
  #FLEX (no TE's)
  frb <- random_player(players, "RB")
  fwr <- random_player(players, "WR")
  fx1 <- c(frb,fwr)[ runif(n=1,min=1,max=3) ]
  players <- subset(players, Name != fx1)
  frb <- random_player(players, "RB")
  fwr <- random_player(players, "WR")
  fx2 <- c(frb,fwr)[ runif(n=1,min=1,max=3) ]
  #DST
  dst <- random_player(players, "DST")
  c(qb1, qb2, rb1, rb2, wr1, wr2, te1, fx1, fx2, dst)
}

# Generate list of 100 samples of teams
samples <- list()
nfl2 <- subset(nfl, Rank <= 25)
for ( index in 1:5000 ) {
  
  team <- build_team(nfl2)
  team_dets <- subset(nfl, Name %in% team)
  this_run <- list( id = index,
                    team = team, 
                    salary = sum(team_dets$Salary), 
                    rank = mean(team_dets$Rank), 
                    value = mean(team_dets$Value) )
  samples <- append(samples, list(this_run) )
}

# Analyze different teams combinations
results <- do.call(rbind.data.frame, lapply(samples, '[', c('id', 'salary','rank','value') ) )

# Top ten teams by rank
results  <- results[ with(results, order(rank, -salary)), ]
ten_rank <- subset(results, salary <= 100000)[1:10, ]
rownames(ten_rank) <- NULL

# Top ten teams by sum of individuals' values
results   <- results[ with(results, order(-value, -salary)), ]
ten_value <- subset(results, salary <= 100000)[1:10,]
rownames(ten_value) <- NULL

# View Teams Selected by Rank & Value
samples[[ ten_rank[1,1]  ]]
nfl[ nfl$Name %in% samples[[ ten_rank[1,1]  ]]$team, ]
samples[[ ten_value[1,1] ]]
nfl[ nfl$Name %in% samples[[ ten_value[1,1] ]]$team, ]

# Plot graph
plot(x=results$rank, y=results$salary)
abline(h=100000)

# Variance measures
x <- nfl[ with(nfl, order(Rank, -Salary)), ]
n <- 10
qb_var  <- mean( subset(x, Position=="QB")[1:n, "Stdev"] )
rb_var  <- mean( subset(x, Position=="RB")[1:n, "Stdev"] )
wr_var  <- mean( subset(x, Position=="WR")[1:n, "Stdev"] )
te_var  <- mean( subset(x, Position=="TE")[1:n, "Stdev"] )
dst_var <- mean( subset(x, Position=="DST")[1:n, "Stdev"] )
c(qb_var, rb_var, wr_var, te_var, dst_var)

# Ordering of variance suggests importance weights for positions
# in the following order
#
# 1st Priority : QB
# 2nd Priority : TE
# 3rd Priority : RB
# 4th Priority : WR
# 5th Priority : DST
