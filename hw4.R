### 1: simulate_monoploy function
simulate_monopoly<-function(n,d){
  # n = number of turns by a player.
  # d = number of sides of each dice.
  community_chest = c(2,17,33)
  chance = c(7,22,36)
  location = 0
  double_counts = 0
  current_position = 0
  turn = 0
  
  cc_cards = rep(0, 16)
  cc_cards[sample(16,2)] = c(1,2)
  cc_cards = rep(cc_cards, ceiling(n/16))
  
  ch_cards = c(0,10,11,24,39,5,-1,-1,-2,-3,rep(-99,6))
  ch_cards = sample(ch_cards)
  ch_cards = rep(ch_cards, ceiling(n/16))
  
  while (turn<n) {
    turn = turn+1
    faces = sample(1:d,2,replace = TRUE)
    if (faces[1] == faces[2]) {
      double_counts = double_counts + 1
    } else {
      double_counts = 0
    }
    
    if (double_counts == 3) {
      current_position = 10
      double_counts = 0
    }
    
    steps = sum(faces[1],faces[2])
    current_position = current_position+steps
    if (current_position>39) {
      current_position=current_position%%40
    }
    if (current_position == 30) {
      current_position = 10
    }
    if (current_position %in% community_chest) {
      card1<-cc_cards[turn]
      if(card1 == 1){current_position == 0}
      if(card1 == 2){current_position == 10}
    }
    if (current_position %in% chance) {
      
      if (ch_cards[turn] >= 0){
        current_position = ch_cards[turn]
      } else if (ch_cards[turn] == -1) {
        if (current_position == 7){
          current_position = 15
        } else if (current_position == 22){
          current_position = 25
        } else {
          current_position = 5
        }
      } else if (ch_cards[turn] == -2) {
        if (current_position == 22){
          current_position = 28
        } else {
          current_position = 12 
        }
      } else if (ch_cards[turn] == -3){
        current_position = current_position - 3
      }
      
    }
    
    location = c(location, current_position)
  }
  return(factor(location, 0:39))
}

### 2: estimate_monopoly function
estimate_monopoly = function (simulations) {
  return(prop.table(table(simulations)))
}

set.seed(95618)
all_probs = sapply(3:6, function(x) estimate_monopoly(simulate_monopoly(10000,x)))

plot(0:39, all_probs[,1], ylim=c(0, max(all_probs)),
     type='l', col="red", ylab="probability", main="Long-term probabilities of each square", xlab="square encoded 0 to 39")
lines(0:39, all_probs[,2], col="blue")
lines(0:39, all_probs[,3], col="green")
lines(0:39, all_probs[,4], col="black")
legend("topright", c("3-sided", "4-sided", "5-sided", "6-sided"), 
       col = c("red", "blue", "green", "black"), lty=1, bty='n')
abline(v=c(0:39), col="grey")

### finding the 3 most likely squares to end a turn
sort(all_probs[,4], decreasing = TRUE)[1:3]
sort(all_probs[,3], decreasing = TRUE)[1:3]
sort(all_probs[,2], decreasing = TRUE)[1:3]
sort(all_probs[,1], decreasing = TRUE)[1:3]

### 3
set.seed(94523)
jail_probs = sapply(1:1000, function(x) estimate_monopoly(simulate_monopoly(10000,6))[11])
sd(jail_probs) # 0.00215198

### 4a
set.seed(95218)
one_time = simulate_monopoly(10000, 6)
bootstrap_samples = lapply(1:1000, function(x) sample(one_time, replace=TRUE))
bootstrap_probs = unlist(lapply(bootstrap_samples, function(x) estimate_monopoly(x)[11]))
bootstrap_probs[1] # 0.05079492

### 4b
time_sim = function() {
  set.seed(94523)
  jail_probs = sapply(1:1000, function(x)
    estimate_monopoly(simulate_monopoly(10000, 6))[11])
  sd(jail_probs) # 0.00215198
}

time_boot = function() {
  set.seed(95218)
  one_time = simulate_monopoly(10000, 6)
  bootstrap_samples = lapply(1:1000, function(x)
    sample(one_time, replace = TRUE))
  bootstrap_probs = unlist(lapply(bootstrap_samples, function(x)
    estimate_monopoly(x)[11]))
}

system.time(time_sim())
system.time(time_boot())

#system.time(time_sim())
# user  system elapsed 
#247.137  58.945 306.221 
#system.time(time_boot())
#user  system elapsed 
#0.782   0.129   0.913

### 5
set.seed(94523)
squares_sd = list()
for (i in 3:6){
  squares_probs = lapply(1:1000, function(x) estimate_monopoly(simulate_monopoly(10000,i)))
  squares_sd[[i]] = apply(do.call("cbind", squares_probs), 1, sd)
}

all_sd = do.call("cbind", squares_sd)

plot(0:39, all_sd[,1], ylim=c(0, max(all_sd)),
     type='l', col="red", ylab="Standard error of long-term probabilies", main="Long-term standard error of probabilities of each square", xlab="square encoded 0 to 39")
lines(0:39, all_sd[,2], col="blue")
lines(0:39, all_sd[,3], col="green")
lines(0:39, all_sd[,4], col="black")
legend("topright", c("3-sided", "4-sided", "5-sided", "6-sided"), 
       col = c("red", "blue", "green", "black"), lty=1, bty='n')
abline(v=c(0:39), col="grey")

### 6 and 7 are explainations, no code.