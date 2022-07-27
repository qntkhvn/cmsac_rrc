#Sport climbing scoring script
#https://en.wikipedia.org/wiki/Rank_product
#https://febs.onlinelibrary.wiley.com/doi/pdf/10.1002/1873-3468.12194
#https://www.sciencedirect.com/science/article/pii/S0014579313000689
#https://www.sciencedirect.com/science/article/pii/S0014579304009354
#https://math.stackexchange.com/questions/1099586/prove-that-the-product-of-three-numbers-is-the-greatest-if-the-numbers-are-equal
library(combinat)
#n = 3
ranks <- combinat::permn(1:5)

scores <- list()
q <- 0
index <- list()
for (i in 1:length(ranks)){print(i)
  for (j in 1:length(ranks)){
    for (k in 1:length(ranks)){
      q <- q + 1
      scores[[q]] <- apply(rbind(ranks[[i]],ranks[[j]],ranks[[k]]), 2, prod)
      index[[q]] <- c(i,j,k)
    }
  }
}
inds[1:10]
m <- 1863
rbind(ranks[[index[[m]][1]]],ranks[[index[[m]][2]]],ranks[[index[[m]][3]]])
apply(rbind(ranks[[index[[m]][1]]],ranks[[index[[m]][2]]],ranks[[index[[m]][3]]]), 2, prod)

apply(rbind(ranks[[index[[m]][1]]],ranks[[index[[m]][2]]],ranks[[index[[m]][3]]]), 2, sum)


table(unlist(lapply(scores, min)))

inds <- which(unlist(lapply(scores, min)) == 15)

for (g in inds[1:10]){
print(scores[[inds[g]]])
}






#Does the sum lead to the same winner as the product?  
ranks <- combinat::permn(1:4)
scores_prod <- list()
scores_sum <- list()
same <- list()
same1 <- list()
q <- 0
index <- list()
for (i in 1:length(ranks)){print(i)
  for (j in 1:length(ranks)){
    for (k in 1:length(ranks)){
      q <- q + 1
      scores_prod[[q]] <- rank(apply(rbind(ranks[[i]],ranks[[j]],ranks[[k]]), 2, prod))
      scores_sum[[q]] <- rank(apply(rbind(ranks[[i]],ranks[[j]],ranks[[k]]), 2, sum))
      same[[q]] <- all(scores_prod[[q]] == scores_sum[[q]])
      same1[[q]] <- (which.min(scores_prod[[q]]) == which.min(scores_sum[[q]]))+0
      index[[q]] <- c(i,j,k)
    }
  }
}

m <- which(same == 0)[1000]
apply(rbind(ranks[[index[[m]][1]]],ranks[[index[[m]][2]]],ranks[[index[[m]][3]]]), 2, prod)
apply(rbind(ranks[[index[[m]][1]]],ranks[[index[[m]][2]]],ranks[[index[[m]][3]]]), 2, sum)
rbind(ranks[[index[[m]][1]]],ranks[[index[[m]][2]]],ranks[[index[[m]][3]]])

mean(unlist(same1))
mean(unlist(same))


#Now simulate with 20. 
#What do you need to do to advance?
ranks_sim <- list()
scores_sim <- list()
set.seed(1234)
for (i in 1:10000){print(i)
a <- sample(c(1:20), 20)
b <- sample(c(1:20), 20)
c <- sample(c(1:20), 20)
ranks_sim[[i]] <- rbind(a,b,c)
scores_sim[[i]] <- apply(rbind(a,b,c),2,prod)

}

#min to advance 
min_adv <- max_adv <- list()
for (m in 1:10000){print(m)
ranks_sim[[m]]
scores_sim[[m]]
which(rank(scores_sim[[m]]) == 8)
adv <- rbind(ranks_sim[[m]][,which(rank(scores_sim[[m]],ties.method = "first") == 1)],
      ranks_sim[[m]][,which(rank(scores_sim[[m]],ties.method = "first") == 2)],
      ranks_sim[[m]][,which(rank(scores_sim[[m]],ties.method = "first") == 3)],
      ranks_sim[[m]][,which(rank(scores_sim[[m]],ties.method = "first") == 4)],
      ranks_sim[[m]][,which(rank(scores_sim[[m]],ties.method = "first") == 5)],
      ranks_sim[[m]][,which(rank(scores_sim[[m]],ties.method = "first") == 6)],
      ranks_sim[[m]][,which(rank(scores_sim[[m]],ties.method = "first") == 7)],
      ranks_sim[[m]][,which(rank(scores_sim[[m]],ties.method = "first") == 8)])
min_adv[[m]] <- apply(adv, 1, min)
max_adv[[m]] <- apply(adv, 1, max)
}


#min
apply(do.call(rbind,min_adv),2,mean)
apply(do.call(rbind,min_adv),2,median)

#max
apply(do.call(rbind,max_adv),2,mean)
apply(do.call(rbind,max_adv),2,median)


ranks_sim[[48]]
scores_sim[[48]]


#If you win ANY even what's the chance of advancing?  





#If you win any event in the finals, what is your chance of a medal?
ranks8_sim <- list()
scores8_sim <- list()
set.seed(1234)
for (i in 1:10000){print(i)
  a <- sample(c(1:8), 8)
  b <- sample(c(1:8), 8)
  c <- sample(c(1:8), 8)
  ranks8_sim[[i]] <- rbind(a,b,c)
  scores8_sim[[i]] <- apply(rbind(a,b,c),2,prod)
}

medal <- list()
for (i in 1:10000){
xxx <- which(apply(ranks8_sim[[i]], 2, function(x){any(x == 1)}))
medal[[i]] <- rank(scores8_sim[[i]])[xxx]
}

scores8_sim[[958]]
ranks8_sim[[958]]

