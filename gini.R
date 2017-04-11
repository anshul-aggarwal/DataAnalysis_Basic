x = factor(c(1,2,3,2,2,1,2,3,1,1,1,1,3,3,2,1,2,3))
y = factor(c(1,0,1,0,1,1,1,1,0,0,0,1,0,1,1,0,0,1))

l = length(x)

gini = 1
for (j in 1:length(levels(x))){
  ctr = 0
  for(i in 1:l){
    if (x[i] == as.integer(levels(x)[j])){
      ctr = ctr + 1
    }
  }
  rt = ctr/l
  gini = gini - rt^2
}

giniA = 0

for(k in 1:length(levels(y))){
  lctr = 0
  ginil = 1
  for(m in 1:l){
    if (y[m] == as.integer(levels(y)[k])){
      lctr = lctr + 1
    }
  }
  for (j in 1:length(levels(x))){
    ctr = 0
    for(i in 1:l){
      if ((x[i] == as.integer(levels(x)[j])) & (y[i] == as.integer(levels(y)[k]))){
        ctr = ctr + 1
      }
    }
    rt = ctr/lctr
    ginil = ginil - rt^2
  }
  giniA = giniA + lctr*ginil/l
}

giniG = gini - giniA
