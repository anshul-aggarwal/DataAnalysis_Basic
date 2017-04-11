#Manual oversampling increases the info gain

x1 = factor(c(1,2,3,2,2,1,2,3,1,1,1,1,3,3,2,1,2,3))
y1 = factor(c(1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

#Oversampling

x = factor(c(1,3,1,3,1,3,1,3,1,3,2,2,2,1,2,3,1,1,1,1,3,3,2,1,2,3))
y = factor(c(1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))


l = length(x)

info = 0
for (j in 1:length(levels(x))){
  ctr = 0
  for(i in 1:l){
    if (x[i] == as.integer(levels(x)[j])){
      ctr = ctr + 1
    }
  }
  rt = ctr/l
  if (rt > 0){
    info = info + (-rt*log10(rt))
  }
}

infoA = 0

for(k in 1:length(levels(y))){
  lctr = 0
  infol = 0
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
    print(rt)
    if (rt > 0){
      infol = infol + (-rt*log10(rt))
    }
  }
  infoA = infoA + lctr*infol/l
}

infoGain = info - infoA
