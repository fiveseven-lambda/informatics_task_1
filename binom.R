k <- 55

if(k <= 19){
	n <- 200 - k
}else{
	n <- k
}

p <- c(.01, .1, .3, .5, .9)
f <- c(dbinom, pbinom)
text <- c("out_A.png", "二項分布 確率関数", "成功回数", "確率", "out_B.png", "二項分布 累積分布関数", "n", "成功回数がn以下となる確率")
dim(text) <- c(4, 2)
for(i in 1:2){
	png(text[1, i])
	for(j in 1:length(p)){
		plot(0:n, f[[i]](0:n, n, p[j]), ylim = 0:1, type = "l", col = j, xlab="", ylab="")
		par(new=T)
	}
	title(text[2, i], xlab = text[3, i], ylab = text[4, i])
}
p <- .3
png("out_C.png")
hist(rbinom(1000, n, p), main = sprintf("確率%.1fで成功する試行を%d回繰り返したときの成功回数", p, n), xlab = "成功回数", ylab = "frequency")
