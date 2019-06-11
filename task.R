k <- 55

if(k <= 19){
	n <- 200 - k
}else{
	n <- k
}

p <- c(.01, .1, .3, .5, .9)
f <- c(dbinom, pbinom)
text <- c("out_1_A.png", sprintf("n=%d 二項分布 確率関数", n), "成功回数", "確率", "out_1_B.png", "二項分布 累積分布関数", "n", "成功回数がn以下となる確率")
dim(text) <- c(4, 2)
for(i in 1:2){
	png(text[1, i])
	for(j in 1:length(p)){
		plot(0:n, f[[i]](0:n, n, p[j]), ylim = 0:1, type = "p", col = j, xlab="", ylab="")
		par(new=T)
	}
	title(text[2, i], xlab = text[3, i], ylab = text[4, i])
	legend(32, .6, legend = format(p), col = 1:length(p), pch = 1)
}
p <- p[3]
png("out_1_C.png")
hist(rbinom(1000, n, p), main = sprintf("確率%.1fで成功する試行を%d回繰り返したときの成功回数", p, n), xlab = "成功回数", ylab = "frequency")

m <- c(k %% 10, k %% 10 + 1)
s <- c(1, 2)

f <- c(dnorm, pnorm)
text <- c("out_2_A.png", sprintf("k=%d 正規分布の密度関数", k), "out_2_B.png", "正規分布の累積分布関数")
dim(text) <- c(2, 2)

x <- seq(0, 11, .01)

for(i in 1:2){
	png(text[1, i])
	legends <- c()
	for(j in 1:2){
		for(k in 1:2){
			plot(x, f[[i]](x, m[j], s[k]), type = "l", col = j * 2 + k - 2, ylim = 0:1, xlab = "", ylab = "")
			legends[j * 2 + k - 2] <- sprintf("mu = %.0f, s^2 = %.0f", m[j], s[k] * s[k])
			par(new=T)
		}
	}
	title(text[2, i])
	legend("topleft", legend = legends, col = 1:4, lty = 1)
}

m <- m[1]
s <- s[1]
png("out_2_C.png")
hist(rnorm(1000, m, s), main = sprintf("正規分布N(%.2f, %.2f)に従う乱数1000個の分布", m, s * s))
