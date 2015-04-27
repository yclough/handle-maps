	plotmat<-function(x,main="",filter=NA,marp=c(2,4,4,5)){
	require(fields)
	par(mar=marp)
	x2<-x
	if(is.na(filter)==F){x2[eval(parse(text=filter))]=NA}
	image.plot(t(x2[nrow(x2):1,]),main=main)
	usr <- par('usr')
	rect(usr[1], usr[3], usr[2], usr[4], col="black")
	image.plot(t(x2[nrow(x2):1,]),main=main,add=TRUE)
	}
