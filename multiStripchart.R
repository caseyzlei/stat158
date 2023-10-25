multiStripchart<-function(response,facnames,data,lines=FALSE,xlab="",ylab="Response",jitter=FALSE,main="",
                          ylim="default",...){
	nfac<-length(facnames)
	mex<-1:nfac
	
	#assume 2 levels per factor
	lex<-list()#c(0.9,1.1)+rep(0:(nfac-1),each=2) #x coordinates for each group
	beginx<-0.5
	for(nam in facnames){
		l<-nlevels(data[,nam])
		lex[[nam]]<-beginx+1:l
		beginx<-beginx+l+1 #makes gap of 2
	}
  if(ylim[1]=="default") ylim<-range(response)
	xlim<-range(unlist(lex))
	if(is.logical(jitter) ){
		if(jitter) jitter<-1 #default in jitter
		else jitter<-0
	}
	plot(0,0,type="n",ylim=ylim,xlim=xlim,xaxt="n",xlab=xlab,ylab=ylab, cex.lab=1,main=main)
	for(ii in mex){
		xcoord<-lex[[facnames[[ii]]]] #lex[c(2*ii-1,2*ii)]
		fac<-data[,facnames[[ii]]]
		points(y=response,x=if(jitter>0) jitter(xcoord[fac]) else xcoord[fac],...)
		facmean<-tapply(response,fac,mean)
		segments(x0=xcoord-.05,x1=xcoord+.05,y0=facmean,y1=facmean,lend=2,lwd=2)
		axis(1,at=mean(xcoord),label=facnames[[ii]],tick=FALSE,line=2)
		axis(1,at=xcoord,label=levels(fac))
		if(lines){
			rangeByLevel<-do.call("rbind",tapply(response,fac,range,simplify=FALSE))
			print(rangeByLevel)
			segments(x0=xcoord,x1=xcoord,y0=rangeByLevel[,1],y1=rangeByLevel[,2])
		}
	}
	abline(h=mean(response),lty=3)
}