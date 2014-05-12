dataclean<-function(X=data,method=c("h"),remove=c("w"),q=c(0.05,0.5,0.95),p=0.9){
# version 2.4.05
# q must contain three values: lower, median (for NA substitution) and upper as upper cutoff
#if(is.data.frame(X)==FALSE) {warning("Data must be stored in data.frame")}
#if(all(sapply(X,class)=="numeric")==FALSE) {warning("remove characters and/or factors columns")}
# BUG - remove "n" a hpd
# X<-as.data.frame(data)
if(method=="h"){library(TeachingDemos)}
if(method=="h"){quant<-apply(X,2,function(X){emp.hpd(na.omit(X),conf=p)})}
if(method=="q"){quant<-apply(X,2,function(X){quantile(X,probs=q,na.rm=TRUE)})}
if(remove=="k"){library(impute)} # only on BioConductor
for(j in 1:ncol(X)){
	for(i in 1:nrow(X)){
		if (remove=="w"){
			if(method=="q"){
				if(is.na(as.numeric(X[i,j]))==TRUE){X[i,j]=quant[2,j]}
				if(as.numeric(X[i,j])<=quant[1,j]){X[i,j]=quant[1,j]}
				if(as.numeric(X[i,j])>=quant[3,j]){X[i,j]=quant[3,j]}
			}	
			else{ # 
				if(is.na(as.numeric(X[i,j]))==TRUE){X[i,j]=quantile(X,probs=0.5,na.rm=TRUE)}
				if(as.numeric(X[i,j])<=quant[1,j]){X[i,j]=quant[1,j]}
				if(as.numeric(X[i,j])>=quant[2,j]){X[i,j]=quant[2,j]}			
			}
		}
		else if (remove=="k"){
				if(is.na(as.numeric(X[i,j]))==TRUE){X[i,j]=quant[2,j]}
				if(as.numeric(X[i,j])<=quant[1,j]){X[i,j]=quant[1,j]}
				if(as.numeric(X[i,j])>=quant[3,j]){X[i,j]=quant[3,j]}		
		}
		else {
			if(is.na(as.numeric(X[i,j]))==TRUE){X[i,j]=quant[2,j]}
			if(as.numeric(X[i,j])<=as.numeric(quant[1,j])|as.numeric(X[i,j])>=as.numeric(quant[3,j])){X[i,j]="NA"}	
		}
	}
}
quant
out<-list(limit=quant,data=X)
return(out)
}
# 