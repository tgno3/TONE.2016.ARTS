# Load library
library(DJL)

# load dataset / parameters
df<-dataset.airplane.2017
n<-subset(df,select=1)
x<-data.frame(FREW=rep(1,28))
y<-subset(df,select=3:7)
t<-subset(df,select=2)
target<-c(25:28)

# Table 1. Dataset
table.1<-data.frame(df[,1:4],PFE=round(exp(1)^df$PFE,3),df[,6:7])
print(table.1[-target,],right=F,row.names=F)

# Table 2. Local Roc of SOA airplanes at the frontier year of 2007
m1<-roc.dea(x,y,t,2007,"vrs","o","min")
soa.r<-which(m1$roc_past>1)
soa.t<-which(m1$roc_local>1)
l.index<-soa.r[is.na(match(soa.r,soa.t))]
table.2<-data.frame(SOA.Airplane=n[soa.t,],
                    Local.RoC=m1$roc_local[soa.t,],
                    Dominated.airplanes=apply(m1$lambda_t[l.index,soa.t],2,
                                              function(x) paste(n[l.index[which(x>0)],],collapse=", ")))
print(table.2,right=F,row.names=F)

# Table 3. Four airplanes concepts in 2007
table.3<-data.frame(Design.concept=1:4,
                    table.1[target,3:7],
                    Delivery.target=c(2010,2010,2013,2015))
print(table.3,right=F,row.names=F)

# Table 4. Results summary
m2<-target.arrival.dea(x,y,t,2007,"vrs","o","min","d")
table.4<-data.frame(Design.concept=table.1[target,1],
                    Reference.airplane=apply(m2$lambda_t[target,soa.t],1,
                                             function(x) paste(n[soa.t[which(x>0)],],collapse=", ")),
                    Planned.EIS=table.3$Delivery.target,
                    Estimated.EIS=round(m2$arrival_seg[25:28,],2),
                    Delayed.EIS=c(2012,2014,2014,2017))
print(table.4,right=F,row.names=F)
