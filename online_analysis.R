
library(sqldf)
library(tcltk)
library(MASS)

author<-"lina"
experimentno<-"150428_hod_nfxpx"
testversion<-"d"
path_location<-"D:\\Users\\n.li\\Desktop\\test\\"
saved_location<-"D:\\Users\\n.li\\Desktop\\test\\"
caldate<-format(Sys.Date(), format="%Y%m%d")
#caldate<-"20150603"


###########################
#### Total
###########################
####set parameter##########
readfile<-paste(path_location,"tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_p1.csv",sep="")
data<-read.table(readfile,header=TRUE,sep=",") 
colnames(data)[1]<-"orderdate"
colnames(data)[2]<-"versionclass"
colnames(data)[3]<-"ordernum"
colnames(data)[4]<-"commissionperorder"
colnames(data)[5]<-"commissiontotal"

setwd(saved_location)
sink(paste("tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_analyzed.txt",sep=""),append=TRUE)
print("-----------------------------------------------------------------------------")
print("Total")



tmp1_ordernum<-sqldf(paste("select orderdate,versionclass,ordernum from data where versionclass=='Control'"))             
tmp2_ordernum<-sqldf(paste("select orderdate,versionclass,ordernum from data where versionclass=='Test'"))
result_ordernum<-sqldf("select a.orderdate, a.ordernum as control, b.ordernum as test, round((1.0*(b.ordernum-a.ordernum)/a.ordernum),3) as increaserate from tmp1_ordernum a join tmp2_ordernum b on a.orderdate=b.orderdate")
  
tmp1_commissionperorder<-sqldf(paste("select orderdate,versionclass,commissionperorder from data where versionclass=='Control'"))             
tmp2_commissionperorder<-sqldf(paste("select orderdate,versionclass,commissionperorder from data where versionclass=='Test'"))
result_commissionperorder<-sqldf("select a.orderdate, a.commissionperorder as control, b.commissionperorder as test, round((1.0*(b.commissionperorder-a.commissionperorder)/a.commissionperorder),3) as increaserate from tmp1_commissionperorder a join tmp2_commissionperorder b on a.orderdate=b.orderdate")
  
tmp1_commissiontotal<-sqldf(paste("select orderdate,versionclass,commissiontotal from data where versionclass=='Control'"))             
tmp2_commissiontotal<-sqldf(paste("select orderdate,versionclass,commissiontotal from data where versionclass=='Test'"))
result_commissiontotal<-sqldf("select a.orderdate, a.commissiontotal as control, b.commissiontotal as test, round((1.0*(b.commissiontotal-a.commissiontotal)/a.commissiontotal),3) as increaserate from tmp1_commissiontotal a join tmp2_commissiontotal b on a.orderdate=b.orderdate")
  
####T-test#####################

  
####order_number####
t_ordernum<-(mean(result_ordernum["test"][1:dim(result_ordernum)[1],])-mean(result_ordernum["control"][1:dim(result_ordernum)[1],]))/mean(result_ordernum["control"][1:dim(result_ordernum)[1],])
ordernum_t1<-sapply(result_ordernum[c("test","control")],function(x)(c(mean=mean(x),sd=sd(x))))
ordernum_ttest<-with(result_ordernum,t.test(test, control, paired=TRUE, alternative="less"))
ordernum_increase<-paste(round(t_ordernum*100,2), "%", sep='')

  
####commission_per_order####
t_commissionperorder<-(mean(result_commissionperorder["test"][1:dim(result_commissionperorder)[1],])-mean(result_commissionperorder["control"][1:dim(result_commissionperorder)[1],]))/mean(result_commissionperorder["control"][1:dim(result_commissionperorder)[1],])
commissionperorder_t1<-sapply(result_commissionperorder[c("test","control")],function(x)(c(mean=mean(x),sd=sd(x))))
commissionperorder_ttest<-with(result_commissionperorder,t.test(test, control, paired=TRUE, alternative="less"))
commissionperorder_increase<-paste(round(t_commissionperorder*100,2), "%", sep='')

  
####commission_total####
t_commissiontotal<-(mean(result_commissiontotal["test"][1:dim(result_commissiontotal)[1],])-mean(result_commissiontotal["control"][1:dim(result_commissiontotal)[1],]))/mean(result_commissiontotal["control"][1:dim(result_commissiontotal)[1],])
commissiontotal_t1<-sapply(result_commissiontotal[c("test","control")],function(x)(c(mean=mean(x),sd=sd(x))))
commissiontotal_ttest<-with(result_commissiontotal,t.test(test, control, paired=TRUE, alternative="less"))
commissiontotal_increase<-paste(round(t_commissiontotal*100,2), "%", sep='')

  
print("order_number/UV")
print(ordernum_increase)
print(ordernum_t1)
print(ordernum_ttest)
print("commission_per_order/UV")
print(commissionperorder_increase)
print(commissionperorder_t1)
print(commissionperorder_ttest)
print("commission_total/UV")
print(commissiontotal_increase)
print(commissiontotal_t1)
print(commissiontotal_ttest)

sink()


####FIG#####################################

####order_number####
setwd(saved_location)

title<-paste("order_number/UV lift",sep="")
filename<-paste("tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_ordernumber",".png",sep="")

png(file=filename, bg="transparent") 
barplot(result_ordernum$increaserate,
        main=title,
        ylab="lift value",
        names=result_ordernum$orderdate)
lines(result_ordernum$averagerate)
dev.off()


####commission_per_order####
setwd(saved_location)

title<-paste("commission_per_order/UV lift",sep="")
filename<-paste("tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_commissionperorder",".png",sep="")

png(file=filename, bg="transparent") 
barplot(result_commissionperorder$increaserate,
        main=title,
        ylab="lift value",
        names=result_commissionperorder$orderdate)
lines(result_commissionperorder$averagerate)
dev.off()

####commission_total####
setwd(saved_location)

title<-paste("commission_total/UV lift",sep="")
filename<-paste("tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_commissiontotal",".png",sep="")

png(file=filename, bg="transparent") 
barplot(result_commissiontotal$increaserate,
        main=title,
        ylab="lift value",
        names=result_commissiontotal$orderdate)
lines(result_commissiontotal$averagerate)
dev.off()




###########################
####Star only
###########################
####set parameter##########
whattodo<-"star"
iters1<-4
condition1<-c("star=2","star=3","star=4","star=5")
readfile<-paste(path_location,"tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_",whattodo,".csv",sep="")
data<-read.table(readfile,header=TRUE,sep=",") 
colnames(data)[1]<-"orderdate"
colnames(data)[2]<-"versionclass"
colnames(data)[3]<-whattodo
colnames(data)[4]<-"ordernum"
colnames(data)[5]<-"commissionperorder"
colnames(data)[6]<-"commissiontotal"

setwd(saved_location)
sink(paste("tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_analyzed.txt",sep=""),append=TRUE)
print("-----------------------------------------------------------------------------")
print("star")
print("condition ordnum_inc ordnum_p comperord_inc comperord_p comtotal_inc comtotal_p")

####Cycle##################################
for (p in 1:(iters1)){
  
condition1a<-condition1[p]
tmp1_ordernum<-sqldf(paste("select orderdate,versionclass,ordernum from data where", condition1a,"and","versionclass=='Control'"))             
tmp2_ordernum<-sqldf(paste("select orderdate,versionclass,ordernum from data where", condition1a,"and","versionclass=='Test'"))
result_ordernum<-sqldf("select a.orderdate, a.ordernum as control, b.ordernum as test, round((1.0*(b.ordernum-a.ordernum)/a.ordernum),3) as increaserate from tmp1_ordernum a join tmp2_ordernum b on a.orderdate=b.orderdate")

tmp1_commissionperorder<-sqldf(paste("select orderdate,versionclass,commissionperorder from data where", condition1a,"and","versionclass=='Control'"))             
tmp2_commissionperorder<-sqldf(paste("select orderdate,versionclass,commissionperorder from data where", condition1a,"and","versionclass=='Test'"))
result_commissionperorder<-sqldf("select a.orderdate, a.commissionperorder as control, b.commissionperorder as test, round((1.0*(b.commissionperorder-a.commissionperorder)/a.commissionperorder),3) as increaserate from tmp1_commissionperorder a join tmp2_commissionperorder b on a.orderdate=b.orderdate")

tmp1_commissiontotal<-sqldf(paste("select orderdate,versionclass,commissiontotal from data where", condition1a,"and","versionclass=='Control'"))             
tmp2_commissiontotal<-sqldf(paste("select orderdate,versionclass,commissiontotal from data where", condition1a,"and","versionclass=='Test'"))
result_commissiontotal<-sqldf("select a.orderdate, a.commissiontotal as control, b.commissiontotal as test, round((1.0*(b.commissiontotal-a.commissiontotal)/a.commissiontotal),3) as increaserate from tmp1_commissiontotal a join tmp2_commissiontotal b on a.orderdate=b.orderdate")

####T-test#####################
setwd(saved_location)
sink(paste("tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_analyzed.txt",sep=""),append=TRUE)

####order_number####
t_ordernum<-(mean(result_ordernum["test"][1:dim(result_ordernum)[1],])-mean(result_ordernum["control"][1:dim(result_ordernum)[1],]))/mean(result_ordernum["control"][1:dim(result_ordernum)[1],])
table_ordernum<-with(result_ordernum,t.test(test, control, paired=TRUE, alternative="less"))
ordernum_increase<-paste(round(t_ordernum*100,2), "%", sep='')
ordernum_pvalue<-table_ordernum$p.value

####commission_per_order####
t_commissionperorder<-(mean(result_commissionperorder["test"][1:dim(result_commissionperorder)[1],])-mean(result_commissionperorder["control"][1:dim(result_commissionperorder)[1],]))/mean(result_commissionperorder["control"][1:dim(result_commissionperorder)[1],])
table_commissionperorder<-with(result_commissionperorder,t.test(test, control, paired=TRUE, alternative="less"))
commissionperorder_increase<-paste(round(t_commissionperorder*100,2), "%", sep='')
commissionperorder_pvalue<-table_commissionperorder$p.value

####commission_total####
t_commissiontotal<-(mean(result_commissiontotal["test"][1:dim(result_commissiontotal)[1],])-mean(result_commissiontotal["control"][1:dim(result_commissiontotal)[1],]))/mean(result_commissiontotal["control"][1:dim(result_commissiontotal)[1],])
table_commissiontotal<-with(result_commissiontotal,t.test(test, control, paired=TRUE, alternative="less"))
commissiontotal_increase<-paste(round(t_commissiontotal*100,2), "%", sep='')
commissiontotal_pvalue<-table_commissiontotal$p.value

print(paste(condition1a,ordernum_increase,ordernum_pvalue,commissionperorder_increase,commissionperorder_pvalue,commissiontotal_increase,commissiontotal_pvalue))

sink()

}







###########################
####citylevel only
###########################
####set parameter##########
whattodo<-"citylevel"
iters1<-4
condition1<-c("citylevel='LevelOne'","citylevel='LevelTwoA'","citylevel='LevelTwoB'","citylevel='LevelThree'")
readfile<-paste(path_location,"tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_",whattodo,".csv",sep="")
data<-read.table(readfile,header=TRUE,sep=",") 
colnames(data)[1]<-"orderdate"
colnames(data)[2]<-"versionclass"
colnames(data)[3]<-whattodo
colnames(data)[4]<-"ordernum"
colnames(data)[5]<-"commissionperorder"
colnames(data)[6]<-"commissiontotal"

setwd(saved_location)
sink(paste("tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_analyzed.txt",sep=""),append=TRUE)
print("-----------------------------------------------------------------------------")
print("citylevel")
print("condition ordnum_inc ordnum_p comperord_inc comperord_p comtotal_inc comtotal_p")

####Cycle##################################
for (p in 1:(iters1)){
  
  condition1a<-condition1[p]
  tmp1_ordernum<-sqldf(paste("select orderdate,versionclass,ordernum from data where", condition1a,"and","versionclass=='Control'"))             
  tmp2_ordernum<-sqldf(paste("select orderdate,versionclass,ordernum from data where", condition1a,"and","versionclass=='Test'"))
  result_ordernum<-sqldf("select a.orderdate, a.ordernum as control, b.ordernum as test, round((1.0*(b.ordernum-a.ordernum)/a.ordernum),3) as increaserate from tmp1_ordernum a join tmp2_ordernum b on a.orderdate=b.orderdate")
  
  tmp1_commissionperorder<-sqldf(paste("select orderdate,versionclass,commissionperorder from data where", condition1a,"and","versionclass=='Control'"))             
  tmp2_commissionperorder<-sqldf(paste("select orderdate,versionclass,commissionperorder from data where", condition1a,"and","versionclass=='Test'"))
  result_commissionperorder<-sqldf("select a.orderdate, a.commissionperorder as control, b.commissionperorder as test, round((1.0*(b.commissionperorder-a.commissionperorder)/a.commissionperorder),3) as increaserate from tmp1_commissionperorder a join tmp2_commissionperorder b on a.orderdate=b.orderdate")
  
  tmp1_commissiontotal<-sqldf(paste("select orderdate,versionclass,commissiontotal from data where", condition1a,"and","versionclass=='Control'"))             
  tmp2_commissiontotal<-sqldf(paste("select orderdate,versionclass,commissiontotal from data where", condition1a,"and","versionclass=='Test'"))
  result_commissiontotal<-sqldf("select a.orderdate, a.commissiontotal as control, b.commissiontotal as test, round((1.0*(b.commissiontotal-a.commissiontotal)/a.commissiontotal),3) as increaserate from tmp1_commissiontotal a join tmp2_commissiontotal b on a.orderdate=b.orderdate")
  
  ####T-test#####################
  setwd(saved_location)
  sink(paste("tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_analyzed.txt",sep=""),append=TRUE)
  
  ####order_number####
  t_ordernum<-(mean(result_ordernum["test"][1:dim(result_ordernum)[1],])-mean(result_ordernum["control"][1:dim(result_ordernum)[1],]))/mean(result_ordernum["control"][1:dim(result_ordernum)[1],])
  table_ordernum<-with(result_ordernum,t.test(test, control, paired=TRUE, alternative="less"))
  ordernum_increase<-paste(round(t_ordernum*100,2), "%", sep='')
  ordernum_pvalue<-table_ordernum$p.value
  
  ####commission_per_order####
  t_commissionperorder<-(mean(result_commissionperorder["test"][1:dim(result_commissionperorder)[1],])-mean(result_commissionperorder["control"][1:dim(result_commissionperorder)[1],]))/mean(result_commissionperorder["control"][1:dim(result_commissionperorder)[1],])
  table_commissionperorder<-with(result_commissionperorder,t.test(test, control, paired=TRUE, alternative="less"))
  commissionperorder_increase<-paste(round(t_commissionperorder*100,2), "%", sep='')
  commissionperorder_pvalue<-table_commissionperorder$p.value
  
  ####commission_total####
  t_commissiontotal<-(mean(result_commissiontotal["test"][1:dim(result_commissiontotal)[1],])-mean(result_commissiontotal["control"][1:dim(result_commissiontotal)[1],]))/mean(result_commissiontotal["control"][1:dim(result_commissiontotal)[1],])
  table_commissiontotal<-with(result_commissiontotal,t.test(test, control, paired=TRUE, alternative="less"))
  commissiontotal_increase<-paste(round(t_commissiontotal*100,2), "%", sep='')
  commissiontotal_pvalue<-table_commissiontotal$p.value
  
  print(paste(condition1a,ordernum_increase,ordernum_pvalue,commissionperorder_increase,commissionperorder_pvalue,commissiontotal_increase,commissiontotal_pvalue))
  
  sink()
  
}






###########################
####hourgroup only
###########################
####set parameter##########
whattodo<-"hourgroup"
iters1<-4
condition1<-c("hourgroup='0AM-7AM'","hourgroup='8AM-12AM'","hourgroup='1PM-5PM'","hourgroup='6PM-11PM'")
readfile<-paste(path_location,"tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_",whattodo,".csv",sep="")
data<-read.table(readfile,header=TRUE,sep=",") 
colnames(data)[1]<-"orderdate"
colnames(data)[2]<-"versionclass"
colnames(data)[3]<-whattodo
colnames(data)[4]<-"ordernum"
colnames(data)[5]<-"commissionperorder"
colnames(data)[6]<-"commissiontotal"

setwd(saved_location)
sink(paste("tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_analyzed.txt",sep=""),append=TRUE)
print("-----------------------------------------------------------------------------")
print("hourgroup")
print("condition ordnum_inc ordnum_p comperord_inc comperord_p comtotal_inc comtotal_p")

####Cycle##################################
for (p in 1:(iters1)){
  
  condition1a<-condition1[p]
  tmp1_ordernum<-sqldf(paste("select orderdate,versionclass,ordernum from data where", condition1a,"and","versionclass=='Control'"))             
  tmp2_ordernum<-sqldf(paste("select orderdate,versionclass,ordernum from data where", condition1a,"and","versionclass=='Test'"))
  result_ordernum<-sqldf("select a.orderdate, a.ordernum as control, b.ordernum as test, round((1.0*(b.ordernum-a.ordernum)/a.ordernum),3) as increaserate from tmp1_ordernum a join tmp2_ordernum b on a.orderdate=b.orderdate")
  
  tmp1_commissionperorder<-sqldf(paste("select orderdate,versionclass,commissionperorder from data where", condition1a,"and","versionclass=='Control'"))             
  tmp2_commissionperorder<-sqldf(paste("select orderdate,versionclass,commissionperorder from data where", condition1a,"and","versionclass=='Test'"))
  result_commissionperorder<-sqldf("select a.orderdate, a.commissionperorder as control, b.commissionperorder as test, round((1.0*(b.commissionperorder-a.commissionperorder)/a.commissionperorder),3) as increaserate from tmp1_commissionperorder a join tmp2_commissionperorder b on a.orderdate=b.orderdate")
  
  tmp1_commissiontotal<-sqldf(paste("select orderdate,versionclass,commissiontotal from data where", condition1a,"and","versionclass=='Control'"))             
  tmp2_commissiontotal<-sqldf(paste("select orderdate,versionclass,commissiontotal from data where", condition1a,"and","versionclass=='Test'"))
  result_commissiontotal<-sqldf("select a.orderdate, a.commissiontotal as control, b.commissiontotal as test, round((1.0*(b.commissiontotal-a.commissiontotal)/a.commissiontotal),3) as increaserate from tmp1_commissiontotal a join tmp2_commissiontotal b on a.orderdate=b.orderdate")
  
  ####T-test#####################
  setwd(saved_location)
  sink(paste("tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_analyzed.txt",sep=""),append=TRUE)
  
  ####order_number####
  t_ordernum<-(mean(result_ordernum["test"][1:dim(result_ordernum)[1],])-mean(result_ordernum["control"][1:dim(result_ordernum)[1],]))/mean(result_ordernum["control"][1:dim(result_ordernum)[1],])
  table_ordernum<-with(result_ordernum,t.test(test, control, paired=TRUE, alternative="less"))
  ordernum_increase<-paste(round(t_ordernum*100,2), "%", sep='')
  ordernum_pvalue<-table_ordernum$p.value
  
  ####commission_per_order####
  t_commissionperorder<-(mean(result_commissionperorder["test"][1:dim(result_commissionperorder)[1],])-mean(result_commissionperorder["control"][1:dim(result_commissionperorder)[1],]))/mean(result_commissionperorder["control"][1:dim(result_commissionperorder)[1],])
  table_commissionperorder<-with(result_commissionperorder,t.test(test, control, paired=TRUE, alternative="less"))
  commissionperorder_increase<-paste(round(t_commissionperorder*100,2), "%", sep='')
  commissionperorder_pvalue<-table_commissionperorder$p.value
  
  ####commission_total####
  t_commissiontotal<-(mean(result_commissiontotal["test"][1:dim(result_commissiontotal)[1],])-mean(result_commissiontotal["control"][1:dim(result_commissiontotal)[1],]))/mean(result_commissiontotal["control"][1:dim(result_commissiontotal)[1],])
  table_commissiontotal<-with(result_commissiontotal,t.test(test, control, paired=TRUE, alternative="less"))
  commissiontotal_increase<-paste(round(t_commissiontotal*100,2), "%", sep='')
  commissiontotal_pvalue<-table_commissiontotal$p.value
  
  print(paste(condition1a,ordernum_increase,ordernum_pvalue,commissionperorder_increase,commissionperorder_pvalue,commissiontotal_increase,commissiontotal_pvalue))
  
  sink()
  
}







###########################
####advancedgroup only
###########################
####set parameter##########
whattodo<-"advancedgroup"
iters1<-5
condition1<-c("advancedgroup=='T+0'","advancedgroup=='T+1'","advancedgroup=='T+2'","advancedgroup=='T+3'","advancedgroup=='T>3'")
readfile<-paste(path_location,"tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_",whattodo,".csv",sep="")
data<-read.table(readfile,header=TRUE,sep=",") 
colnames(data)[1]<-"orderdate"
colnames(data)[2]<-"versionclass"
colnames(data)[3]<-whattodo
colnames(data)[4]<-"ordernum"
colnames(data)[5]<-"commissionperorder"
colnames(data)[6]<-"commissiontotal"

setwd(saved_location)
sink(paste("tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_analyzed.txt",sep=""),append=TRUE)
print("-----------------------------------------------------------------------------")
print("advancedgroup")
print("condition ordnum_inc ordnum_p comperord_inc comperord_p comtotal_inc comtotal_p")

####Cycle##################################
for (p in 1:(iters1)){
  
  condition1a<-condition1[p]
  tmp1_ordernum<-sqldf(paste("select orderdate,versionclass,ordernum from data where", condition1a,"and","versionclass=='Control'"))             
  tmp2_ordernum<-sqldf(paste("select orderdate,versionclass,ordernum from data where", condition1a,"and","versionclass=='Test'"))
  result_ordernum<-sqldf("select a.orderdate, a.ordernum as control, b.ordernum as test, round((1.0*(b.ordernum-a.ordernum)/a.ordernum),3) as increaserate from tmp1_ordernum a join tmp2_ordernum b on a.orderdate=b.orderdate")
  
  tmp1_commissionperorder<-sqldf(paste("select orderdate,versionclass,commissionperorder from data where", condition1a,"and","versionclass=='Control'"))             
  tmp2_commissionperorder<-sqldf(paste("select orderdate,versionclass,commissionperorder from data where", condition1a,"and","versionclass=='Test'"))
  result_commissionperorder<-sqldf("select a.orderdate, a.commissionperorder as control, b.commissionperorder as test, round((1.0*(b.commissionperorder-a.commissionperorder)/a.commissionperorder),3) as increaserate from tmp1_commissionperorder a join tmp2_commissionperorder b on a.orderdate=b.orderdate")
  
  tmp1_commissiontotal<-sqldf(paste("select orderdate,versionclass,commissiontotal from data where", condition1a,"and","versionclass=='Control'"))             
  tmp2_commissiontotal<-sqldf(paste("select orderdate,versionclass,commissiontotal from data where", condition1a,"and","versionclass=='Test'"))
  result_commissiontotal<-sqldf("select a.orderdate, a.commissiontotal as control, b.commissiontotal as test, round((1.0*(b.commissiontotal-a.commissiontotal)/a.commissiontotal),3) as increaserate from tmp1_commissiontotal a join tmp2_commissiontotal b on a.orderdate=b.orderdate")
  
  ####T-test#####################
  setwd(saved_location)
  sink(paste("tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_analyzed.txt",sep=""),append=TRUE)
  
  ####order_number####
  t_ordernum<-(mean(result_ordernum["test"][1:dim(result_ordernum)[1],])-mean(result_ordernum["control"][1:dim(result_ordernum)[1],]))/mean(result_ordernum["control"][1:dim(result_ordernum)[1],])
  table_ordernum<-with(result_ordernum,t.test(test, control, paired=TRUE, alternative="less"))
  ordernum_increase<-paste(round(t_ordernum*100,2), "%", sep='')
  ordernum_pvalue<-table_ordernum$p.value
  
  ####commission_per_order####
  t_commissionperorder<-(mean(result_commissionperorder["test"][1:dim(result_commissionperorder)[1],])-mean(result_commissionperorder["control"][1:dim(result_commissionperorder)[1],]))/mean(result_commissionperorder["control"][1:dim(result_commissionperorder)[1],])
  table_commissionperorder<-with(result_commissionperorder,t.test(test, control, paired=TRUE, alternative="less"))
  commissionperorder_increase<-paste(round(t_commissionperorder*100,2), "%", sep='')
  commissionperorder_pvalue<-table_commissionperorder$p.value
  
  ####commission_total####
  t_commissiontotal<-(mean(result_commissiontotal["test"][1:dim(result_commissiontotal)[1],])-mean(result_commissiontotal["control"][1:dim(result_commissiontotal)[1],]))/mean(result_commissiontotal["control"][1:dim(result_commissiontotal)[1],])
  table_commissiontotal<-with(result_commissiontotal,t.test(test, control, paired=TRUE, alternative="less"))
  commissiontotal_increase<-paste(round(t_commissiontotal*100,2), "%", sep='')
  commissiontotal_pvalue<-table_commissiontotal$p.value
  
  print(paste(condition1a,ordernum_increase,ordernum_pvalue,commissionperorder_increase,commissionperorder_pvalue,commissiontotal_increase,commissiontotal_pvalue))
  
  sink()
  
}





###########################
####usernormaltype only
###########################
####set parameter##########
whattodo<-"usernormaltype"
iters1<-2
condition1<-c("usernormaltype=='ordinary'","usernormaltype=='tickettout'")
readfile<-paste(path_location,"tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_",whattodo,".csv",sep="")
data<-read.table(readfile,header=TRUE,sep=",") 
colnames(data)[1]<-"orderdate"
colnames(data)[2]<-"versionclass"
colnames(data)[3]<-whattodo
colnames(data)[4]<-"ordernum"
colnames(data)[5]<-"commissionperorder"
colnames(data)[6]<-"commissiontotal"

setwd(saved_location)
sink(paste("tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_analyzed.txt",sep=""),append=TRUE)
print("-----------------------------------------------------------------------------")
print("usernormaltype")
print("condition ordnum_inc ordnum_p comperord_inc comperord_p comtotal_inc comtotal_p")

####Cycle##################################
for (p in 1:(iters1)){
  
  condition1a<-condition1[p]
  tmp1_ordernum<-sqldf(paste("select orderdate,versionclass,ordernum from data where", condition1a,"and","versionclass=='Control'"))             
  tmp2_ordernum<-sqldf(paste("select orderdate,versionclass,ordernum from data where", condition1a,"and","versionclass=='Test'"))
  result_ordernum<-sqldf("select a.orderdate, a.ordernum as control, b.ordernum as test, round((1.0*(b.ordernum-a.ordernum)/a.ordernum),3) as increaserate from tmp1_ordernum a join tmp2_ordernum b on a.orderdate=b.orderdate")
  
  tmp1_commissionperorder<-sqldf(paste("select orderdate,versionclass,commissionperorder from data where", condition1a,"and","versionclass=='Control'"))             
  tmp2_commissionperorder<-sqldf(paste("select orderdate,versionclass,commissionperorder from data where", condition1a,"and","versionclass=='Test'"))
  result_commissionperorder<-sqldf("select a.orderdate, a.commissionperorder as control, b.commissionperorder as test, round((1.0*(b.commissionperorder-a.commissionperorder)/a.commissionperorder),3) as increaserate from tmp1_commissionperorder a join tmp2_commissionperorder b on a.orderdate=b.orderdate")
  
  tmp1_commissiontotal<-sqldf(paste("select orderdate,versionclass,commissiontotal from data where", condition1a,"and","versionclass=='Control'"))             
  tmp2_commissiontotal<-sqldf(paste("select orderdate,versionclass,commissiontotal from data where", condition1a,"and","versionclass=='Test'"))
  result_commissiontotal<-sqldf("select a.orderdate, a.commissiontotal as control, b.commissiontotal as test, round((1.0*(b.commissiontotal-a.commissiontotal)/a.commissiontotal),3) as increaserate from tmp1_commissiontotal a join tmp2_commissiontotal b on a.orderdate=b.orderdate")
  
  ####T-test#####################
  setwd(saved_location)
  sink(paste("tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_analyzed.txt",sep=""),append=TRUE)
  
  ####order_number####
  t_ordernum<-(mean(result_ordernum["test"][1:dim(result_ordernum)[1],])-mean(result_ordernum["control"][1:dim(result_ordernum)[1],]))/mean(result_ordernum["control"][1:dim(result_ordernum)[1],])
  table_ordernum<-with(result_ordernum,t.test(test, control, paired=TRUE, alternative="less"))
  ordernum_increase<-paste(round(t_ordernum*100,2), "%", sep='')
  ordernum_pvalue<-table_ordernum$p.value
  
  ####commission_per_order####
  t_commissionperorder<-(mean(result_commissionperorder["test"][1:dim(result_commissionperorder)[1],])-mean(result_commissionperorder["control"][1:dim(result_commissionperorder)[1],]))/mean(result_commissionperorder["control"][1:dim(result_commissionperorder)[1],])
  table_commissionperorder<-with(result_commissionperorder,t.test(test, control, paired=TRUE, alternative="less"))
  commissionperorder_increase<-paste(round(t_commissionperorder*100,2), "%", sep='')
  commissionperorder_pvalue<-table_commissionperorder$p.value
  
  ####commission_total####
  t_commissiontotal<-(mean(result_commissiontotal["test"][1:dim(result_commissiontotal)[1],])-mean(result_commissiontotal["control"][1:dim(result_commissiontotal)[1],]))/mean(result_commissiontotal["control"][1:dim(result_commissiontotal)[1],])
  table_commissiontotal<-with(result_commissiontotal,t.test(test, control, paired=TRUE, alternative="less"))
  commissiontotal_increase<-paste(round(t_commissiontotal*100,2), "%", sep='')
  commissiontotal_pvalue<-table_commissiontotal$p.value
  
  print(paste(condition1a,ordernum_increase,ordernum_pvalue,commissionperorder_increase,commissionperorder_pvalue,commissiontotal_increase,commissiontotal_pvalue))
  
  sink()
  
}






###########################
####consumptiontag only
###########################
####set parameter##########
whattodo<-"consumptiontag"
iters1<-3
condition1<-c("consumptiontag=0","consumptiontag=1","consumptiontag=2")
readfile<-paste(path_location,"tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_",whattodo,".csv",sep="")
data<-read.table(readfile,header=TRUE,sep=",") 
colnames(data)[1]<-"orderdate"
colnames(data)[2]<-"versionclass"
colnames(data)[3]<-whattodo
colnames(data)[4]<-"ordernum"
colnames(data)[5]<-"commissionperorder"
colnames(data)[6]<-"commissiontotal"

setwd(saved_location)
sink(paste("tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_analyzed.txt",sep=""),append=TRUE)
print("-----------------------------------------------------------------------------")
print("consumptiontag")
print("condition ordnum_inc ordnum_p comperord_inc comperord_p comtotal_inc comtotal_p")

####Cycle##################################
for (p in 1:(iters1)){
  
  condition1a<-condition1[p]
  tmp1_ordernum<-sqldf(paste("select orderdate,versionclass,ordernum from data where", condition1a,"and","versionclass=='Control'"))             
  tmp2_ordernum<-sqldf(paste("select orderdate,versionclass,ordernum from data where", condition1a,"and","versionclass=='Test'"))
  result_ordernum<-sqldf("select a.orderdate, a.ordernum as control, b.ordernum as test, round((1.0*(b.ordernum-a.ordernum)/a.ordernum),3) as increaserate from tmp1_ordernum a join tmp2_ordernum b on a.orderdate=b.orderdate")
  
  tmp1_commissionperorder<-sqldf(paste("select orderdate,versionclass,commissionperorder from data where", condition1a,"and","versionclass=='Control'"))             
  tmp2_commissionperorder<-sqldf(paste("select orderdate,versionclass,commissionperorder from data where", condition1a,"and","versionclass=='Test'"))
  result_commissionperorder<-sqldf("select a.orderdate, a.commissionperorder as control, b.commissionperorder as test, round((1.0*(b.commissionperorder-a.commissionperorder)/a.commissionperorder),3) as increaserate from tmp1_commissionperorder a join tmp2_commissionperorder b on a.orderdate=b.orderdate")
  
  tmp1_commissiontotal<-sqldf(paste("select orderdate,versionclass,commissiontotal from data where", condition1a,"and","versionclass=='Control'"))             
  tmp2_commissiontotal<-sqldf(paste("select orderdate,versionclass,commissiontotal from data where", condition1a,"and","versionclass=='Test'"))
  result_commissiontotal<-sqldf("select a.orderdate, a.commissiontotal as control, b.commissiontotal as test, round((1.0*(b.commissiontotal-a.commissiontotal)/a.commissiontotal),3) as increaserate from tmp1_commissiontotal a join tmp2_commissiontotal b on a.orderdate=b.orderdate")
  
  ####T-test#####################
  setwd(saved_location)
  sink(paste("tmp_",author,"_",caldate,"_",experimentno,"_",testversion,"_analyzed.txt",sep=""),append=TRUE)
  
  ####order_number####
  t_ordernum<-(mean(result_ordernum["test"][1:dim(result_ordernum)[1],])-mean(result_ordernum["control"][1:dim(result_ordernum)[1],]))/mean(result_ordernum["control"][1:dim(result_ordernum)[1],])
  table_ordernum<-with(result_ordernum,t.test(test, control, paired=TRUE, alternative="less"))
  ordernum_increase<-paste(round(t_ordernum*100,2), "%", sep='')
  ordernum_pvalue<-table_ordernum$p.value
  
  ####commission_per_order####
  t_commissionperorder<-(mean(result_commissionperorder["test"][1:dim(result_commissionperorder)[1],])-mean(result_commissionperorder["control"][1:dim(result_commissionperorder)[1],]))/mean(result_commissionperorder["control"][1:dim(result_commissionperorder)[1],])
  table_commissionperorder<-with(result_commissionperorder,t.test(test, control, paired=TRUE, alternative="less"))
  commissionperorder_increase<-paste(round(t_commissionperorder*100,2), "%", sep='')
  commissionperorder_pvalue<-table_commissionperorder$p.value
  
  ####commission_total####
  t_commissiontotal<-(mean(result_commissiontotal["test"][1:dim(result_commissiontotal)[1],])-mean(result_commissiontotal["control"][1:dim(result_commissiontotal)[1],]))/mean(result_commissiontotal["control"][1:dim(result_commissiontotal)[1],])
  table_commissiontotal<-with(result_commissiontotal,t.test(test, control, paired=TRUE, alternative="less"))
  commissiontotal_increase<-paste(round(t_commissiontotal*100,2), "%", sep='')
  commissiontotal_pvalue<-table_commissiontotal$p.value
  
  print(paste(condition1a,ordernum_increase,ordernum_pvalue,commissionperorder_increase,commissionperorder_pvalue,commissiontotal_increase,commissiontotal_pvalue))
  
  sink()
  
}








