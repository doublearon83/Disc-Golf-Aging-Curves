require(lme4)
require(lmerTest)
require(MuMIn)
require(cAIC4)

# read in age and PDGA data
MPO_ar <- read.csv("MPO_ar.csv",header=T)

#MPO_ar dataset comes from "MPO_Aging.r" script
MPO_ar$years<-scale(MPO_ar$year)
MPO_ar$ages<-scale(MPO_ar$Age)

#combine ratings and classes (data comes from Player_type.r) for cross-validation of ratings
#can I make this betterc(faster) by pasting all at once then just using match to subset rows?
MPO_ar_tune <- data.frame(matrix(0,nrow=nrow(MPO_ar),ncol=ncol(cbind(clss,clss_sc,clss_hc,clss_kmeans))))
for (i in 1:nrow(MPO_ar))
{
  if (any(grepl(MPO_ar$Name[i],clustdm_cv$names))) {MPO_ar_tune[i,]<-as.numeric(clustdm_cv[grep(paste(MPO_ar$Name_f[i],MPO_ar$Name[i],sep = " "),clustdm_cv$names),c(9:ncol(clustdm_cv))])} 
  else (MPO_ar_tune[i,]<-"NA")
}

#remove NAs and player type 3 (not enough age data for player type 3)
#MPO_ar_pt <- subset(MPO_ar,MPO_ar$ptype!="NA" & MPO_ar$ptype!="3")


# run cross-validation to compare predictions of age curves including 
# player type versus excluding player type

#create dataframe for tuning and remove NAs
MPO_ar_tune2 <- data.frame(MPO_ar,MPO_ar_tune)
names(MPO_ar_tune2)[(ncol(MPO_ar_tune2)-64):ncol(MPO_ar_tune2)] <- names(clustdm_cv)[(ncol(clustdm_cv)-64):ncol(clustdm_cv)]
MPO_ar_pt <- subset(MPO_ar_tune2,MPO_ar_tune2$nc2!="NA")

#run loops

mean_SS_t <- data.frame(matrix(0,nrow=ncol(cbind(clss,clss_sc,clss_hc,clss_kmeans)),ncol=8))

for (j in (ncol(MPO_ar_tune2)-64):ncol(MPO_ar_tune2)) {

sumsq_p<-numeric(1000)
sumsq<-numeric(1000)
#training size
tz <- round(length(unique(MPO_ar_pt$PDGA))*.8)
for (i in 1:1000){
train <- sample(unique(MPO_ar_pt$PDGA),tz)
test <- unique(MPO_ar_pt$PDGA)[!unique(MPO_ar_pt$PDGA) %in% train]

MPO_ar_pt_train <- MPO_ar_pt[MPO_ar_pt$PDGA %in% train,]
MPO_ar_pt_test <- MPO_ar_pt[MPO_ar_pt$PDGA %in% test,]

pt_type <- MPO_ar_pt_train[,j]
pt_ages <- MPO_ar_pt_train$ages
pt_ratr <- MPO_ar_pt_train$ratr
MPO_ar_pt_train2 <- data.frame(pt_type,pt_ages,pt_ratr)

outp<-lm(pt_ratr~pt_type*pt_ages+pt_type*I(pt_ages^2)+pt_type*I(pt_ages^3)+pt_type*I(pt_ages^4),data=MPO_ar_pt_train2)

pt_type <- MPO_ar_pt_test[,j]
pt_ages <- MPO_ar_pt_test$ages
pt_ratr <- MPO_ar_pt_test$ratr
MPO_ar_pt_test2 <- data.frame(pt_type,pt_ages,pt_ratr)

#what is our method of measuring model predictive power
sumsq_p[i] <- sum((predict(outp,MPO_ar_pt_test2)-MPO_ar_pt_test2$pt_ratr)^2)

######### i have no idea if this works ###########

#use r squred to find some model fits

adj_r2[i] <- summary(outp)$adj.r.squared

# look at MSE

mse_p[i] <- mean((predict(outp, MPO_ar_pt_test2) - MPO_ar_pt_test2$pt_ratr)^2, na.rm = TRUE)

# use AIC and or BIC to see if using more complex 
#models is better (especially with the use of the fourth degree)
  
################################################

out<-lm(ratr~ages+I(ages^2)+I(ages^3)+I(ages^4),data=MPO_ar_pt_train)
sumsq[i] <- sum((predict(out,MPO_ar_pt_test)-MPO_ar_pt_test$ratr)^2)

mse[i] <- mean((predict(out, MPO_ar_pt_test) - MPO_ar_pt_test$ratr)^2, na.rm = TRUE)


}
mean_SS_t[j+1-13,1] <- mean(sumsq_p)
mean_SS_t[j+1-13,2] <- mean(sumsq)

print(j)
}



#####extra acode Prof H was experimenting with#######
(mean(sumsq)-mean(sumsq_p))/mean(sumsq)

out<-lm(ratr~-1+ptype*ages+ptype*I(ages^2)+ptype*I(ages^3),data=MPO_ar_pt)
summary(out)

ag<-seq(-1,2,0.01)
plot((c(1004.8197+ag*0.8171+ag^2*-16.0802+ag^3*4.5075))~ag,type="l",
   lwd=2,ylim=c(980,1060),xlab="Standardized Age", ylab="Change in Rating")
points((c(1023.6632+ag*0.8171+ag^2*-16.0802+ag^3*4.5075+ag*12.2052+ag^2*-6.4861+ag^3*-8.7655))~ag,type="l",
       lwd=2,col="green")
points((c(ag*2.103+ag^2*-4.94)-min(c(ag*2.103+ag^2*-4.94)))~ag,type="l",
       lwd=2,col="red")

out<-lmer(ratr~-1+ptype/ages+ptype/I(ages^2)+(1|PDGA/years/ratrd),data=MPO_ar_pt)
summary(out)
anova(out)

r.squaredGLMM(out)

out<-lmer(ratr~-1+ptype/ages+ptype/I(ages^2)+(1|PDGA),data=MPO_ar)

ag<-seq(-1,1,0.01)
plot((c(ag*12.91+ag^2*-35.6)-min(c(ag*12.91+ag^2*-35.6)))~ag,type="l",
     lwd=2,ylim=c(0,60),xlab="Standardized Age", ylab="Change in Rating")
points((c(ag*4.27+ag^2*-15.34)-min(c(ag*4.27+ag^2*-15.34)))~ag,type="l",
       lwd=2,col="green")
points((c(ag*2.103+ag^2*-4.94)-min(c(ag*2.103+ag^2*-4.94)))~ag,type="l",
       lwd=2,col="red")



$control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=10000))

require(splines)
out<-lmer(ratr~years+ptype*ns(ages,df=3)+(1|PDGA/years/ratrd),data=MPO_ar)

summary(out)
r.squaredGLMM(out)
