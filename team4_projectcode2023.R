#Team 4 project code
#QB2023
#Thomas Zambiasi and Anna Werkowski

#setting wd
#NOTE: don't run the line that references a different computer! (this is a note for Thomas (myself))
setwd("C:/Users/tmzam/GitHub/team4")

#loading packages
library("dplyr")
library("vegan")
library("ade4")
library("viridis")
library("gplots")
library("indicspecies")

#data wrangling####
#loading data
kbsdata1<-read.csv("data/kbs_set_1.csv")
#removing first row (has extra unneeded characters)
kbsdata1<-kbsdata1[-1,]
#isolating relevant data columns
kbsdata2<-kbsdata1[,c("replicate","disturbed","fertilized",
                      "species.name","biomass_g_m2")]
#sites will be determined by rep+treatment combo
#need to combine rep+treatment combos into single label column
kbsdata2$site<-paste(kbsdata2$replicate,kbsdata2$disturbed,kbsdata2$fertilized)
#rearranging data again
kbsdata3<-kbsdata2[,c("site","species.name","biomass_g_m2")]
#creating list of sites 
kbssitelist<-as.data.frame(unique(kbsdata3$site))
#creating list of species
kbsspplist<-unique(kbsdata3$species.name)
#reworking into dataframe
kbsspplist<-as.data.frame(kbsspplist)
#renaming column to be able to join matrix components easier
colnames(kbsspplist)<-c("species.name")

#creating site-specific data and adding to sXspp. by spp. name, steps
#repeat for each site
#r1, dist. unfert.
R1du<-kbsdata3[kbsdata3$site=="R1 disturbed unfertilized",] #isolating site data
R1du<-R1du[,2:3] #removing site column
sXspp<-left_join(kbsspplist,R1du,by="species.name") #joining to spp. list
colnames(sXspp)<-c("species.name","R1du") #renaming sXspp columns
#r1, dist. fert.
R1df<-kbsdata3[kbsdata3$site=="R1 disturbed fertilized",]
R1df<-R1df[,2:3]
sXspp<-left_join(sXspp,R1df,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df")
#r1, undist. unfert.
R1uu<-kbsdata3[kbsdata3$site=="R1 undisturbed unfertilized",]
R1uu<-R1uu[,2:3]
sXspp<-left_join(sXspp,R1uu,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu")
#r1, undist. fert.
R1uf<-kbsdata3[kbsdata3$site=="R1 undisturbed fertilized",]
R1uf<-R1uf[,2:3]
sXspp<-left_join(sXspp,R1uf,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf")
#r2, dist. unfert.
R2du<-kbsdata3[kbsdata3$site=="R2 disturbed unfertilized",]
R2du<-R2du[,2:3]
sXspp<-left_join(sXspp,R2du,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du")
#r2, dist. fert.
R2df<-kbsdata3[kbsdata3$site=="R2 disturbed fertilized",]
R2df<-R2df[,2:3]
sXspp<-left_join(sXspp,R2df,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df")
#r2, undist. unfert.
R2uu<-kbsdata3[kbsdata3$site=="R2 undisturbed unfertilized",]
R2uu<-R2uu[,2:3]
sXspp<-left_join(sXspp,R2uu,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df",
                   "R2uu")
#r2, undist. fert.
R2uf<-kbsdata3[kbsdata3$site=="R2 undisturbed fertilized",]
R2uf<-R2uf[,2:3]
sXspp<-left_join(sXspp,R2uf,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df",
                   "R2uu","R2uf")
#r3, dist. unfert.
R3du<-kbsdata3[kbsdata3$site=="R3 disturbed unfertilized",]
R3du<-R3du[,2:3]
sXspp<-left_join(sXspp,R3du,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df",
                   "R2uu","R2uf","R3du")
#r3, dist. fert.
R3df<-kbsdata3[kbsdata3$site=="R3 disturbed fertilized",]
R3df<-R3df[,2:3]
sXspp<-left_join(sXspp,R3df,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df",
                   "R2uu","R2uf","R3du","R3df")
#r3, undist. unfert.
R3uu<-kbsdata3[kbsdata3$site=="R3 undisturbed unfertilized",]
R3uu<-R3uu[,2:3]
sXspp<-left_join(sXspp,R3uu,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df",
                   "R2uu","R2uf","R3du","R3df","R3uu")
#r3, undist. fert.
R3uf<-kbsdata3[kbsdata3$site=="R3 undisturbed fertilized",]
R3uf<-R3uf[,2:3]
sXspp<-left_join(sXspp,R3uf,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df",
                   "R2uu","R2uf","R3du","R3df","R3uu","R3uf")
#r4, dist. unfert.
R4du<-kbsdata3[kbsdata3$site=="R4 disturbed unfertilized",]
R4du<-R4du[,2:3]
sXspp<-left_join(sXspp,R4du,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df",
                   "R2uu","R2uf","R3du","R3df","R3uu","R3uf","R4du")
#r4, dist. fert.
R4df<-kbsdata3[kbsdata3$site=="R4 disturbed fertilized",]
R4df<-R4df[,2:3]
sXspp<-left_join(sXspp,R4df,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df",
                   "R2uu","R2uf","R3du","R3df","R3uu","R3uf","R4du","R4df")
#r4, undist. unfert.
R4uu<-kbsdata3[kbsdata3$site=="R4 undisturbed unfertilized",]
R4uu<-R4uu[,2:3]
sXspp<-left_join(sXspp,R4uu,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df",
                   "R2uu","R2uf","R3du","R3df","R3uu","R3uf","R4du","R4df",
                   "R4uu")
#r4, undist. fert.
R4uf<-kbsdata3[kbsdata3$site=="R4 undisturbed fertilized",]
R4uf<-R4uf[,2:3]
sXspp<-left_join(sXspp,R4uf,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df",
                   "R2uu","R2uf","R3du","R3df","R3uu","R3uf","R4du","R4df",
                   "R4uu","R4uf")
#r5, dist. unfert.
R5du<-kbsdata3[kbsdata3$site=="R5 disturbed unfertilized",]
R5du<-R5du[,2:3]
sXspp<-left_join(sXspp,R5du,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df",
                   "R2uu","R2uf","R3du","R3df","R3uu","R3uf","R4du","R4df",
                   "R4uu","R4uf","R5du")
#r5, dist. fert.
R5df<-kbsdata3[kbsdata3$site=="R5 disturbed fertilized",]
R5df<-R5df[,2:3]
sXspp<-left_join(sXspp,R5df,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df",
                   "R2uu","R2uf","R3du","R3df","R3uu","R3uf","R4du","R4df",
                   "R4uu","R4uf","R5du","R5df")
#r5, undist. unfert.
R5uu<-kbsdata3[kbsdata3$site=="R5 undisturbed unfertilized",]
R5uu<-R5uu[,2:3]
sXspp<-left_join(sXspp,R5uu,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df",
                   "R2uu","R2uf","R3du","R3df","R3uu","R3uf","R4du","R4df",
                   "R4uu","R4uf","R5du","R5df","R5uu")
#r5, undist. fert.
R5uf<-kbsdata3[kbsdata3$site=="R5 undisturbed fertilized",]
R5uf<-R5uf[,2:3]
sXspp<-left_join(sXspp,R5uf,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df",
                   "R2uu","R2uf","R3du","R3df","R3uu","R3uf","R4du","R4df",
                   "R4uu","R4uf","R5du","R5df","R5uu","R5uf")
#r6, dist. unfert.
R6du<-kbsdata3[kbsdata3$site=="R6 disturbed unfertilized",]
R6du<-R6du[,2:3]
sXspp<-left_join(sXspp,R6du,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df",
                   "R2uu","R2uf","R3du","R3df","R3uu","R3uf","R4du","R4df",
                   "R4uu","R4uf","R5du","R5df","R5uu","R5uf","R6du")
#r6, dist. fert.
R6df<-kbsdata3[kbsdata3$site=="R6 disturbed fertilized",]
R6df<-R6df[,2:3]
sXspp<-left_join(sXspp,R6df,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df",
                   "R2uu","R2uf","R3du","R3df","R3uu","R3uf","R4du","R4df",
                   "R4uu","R4uf","R5du","R5df","R5uu","R5uf","R6du","R6df")
#r6, undist. unfert.
R6uu<-kbsdata3[kbsdata3$site=="R6 undisturbed unfertilized",]
R6uu<-R6uu[,2:3]
sXspp<-left_join(sXspp,R6uu,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df",
                   "R2uu","R2uf","R3du","R3df","R3uu","R3uf","R4du","R4df",
                   "R4uu","R4uf","R5du","R5df","R5uu","R5uf","R6du","R6df",
                   "R6uu")
#r6, undist. fert.
R6uf<-kbsdata3[kbsdata3$site=="R6 undisturbed fertilized",]
R6uf<-R6uf[,2:3]
sXspp<-left_join(sXspp,R6uf,by="species.name")
colnames(sXspp)<-c("species.name","R1du","R1df","R1uu","R1uf","R2du","R2df",
                   "R2uu","R2uf","R3du","R3df","R3uu","R3uf","R4du","R4df",
                   "R4uu","R4uf","R5du","R5df","R5uu","R5uf","R6du","R6df",
                   "R6uu","R6uf")
#converting column 1 to row names (species names)
row.names(sXspp)<-sXspp$species.name
sXspp<-sXspp[,-1]
#converting all NAs to zeros
sXspp[is.na(sXspp)]<-0
#converting to numeric (columns all chr)
sXspp<-mutate_all(sXspp,function(x)as.numeric(x))
#also need to convert to matrix and transform
sXspp.mtx<-t(as.matrix(sXspp))

#Beta diversity and visualization####
beta.w<-function(site.by.species="",sitenum1="",sitenum2="",pairwise=FALSE){
  #IF we specify pairwise as true, function should run this part:
  if(pairwise==TRUE){
    #code to print an error if we don't give necessary arguments
    if(sitenum1==""|sitenum2==""){
      print("Error: you need to specify some sites to compare")
      return(NA)
    }
    #now code for calculating pairwise beta diversity
    site1=site.by.species[sitenum1,] #selecting site 1
    site2=site.by.species[sitenum2,] #selecting site 2
    site1=subset(site1,select=site1>0) #removes absences in site 1
    site2=subset(site2,select=site2>0) #removes absences in site 2
    gamma=union(colnames(site1),colnames(site2)) #creates gamma species pool
    s=length(gamma) #gamma richness
    a.bar=mean(c(specnumber(site1),specnumber(site2))) #mean sample richness
    b.w=round(s/a.bar-1,3)
    return(b.w)
  }
  #otherwise, function defaults to false and runs this:
  SbyS.pa<-decostand(site.by.species,method="pa") #convert to presence/absence
  S<-ncol(SbyS.pa[,which(colSums(SbyS.pa)>0)]) #number of species in the region
  a.bar<-mean(specnumber(SbyS.pa)) #average richness at each site
  b.w<-round(S/a.bar,3) #rounding to 3 decimals
  return(b.w)
}

#finding b-diversity value across whole sXspp matrix
beta.w(sXspp.mtx,pairwise=FALSE)

#building resemblance matrix using Bray-Curtis
kbs.db<-vegdist(sXspp.mtx,method="bray")
kbs.db

#setting up cluster analysis 
kbs.ward<-hclust(kbs.db,method="ward.D2")
#cluster analysis figure
par(mar=c(1,5,2,2)+0.1)
plot(kbs.ward,main="KBS Early Successional Microsite (2014): Ward's Clustering",
     ylab="Squared Bray-Curtis Dsitance")

#B-diversity hypothesis testing####
#reorganizing sXspp matrix so it will line up better with factor vector
#will be using treatments (df, du, uf, uu) as distinct treatment groups
sXspp.treat<-sXspp.mtx[c("R1du","R2du","R3du","R4du","R5du","R6du",
                         "R1df","R2df","R3df","R4df","R5df","R6df",
                         "R1uu","R2uu","R3uu","R4uu","R5uu","R6uu",
                         "R1uf","R2uf","R3uf","R4uf","R5uf","R6uf"),]
#Will make three factor vectors; one for treat combo, one for each individually
#treatment combo factors
combo.factor<-c(rep("DU",6),rep("DF",6),rep("UU",6),rep("UF",6))
#disturbance factors
dist.factor<-c(rep("DIST",12),rep("UNDIST",12))
#fertilizer factors
fert.factor<-c(rep("UNFERT",6),rep("FERT",6),rep("UNFERT",6),rep("FERT",6))

#will use multivariate/categorical hypothesis tests to look for spp. association
#w/ particular treatments

#will start with full combo treatments
#PERMANOVA - does community comp. differ between treatment combos
adonis2(sXspp.treat~combo.factor,method="bray",permutations=999)
#Indicator Value - do any spp. presences indicate particular treatment combo
combo.indval<-multipatt(sXspp.treat,cluster=combo.factor,func="IndVal.g",
                        control=how(nperm=999))
summary(combo.indval)

#disturbance treatments
#PERMANOVA - does community comp. differ between disturbed and not
adonis2(sXspp.treat~dist.factor,method="bray",permutations=999)
#Indicator Value - do any spp. presences indicate disturbed v. undisturbed
dist.indval<-multipatt(sXspp.treat,cluster=dist.factor,func="IndVal.g",
                       control=how(nperm=999))
summary(dist.indval)

#fertilizer treatments
#PERMANOVA - does community comp. differ between fertilized and not
adonis2(sXspp.treat~fert.factor,method="bray",permutations=999)
#does not seem to show differences between fertilized and unfertilized 
#communities, so will focus on dist/undist and interaction treatments

#Principal coordinates analysis (ordination)####
#this should show organization of sites based on community comp. and 
#show which species are associated with each

#first need to create species codes to make ordination legible later
kbsspplist$spp.code<-c("A.theo","A.mill","A.retr","A.arte","A.cann","A.thal",
                       "A.syri","A.plat","A.pilo","A.sagi","B.vulg","B.iner",
                       "C.orbi","C.stoe","C.vulg","C.albu","C.escu","D.glom",
                       "D.caro","D.arme","D.isch","D.sang","E.repe","E.cili",
                       "E.annu","E.gram","H.sp","H.perf","J.tenu","L.corn",
                       "M.vert","O.stri","P.capi","P.dich","P.macu","P.arun",
                       "Ph.prat","P.comp","Po.prat","P.pens","P.oler","P.norv",
                       "P.rect","R.alle","R.flag","R.occi","R.cris","R.obtu",
                       "S.fabe","S.pumi","S.alba","S.sp","S.cana","T.offi",
                       "T.flav","T.camp","T.hybr","T.prat","T.repe","V.sp")
#reassigning column names so they'll be legible in the ordination
colnames(sXspp.treat)<-kbsspplist$spp.code

#pcoa code
#first need to build resemblance matrix (using Bray-Curtis %diff.)
kbs.db2<-vegdist(sXspp.treat,method="bray")
#doing pcoa test
kbs.pcoa<-cmdscale(kbs.db2,eig=TRUE,k=3)
#finding variance explained by pcoa
explvar1<-round(kbs.pcoa$eig[1]/sum(kbs.pcoa$eig),3)*100
explvar2<-round(kbs.pcoa$eig[2]/sum(kbs.pcoa$eig),3)*100
explvar3<-round(kbs.pcoa$eig[3]/sum(kbs.pcoa$eig),3)*100
sum.eig<-sum(explvar1,explvar2,explvar3)
#creating pcoa plot
#plot parameters
par(mar=c(5,5,1,2)+0.1)
#starting plot
plot(kbs.pcoa$points[,1],kbs.pcoa$points[,2],ylim=c(-1.0,1.0),
     xlim=c(-1.25,1.0),xlab=paste("PCoA 1 (", explvar1, "%)",sep=""),
     ylab=paste("PCoA 2 (", explvar2, "%)",sep=""),
     pch=16,cex=2.0,type="n",cex.lab=1.5,cex.axis=1.2,axes=FALSE)
#adding axes to plot
axis(side=1,labels=T,lwd.ticks=2,cex.axis=1.2,las=1)
axis(side=2,labels=T,lwd.ticks=2,cex.axis=1.2,las=1)
abline(h=0,v=0,lty=3)
box(lwd=2)
#adding points and labels
points(kbs.pcoa$points[,1],kbs.pcoa$points[,2],pch=19,cex=3.0,
       bg="gray",col="gray")
text(kbs.pcoa$points[,1],kbs.pcoa$points[,2],
     labels=row.names(kbs.pcoa$points))
#now finding gummy species scores so they can be included in ordination
#calculating relative abundances of spp. at each site
kbsREL<-sXspp.treat
for(i in 1:nrow(sXspp.treat)){
  kbsREL[i,]=sXspp.treat[i,]/sum(sXspp.treat[i,])
}
#using relative abundance to calculate and add spp. scores to figure
#reading in new spec.score function
source("C:/Users/tmzam/OneDrive/Documents/R/Functions_SourceCodes/spec.scores.function.R")
kbs.pcoa<-add.spec.scores.class(kbs.pcoa,kbsREL,method="pcoa.scores")
text(kbs.pcoa$cproj[,1],kbs.pcoa$cproj[,2],
     labels=row.names(kbs.pcoa$cproj),col="blue")
