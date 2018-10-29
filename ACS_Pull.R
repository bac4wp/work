

pop<-as.data.table(pop)
hh <- as.data.table(hh)
pop[,puma_num00:= st*1e5+puma00]


#pop[,puma_num:= ifelse(puma10<0, st*1e5+puma00, st*1e5+puma10)]
#pop[,puma_year:= ifelse(puma10<0, "2010", "2012")]
#pop[,puma_numyr:= paste0(puma_year, puma_num, collapse=NULL)]


#Age
pop$agep<-as.numeric(pop$agep)


#Race definition
pop[,Hispanic:= ifelse(hisp!=1,1,0)]
pop[,White:=ifelse((hisp==1)&(rac1p==1),1,0)]
pop[,AfAm:=ifelse((hisp==1)&(rac1p==2),1,0)]
pop[,Asian:=ifelse((hisp==1)&(rac1p %in% c(6,7)),1,0)]
pop[,Other_Race:=ifelse((hisp==1)&(rac1p %in% c(3,4,5,8,9)),1,0)]
pop[,Race := factor(White*1+Hispanic*2+AfAm*3+Asian*4+Other_Race*5, levels=1:5, 
                        labels=c("White", "Hispanic","AfAm","Asian","Other"))]


pop[,Accult:=ifelse((agep>=5)&(Hispanic==1)&(is.na(lanp)),1,0)]
pop[,Bicult:=ifelse((agep>=5)&(Hispanic==1)&(lanp==1200)&(eng==1),1,0)]
pop[,Unaccult:=ifelse((agep>=5)&(Hispanic==1)&(lanp==1200)&(eng%in%c(2,3,4)),1,0)]
pop[,Other_acc:=ifelse(Hispanic==1&Accult==0&Bicult==0&Unaccult==0,1,0)]
pop[,Acculturation:=ifelse((agep>=5)&(Hispanic==1)&(is.na(lanp)),"Accult", ifelse((agep>=5)&(Hispanic==1)&(lanp==625)&(eng==1),"Bicult", 
                                                                                      ifelse((agep>=5)&(Hispanic==1)&(lanp==625)&(eng%in%c(2,3,4)),"Unaccult",ifelse(Hispanic==1&Accult==0&Bicult==0&Unaccult==0,"Other","NH"))))]


pop[Hispanic==1 & Other_acc==1 ,sum(pwgtp)]
pop[Hispanic==1 & nativity==1 ,sum(pwgtp)]
pop[Hispanic==1 & agep>34 & Bicult==1 ,sum(pwgtp)]
pop[Hispanic==1 & agep>34 & Unaccult==1 ,sum(pwgtp)]
pop[Hispanic==1 & agep>17 & agep<35,sum(pwgtp)]
pop[Hispanic==1 & agep>34,sum(pwgtp)]


pop[,mom:=ifelse((paoc %in% c(1,2,3)),1,0)]


pop[Hispanic==1 & sex==2 & mom==1,sum(pwgtp)]
pop[White==1& sex==2 & mom==1,sum(pwgtp)]
pop[AfAm==1& sex==2 & mom==1,sum(pwgtp)]
pop[Asian==1& sex==2 & mom==1,sum(pwgtp)]
pop[Other_Race==1 & sex==2 & mom==1,sum(pwgtp)]

pop[Hispanic==1 & sex==2 & agep<35 &agep>17 ,sum(pwgtp)]
pop[White==1& sex==2& agep<35 & mom==1,sum(pwgtp)]
pop[AfAm==1& sex==2 & agep<35 & mom==1,sum(pwgtp)]
pop[Asian==1& sex==2 & agep<35 & mom==1,sum(pwgtp)]
pop[Other_Race==1& sex==2 & agep>17 ,sum(pwgtp)]


#hh family
pop_set <- subset(pop, select=c("agep", "serialno", "schg", "relp"))
pop_set[,under6:= ifelse(agep<6,1,0)]
pop_set[,to12:= ifelse(agep<12 &agep>5,1,0)]
pop_set[,under12:= ifelse(agep<13,1,0)]
pop_set[,to17:= ifelse(agep>13&agep<18,1,0)]
pop_set[,college:= ifelse(agep>17& schg %in% c(15,16),1,0)]
pop_set[,emptynest:= ifelse(agep>39&agep<61,1,0)]
pop_set[,spouse:= ifelse(relp==1,1,0)]
pop_set[,partner:= ifelse(relp==13,1,0)]

pop_out <- pop_set[,lapply(.SD, sum), by="serialno"]
pop_out2 <- subset(pop_out, select=c("serialno", "under6","to12","under12", "to17","college", "emptynest", "spouse","partner"))

#pop_out <- pop_out[order(pop_out$serialno),]
#pop_out$sumunder12 <- pop_out$under12
#pop_out$sumto17 <- pop_out$to17


popcomb<-join(pop,pop_out2,by="serialno")
combine<-join(pop,hh,by="serialno")
combine<-as.data.table(combine)


combine[,family:=ifelse((hht %in% c(1,2,3)),1,0)]
combine[,family:=ifelse((hht %in% c(1,2,3)),1,0)]

combine[,mom:=ifelse((family==1)&(relp==0)&(under12>0)&(to17==0),1,0)]
combine[,preK:=ifelse((family==1)&(relp==0)&(under6>0),1,0)]
combine[,tween:=ifelse((family==1)&(relp==0)&(to12>0),1,0)]
combine[,teen:=ifelse((family==1)&(relp==0)&(to17>0),1,0)]
combine[,collfam:=ifelse((family==1)&(relp==0)&(college>0),1,0)]
combine[,empnest:=ifelse((family==1)&(relp==0)&(under12==0)&(to17==0)&(agep>39)&(agep<61),1,0)]

combine[,coupm:=ifelse((relp==0)&(spouse>0)&(agep<40),1,0)]
combine[,coupum:=ifelse((relp==0)&(partner>0)&hht %in% c(4,5,6,7)&(agep<40),1,0)]


int_data<-subset(combine, select=c( "wgtp","Race", "youngfam", "preK", "tween" , "teen" , "collfam", "empnest","coupm","coupum"))

finaloutput <-aggregate(int_data$wgtp, by=list(Category=int_data$Race, int_data$youngfam, int_data$preK, int_data$tween, int_data$teen, int_data$collfam, 
                                 int_data$empnest, int_data$coupm, int_data$coupum ), FUN=sum)


combine[,own:=ifelse((ten %in% c (1,2)),1,0)]

combine[agep>17 &Hispanic==1&own==1 &relp==0,sum(wgtp)]
combine[agep>17  &White==1&own==1 &relp==0,sum(wgtp)]
combine[agep>17  &AfAm==1&own==1 &relp==0,sum(wgtp)]
combine[agep>17  &Asian==1&own==1 &relp==0,sum(wgtp)]
combine[agep>17  &Other_Race==1&own==1 &relp==0,sum(wgtp)]



combine[agep<40&relp==0&Hispanic==1&hht==1,sum(wgtp)]
combine[agep<40&relp==0&Hispanic==1&hht==1&coupm==0,sum(wgtp)]

combine[agep<40&relp==0&hht %in% c(4,5,6,7)&coupum==1&Hispanic==1 ,sum(wgtp)]
combine[agep<40&relp==0&coupum==1&Hispanic==1&hht==3 ,sum(wgtp)]

combine[agep<40&relp==0&coupum==1&Hispanic==1&hht  ,sum(wgtp)]


combine[(family==1)&(Hispanic==1)&(relp==0)&(under12==0)&(to17==0)&(agep>39)&(agep<61),sum(wgtp)]
combine[(family==1)&(Hispanic==1)&(relp==0)&(under12==0)&(to17==0),sum(wgtp)]
combine[(family==1)&(Hispanic==1)&(relp==0)&(under12>0)&(to17==0),sum(wgtp)]
combine[(family==1)&(Hispanic==1)&(relp==0)&(under12==0)&(to17>0),sum(wgtp)]
combine[(family==1)&(Hispanic==1)&(relp==0)&(under12>0)&(to17>0),sum(wgtp)]

combine[(family==1)&(Hispanic==1)&(relp==0)&(to17>0),sum(wgtp)]
combine[(family==1)&(Hispanic==1)&(relp==0),sum(wgtp)]

#income definition
combine$hincp<-as.numeric(combine$hincp)
combine$adjinc<-as.numeric(combine$adjinc)
combine[, inc:=hincp*(adjinc/1000000.0)]
combine[,Income_Bucket:= ifelse(inc<25000,"<$25K",ifelse(inc<75000,"$25K-$75K",ifelse(inc>=75000,">$75K",0)))]
combine[is.na(inc),sum(pwgtp)]
#age definition
combine[, Age_Bucket := ifelse(agep < 18,"0-17", ifelse(agep < 35, "18-34", 
                                                 ifelse(agep < 50, "35-49", 
                                                 ifelse(agep < 65, "50-64", 
                                                 ifelse(agep >= 65, "65+",0)))))]





combine[inc>74999&Hispanic==1&agep>17&manager==1,sum(pwgtp)]


combine$agep<-as.numeric(combine$agep)
combine[, mil := ifelse((agep>17)&(agep<35),1,0)]

combine[(mil==1)&(Hispanic==1),sum(pwgtp)]

combine$hht<-as.numeric(combine$hht)
combine[,Family:= ifelse(hht<4,1,0)]

combine[(Family==1) &(relp==0),sum(wgtp)]
#Race definition
combine[,Hispanic:= ifelse(hisp!=1,1,0)]
combine[,White:=ifelse((hisp==1)&(rac1p==1),1,0)]
combine[,AfAm:=ifelse((hisp==1)&(rac1p==2),1,0)]
combine[,Asian:=ifelse((hisp==1)&(rac1p==6),1,0)]
combine[,Other_Race:=ifelse((hisp==1)&(rac1p %in% c(3,4,5,7,8,9)),1,0)]
combine[,Race := factor(White*1+Hispanic*2+AfAm*3+Asian*4+Other_Race*5, levels=1:5, 
                        labels=c("White", "Hispanic","AfAm","Asian","Other"))]



#hh type definition
combine$hht<-as.numeric(combine$hht)
combine[,family:=ifelse((hht %in% c(1,2,3)),1,0)]
combine[,nonfamily:=ifelse((hht %in% c(4,5,6,7)),1,0)]
combine[,othhh:=ifelse(is.na(hht),1,0)]

combine[othmar==1,sum(pwgtp)]


#marital status definition
combine$mar<-as.numeric(combine$mar)
combine[,married:=ifelse(mar==1,1,0)]
combine[,single:=ifelse(mar==5,1,0)]
combine[,othmar:=ifelse((mar %in% c(2,3,4)),1,0)]


#Acculturation definition
combine$lanp<-as.numeric(combine$lanp)

combine[,Accult:=ifelse((agep>=5)&(Hispanic==1)&(is.na(lanp)),1,0)]
combine[,Bicult:=ifelse((agep>=5)&(Hispanic==1)&(lanp==625)&(eng==1),1,0)]
combine[,Unaccult:=ifelse((agep>=5)&(Hispanic==1)&(lanp==625)&(eng%in%c(2,3,4)),1,0)]
combine[,Other_acc:=ifelse(Hispanic==1&Accult==0&Bicult==0&Unaccult==0,1,0)]
combine[,Acculturation:=ifelse((agep>=5)&(Hispanic==1)&(is.na(lanp)),"Accult", 
                               ifelse((agep>=5)&(Hispanic==1)&(lanp==625)&(eng==1),"Bicult", 
                               ifelse((agep>=5)&(Hispanic==1)&(lanp==625)&(eng%in%c(2,3,4)),"Unaccult",
                               ifelse(Hispanic==1&Accult==0&Bicult==0&Unaccult==0,"Other","NH"))))]
#factor(Accult*1+Bicult*2+Unaccult*3+Other_acc*4, levels=1:4, labels=c("Acculturated", "Bicultural","Unacculturated","Other"))]

# Hispanic origin
combine[,Hisp_Orig:= ifelse(hisp == 2, "Mexican", ifelse(hisp == 3, "PR", ifelse(hisp == 4, "Cuban", ifelse(hisp == 5, "Dom",
                                              ifelse(hisp %in% 6:12, "C_American",
                                              ifelse(hisp %in% 13:22,"S_American", "Other"))))))]



###occp definition
combine$occp<-as.numeric(combine$occp)
combine[,manager := ifelse(as.numeric(occp)>0 & as.numeric(occp) <500, 1, 0)]
combine[,business := ifelse(as.numeric(occp)>=500 & as.numeric(occp) <800, 1, 0)]
combine[,finance :=  ifelse(as.numeric(occp)>=800 & as.numeric(occp) <1000, 1, 0)]
combine[,computers :=  ifelse(as.numeric(occp)>=1000 & as.numeric(occp) <1300, 1, 0)]
combine[,engineer :=  ifelse(as.numeric(occp)>=1300 & as.numeric(occp) <1600, 1, 0)]
combine[,science :=  ifelse(as.numeric(occp)>=1600 & as.numeric(occp) <2000, 1, 0)]
combine[,community :=  ifelse(as.numeric(occp)>=2000 & as.numeric(occp) <2100, 1, 0)]
combine[,legal :=  ifelse(as.numeric(occp)>=2100 & as.numeric(occp) <2200, 1, 0)]
combine[,education :=  ifelse(as.numeric(occp)>=2200 & as.numeric(occp) <2600, 1, 0)]
combine[,entertainment :=  ifelse(as.numeric(occp)>=2600 & as.numeric(occp) <3000, 1, 0)]
combine[,medical :=  ifelse(as.numeric(occp)>=3000 & as.numeric(occp) <3600, 1, 0)]
combine[,healthservices :=  ifelse(as.numeric(occp)>=3600 & as.numeric(occp) <3700, 1, 0)]
combine[,policefire :=  ifelse(as.numeric(occp)>=3700 & as.numeric(occp) <4000, 1, 0)]
combine[,foodservices :=  ifelse(as.numeric(occp)>=4000 & as.numeric(occp) <4200, 1, 0)]
combine[,cleaners :=  ifelse(as.numeric(occp)>=4200 & as.numeric(occp) <4300, 1, 0)]
combine[,personalservices :=  ifelse(as.numeric(occp)>=4300 & as.numeric(occp) <4700, 1, 0)]
combine[,sales :=  ifelse(as.numeric(occp)>=4700 & as.numeric(occp) < 5000, 1, 0)]
combine[,office :=  ifelse(as.numeric(occp) >= 5000 & as.numeric(occp) < 6000, 1, 0)]
combine[,agriculture :=  ifelse(as.numeric(occp) >= 6000 & as.numeric(occp) < 6200, 1, 0)]
combine[,construction :=  ifelse(as.numeric(occp) >= 6200 & as.numeric(occp) < 6800, 1, 0)]
combine[,extractive :=  ifelse(as.numeric(occp) >= 6800 & as.numeric(occp) < 7000, 1, 0)]
combine[,repair :=  ifelse(as.numeric(occp) >= 7000 & as.numeric(occp) < 7700, 1, 0)]
combine[,manufacturing :=  ifelse(as.numeric(occp) >= 7700 & as.numeric(occp) < 9000, 1, 0)]
combine[,transport :=  ifelse(as.numeric(occp) >= 9000 & as.numeric(occp) < 9800, 1, 0)]
combine[,military :=  ifelse(as.numeric(occp) >= 9800 & as.numeric(occp) < 9920, 1, 0)]
combine[,NILF := ifelse((is.na(as.numeric(occp))|as.numeric(occp)==9920),1,0)]


combine[managers==1&Hispanic==1&agep>17&inc>74999,sum(pwgtp)]



# segment definitions

int_data<-subset(combine, select=c("Hispanic", "wgtp","agep", "under12", "under12"  #"Hisp_Orig"#,"Acculturation", "Hisp_Orig"
                                   #"manager","business","finance","computers","engineer","science","community","legal","education","entertainment",
                                   #"medical","healthservices","policefire","foodservices","cleaners","personalservices","sales","office","agriculture",
                                   #"construction","extractive","repair","manufacturing","transport","military","NILF"
                                   ))

int_data[,USHisp:=ifelse(Hispanic==1&nativity==1,pwgtp,0)]
int_data[,FBHisp:=ifelse(Hispanic==1&nativity==2,pwgtp,0)]
int_data[,NHisp:=ifelse(Hispanic==0,pwgtp,0)]



int_data[,Hisp18:=ifelse(Hispanic==1&agep>17,pwgtp,0)]
int_data[,NHisp18:=ifelse(Hispanic==0&agep>17,pwgtp,0)]
int_data[,Hisp75k:=ifelse(Hispanic==1&agep>17& inc>74999 &!is.na(inc),pwgtp,0)]
int_data[,NHisp75k:=ifelse(Hispanic==0&agep>17& inc>74999 &!is.na(inc),pwgtp,0)]

int_data[,Hisp_acc:=ifelse(Hispanic==1& agep>17 &inc>74999 &!is.na(inc) & Acculturation=="Accult",pwgtp,0)]
int_data[,Hisp_bic:=ifelse(Hispanic==1& agep>17 &inc>74999 &!is.na(inc) & Acculturation=="Bicult",pwgtp,0)]
int_data[,Hisp_unac:=ifelse(Hispanic==1& agep>17 &inc>74999 &!is.na(inc) & Acculturation=="Unaccult",pwgtp,0)]

int_data[,Hisp_mex:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & Hisp_Orig=="Mexican",pwgtp,0)]
int_data[,Hisp_pr:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & Hisp_Orig=="PR",pwgtp,0)]
int_data[,Hisp_cub:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & Hisp_Orig=="Cuban",pwgtp,0)]
int_data[,Hisp_dom:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & Hisp_Orig=="Dom",pwgtp,0)]
int_data[,Hisp_cam:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & Hisp_Orig=="C_American",pwgtp,0)]
int_data[,Hisp_sam:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & Hisp_Orig=="S_American",pwgtp,0)]
int_data[,Hisp_oth:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & Hisp_Orig=="Other",pwgtp,0)]

int_data[,Hisp_manager:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & manager==1&!is.na(manager) ,pwgtp,0)]
int_data[,Hisp_business:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & business==1&!is.na(business) ,pwgtp,0)]
int_data[,Hisp_finance:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & finance==1&!is.na(finance) ,pwgtp,0)]
int_data[,Hisp_computers:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & computers==1&!is.na(computers) ,pwgtp,0)]
int_data[,Hisp_engineer:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & engineer==1&!is.na(engineer) ,pwgtp,0)]
int_data[,Hisp_science:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & science==1&!is.na(science) ,pwgtp,0)]
int_data[,Hisp_community:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & community==1&!is.na(community) ,pwgtp,0)]
int_data[,Hisp_legal:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & legal==1&!is.na(legal) ,pwgtp,0)]
int_data[,Hisp_education:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & education==1&!is.na(education) ,pwgtp,0)]
int_data[,Hisp_entertainment:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & entertainment==1&!is.na(entertainment) ,pwgtp,0)]
int_data[,Hisp_medical:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & medical==1&!is.na(medical) ,pwgtp,0)]
int_data[,Hisp_healthservices:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & healthservices==1&!is.na(healthservices) ,pwgtp,0)]
int_data[,Hisp_policefire:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & policefire==1&!is.na(policefire) ,pwgtp,0)]
int_data[,Hisp_foodservices:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & foodservices==1&!is.na(foodservices) ,pwgtp,0)]
int_data[,Hisp_cleaners:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & cleaners==1&!is.na(cleaners) ,pwgtp,0)]
int_data[,Hisp_personalservices:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & personalservices==1&!is.na(personalservices) ,pwgtp,0)]
int_data[,Hisp_sales:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & sales==1&!is.na(sales) ,pwgtp,0)]
int_data[,Hisp_office:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & office==1&!is.na(office) ,pwgtp,0)]
int_data[,Hisp_agriculture:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & agriculture==1&!is.na(agriculture) ,pwgtp,0)]
int_data[,Hisp_construction:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & construction==1&!is.na(construction) ,pwgtp,0)]
int_data[,Hisp_extractive:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & extractive==1&!is.na(extractive) ,pwgtp,0)]
int_data[,Hisp_repair:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & repair==1&!is.na(repair) ,pwgtp,0)]
int_data[,Hisp_manufacturing:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & manufacturing==1&!is.na(manufacturing) ,pwgtp,0)]
int_data[,Hisp_transport:=ifelse(Hispanic==1& agep>17 &inc>74999&!is.na(inc) & transport==1&!is.na(transport) ,pwgtp,0)]
int_data[,Hisp_military:=ifelse(Hispanic==1& agep>17 &inc>74999 &!is.na(inc) & military==1&!is.na(military) ,pwgtp,0)]
int_data[,Hisp_NILF:=ifelse(Hispanic==1& agep>17 &inc>74999 &!is.na(inc) & NILF==1&!is.na(NILF) ,pwgtp,0)]




puma_set <- subset(int_data, select=c("USHisp", "pwgtp","FBHisp", "NHisp" ,"agep" #"Hisp_acc", "Hisp_bic", "Hisp_unac", "Hisp_mex",
                                      #"Hisp_pr","Hisp_cub","Hisp_dom","Hisp_cam","Hisp_sam","Hisp_oth"#"Hisp_manager","Hisp_business","Hisp_finance", "Hisp_computers",
                                      #"Hisp_engineer","Hisp_science","Hisp_community","Hisp_legal","Hisp_education",
                                      #"Hisp_entertainment","Hisp_medical","Hisp_healthservices","Hisp_policefire","Hisp_foodservices",
                                      #"Hisp_cleaners","Hisp_personalservices","Hisp_sales","Hisp_office","Hisp_agriculture","Hisp_construction",
                                      #"Hisp_extractive","Hisp_repair","Hisp_manufacturing","Hisp_transport","Hisp_military","Hisp_NILF"
                                      ))

puma_out <- puma_set[,lapply(.SD, sum), by="agep"]

write.csv(puma_out, "puma_out.csv")



sum(int_data$Hisp18)
sum(int_data$Hisp_engineer)
int_data[,sum(Hisp75k)]

#test
combine[Hisp_Orig == "Mexican", sum(pwgtp)]
head(combine)


#aggregate
pp<-subset(combine,select=c("Age_Bucket","Race","Income_Bucket", "Acculturation", "pwgtp"))
pp<-pp[, lapply(.SD, sum), by=list(Race, Age_Bucket, Income_Bucket, Acculturation), .SDcols="pwgtp"]


#export
write.csv(pp, "Pop Inputs by Acculturation.csv")


#immig<-subset(p,yoep==2011,select=c("agep","sex","Hispanic","Acculturation", "pwgtp"))
#toclip(xtabs(immig$pwgtp~immig$Acculturation+immig$agep+immig$sex, immig))
#toclip(xtabs(pwgtp~immig$Race, immig))
#immig<-immig[order(immig$agep,immig$sex)]
#head(immig)
#immig_out<-dcast(immig, agep+sex+Hispanic~Acculturation, sum)
#write.csv(immig_out,"immig_acc_dist.csv")

#pp<-subset(combine,select=c("agep","Race","Acculturation","pwgtp"))
#pp<-pp[order(pp$agep,pp$Race,pp$Acculturation),]
#pp2<-dcast(pp,agep+Acculturation+Race, sum)
#head(pp2)
#ppac<-dcast(pp[Race=="Hispanic"],agep+Race~Acculturation,sum)
#head(ppac)
#pptot<-cbind(pp2,ppac[,3:6])


#set2<-aggregate(cbind(Acculturation, Race, age_Bucket, Hisp_exch, Hisp_200, Hisp_400)
#                     ~puma_numyr+st,data=puma_set,sum)




#setwd("C:\\Users\\Neeraj\\Documents")

#combine[,Age_Bucket:= ifelse(agep<18,"0-17",ifelse(agep<35, "18-34", ifelse(agep<50,"35-49",ifelse(agep<65,"50-64",ifelse(agep>=65,"65+",0)))))]
#head(combine)

#setwd("C:/Users/Cristina/Documents/Pop Model/Inputs")
#st_name<-read.csv("State_num_names.csv")
#combine<-join(combine,st_name,by="st")

#poptot<-subset(combine,select=c("State","Race","Income_Bucket","Age_Bucket","pwgtp"))
#names(poptot)[1]<-"StateName"
#unique(poptot$Income_Bucket)
#head(poptot)
#poptot<-aggregate(.~StateName+Race+Income_Bucket+Age_Bucket,poptot,sum)
#sum(poptot$pwgtp)
