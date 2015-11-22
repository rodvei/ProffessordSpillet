# 0= blue, 1=green, 2=purple, 3=burgunder

cardsCol<-cbind(c(0,1,2,1),c(1,3,2,1),c(2,3,0,3),c(1,1,2,3),c(2,1,0,3),c(0,1,1,3),c(0,3,2,1),c(2,1,0,3),c(2,3,0,1),c(0,2,1,3),c(1,0,2,3),c(3,2,0,1),c(1,3,2,0),c(2,0,3,1),c(3,3,0,0),c(2,1,3,3))
# 1=head, 0=bottum
cardsPar<-c(0,1,1,0)

cards<-matrix(0,4,4)
cardspos<-matrix(0,4,4)
card<-1:16
order<-cbind(c(1:4),c(4,1:3),c(3,4,1,2),c(2:4,1))

numOfInt<-1


for(a in card){
  for(ar in 1:4){
    
    for(b in card[-a]){
      for(br in 1:4){
        if(cardsCol[order[,ar],a][2]!=cardsCol[order[,br],b][4]){next}
        if(cardsPar[order[,ar]][2]==cardsPar[order[,br]][4]){next}
          
        for(c in card[c(-a,-b)]){
          for(cr in 1:4){
            if(cardsCol[order[,br],b][2]!=cardsCol[order[,cr],c][4]){next}
            if(cardsPar[order[,br]][2]==cardsPar[order[,cr]][4]){next}
              
            for(d in card[c(-a,-b,-c)]){
              for(dr in 1:4){
                if(cardsCol[order[,cr],c][2]!=cardsCol[order[,dr],d][4]){next}
                if(cardsPar[order[,cr]][2]==cardsPar[order[,dr]][4]){next}
                  
                for(e in card[c(-a,-b,-c,-d)]){
                  for(er in 1:4){
                    if(cardsCol[order[,ar],a][3]!=cardsCol[order[,er],e][1]){next}
                    if(cardsPar[order[,ar]][3]==cardsPar[order[,er]][1]){next}
                      
                    for(f in card[c(-a,-b,-c,-d,-e)]){
                      for(fr in 1:4){
                        if(cardsCol[order[,br],b][3]!=cardsCol[order[,fr],f][1]){next}
                        if(cardsCol[order[,er],e][2]!=cardsCol[order[,fr],f][4]){next}
                        if(cardsPar[order[,br]][3]==cardsPar[order[,fr]][1]){next}
                        if(cardsPar[order[,er]][2]==cardsPar[order[,fr]][4]){next}
                          
                        for(g in card[c(-a,-b,-c,-d,-e,-f)]){
                          for(gr in 1:4){
                            if(cardsCol[order[,cr],c][3]!=cardsCol[order[,gr],g][1]){next}
                            if(cardsCol[order[,fr],f][2]!=cardsCol[order[,gr],g][4]){next}
                            if(cardsPar[order[,cr]][3]==cardsPar[order[,gr]][1]){next}
                            if(cardsPar[order[,fr]][2]==cardsPar[order[,gr]][4]){next}
                              
                            for(h in card[c(-a,-b,-c,-d,-e,-f,-g)]){
                              for(hr in 1:4){
                                if(cardsCol[order[,dr],d][3]!=cardsCol[order[,hr],h][1]){next}
                                if(cardsCol[order[,gr],g][2]!=cardsCol[order[,hr],h][4]){next}
                                if(cardsPar[order[,dr]][3]==cardsPar[order[,hr]][1]){next}
                                if(cardsPar[order[,gr]][2]==cardsPar[order[,hr]][4]){next}
                                  
                                for(i in card[c(-a,-b,-c,-d,-e,-f,-g,-h)]){
                                  for(ir in 1:4){
                                    if(cardsCol[order[,er],e][3]!=cardsCol[order[,ir],i][1]){next}
                                    if(cardsPar[order[,er]][3]==cardsPar[order[,ir]][1]){next}
                                      
                                    for(j in card[c(-a,-b,-c,-d,-e,-f,-g,-h,-i)]){
                                      for(jr in 1:4){
                                        if(cardsCol[order[,fr],f][3]!=cardsCol[order[,jr],j][1]){next}
                                        if(cardsCol[order[,ir],i][2]!=cardsCol[order[,jr],j][4]){next}
                                        if(cardsPar[order[,fr]][3]==cardsPar[order[,jr]][1]){next}
                                        if(cardsPar[order[,ir]][2]==cardsPar[order[,jr]][4]){next}
                                          
                                        for(k in card[c(-a,-b,-c,-d,-e,-f,-g,-h,-i,-j)]){
                                          for(kr in 1:4){
                                            if(cardsCol[order[,gr],g][3]!=cardsCol[order[,kr],k][1]){next}
                                            if(cardsCol[order[,jr],j][2]!=cardsCol[order[,kr],k][4]){next}
                                            if(cardsPar[order[,gr]][3]==cardsPar[order[,kr]][1]){next}
                                            if(cardsPar[order[,jr]][2]==cardsPar[order[,kr]][4]){next}
                                              
                                            for(l in card[c(-a,-b,-c,-d,-e,-f,-g,-h,-i,-j,-k)]){
                                              for(lr in 1:4){
                                                if(cardsCol[order[,hr],h][3]!=cardsCol[order[,lr],l][1]){next}
                                                if(cardsCol[order[,kr],k][2]!=cardsCol[order[,lr],l][4]){next}
                                                if(cardsPar[order[,hr]][3]==cardsPar[order[,lr]][1]){next}
                                                if(cardsPar[order[,kr]][2]==cardsPar[order[,lr]][4]){next}
                                                  
                                                for(m in card[c(-a,-b,-c,-d,-e,-f,-g,-h,-i,-j,-k,-l)]){
                                                  for(mr in 1:4){
                                                    if(cardsCol[order[,ir],i][3]!=cardsCol[order[,mr],m][1]){next}
                                                    if(cardsPar[order[,ir]][3]==cardsPar[order[,mr]][1]){next}
                                                      
                                                    for(n in card[c(-a,-b,-c,-d,-e,-f,-g,-h,-i,-j,-k,-l,-m)]){
                                                      for(nr in 1:4){
                                                        if(cardsCol[order[,jr],j][3]!=cardsCol[order[,nr],n][1]){next}
                                                        if(cardsCol[order[,mr],m][2]!=cardsCol[order[,nr],n][4]){next}
                                                        if(cardsPar[order[,jr]][3]==cardsPar[order[,nr]][1]){next}
                                                        if(cardsPar[order[,mr]][2]==cardsPar[order[,nr]][4]){next}
                                                          
                                                        for(o in card[c(-a,-b,-c,-d,-e,-f,-g,-h,-i,-j,-k,-l,-m,-n)]){
                                                          for(or in 1:4){
                                                            if(cardsCol[order[,kr],k][3]!=cardsCol[order[,or],o][1]){next}
                                                            if(cardsCol[order[,nr],n][2]!=cardsCol[order[,or],o][4]){next}
                                                            if(cardsPar[order[,kr]][3]==cardsPar[order[,or]][1]){next}
                                                            if(cardsPar[order[,nr]][2]==cardsPar[order[,or]][4]){next}
                                                              
                                                            p<-card[c(-a,-b,-c,-d,-e,-f,-g,-h,-i,-j,-k,-l,-m,-n,-o)]
                                                            for(pr in 1:4){
                                                              if(cardsCol[order[,lr],l][3]!=cardsCol[order[,pr],p][1]){next}
                                                              if(cardsCol[order[,or],o][2]!=cardsCol[order[,pr],p][4]){next}
                                                              if(cardsPar[order[,lr]][3]==cardsPar[order[,pr]][1]){next}
                                                              if(cardsPar[order[,or]][2]==cardsPar[order[,pr]][4]){next}
                                                              cat("Answer Number",numOfInt,"\n")
                                                              cat(a,ar," ",b,br," ",c,cr," ",d,dr,"\n")
                                                              cat(e,er," ",f,fr," ",g,gr," ",h,hr,"\n")
                                                              cat(i,ir," ",j,jr," ",k,kr," ",l,lr,"\n")
                                                              cat(m,mr," ",n,nr," ",o,or," ",p,pr,"\n")
                                                              numOfInt<-numOfInt+1
                                                              #readline("Press <return to continue")
                                                            }
                                                          #p
                                                          }
                                                        }#o
                                                      }
                                                    }#n
                                                  }
                                                }#m
                                              }
                                            }#l
                                          }
                                        }#k
                                      }
                                    }#j
                                  }
                                }#i
                              }
                            }#h
                          }
                        }#g
                      }
                    }#f
                  }
                }#e
              }
            }
          }
        }
      }
    }
  }
}
# a=1;ar=1
# b=2;br=1
# c=3;cr=1
# d=4;dr=1
# e=5;er=1
# f=6;fr=1
# g=7;gr=1
# h=8;hr=1
# i=9;ir=1
# j=10;jr=1
# k=11;kr=1
# l=12;lr=1
# m=13;mr=1
# n=14;nr=1
# o=15;or=1
# p=16;pr=1