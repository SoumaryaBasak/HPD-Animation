########## Animation for Credible set
#### Binomial Beta Example

### x|theta follows bin(n,theta)
### prior:  theta follows beta (a,b)
### fix: x_star= 2
### posterior:  theta|x_star follows beta  ( n+a , n-x*+b )



hpd_animation<- function(a,b,n,x_star){  
  
  # (a,b) = hyper parameters
  # n= no of trails in Binomial model
  # x_star= fix x*
  
  set.seed(77)
  ##prior
  theta<-rbeta(100,a,b)
  theta_r<-sort(theta)
  d_theta<-dbeta(theta_r,2,2)
  
  ### prior distribution
  #plot(theta_r,d_theta,'l',main = "Prior distribution of theta",col="red",ylab=" ")
  
  
  ### Posterior 
  a_dash<- a+x_star
  b_dash<- n-x_star+b
  
  z<-rbeta(20000,a_dash,b_dash)  ## z : posterior theta
  ## here Beta (4,3)
  z_r<-sort(z)
  d_z <- dbeta(z_r,a_dash,b_dash)
  d_z<-round(d_z,3)
  
  
  ### Animation 
  
  plot(z_r,d_z,'l',lwd=2,
       col="blue",
       ylab = " ",xlab = expression(theta~" | data"),
       xlim = c(-0.1,(max(z_r)+0.1)),
       main = expression("HPD Cdredible Set for"~theta~"|data for Binomial Beta Example"),
       sub = paste("Prior: Beta(",a,",",b,") ; Posterior: Beta(",a_dash,",",b_dash,")")
  )
  
  Sys.sleep(1)
  
  
  k1<- seq(0.1,2,by=0.07)
  l<-c()
  u<-c()
  v<-c() #set of cof_int
  
  for(i in 1:length(k1)) {
    
    l1<- z_r[min(which(d_z>k1[i]))]
    u1<- z_r[max(which(d_z>k1[i]))]
    c1<- ((sum(l1<=z_r & z_r <=u1))/length(z_r))*100
    c1  # the cont_level
    
    l<-c(l,l1)
    u<-c(u,u1)
    v<-c(v,c1)
    
    if(c1<=94.3){
      break
    }
    
    y<- rep(k1[i],length(z_r))
    lines(z_r,y,lty=2)
    text(0.0,k1[i],labels=paste("k1=",k1[i]))
    text(max(z_r)+0.05,k1[i],labels=paste("Arear=",c1))
    
    
    
    polygon( c(z_r[l1<=z_r &z_r<=u1],
               rev(z_r[l1<=z_r & z_r<=u1])),
             c(y[l1<=z_r & z_r<=u1],
               rev(d_z[l1<=z_r & z_r<=u1])),
             col=rgb(0,0.6,0,alpha = i/(length(k1))),
             # density=18,
             # angle=45
    )
    Sys.sleep(1.5)
    if(c1<95){
      polygon( c(z_r[l1<=z_r &z_r<=u1],
                 rev(z_r[l1<=z_r & z_r<=u1])),
               c(y[l1<=z_r & z_r<=u1],
                 rev(d_z[l1<=z_r & z_r<=u1])),
               col=rgb(1,0.2,0.2 ))
      
    }
  }
  
  #l  # collection of lower bounds
  #u  # collection of upper bounds
  #v  # collection of conf. coeff.
  
  
  j<-max(which(v>=95))
  l[j]
  u[j]
  k1[j]
  
  lines(rep(l[j],2),c(-1,k1[j]),col='red',lwd=2)
  text(l[j]+0.05,0,labels = paste( round(l[j],3)) )
  lines(rep(u[j],2),c(-1,k1[j]),col='red',lwd=2)
  text(u[j]-0.05,0,labels = paste( round(u[j],3)) )
  
  
  
  
}

# (a,b) = hyper parameters
# n= no of trails in Binomial model
# x_star= fix x*



hpd_animation(2,2,5,2)

hpd_animation(2,2,10,5)

hpd_animation(1,1,7,5)

hpd_animation(0.5,0.2,5,3)

