
sir.test<-function(a, b, y, i0, r0, times){
  t <- seq(0, times, 1)
  len <- length(t)
  s <- numeric(len)
  i <- numeric(len)
  r <- numeric(len)
 
  for(ti in (1:(len-1)) ){

    s[1] <- (1- i0- r0)
    i[1] <- i0
    r[1] <- r0
    s[ti+1] <- (s[ti]-(a+b)*i[ti]*s[ti] - y*r[ti]*s[ti])
    i[ti+1] <- (a*i[ti]*s[ti]+i[ti])
    r[ti+1] <- (b*i[ti]*s[ti] + y*r[ti]*s[ti] +r[ti])
    
  }
  
  plot(t, ylim = c(0, 1.2), xlab = "times", ylab = "density")
  lines(t, s, ylim = c(0, 1.2), lwd =3, col= "blue")
  lines(t, i, ylim = c(0, 1.2), lwd =3,  col= "red")
  lines(t, r, ylim = c(0, 1.2), lwd =3,  col= "green")

}

sir.test(0.24, 0.08, 0.05, 0.02, 0, 40)







