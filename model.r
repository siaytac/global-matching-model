
# Osth & Dennis (2015) - Global Matching Model: Item Memory
osth <- function(l,ritem,Mss,Mtt,Vss,Vtt,Vti,p,beta) {
  
  mOld <- ritem*Mss*Mtt
  mNew <- 0
  
  vOld <- ritem^2*(Mss^2*Vtt + Mtt^2*Vss + Vss*Vtt) + #self match
    ritem^2*(l-1)*(Mss^2*Vti + Vss*Vti)             + #item noise
    (Mtt^2*p + p*Vtt)                               + #context noise
    beta                                              #background noise
  
  vNew <- ritem^2*l*(Mss^2*Vti + Vss*Vti) + #item noise
    (Mtt^2*p + p*Vtt)                     + #context noise
    beta                                    #background noise
  
  return(c(mOld=mOld, mNew=mNew, vOld=vOld, vNew=vNew))
}