
# Osth & Dennis (2015) source memory extension of their global matching model; Osth et al. (2018)
osthSource <- function(l,ritem,rsource,nitem,Mss,Mtt,Maa,Mbb,Vss,Vtt,Vti,Vsu,Vaa,Vbb,Vab,Vba,Vac,Vbc) {
  
  nitem <- nitem # number of prior occurrences of items
  na=nb <- nitem*.05
  nc <- nitem*.9
  
  mitem <- 10e6 # total number of background memories; fixed to 10e6
  ma=mb <- mitem*.05
  mc <- mitem*.9
  
  
  #Source A distribution
  maa <- rsource*Mtt*Mss*Maa # mean of the source A distribution when source A is a cue
  mba <- 0                   # mean of the source A distribution when source B is a cue
  
  vaa <- rsource^2*(Vtt + Mtt^2)*(Vss + Mss^2)*(Vaa + Maa^2) - (Mtt^2*Mss^2*Maa^2) + # self match AA
    rsource^2*(l/2-1)*(Vti*(Vss + Mss^2)*(Vaa + Maa^2))                       + # item noise AA
    l/2*rsource^2*(Vti*Vab*(Vss + Mss^2))                                     + # item noise AB
    na*(Vsu*(Vtt + Mtt^2)*(Vaa + Maa^2))                                      + # context noise AA
    nb*(Vsu*(Vtt*Vab))                                                        + # context noise AB
    nc*(Vsu*(Vtt*Vac))                                                        + # context noise AC (other sources)
    ma*((Vti*Vsu)*(Vaa + Maa^2))                                              + # background noise AA
    mb*(Vti*Vsu*Vab)                                                          + # background noise AB
    mc*(Vti*Vsu*Vac)                                                            # background noise AC
  
  vba <- rsource^2*(Vba*(Vtt + Mtt^2)*(Vss + Mss^2)) + # self match
    rsource^2*(l/2-1)*(Vti*Vba*(Vss + Mss^2))   + # item noise
    l/2*rsource^2*(Vti*Vbb*(Vss + Mss^2))       + # item noise
    na*(Vsu*Vba*(Vtt + Mtt^2))                  + # context noise
    nb*(Vsu*(Vtt + Mtt^2)*(Vbb + Mbb^2))        + # context noise
    nc*(Vsu*Vbc*(Vtt + Mtt^2))                  + # context noise
    ma*(Vti*Vsu*Vba)                            + # background noise
    mb*(Vti*Vsu*(Vbb + Mbb^2))                  + # background noise
    mc*(Vti*Vsu*Vbc)                              # background noise
  
  
  #Source B distribution
  mbb <- rsource*Mtt*Mss*Mbb # mean of the source B distribution when source B is a cue
  mab <- 0                   # mean of the source B distribution when source A is a cue
  
  vbb <- rsource^2*(Vtt + Mtt^2)*(Vss + Mss^2)*(Vbb + Mbb^2) - (Mtt^2*Mss^2*Mbb^2) + # self match
    rsource^2*(l/2-1)*(Vti*(Vss + Mss^2)*(Vbb + Mbb^2))                       + # item noise
    l/2*rsource^2*(Vti*Vba*(Vss + Mss^2))                                     + # item noise
    na*(Vsu*(Vtt + Mtt^2)*(Vbb + Mbb^2))                                      + # context noise
    nb*(Vsu*(Vtt*Vba))                                                        + # context noise
    nc*(Vsu*(Vtt*Vbc))                                                        + # context noise
    ma*((Vti*Vsu)*(Vbb + Mbb^2))                                              + # background noise
    mb*(Vti*Vsu*Vba)                                                          + # background noise
    mc*(Vti*Vsu*Vbc)                                                            # background noise
  
  vab <- rsource^2*(Vab*(Vtt + Mtt^2)*(Vss + Mss^2)) + # self match
    rsource^2*(l/2-1)*(Vti*Vab*(Vss + Mss^2))   + # item noise
    l/2*rsource^2*(Vti*Vaa*(Vss + Mss^2))       + # item noise
    na*(Vsu*Vab*(Vtt + Mtt^2))                  + # context noise
    nb*(Vsu*(Vtt + Mtt^2)*(Vaa + Maa^2))        + # context noise
    nc*(Vsu*Vac*(Vtt + Mtt^2))                  + # context noise
    ma*(Vti*Vsu*Vab)                            + # background noise
    mb*(Vti*Vsu*(Vaa + Maa^2))                  + # background noise
    mc*(Vti*Vsu*Vac)                              # background noise
  
  #Old-New Distributions
  mOld <- ritem*Mtt*Mss
  mNew <- 0
  
  vOld <- ritem^2*((Vtt + Mtt^2)*(Vss + Mss^2)-(Mtt^2*Mss^2)) + # self match
    ritem^2*(l-1)*(Vti*(Vss + Mss^2))                         + # item noise
    nitem*(Vsu*(Vtt + Mtt^2))                                 + # context noise
    mitem*(Vti*Vsu)                                             # background noise
  
  vNew <- ritem^2*l*(Vti*(Vss + Mss^2)) + # item noise
    nitem*(Vsu*(Vtt + Mtt^2))           + # context noise
    mitem*(Vti*Vsu)                       # background noise
  
  return(c(mOld=mOld, mNew=mNew, vOld=vOld, vNew=vNew, 
           mA=maa-mba, mB=mab-mbb, vA=vaa+vba, vB=vbb+vab))
  
}
