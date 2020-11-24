model
{
  for (i in 1:n)
  {
    # Linear predictors for individual traits
    height[i] <- ter[1,1,species[i]] +
      ter[1,2,species[i]]*NH4[i]  + ter[1,3,species[i]]*P[i]        +
      ter[1,4,species[i]]*K[i]    + ter[1,5,species[i]]*SALINITY[i] +
      ter[1,6,species[i]]*SILT[i] + ter[1,7,species[i]]*PH[i]       +
      ter[1,8,species[i]]*URP[i]  + ter[1,9,species[i]]*HH[i]

    sla[i]    <- ter[2,1,species[i]] +
      ter[2,2,species[i]]*NH4[i]  + ter[2,3,species[i]]*P[i]        +
      ter[2,4,species[i]]*K[i]    + ter[2,5,species[i]]*SALINITY[i] +
      ter[2,6,species[i]]*SILT[i] + ter[2,7,species[i]]*PH[i]       +
      ter[2,8,species[i]]*URP[i]  + ter[2,9,species[i]]*HH[i]

    wd[i]     <- ter[3,1,species[i]] +
      ter[3,2,species[i]]*NH4[i]  + ter[3,3,species[i]]*P[i]        +
      ter[3,4,species[i]]*K[i]    + ter[3,5,species[i]]*SALINITY[i] +
      ter[3,6,species[i]]*SILT[i] + ter[3,7,species[i]]*PH[i]       +
      ter[3,8,species[i]]*URP[i]  + ter[3,9,species[i]]*HH[i]
    sc[i]     <- ter[4,1,species[i]] +
      ter[4,2,species[i]]*NH4[i]  + ter[4,3,species[i]]*P[i] +
      ter[4,4,species[i]]*K[i]    + ter[4,5,species[i]]*SALINITY[i] +
      ter[4,6,species[i]]*SILT[i] + ter[4,7,species[i]]*PH[i]       +
      ter[4,8,species[i]]*URP[i]  + ter[4,9,species[i]]*HH[i]

    # Definition of the likelihood
    mu[i,1]<-height[i]
    mu[i,2]<-sla[i]
    mu[i,3]<-wd[i]
    mu[i,4]<-sc[i]

    traits[i,]~dmnorm(mu[i,], omega[species[i],,])
  }

  # species-specific TER model
%s

  # species-species TTR model
%s

  # data # n, nt, ne, traits, NH4, P, K, SALINITY, SILT, PH, URP, HH, ns, species
}
