model
{
  for (i in 1:n)
  {
    # Linear predictors for individual traits
    height[i] <- avter[1,1] +
      avter[1,2]*NH4[i]  + avter[1,3]*P[i]        +
      avter[1,4]*K[i]    + avter[1,5]*SALINITY[i] +
      avter[1,6]*SILT[i] + avter[1,7]*PH[i]       +
      avter[1,8]*URP[i]  + avter[1,9]*HH[i]

    sla[i]    <- avter[2,1] +
      avter[2,2]*NH4[i]  + avter[2,3]*P[i]        +
      avter[2,4]*K[i]    + avter[2,5]*SALINITY[i] +
      avter[2,6]*SILT[i] + avter[2,7]*PH[i]       +
      avter[2,8]*URP[i]  + avter[2,9]*HH[i]

    wd[i]     <- avter[3,1] +
      avter[3,2]*NH4[i]  + avter[3,3]*P[i]        +
      avter[3,4]*K[i]    + avter[3,5]*SALINITY[i] +
      avter[3,6]*SILT[i] + avter[3,7]*PH[i]       +
      avter[3,8]*URP[i]  + avter[3,9]*HH[i]

    sc[i]     <- avter[4,1] +
      avter[4,2]*NH4[i]  + avter[4,3]*P[i]        +
      avter[4,4]*K[i]    + avter[4,5]*SALINITY[i] +
      avter[4,6]*SILT[i] + avter[4,7]*PH[i]       +
      avter[4,8]*URP[i]  + avter[4,9]*HH[i]

    # Definition of the likelihood
    mu[i,1]<-height[i]
    mu[i,2]<-sla[i]
    mu[i,3]<-wd[i]
    mu[i,4]<-sc[i]

    traits[i,]~dmnorm(mu[i,], avomega[,])
  }

  # Single TER model
%s

  # Single TTR model
%s

  # data # n, nt, ne, traits, NH4, P, K, SALINITY, SILT, PH, URP, HH, ttrI
}
