  # Setting up the regression coefficients matrix
  avtertau ~ dgamma(0.0001,0.01)
  tertau ~ dgamma(0.0001,0.01)

  for (k in 1:ne) # Number of environmental drivers
  {
    for (j in 1:nt) # Number of traits
    {
      avter[j,k] ~ dnorm(0,avtertau)

      for (m in 1:ns) # Number of species
      {
        ter[j,k,m] ~ dnorm(avter[j,k],tertau)
      }
    }
  }
  # monitor # ter, avter, tertau, avtertau
