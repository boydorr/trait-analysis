  # Setting up the regression coefficients matrix
  avtertau ~ dgamma(0.0001,0.01)

  for (k in 1:ne) # Number of environmental drivers
  {

    for (j in 1:nt) # Number of traits
    {
      avter[j,k] ~ dnorm(0,avtertau)
    }
  }
  # monitor # avter, avtertau
