  # Setting up the precision matrices
  avomega ~ dwish(ttrI, nt + 1)
  # Conversion of precision matrix into var-cov matrix
  avsigma <- inverse(avomega)
  for (m in 1:ns)
  {
    omega[m,1:nt,1:nt] ~ dwish(avsigma, nt + 1)
    # Conversion of precision matrix into var-cov matrix
    sigma[m,1:nt,1:nt] <- inverse(omega[m,1:nt,1:nt])
  }
  # monitor # sigma, avsigma
