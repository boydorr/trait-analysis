  # Setting up the precision matrix
  avomega ~ dwish(ttrI, nt + 1)
  # Conversion of precision matrix into var-cov matrix
  avsigma <- inverse(avomega)

  # monitor # avsigma
