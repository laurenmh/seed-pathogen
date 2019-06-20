## Read in Hunter's pathogen data from google drive
## This should be universal across collaborators

# load package
library(googlesheets)

# which google sheets do you have access to?
# may ask you to authenticate in a browser!
gs_ls()

# Black box matrix -----------------------------------------------

# set the gs folder
bbm <- gs_title("Black box matrix")

# get the datasheet
bbm_dat <- gs_read(ss=bbm, skip=0)

# convert to data.frame
bbm_dat <- as.data.frame(bbm_dat)

rm(bbm)

# Festuca pathogen presence -----------------------------------------------

# set the gs folder
fpath <- gs_title("Fes roe pathogen presence")

# get the datasheet
fes_path <- gs_read(ss=fpath, skip=0)

# convert to data.frame
fes_path <- as.data.frame(fes_path)

rm(fpath)


# Festuca cleist vs chas --------------------------------------------------

# set the gs folder
clch <- gs_title("Black box cleist/chas/fesroe dataset")

# get the datasheet
cleis <- gs_read(ss=clch, skip=0)

# convert to data.frame
cleis <- as.data.frame(cleis)

rm(clch)


# Danthonia presence-absence ----------------------------------------------

# set the gs folder
dcpa <- gs_title("Dan Cal pathogen presence")

# get the data sheet
dan_pa <- gs_read(ss=dcpa, skip=0)

# convert to data.frame
dan_pa <- as.data.frame(dan_pa)

rm(dcpa)

