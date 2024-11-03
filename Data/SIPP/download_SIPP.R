###################################################
###################################################
#### File to download SIPP from NBER and Census ###
#### for project JSLC ####
#### created by Lukas Nord on 31/08/2021 ####
###################################################
###################################################

###################################################
#### Housekeeping and packages ####
###################################################


###################################################
#### Define download paths and options ####
###################################################

nber_path <- "https://data.nber.org/sipp/"

census_path <- "https://www2.census.gov/programs-surveys/sipp/data/datasets/"

store_path <- "C:/Users/phili/Dropbox/JSLC/JME/Programs/Data/SIPP"

# set to 1 if you want to delete zip files after unpacking
rm_zip <- 1

###################################################
#### Core files 1990-2008 (NBER) ####
###################################################

# select relevant waves: enter a new row with year / first wave / last wave
waves <- rbind( c(1990,1,8),
                c(1991,1,8),
                c(1992,1,9),
                c(1993,1,9),
                c(1996,1,12),
                c(2001,1,9),
                c(2004,1,12),
                c(2008,1,16)
                )
colnames(waves) <- c("year","wave_start","wave_end")

# select file types to download
files <- c('.do','.dct','.zip')

for (i_year in 1:dim(waves)[1] ) {
   for (i_wave in waves[i_year,'wave_start']:waves[i_year,'wave_end']) {
        for (i_file in 1:length(files)) {

            # generate short version of year
            ys <- waves[i_year,'year'] %% 100
            if ((ys < 10)) {
               ys <- paste(0,ys,sep="")
            } else {
               ys <- paste(ys,sep="")
            }

            # make sure storage file path exists
            ifelse(!dir.exists(paste(store_path, waves[i_year,'year'], sep="")), dir.create(paste(store_path, waves[i_year,'year'], sep="")), FALSE)

            # select file  
            if (files[i_file]=='.zip') {

                if ((waves[i_year,'year'] == 1990)&&(i_wave<5)) {
                    file_path_web <- paste(nber_path,waves[i_year,'year'],"/sipp90w",i_wave,".dat.Z", sep="")
                    file_path_store <-  paste(store_path,waves[i_year,'year'],"/sipp90w",i_wave,".dat.Z", sep="")
                } else if (waves[i_year,'year'] < 1996) {
                    file_path_web <- paste(nber_path,waves[i_year,'year'],"/sipp",ys,"w",i_wave,".zip", sep="")
                    file_path_store <-  paste(store_path,waves[i_year,'year'],"/sipp",ys,"w",i_wave,".zip", sep="")
                } else if (waves[i_year,'year']==1996) {
                    file_path_web <- paste(nber_path,waves[i_year,'year'],"/sipp",ys,"l",i_wave,".zip", sep="")
                    file_path_store <-  paste(store_path,waves[i_year,'year'],"/sipp",ys,"l",i_wave,".zip", sep="")
                } else {
                    file_path_web <- paste(nber_path,waves[i_year,'year'],"/l",ys,"puw",i_wave,".zip", sep="")
                    file_path_store <-  paste(store_path,waves[i_year,'year'],"/l",ys,"puw",i_wave,".zip", sep="")
                }   

            }  else {

                if (waves[i_year,'year']==1996) {
                    file_path_web <- paste(nber_path,waves[i_year,'year'],"/sip",ys,"l",i_wave,files[i_file], sep="")
                    file_path_store <-  paste(store_path,waves[i_year,'year'],"/sip",ys,"l",i_wave,files[i_file], sep="")
                } else if (waves[i_year,'year'] > 2001) {
                    file_path_web <- paste(nber_path,waves[i_year,'year'],"/sippl",ys,"puw",i_wave,files[i_file], sep="")
                    file_path_store <-  paste(store_path,waves[i_year,'year'],"/sippl",ys,"puw",i_wave,files[i_file], sep="")
                } else {
                    file_path_web <- paste(nber_path,waves[i_year,'year'],"/sip",ys,"w",i_wave,files[i_file], sep="")
                    file_path_store <-  paste(store_path,waves[i_year,'year'],"/sip",ys,"w",i_wave,files[i_file], sep="")
                }

            }

            # download
            download.file(file_path_web, file_path_store)

            # unzip if necessary
            if ((files[i_file]=='.zip')&&(!((waves[i_year,'year'] == 1990)&&(i_wave<5)))) {
                    unzip(zipfile=file_path_store,exdir=paste(store_path,waves[i_year,'year'], sep=""))
                    if (rm_zip == 1) file.remove(file_path_store)
            }

            # correct path in file if necessary
            if (waves[i_year,'year']<2001) {
                if (files[i_file]=='.dct') {
                    tx  <- readLines(file_path_store)
                    old_path <- "/homes/data/sipp/"
                    tx2  <- gsub(pattern = old_path, replace = store_path, x = tx)
                    writeLines(tx2, con=file_path_store)
                }
            } else {
                if (files[i_file]=='.do') {
                    tx  <- readLines(file_path_store)
                    old_ <- "/homes/data/sipp/"
                    tx2  <- gsub(pattern = old_path, replace = store_path, x = tx)
                    writeLines(tx2, con=file_path_store)
                }
            }
            
        }
    }
}


###################################################
#### Topical modules 1990-2008 (NBER) ####
###################################################

# select relevant waves: enter a new row with year / wave for every file you want to load
waves <- rbind( c(1990,4),
                c(1990,7),
                c(1991,4),
                c(1991,7),
                c(1992,4),
                c(1992,7),
                c(1993,4),
                c(1993,7),
                c(1996,3),
                c(1996,6),
                c(1996,9),
                c(1996,12),
                c(2001,3),
                c(2001,6),
                c(2001,9),
                c(2004,3),
                c(2004,6),
                c(2008,4),
                c(2008,7),
                c(2008,10)
                )
colnames(waves) <- c("year","wave")

# select file types to download
files <- c('.do','.dct','.zip')

for (i_wave in 1:dim(waves)[1]) {
    for (i_file in 1:length(files)) {

        # generate short version of year
        ys <- waves[i_wave,'year'] %% 100
        if ((ys < 10)) {
            ys <- paste(0,ys,sep="")
        } else {
            ys <- paste(ys,sep="")
        }

        # make sure storage file path exists
        ifelse(!dir.exists(paste(store_path, waves[i_wave,'year'], sep="")), dir.create(paste(store_path, waves[i_wave,'year'], sep="")), FALSE)


        # select file  
        if (files[i_file]=='.zip') {

            if ((waves[i_wave,'year'] == 1990)&&(waves[i_wave,'wave']<5)) {
                file_path_web <- paste(nber_path,waves[i_wave,'year'],"/sipp90t",waves[i_wave,'wave'],".dat.Z", sep="")
                file_path_store <-  paste(store_path,waves[i_wave,'year'],"/sipp90t",waves[i_wave,'wave'],".dat.Z", sep="")
            } else if (waves[i_wave,'year'] <= 1996) {
                file_path_web <- paste(nber_path,waves[i_wave,'year'],"/sipp",ys,"t",waves[i_wave,'wave'],".zip", sep="")
                file_path_store <-  paste(store_path,waves[i_wave,'year'],"/sipp",ys,"t",waves[i_wave,'wave'],".zip", sep="")
            } else {
                file_path_web <- paste(nber_path,waves[i_wave,'year'],"/p",ys,"putm",waves[i_wave,'wave'],".zip", sep="")
                file_path_store <-  paste(store_path,waves[i_wave,'year'],"/p",ys,"putm",waves[i_wave,'wave'],".zip", sep="")
            }   

        }  else {

            if (waves[i_wave,'year'] > 2001) {
                file_path_web <- paste(nber_path,waves[i_wave,'year'],"/sippp",ys,"putm",waves[i_wave,'wave'],files[i_file], sep="")
                file_path_store <-  paste(store_path,waves[i_wave,'year'],"/sippp",ys,"putm",waves[i_wave,'wave'],files[i_file], sep="")
            } else {
                file_path_web <- paste(nber_path,waves[i_wave,'year'],"/sip",ys,"t",waves[i_wave,'wave'],files[i_file], sep="")
                file_path_store <-  paste(store_path,waves[i_wave,'year'],"/sip",ys,"t",waves[i_wave,'wave'],files[i_file], sep="")
            }

        }            


        # download
        download.file(file_path_web, file_path_store)

        # unzip if necessary
        if ((files[i_file]=='.zip')&&(!((waves[i_wave,'year'] == 1990)&&(waves[i_wave,'wave']<5)))) {
                unzip(zipfile=file_path_store,exdir=paste(store_path,waves[i_wave,'year'], sep=""))
                if (rm_zip == 1) file.remove(file_path_store)
        }

        # correct path in file if necessary
        if ((waves[i_wave,'year']<2001)&&(!((waves[i_wave,'year']==1990)&&(waves[i_wave,'wave']==7)))) {
            if (files[i_file]=='.dct') {
                tx  <- readLines(file_path_store)
                old_path <- "/homes/data/sipp/"
                tx2  <- gsub(pattern = old_path, replace = store_path, x = tx)
                writeLines(tx2, con=file_path_store)
            }
        } else {
            if (files[i_file]=='.do') {
                tx  <- readLines(file_path_store)
                old_ <- "/homes/data/sipp/"
                tx2  <- gsub(pattern = old_path, replace = store_path, x = tx)
                writeLines(tx2, con=file_path_store)
            }
        }
    }
}


###################################################
#### Core files 2014 (Census) ####
###################################################

# select relevant waves: enter a new row with year / first wave / last wave
waves <- rbind( c(2014,1,4)
                )
colnames(waves) <- c("year","wave_start","wave_end")

for (i_year in 1:dim(waves)[1] ) {
   for (i_wave in waves[i_year,'wave_start']:waves[i_year,'wave_end']) {

            # make sure storage file path exists
            ifelse(!dir.exists(paste(store_path, waves[i_year,'year'], sep="")), dir.create(paste(store_path, waves[i_year,'year'], sep="")), FALSE)

            # select file
            file_path_web <- paste(census_path,waves[i_year,'year'],"/w",i_wave,"/pu",waves[i_year,'year'],"w",i_wave,"_v13.zip", sep="")
            file_path_store <-  paste(store_path,waves[i_year,'year'],"/pu",waves[i_year,'year'],"w",i_wave,"_v13.zip", sep="")

            # download
            download.file(file_path_web, file_path_store)

            # # unzip files
            # unzip(zipfile=file_path_store,exdir=paste(store_path,waves[i_year,'year'], sep=""))
            # if (rm_zip == 1) file.remove(file_path_store)
    }
}
