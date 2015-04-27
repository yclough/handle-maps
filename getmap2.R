# Land-use data import and reshaping function

# 'getmap2' : 	reads in a land-cover matrix (.txt or .mat) and samples square landscapes from that based on coordinates, returns a list.
#			the input takes:
#			map_path=map_path   # the path to the directory where the raster information is stored
#			origin_region=list(E=1302000,N=6410000), # the coordinates for the soutwestern corner of the input land-cover matrix in RT90
#			origin_sample=list(E= sample.coord[,1],N=sample.coord[,2]), # the coordinates for the soutwestern corners of the output land-cover matrices in RT90
#			size_landsc_m=10000 # the size of the square landscape in metres
#			cell.size=25 # the size of a raster cell in metres
#			year = 2013 # the year for which the land-use is mapped in the input file
#			prefix="eSMD_Ska"  # the input file name prefix (the part of the filename before the year)
#			suffix="", # the input file name suffix (the part of the filename after the year)
#			filetype="mat" # the input file type (mat or txt)
#			the output is a list with the following
#			List of 2
#			 $ info    :List of 6
#			  ..$ origin_sample:List of 2
#			  .. ..$ E: num [] Easting coordinates
#			  .. ..$ N: num [] Northing coordinates
#			  ..$ size_landsc_m: num 10000
#			  ..$ cell.size    : num 25
#			  ..$ year         : num 2012
#			  ..$ map          : chr "cSMD_Sk2012"
#			  ..$ orientation  : chr "row: N to S; col: W to E" # meaning of increasing row and column numbers in terms of cardinal directions
#			 $ matrices:List of 16
#			  ..$ : num [1:401, 1:401] 8 8 8 8 8 8 8 8 8 8 ...
#				...

getmap2<-function(map_path=map_path,
	origin_region=list(E=1302000,N=6410000),
	origin_sample=list(E= sample.coord[,1],N=sample.coord[,2]), #changes here
	size_landsc_m=10000, cell.size=25, year = 2013,prefix="eSMD_Ska",suffix="",filetype="mat"){
		oldwd<-getwd()
		setwd(map_path)
		rastercode<-read.table(rastercodefile,h=T)
		size_landsc<-size_landsc_m/cell.size
		mat_orig_sample<-list(E=round((origin_sample$E-origin_region$E)/25),
						N=round((origin_sample$N-origin_region$N)/25))
		map.curr<-paste(prefix,as.character(year),sep="")
		map.curr<-paste(map.curr,suffix,sep="")
	
		if(filetype=="txt"){
		map.curr.file<-paste(map.curr,".txt",sep="")
		assign("x", as.matrix(read.table(map.curr.file,h=T,colClasses="numeric")), envir = .GlobalEnv )}
		if(filetype=="mat"){
		require(R.matlab)
		map.curr.file<-paste(map.curr,".mat",sep="")
		assign("x", readMat(map.curr.file)[[1]], envir = .GlobalEnv )
		}
		x_size<-list(E=dim(x)[2],N=dim(x)[1])
		map.sample.curr <- vector("list", length(mat_orig_sample$E))
		for(i in 1:length(mat_orig_sample$E)){	
		tempmat<-x[nrow(x):1,][x_size$N-((mat_orig_sample$N[i]+size_landsc):mat_orig_sample$N[i]),
		(mat_orig_sample$E[i]:(mat_orig_sample$E[i]+size_landsc))]
		map.sample.curr[[i]]<-tempmat
		}
		return(list(info=list(origin_sample=origin_sample,size_landsc_m=size_landsc_m, cell.size=cell.size, year = year, map=map.curr,orientation="row: N to S; col: W to E"),matrices=map.sample.curr))
		rm(list="x")
	}
