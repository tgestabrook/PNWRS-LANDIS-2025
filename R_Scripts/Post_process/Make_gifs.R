#-----------------------------------------------------------------------------------------------------------------------
gc()

## FIRE MAPS. Time since fire and Severity GIF: ----
if(dir.exists(fireOutput)){
  fireGIF <- function(input_stack, name='unnamed.gif', path=fireOutput, yrs=1:simLength, fine_fuels=F){
    cat("\nGenerating GIF...")
    
    yrs.since.fire.r <- ifel(!is.na(input_stack), 0, 50)  # set up initial years since fire layer
    
    for (yr in yrs){  # loop through years
      cat(paste0(yr, '...'))
      
      if(!fine_fuels){  # prepare fine fuel raster, or background raster
        yrs.since.fire.r <- yrs.since.fire.r + 1
      }
      
      ## Plot frame:
      png(file.path(path,paste0('_GIF_frames_',yr,'.png')),width=6,height=8.32,res=100,units='in')
      par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(1,0.01,0),tck=-0.002,ps=10,cex=1)
      plot.new()
      plot.window(xlim=ext(ecos.r)[1:2], ylim=ext(ecos.r)[3:4],xaxs="i",yaxs="i",asp=1)
      plot(ecos.r,col=colorRampPalette(c('black','black'))(20),legend=F,add=T)  
      if(yr>1){plot(input_stack[[yr-1]],col=severityColsClassified,legend=F,add=T,alpha=0.75,all_levels=T)}  # fade-out for fire severity
      if(fine_fuels==T){
        plot(fineFuelStack.r[[yr]], col=colorRampPalette(c('#ffffcc', '#c2e699', '#78c679', '#31a354', '#006837'))(100), legend=F, range=c(0,simOpts$max.fine.fuels), fill_range=T, add=T)
      } else{
        plot(yrs.since.fire.r,col=colorRampPalette(c('grey80','grey30','grey20','grey10','grey10'))(simLength),legend=F,add=T,alpha=0.5, range=c(0,50), fill_range=T, type = 'continuous')
      }
      plot(input_stack[[yr]],col=severityColsClassified,legend=F,add=T, all_levels=T)
      plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
      plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
      legend(fill=severityColsClassified,legend = rep('',4),box.col=NA,border='black',inset=c(0.02,0.01),bg=NA,
             x='bottomleft',title.adj = 0.5, title=NA,cex=1.5,x.intersp=-0.65,horiz=T)
      legend(fill=NA,legend = rep('',4),box.col=NA,border=NA,inset=c(0.05,0.03),bg=NA,
             x='bottomleft',title.adj = 0.5, title=paste0('Burn severity\n UB   low  mod  high'),cex=0.8,x.intersp=0,horiz=T)
      mtext(paste0('year: ',simOpts$base.year+as.numeric(yr)),font=2,line=-1,adj=0.95,cex=1)
      dev.off()
      
    }
    
    ## Now load images and make gif
    cat('\nLoading images...\n')
    imgs <- dir(path)[grepl(paste0('_GIF_frames_'),dir(path))]
    imgs<-imgs[order(as.numeric(substr(imgs,13,nchar(imgs)-4)))]
    img_list <- lapply(file.path(path,imgs), image_read)
    
    ## join the images together
    img_joined <- image_join(img_list)
    ## animate at 2 frames per second
    img_animated <- image_animate(img_joined, fps = 2)
    
    cat('Writing GIF...')
    ## save to disk
    image_write(image = img_animated,path = file.path(path,'..',name))
    cat('Success!\n\n')
  }
  
  cat('Fire Severity GIF...\n')
  fireGIF(severityStackSmoothedClassified.r, name = 'Fire_Severity.gif')
  
  cat('Fire Severity GIF...\n')
  fireGIF(severityStackSmoothedClassified.r, name = 'Fire_Severity_fine_fuels.gif', fine_fuels = T)
}

if(dir.exists(harvestOutput)){
  ### Make harvest gif: ----
  cat('\n\nWriting Harvest GIF...')
  
  ### Write frames: ----
  yrs.since.harvest <- rast(harvestPrescripts.r[[1]], vals = 99)
  
  for(yr in 1:simLength){
    cat(paste0('...', yr))
    ## Harvest type maps: 
    r<- ifel(harvestPrescripts.r[[yr]] <= 1, NA, harvestPrescripts.r[[yr]] - 1) |> # Must subtract 1 because a value of 1 indicates it is an active site that wasn't harvested.
      as.factor() 
    levels(r)<-harvestColsClassified[,c(3,4)]
    
    yrs.since.harvest <- ifel(r > 0, 0, yrs.since.harvest + 1)
    
    png(file.path(harvestOutput,paste0('_GIF_frames_',yr,'.png')),width=6,height=8.32,res=100,units='in')
    
    par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(1,0.01,0),tck=-0.002,ps=10,cex=1)
    plot.new()
    plot.window(xlim=ext(ecos.r)[1:2], ylim=ext(ecos.r)[3:4],xaxs="i",yaxs="i",asp=1)
    plot(ecos.r,col=colorRampPalette(c('black','black'))(20),legend=F,add=T)
    
    plot(yrs.since.harvest,col=colorRampPalette(c('grey80','grey30','grey20','grey10','grey10'))(simLength), range=c(0, simLength), legend=F,add=T,alpha=0.5, fill_range=T)
    plot(r,col=harvestColsClassified[,2],legend=F,add=T, all_levels=T)
    
    plot(dem.r,col=demCols(50),add=T,alpha=0.15,legend=F)
    plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.15,legend=F);box(lwd=3)
    legend(fill=harvestColsClassified[,'color'],legend = harvestColsClassified[,'Treatment'],box.col=NA,border='black',inset=c(0.01,0.01),bg=NA,
           x='bottomleft',title.adj = 0.25, title=expression(bold('Prescription')),cex=0.8,x.intersp=0.5,y.intersp=0.7,horiz=F)
    mtext(paste0('Year: ',simOpts$base.year+as.numeric(yr)),font=2,line=-1,adj=0.95,cex=1)
    
    dev.off()
    if(yr%%10==0){gc()}
  }
  
  ### Write final harvest image: ----
  png(file.path(harvestOutput,'..','Harvest.png'),width=6,height=8.32,res=300,units='in')
  
  par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(1,0.01,0),tck=-0.002,ps=10,cex=1)
  plot.new()
  plot.window(xlim=ext(ecos.r)[1:2], ylim=ext(ecos.r)[3:4],xaxs="i",yaxs="i",asp=1)
  plot(ecos.r,col=colorRampPalette(c('black','black'))(20),legend=F,add=T)
  plot(yrs.since.harvest,col=rev(c(colorRampPalette(c('black','black','black','grey20','darkslategray'))(90),colorRampPalette(c('darkslategray','goldenrod1','tomato3','firebrick4'))(10)))[1:11],legend=F,add=T)
  plot(dem.r,col=demCols(50),add=T,alpha=0.1,legend=F)
  plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.1,legend=F);box(lwd=3)
  legend(fill=rev(c(colorRampPalette(c('black','black','black','grey20','darkslategray'))(90),colorRampPalette(c('darkslategray','goldenrod1','tomato3','firebrick4'))(10)))[1:11],legend = rep('',11),box.col=NA,border=NA,inset=c(0.02,0.02),bg=NA,
         x='bottomleft',title.adj = 0, title=paste0('       Years since harvest\n       0 10 20 30 40 50 60 70 80 90 100'),
         cex=0.8,x.intersp=0,horiz=T)
  mtext(paste0('year: ',simOpts$base.year+as.numeric(yr)),font=2,line=-1,adj=0.95,cex=1)
  
  dev.off()
  
  ## Now load images and make gif
  cat('\nLoading images...\n')
  imgs <- dir(harvestOutput)[grepl(paste0('_GIF_frames_'),dir(harvestOutput))]
  imgs<-imgs[order(as.numeric(substr(imgs,13,nchar(imgs)-4)))]
  img_list <- lapply(file.path(harvestOutput,imgs), image_read)
  
  ## join the images together
  img_joined <- image_join(img_list)
  ## animate at 2 frames per second
  img_animated <- image_animate(img_joined, fps = 2.5)
  
  
  cat('Writing GIF...')
  ## save to disk
  image_write(image = img_animated,path = file.path(harvestOutput,'..',"harvest_prescriptions.gif"))
  cat('Success!\n\n')
  
}


cat(paste('\n\nGIF routine complete.', Sys.time(), '\n'), file = outFile, append = T)

#-----------------------------------------------------------------------------------------------------------------------
### Biomass GIFs: ----
# ## Veg Type gif: ----
# ## Classify into hardwood, conifer (maybe by species?), non-tree, and no-veg. Make GIFs that show dynamics over time. 
# vegTypeGIF<-function(allMaps=biomassMaps, yr.list=yrs.forGif,name='unnamed.gif',path=biomassOutput){
#   
#   unlisted<-unlist(strsplit(allMaps,"-"))
#   yrs<-unique(unlisted[seq(2,length(unlisted),3)])
#   yrs<-yrs[order(as.numeric(yrs))]
#   
#   if(is.null(yr.list)) yr.list<-yrs else yr.list<-yrs[yrs%in%yr.list]
#   
#   # if(grepl('biomass', path,ignore.case = T)) maxValue<-maxBiomassvalue else maxValue<-maxAgevalue
#   
#   for(yr in yr.list){
#     cat('\n\n\n\n~~~~~~~~~~~~~~~~~~~~~~~ Year',yr,'~~~~~~~~~~~~~~~~~~~~~~~~~~\n')
#     
#     focal.yr.maps<-allMaps[grepl(paste0("-",yr,"-"),allMaps)]
#     unlisted<-unlist(strsplit(focal.yr.maps,"-"))
#     spp<-unlisted[seq(1,length(unlisted),3)]
#     
#     conifer.xeric.maps<-focal.yr.maps[spp%in%conifers.xeric]
#     conifer.mesic.maps<-focal.yr.maps[spp%in%conifers.mesic]
#     conifer.subalpine.maps<-focal.yr.maps[spp%in%conifers.subalpine]
#     hardwood.maps<-focal.yr.maps[spp%in%hardwoods]
#     shrub.maps<-focal.yr.maps[spp%in%shrubs]
#     grass.maps<-focal.yr.maps[spp%in%grass]
#     if(yr==0)
#       grass.maps<-gsub('0','10',grass.maps)
#     
#     ## Generate functional group rasters
#     cat('\nCompiling conifer rasters...\n')
#     conifer.xeric.r<-makeRaster(conifer.xeric.maps,path)
#     conifer.mesic.r<-makeRaster(conifer.mesic.maps,path)
#     conifer.subalpine.r<-makeRaster(conifer.subalpine.maps,path)
#     #   cat('\n\n')
#     #   print(summary(values(conifer.subalpine.r)))
#     # }
#     
#     cat('Compiling hardwood raster...\n')
#     hardwood.r<-makeRaster(hardwood.maps,path)
#     
#     cat('Compiling shrub and grass rasters...\n')
#     if(length(shrub.maps)>0)  shrub.r<-makeRaster(shrub.maps,path) else {
#       shrub.r<-ecos.r
#       values(shrub.r)<-0
#     }
#     if(length(grass.maps)>0)  grass.r<-makeRaster(grass.maps,path) else {
#       grass.r<-ecos.r
#       values(grass.r)<-0
#     }
#     
#     cat('\n------------------------------------------\n')
#     
#     ## For consistent color scales
#     if(yr==yr.list[1])
#       maxValue<-list()
#     for(i in c('conifer.mesic.r','conifer.xeric.r','conifer.subalpine.r','hardwood.r','grass.r','shrub.r')){
#       r<-eval(parse(text=i))
#       
#       # Set max value to the max biomass at timestep 0:
#       if(yr==yr.list[1]){
#         maxValue[[i]] = quantile(values(r),0.8,na.rm=T)
#         if(grepl('ageoutput', path,ignore.case = T)&maxValue[[i]]>1000) maxValue[[i]]<-1000
#         
#       }
#       cat('Limiting',i,'from',max(values(r),na.rm=T),'to',maxValue[[i]],'...\n')
#       
#       r[1,1]<-maxValue[[i]]
#       r[r>=maxValue[[i]]]<-maxValue[[i]]
#       assign(i,r)
#     }
#     cat('\n------------------------------------------\n')
#     
#     ### Now NA for all rasters but the veg type with the most biomass per pixel:
#     combined.raster<-max(conifer.mesic.r,conifer.xeric.r,conifer.subalpine.r,hardwood.r,grass.r,shrub.r,na.rm=T)
#     for(i in c('conifer.mesic.r','conifer.xeric.r','conifer.subalpine.r','hardwood.r','grass.r','shrub.r')){
#       r<-eval(parse(text=i))
#       
#       r[r!=combined.raster]<-NA
#       
#       assign(i,r)
#     }
#     
#     png(file.path(path,paste0('_GIF_frames_',yr,'.png')),width=6,height=8.32,res=300,units='in')
#     par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(1,0.01,0),tck=-0.002,ps=10,cex=1)
#     
#     plot.new()
#     plot.window(xlim=ext(ecos.r)[1:2], ylim=ext(ecos.r)[3:4],xaxs="i",yaxs="i",asp=1)
#     plot(ecos.r,col=colorRampPalette(c('black','black'))(20),legend=F,add=T)
#     
#     plot(grass.r,col='wheat',legend=F,add=T)
#     plot(shrub.r,col=hcl.colors(10,'Oranges',rev=T),legend=F,add=T,alpha=0.75)
#     plot(conifer.xeric.r,col=hcl.colors(10,'ag_GrnYl',rev=T),legend=F,add=T,alpha=0.9)
#     plot(conifer.mesic.r,col=hcl.colors(10,'Teal',rev=T),legend=F,add=T,alpha=0.9)
#     plot(conifer.subalpine.r,col=hcl.colors(10,'BuPu',rev=T),legend=F,add=T,alpha=0.9)
#     plot(hardwood.r,col=hcl.colors(10,'SunsetDark',rev=T),legend=F,add=T,alpha=0.9)
#     
#     plot(dem.r,col=demCols,add=T,alpha=0.2,legend=F)
#     plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.2,legend=F);box(lwd=3)
#     
#     legend(fill=rep('wheat',10),  legend = rep('',10),box.col=NA,border=NA,inset=c(0.02,0.00),cex=0.8,bg=NA, x='bottomleft',title.adj = 0, title="    Grass",x.intersp=-1,horiz=T)
#     legend(fill=hcl.colors(10,'Oranges',rev=T),  legend = rep('',10),box.col=NA,border=NA,inset=c(0.02,0.04),cex=0.8,bg=NA, x='bottomleft',title.adj = 0, title="    Shrubs",x.intersp=-1,horiz=T)
#     legend(fill=hcl.colors(10,'SunsetDark',rev=T),legend = rep('',10),box.col=NA,border=NA,inset=c(0.02,0.08),cex=0.8,bg=NA, x='bottomleft',title.adj = 0, title="    Hardwoods",x.intersp=-1,horiz=T)
#     legend(fill=hcl.colors(10,'ag_GrnYl',rev=T),   legend = rep('',10),box.col=NA,border=NA,inset=c(0.02,0.12),cex=0.8,bg=NA, x='bottomleft',title.adj = 0, title="    Conifer (dry)",x.intersp=-1,horiz=T)
#     legend(fill=hcl.colors(10,'Teal',rev=T),   legend = rep('',10),box.col=NA,border=NA,inset=c(0.02,0.16),cex=0.8,bg=NA, x='bottomleft',title.adj = 0, title="    Conifer (moist)",x.intersp=-1,horiz=T)
#     legend(fill=hcl.colors(10,'BuPu',rev=T),   legend = rep('',10),box.col=NA,border=NA,inset=c(0.02,0.20),cex=0.8,bg=NA, x='bottomleft',title.adj = 0, title="    Conifer (subalpine)",x.intersp=-1,horiz=T)
#     
#     mtext(paste0('Year\n',simOpts$base.year+as.numeric(yr)),font=2,line=-1,adj=0.95,cex=1.2)
#     
#     ## NOTE: The only way to change the legend size is to change cex, which also changes the font size. Grrrrrr.
#     
#     dev.off()
#   }
#   
#   cat('\n\n********************************* Generating GIF *********************************\n\n')
#   cat('\nLoading images...\n')
#   imgs <- dir(path)[grepl('_GIF_frames',dir(path))]
#   imgs<-imgs[order(readr::parse_number(imgs))]
#   
#   img_list <- lapply(file.path(path,imgs), image_read)
#   
#   ## join the images together
#   img_joined <- image_join(img_list)
#   
#   ## animate at 2 frames per second
#   img_animated <- image_animate(img_joined, fps = 2)
#   
#   ## view animated image
#   img_animated
#   
#   cat('Writing GIF...\n')
#   ## save to disk
#   image_write(image = img_animated,
#               path = file.path(path,'..',name))
#   cat('Success!\n\n')
# }
# vegTypeGIF(biomassMaps,name='Biomass.gif',path=biomassOutput)
# vegTypeGIF(ageMaps,name='Age.gif',path=ageOutput)


## One species or group gif: ----
## One or multiple species succession gif
# speciesGIF<-function(focal.spp='PseuMenz', allMaps=biomassMaps, yr.list=yrs.forGif, name='unnamed.gif',path=biomassOutput){
#   unlisted<-unlist(strsplit(allMaps,"-"))
#   yrs<-unique(unlisted[seq(2,length(unlisted),3)])
#   yrs<-yrs[order(as.numeric(yrs))]
#   
#   if(is.null(yr.list)) yr.list<-yrs else yr.list<-yrs[yrs%in%yr.list]
#   
#   spp<-unlisted[seq(1,length(unlisted),3)]
#   
#   focalSpeciesMaps<-allMaps[spp%in%focal.spp]
#   
#   # if(grepl('biomass', path,ignore.case = T)) maxValue<-maxBiomassvalue else maxValue<-maxAgevalue
#   
#   for(yr in yr.list){
#     cat(yr,'...')
#     focal.yr.maps<-focalSpeciesMaps[grepl(paste0("-",yr,"-"),focalSpeciesMaps)]
#     
#     r<-makeRaster(focal.yr.maps,path)
#     
#     if(length(focal.yr.maps)>1){
#       label<-paste(focal.spp,collapse='\n')
#       filename<-paste(focal.spp,collapse='_')
#     } else {
#       label<-latin[focal.spp]
#       filename<-focal.spp
#     }
#     
#     ## For consistent color scales
#     # Set max value to the max biomass or age at timestep 0:
#     if(yr==yr.list[1]){
#       maxValue = quantile(values(r),0.8,na.rm=T)
#       if(grepl('ageoutput', path,ignore.case = T)&maxValue>1000) maxValue<-1000
#     }
#     cat('Limiting',focal.spp,'from',max(values(r),na.rm=T),'to',maxValue,'...\n')
#     
#     r[1,1]<-maxValue
#     r[r>=maxValue]<-maxValue
#     
#     
#     png(file.path(path,paste0('_spGIF_frames_',filename,yr,'.png')),width=6,height=8.32,res=300,units='in')
#     par(mfrow=c(1,1),oma=c(0,0,0,0),mar=c(0,0,0,0),mgp=c(1,0.01,0),tck=-0.002,ps=10,cex=1)
#     
#     plot.new()
#     plot.window(xlim=ext(ecos.r)[1:2], ylim=ext(ecos.r)[3:4],xaxs="i",yaxs="i",asp=1)
#     plot(ecos.r,col=colorRampPalette(c('black','black'))(20),legend=F,add=T)
#     
#     plot(r,col=hcl.colors(10,'ag_GrnYl',rev=T),legend=F,add=T,alpha=1)
#     
#     plot(dem.r,col=demCols,add=T,alpha=0.25,legend=F)
#     plot(hillshade.r,col=colorRampPalette(c('black','grey20','white'))(100),add=T,alpha=0.25,legend=F);box(lwd=3)
#     
#     legend(fill=hcl.colors(10,'ag_GrnYl',rev=T),legend = rep('',10),box.col=NA,border=NA,inset=c(0.02,0.02),bg=NA, 
#            x='bottomleft',title.adj = 0.25, title=paste0(label,'\nbiomass'),cex=0.8,x.intersp=-1,horiz=T)
#     mtext(paste0('year: ',simOpts$base.year+as.numeric(yr)),font=2,line=-1,adj=0.95,cex=1)
#     
#     dev.off()
#   }
#   
#   cat('\nLoading images...\n')
#   imgs <- dir(path)[grepl(paste0('_spGIF_frames_',filename),dir(path))]
#   imgs<-imgs[order(readr::parse_number(imgs))]
#   img_list <- lapply(file.path(path,imgs), image_read)
#   
#   ## join the images together
#   img_joined <- image_join(img_list)
#   
#   ## animate at 2 frames per second
#   img_animated <- image_animate(img_joined, fps = 2)
#   
#   ## view animated image
#   img_animated
#   
#   cat('Writing GIF...')
#   ## save to disk
#   image_write(image = img_animated,
#               path = file.path(path,'..',name))
#   cat('Success!\n\n')
# }
# 
# speciesGIF('PseuMenz',name='PSME.gif')
# speciesGIF('PinuPond',name='PIPO.gif')
# speciesGIF(conifers.mesic,name='Conifers_mesic.gif')
# speciesGIF(conifers.xeric,name='Conifers_xeric.gif')
# speciesGIF(conifers.subalpine,name='Conifers_subalipine.gif')
# speciesGIF(shrubs,name='Shrubs.gif')
# speciesGIF("Grass_Forb",name='Grasses.gif')

























