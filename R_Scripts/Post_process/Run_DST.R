


#-----------------------------------------------------------------------------------------------------------------------
######  LOAD DATA LAYERS  ###### ----

### LOAD DST STRUCTURE (defined in DST_Structure.csv): ---- 
dst.structure<-read.csv(file.path(dataDir,'DST_Structure.csv'))

## DROP SOME METRICS BY SETTING PREMISE TO NA:
dst.structure<-dst.structure[!is.na(dst.structure$Premise),]

dst.structure$Topic<-gsub(' ','.',dst.structure$Topic)

dst.structure$Direction<-1
dst.structure[grepl('Less',dst.structure$Premise),'Direction']<- -1
dst.structure$Direction













































