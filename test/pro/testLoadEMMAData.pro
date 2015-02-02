@../../pro/loadEMMAData.pro
@../../pro/loadEMMAStations.pro

files="../dat/"+["EMMA_FLRDEN_2015_001.txt","EMMA_FLRDEN_2015_002.txt",$
                 "EMMA_FLRDEN_2015_003.txt"]

d=loadEMMAData(files,/sort,stations=loadEMMAStations("../../dat/stations.txt"))

end
