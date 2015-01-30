function loadEMMAData,xfiles,epoch=epoch,sort=sort

;+
; NAME:
;      loadEMMAData
; PURPOSE:
;      Load a sequence of EMMA density files (1 or more) which are
;      found at  http://plasmonserver.aquila.infn.it/EMMA_FLR_DENSITY/
; CALLING SEQUENCE:
;      d=loadEMMAData(files)
; INPUTS:
;      FILES=STRING OR STRARR. One or more files to be loaded and
;      concatenated in sequential order.
; OUTPUTS:
;      Choose from multiple output data formats. If nothing else is
;      specified then the output is just one line at a time one
;      observation, 
;      D=ARRAY(X)
; 
;      If SORT is set then D is a structure of arrays, each array is
;      an array of the structure X, e.g.
;      D={P01:ARRAY(X),P02:ARRAY(X),...}
;      
;      X={NAME=STRING, Station pair name
;         LSHELL=FLOAT. Station pair L-shell
;         YEAR=LONG. Year
;         DOY=LONG. Day of year (January 1 is DOY=1)
;         HRUT=FLOAT. UT time in hours
;         LT=FLOAT. Local time (not MLT)
;         REQ=FLOAT. Equatorial radius of field line
;         MHZ=FLOAT. FLR Frequency in mHz
;         AMUCC=FLOAT. Mass density in AMU/CC
;         HRSW=FLOAT. UT hour for solar wind observation
;         BY=FLOAT. Solar wind By in nT
;         BZ=FLOAT. Solar wind Bz in nT
;         P=FLOAT. Solar wind pressure
;         HRG=FLOAT. Hour for G
;         G1=FLOAT.
;         G2=FLOAT.
;         HRDST=FLOAT. UT hour for Dst data
;         DST=FLOAT. Dst value
;        }
; KEYWORDS:
;      SORT. If set then separate into arrays of X, sorted by name of
;      station pair.
;      EPOCH. If set then add the tage EPOCH to the structure X, which
;      will contain the CDF TT200 epoch value. 
;-

  files=[xfiles]

  n=2
  i=0
  template={name:"",l:0.,year:0l,doy:0l,hrut:0.,lt:0.,req:0.,mhz:0.,$
            amucc:0.,hrsw:0.,by:0.,bz:0.,p:0.,hrg:0.,g1:0.,g2:0.,hrdst:0.,$
            dst:0.}
  ; If EPOCH is set then add the tag epoch
  if keyword_set(epoch) then $
     template=create_struct(template,'epoch',0d)
  data=replicate(template,n)

  for i=0,n_elements(xfiles)-1 do begin
     openr,un,xfiles[i],/get_lun
     line=''
     readf,un,line
     readf,un,line
     while not(eof(un)) do begin
        readf,un,line
        fields=str_sep(strcompress(strtrim(line,2))," ")
        if i eq n then begin
           data=[data,data]
           n*=2
        endif
        data[i].name=fields[0]
        data[i].l=float(fields[1])
        data[i].year=long(fields[2])
        data[i].doy=long(fields[3])
        data[i].hrut=float(fields[4])
        data[i].lt=float(fields[5])
        data[i].req=float(fields[6])
        data[i].mhz=float(fields[7])
        data[i].amucc=float(fields[8])
        data[i].hrsw=float(fields[9])
        data[i].by=float(fields[10])
        data[i].bz=float(fields[11])
        data[i].p=float(fields[12])
        data[i].hrg=float(fields[13])
        data[i].g1=float(fields[14])
        data[i].g2=float(fields[15])
        data[i].hrdst=float(fields[16])
        data[i].dst=float(fields[17])
     endwhile
     free_lun,un
  endfor
  i--
  
  data=data[0:i]

  ; If EPOCH is set then add the epoch data
  if keyword_set(epoch) then $
     for i=0,n_elements(data)-1 do begin
     cdf_tt2000,xepoch,data[i].year,0,data[i].doy,/compute_epoch
     xepoch+=data[i].hrut*3600d*1e9
     data[i].epoch=xepoch
  endfor

  ; If SORT is set then sort by station pair and time
  names=data.names[uniq(data.name,sort(data.name))]
  help,names
  print,names
  
  
  return,data
end

