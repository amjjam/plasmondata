function loadEMMAData,xfiles,sort=sort,stations=stations
  
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
;         LOCALTIME=FLOAT. Local time (not MLT)
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
;      STATIONS. If set then the MLT will be computed based on the
;      magnetic longitude supplied. In that case this should be an
;      array of structures  which contain the tags ID=STRING (3-letter
;      station ID), and MLON=FLOAT (magnetic longitude in degrees). 
;      The program will average the magnetic longitude of the two 
;      stations in the structure and add that to UT to get MLT. If not 
;      specified then 100 degree magnetic longitude is assumed for all 
;      stations. 
;-

  files=[xfiles]
  
  n=2l
  i=0l
  template={name:"",l:0.,year:0l,doy:0l,hrut:0.,epoch:0d,localtime:0.,$
            mlt:0.,req:0.,mhz:0.,amucc:0.,hrsw:0.,by:0.,bz:0.,p:0.,hrg:0.,$
            g1:0.,g2:0.,hrdst:0.,dst:0.}

  data=replicate(template,n)
  
  for j=0,n_elements(xfiles)-1 do begin
     openr,un,xfiles[j],/get_lun
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
        data[i].localtime=float(fields[5])
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
        i++
     endwhile
     free_lun,un
  endfor
  i--
  
  data=data[0:i]
  
  ; Add EPOCH
  for i=0,n_elements(data)-1 do begin
     cdf_tt2000,xepoch,data[i].year,0,data[i].doy,/compute_epoch
     xepoch+=data[i].hrut*3600d*1e9
     data[i].epoch=xepoch
  endfor
  
  ; Add MLT data
  if keyword_set(stations) then begin
     for i=0,n_elements(stations)-1 do $
        stations[i].id=strlowcase(stations[i].id)
     mlon=fltarr(2)
     for i=0,n_elements(data)-1 do begin
        for j=0,1 do begin
           stn=strlowcase(strmid(data[i].name,3*j,3))
           index=where(stations.id eq stn)
           if index[0] eq -1 then begin
              message,"Could not find station "+stn,/information
           endif
           if n_elements(index) gt 1 then begin
              message,"More than one entry for station "+stn
           endif
           mlon[j]=stations[index[0]].mlon
        endfor
        amlon=(mlon[0]+mlon[1])/2.
        data[i].mlt=(data[i].hrut+amlon/15.+24.) mod 24.
     endfor
  endif else begin
     for i=0,n_elements(data)-1 do begin
        data[i].mlt=(data[i].hrut+100./15.) mod 24.
     endfor
  endelse

  ; If SORT is set then sort by station pair and time
  if keyword_set(sort) then begin
     names=data.name
     index=uniq(names,sort(names))
     names=names[index]
     lshells=data[index].l
     index=reverse(sort(lshells))
     names=names[index]
     lshells=lshells[index]

     for i=0,n_elements(names)-1 do begin
        index=where(data.name eq names[i])
        if i eq 0 then $
           d=create_struct(names[i],data[index]) $
        else $
           d=create_struct(d,names[i],data[index])
     endfor

     ; Get the longitude of stations and sort that list by L from 
     ; maximum to minimum L
     template={name:"",l:0.,mlon:0.}
     pairs=replicate(template,n_elements(data))
     pairs.name=data.name
     pairs.l=data.l
     pairs.mlon=((data.mlt-data.hrut+48) mod 24)*15.
     pairs=pairs[sort(pairs.name)]
     pairs=pairs[uniq(pairs.name)]
     pairs=pairs[reverse(sort(pairs.l))]
     d=create_struct('pairs',pairs,d)
     
     data=d
  endif
  
  return,data
end

