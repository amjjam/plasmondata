function loadEMMAStations,file

;+
; NAME:
;      loadEMMAStations
; PURPOSE:
;      Loads a file of EMMA stations information
; CALLING SEQUENCE:
;      d=loadEMMAStations(file)
; INPUTS:
;      FILE=STRING. Name of the file to load.
; OUTPUTS:
;      D=ARRAY({ID=STRING, 3-Letter stations ID
;               NAME=STRING, Station name
;               GLAT=FLOAT, Geographic latitude in degrees
;               GLON=FLOAT, Geographic longitude in degrees
;               L=FLOAT, L-shell
;               MLAT=FLOAT, Magnetic latitude in degrees
;               MLON=FLOAT, Magnetic longitude in degrees
;              })
; FILE FORMAT:
;     First line is ignored, assuming it is heading
;     Lines are ID, Name, Glat, Glon, L, MLat, Mlon, separated by
;     spaces. Make sure Name does not contain spaces. Use '_' instead
;     of space. 
;-

template={id:"",name:'',glat:0.,glon:0.,l:0.,mlat:0.,mlon:0.}
d=[template]
n=1
i=0

line=''
openr,un,file,/get_lun
readf,un,line

while not(eof(un)) do begin
   readf,un,line
   tmp=str_sep(strcompress(strtrim(line,2))," ")
   if i eq n then begin
      d=[d,d]
      n*=2
   endif
   d[i].id=tmp[0]
   d[i].name=tmp[1]
   d[i].glat=float(tmp[2])
   d[i].glon=float(tmp[3])
   d[i].l=float(tmp[4])
   d[i].mlat=float(tmp[5])
   d[i].mlon=float(tmp[6])
   i++
endwhile
i--
free_lun,un

return,d[0:i]

end
