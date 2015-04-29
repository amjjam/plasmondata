pro writeStationsLGlon,d,file

;+
; NAME:
;   writeStationsLGlon
; PURPOSE:
;   Write a file which contains the name, L-shell, and geographic
;   longitude of stations contained in a data structure.
;
;   It turns out that with the way DGCPM is currently set up the
;   geographic longitude is a more accurate longitude to use. 
; CALLING SEQUENCE:
;   writeStationsLGlon,d,file
; INPUTS:
;   d={}. The sorted structured returned by loadEMMAData called with
;   the SORT and STATIONS keywords supplied.
;   FILE=STRING. Name of the file to write to.
; OUTPUTS:
;   Writes to FILE the following. Sets of null-terminated station
;   name strings, l-shell (float), and magnetic longitude (float) in
;   degrees.
; PROCEDURE:
;   Finds all stations pairs that are in the data structure and uses
;   the first one of each. The name is the string NAME, the L-shell
;   used is LSHELL. For geographic longitude first look for tag
;   glon. If it is found it is assumed to be in degrees. If it is not
;   found then use tag GLT (assumed to be geographic local time in
;   hours), and subtract HRUT (assumed to be in hours UT). If that is
;   not found then use tag LOCALTIME (assumed to be in hours), and
;   subtract tag HRUT (assumed to be in hours). Bound glon to 0-360
;   degrees. 
;-
  
  openw,un,file,/get_lun

  for i=0,n_tags(d)-1 do begin
     tmp=tag_names(d.(i)[0])
     if (where(tmp eq 'GLON'))[0] ne -1 then $
        glon=tmp.glon $
     else if(where(tmp eq 'GLT'))[0] ne -1 then $
        glon=((tmp.glt-tmp.hrut+48.)*15) mod 360. $
     else if (where(tmp eq 'LOCALTIME'))[0] ne -1 then $
        glon=((tmp.localtime-tmp.hrut+48.)*15) mod 360. $
     else $
        message,'Could not find a tag for local time of longitude'
     printf,un,d.(i)[0].name,' ',float(d.(i)[0].l),' ',glon
  endfor
  
  free_lun,un
  
end
