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
;   used is LSHELL, and the geographic longitude is the GLT minus the
;   HRUT, converted to degrees (multiplied by 15), and bounded in the
;   0-360- range.
;-
  
  openw,un,file,/get_lun

  for i=0,n_tags(d)-1 do begin
     glon=((d.(i)[0].glt-d.(i)[0].hrut+48)*15) mod 360.
     printf,un,d.(i)[0].name,' ',float(d.(i)[0].l),' ',$
            float(((d.(i)[0].glt-d.(i)[0].hrut+48)*15) mod 360.)
  endfor
  
  free_lun,un
  
end
