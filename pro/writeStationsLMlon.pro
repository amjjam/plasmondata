pro writeStationsLMlon,d,file

;+
; NAME:
;   writeStationsLMlon
; PURPOSE:
;   Write a file which contains the name, L-shell, and magnetic
;   longitude of stations contained in a data structure.
; CALLING SEQUENCE:
;   writeStationsLMlon,d,file
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
;   used is LSHELL, and the magnetic longitude is the MLT minus the
;   HRUT, converted to degrees (multiplied by 15), and bounded in the
;   0-360- range.
;-
  
  openw,un,file,/get_lun

  for i=0,n_tags(d)-1 do begin
     mlon=((d.(i)[0].mlt-d.(i)[0].hrut+48)*15) mod 360.
     printf,un,d.(i)[0].name,' ',float(d.(i)[0].l),' ',$
            float(((d.(i)[0].mlt-d.(i)[0].hrut+48)*15) mod 360.)
  endfor
  
  free_lun,un
  
end
