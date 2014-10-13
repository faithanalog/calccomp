;+----------------------------------------+
;|      Doors CSE v8.0 Include File       |
;|           TI-84+CSE Version            |
;|        Revised October 8, 2013         |
;|        by Christopher Mitchell         |
;|           aka Kerm Martian             |
;|                                        |
;| Please see http://dcs.cemetech.net for |
;| information and updates including a    |
;| full Developers' SDK.                  |
;+----------------------------------------+

; Graphics Routines
ClearLCDFull            = $402A
ColorLine               = $402D
ColorPixel				= $4030
ColorRectangle          = $4033
DrawSprite_1Bit         = $4036
DrawSprite_2Bit         = $4039
DrawSprite_4Bit         = $403C
DrawSprite_4Bit_Enlarge = $403F
DrawSprite_8Bit			= $4042

; Math Routines
MultHE                  = $404B
MultDEBC                = $404E
DivHLC                  = $4051
RandInt                 = $4054

; Misc Routines
RunProg					= $405A

; Header Constants
ASMHEADER_FIELD_TYPE_DESC .equ 0
ASMHEADER_FIELD_TYPE_ICON .equ 1
ASMHEADER_FIELD_TYPE_AUTH .equ 2
ASMHEADER_FIELD_TYPE_LIB  .equ 3
ASMHEADER_FIELD_TYPE_HALFRES .equ 4

;End of file
;end
