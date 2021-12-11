/*
	Copyright (C) 2008-2012, 2015-2016,
	2021 Free Software Foundation, Inc.
	Written by Roger While, Simon Sobisch, Christian Lademann
	
	This file is part of GnuCOBOL.
	
	The GnuCOBOL compiler is free software: you can redistribute
	it and/or modify it under the terms of the GNU General Public
	License as published by the Free Software Foundation, either
	version 3 of the License, or (at your option) any later
	version.
	
	GnuCOBOL is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public
	License along with GnuCOBOL.
	If not, see <https://www.gnu.org/licenses/>.
*/

/* Definitions taken from screenio.copy */

#ifndef COB_SCREENIO_COMMON_H
#define COB_SCREENIO_COMMON_H

/* Colors */
#define COB_COLOR_BLACK			0
#define COB_COLOR_BLUE			1
#define COB_COLOR_GREEN			2
#define COB_COLOR_CYAN			3
#define COB_COLOR_RED			4
#define COB_COLOR_MAGENT		5
#define COB_COLOR_YELLOW		6
#define COB_COLOR_WHITE			7

/* mouse mask, apply to COB_MOUSE_FLAGS */
#define COB_AUTO_MOUSE_HANDLING		1
#define COB_ALLOW_LEFT_DOWN		2
#define COB_ALLOW_LEFT_UP		4
#define COB_ALLOW_LEFT_DOUBLE		8
#define COB_ALLOW_MIDDLE_DOWN		16
#define COB_ALLOW_MIDDLE_UP		32
#define COB_ALLOW_MIDDLE_DOUBLE		64
#define COB_ALLOW_RIGHT_DOWN		128
#define COB_ALLOW_RIGHT_UP		256
#define COB_ALLOW_RIGHT_DOUBLE		512
#define COB_ALLOW_MOUSE_MOVE		1024
#define COB_ALLOW_ALL_SCREEN_ACTIONS	16384	/* reserved */

/* Values that may be returned in CRT STATUS (or COB_CRT_STATUS) */

/* Normal return - Value 0000 */
#define COB_SCR_OK			 0
                                        
/* Function keys - Values 1xxx */
#define COB_SCR_FUNCTION_KEYS_MIN	1001
#define COB_SCR_FUNCTION_KEYS_MAX	1064

#define COB_SCR_F0			1000	/* Add function key to this */
#define COB_SCR_F1			1001
#define COB_SCR_F2			1002
#define COB_SCR_F3			1003
#define COB_SCR_F4			1004
#define COB_SCR_F5			1005
#define COB_SCR_F6			1006
#define COB_SCR_F7			1007
#define COB_SCR_F8			1008
#define COB_SCR_F9			1009
#define COB_SCR_F10			1010
#define COB_SCR_F11			1011
#define COB_SCR_F12			1012
#define COB_SCR_F13			1013
#define COB_SCR_F14			1014
#define COB_SCR_F15			1015
#define COB_SCR_F16			1016
#define COB_SCR_F17			1017
#define COB_SCR_F18			1018
#define COB_SCR_F19			1019
#define COB_SCR_F20			1020
#define COB_SCR_F21			1021
#define COB_SCR_F22			1022
#define COB_SCR_F23			1023
#define COB_SCR_F24			1024
#define COB_SCR_F25			1025
#define COB_SCR_F26			1026
#define COB_SCR_F27			1027
#define COB_SCR_F28			1028
#define COB_SCR_F29			1029
#define COB_SCR_F30			1030
#define COB_SCR_F31			1031
#define COB_SCR_F32			1032
#define COB_SCR_F33			1033
#define COB_SCR_F34			1034
#define COB_SCR_F35			1035
#define COB_SCR_F36			1036
#define COB_SCR_F37			1037
#define COB_SCR_F38			1038
#define COB_SCR_F39			1039
#define COB_SCR_F40			1040
#define COB_SCR_F41			1041
#define COB_SCR_F42			1042
#define COB_SCR_F43			1043
#define COB_SCR_F44			1044
#define COB_SCR_F45			1045
#define COB_SCR_F46			1046
#define COB_SCR_F47			1047
#define COB_SCR_F48			1048
#define COB_SCR_F49			1049
#define COB_SCR_F50			1050
#define COB_SCR_F51			1051
#define COB_SCR_F52			1052
#define COB_SCR_F53			1053
#define COB_SCR_F54			1054
#define COB_SCR_F55			1055
#define COB_SCR_F56			1056
#define COB_SCR_F57			1057
#define COB_SCR_F58			1058
#define COB_SCR_F59			1059
#define COB_SCR_F60			1060
#define COB_SCR_F61			1061
#define COB_SCR_F62			1062
#define COB_SCR_F63			1063
#define COB_SCR_F64			1064

/* Exception keys _ Values 2xxx */
#define COB_SCR_EXCEPTION_KEYS_MIN	2001
#define COB_SCR_EXCEPTION_KEYS_MAX	2110

#define COB_SCR_PAGE_UP			2001
#define COB_SCR_PAGE_DOWN		2002
#define COB_SCR_KEY_UP			2003
#define COB_SCR_KEY_DOWN		2004
#define COB_SCR_ESC			2005
#define COB_SCR_PRINT			2006
#define COB_SCR_TAB			2007
#define COB_SCR_BACK_TAB		2008
#define COB_SCR_KEY_LEFT		2009
#define COB_SCR_KEY_RIGHT		2010

/* The following exception keys are currently *only* returned
   on ACCEPT OMITTED */
#define COB_SCR_INSERT			2011
#define COB_SCR_DELETE			2012
#define COB_SCR_BACKSPACE		2013
#define COB_SCR_KEY_HOME		2014
#define COB_SCR_KEY_END			2015

/* Exception keys for mouse handling */
#define COB_SCR_MOUSE_MOVE		2040
#define COB_SCR_LEFT_PRESSED		2041
#define COB_SCR_LEFT_RELEASED		2042
#define COB_SCR_LEFT_DBL_CLICK		2043
#define COB_SCR_MID_PRESSED		2044
#define COB_SCR_MID_RELEASED		2045
#define COB_SCR_MID_DBL_CLICK		2046
#define COB_SCR_RIGHT_PRESSED		2047
#define COB_SCR_RIGHT_RELEASED		2048
#define COB_SCR_RIGHT_DBL_CLICK		2049
#define COB_SCR_SHIFT_MOVE		2050
#define COB_SCR_SHIFT_LEFT_PRESSED	2051
#define COB_SCR_SHIFT_LEFT_RELEASED	2052
#define COB_SCR_SHIFT_LEFT_DBL_CLICK	2053
#define COB_SCR_SHIFT_MID_PRESSED	2054
#define COB_SCR_SHIFT_MID_RELEASED	2055
#define COB_SCR_SHIFT_MID_DBL_CLICK	2056
#define COB_SCR_SHIFT_RIGHT_PRESSED	2057
#define COB_SCR_SHIFT_RIGHT_RELEASED	2058
#define COB_SCR_SHIFT_RIGHT_DBL_CLICK	2059
#define COB_SCR_CTRL_MOVE		2060
#define COB_SCR_CTRL_LEFT_PRESSED	2061
#define COB_SCR_CTRL_LEFT_RELEASED	2062
#define COB_SCR_CTRL_LEFT_DBL_CLICK	2063
#define COB_SCR_CTRL_MID_PRESSED	2064
#define COB_SCR_CTRL_MID_RELEASED	2065
#define COB_SCR_CTRL_MID_DBL_CLICK	2066
#define COB_SCR_CTRL_RIGHT_PRESSED	2067
#define COB_SCR_CTRL_RIGHT_RELEASED	2068
#define COB_SCR_CTRL_RIGHT_DBL_CLICK	2069
#define COB_SCR_ALT_MOVE		2070
#define COB_SCR_ALT_LEFT_PRESSED	2071
#define COB_SCR_ALT_LEFT_RELEASED	2072
#define COB_SCR_ALT_LEFT_DBL_CLICK	2073
#define COB_SCR_ALT_MID_PRESSED		2074
#define COB_SCR_ALT_MID_RELEASED	2075
#define COB_SCR_ALT_MID_DBL_CLICK	2076
#define COB_SCR_ALT_RIGHT_PRESSED	2077
#define COB_SCR_ALT_RIGHT_RELEASED	2078
#define COB_SCR_ALT_RIGHT_DBL_CLICK	2079
#define COB_SCR_WHEEL_UP		2080
#define COB_SCR_WHEEL_DOWN		2081
#define COB_SCR_WHEEL_LEFT		2082	/* reserved */
#define COB_SCR_WHEEL_RIGHT		2083	/* reserved */
#define COB_SCR_SHIFT_WHEEL_UP		2084
#define COB_SCR_SHIFT_WHEEL_DOWN	2085
#define COB_SCR_SHIFT_WHEEL_LEFT	2086	/* reserved */
#define COB_SCR_SHIFT_WHEEL_RIGHT	2087	/* reserved */
#define COB_SCR_CTRL_WHEEL_UP		2088
#define COB_SCR_CTRL_WHEEL_DOWN		2089
#define COB_SCR_CTRL_WHEEL_LEFT		2090	/* reserved */
#define COB_SCR_CTRL_WHEEL_RIGHT	2091	/* reserved */
#define COB_SCR_ALT_WHEEL_UP		2092
#define COB_SCR_ALT_WHEEL_DOWN		2093
#define COB_SCR_ALT_WHEEL_LEFT		2094	/* reserved */
#define COB_SCR_ALT_WHEEL_RIGHT		2095	/* reserved */

#define COB_SCR_CARRIAGE_RETURN		2096	/* Move to first field in the next line */
#define COB_SCR_STOP_RUN  		2097	/* Terminate program */
#define COB_SCR_NEXT_FIELD		2098	/* Move to next field */
#define COB_SCR_PREV_FIELD		2099	/* Move to previous field */
#define COB_SCR_TOGGLE_CASE		2100	/* Toggle case of character under cursor */
#define COB_SCR_RETYPE_CHAR		2101
#define COB_SCR_RESTORE_CHAR		2102
#define COB_SCR_CLEAR_EOS		2103	/* Clear fields upto end of screen */
#define COB_SCR_CLEAR_FIELD		2104	/* Clear field */
#define COB_SCR_CLEAR_SCREEN		2105	/* Clear all fields */
#define COB_SCR_INSERT_MODE		2106	/* Switch to insert mode */
#define COB_SCR_REPLACE_MODE		2107	/* Switch to replace mode */
#define COB_SCR_UNDO			2108	/* Revert field */

#define COB_SCR_ENTER_FIELD		2109	/* Enter field */
#define COB_SCR_LEAVE_FIELD		2110	/* Leave field */

/* Input validation - Values 8xxx */
#define COB_SCR_NO_FIELD		8000
#define COB_SCR_TIME_OUT		8001

/*  Other errors - Values 9xxx */
#define COB_SCR_FATAL			9000
#define COB_SCR_MAX_FIELD		9001

#endif /* COB_SCREENIO_COMMON_H */
