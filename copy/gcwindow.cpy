      *>  Copyright (C) 2024
      *>  Free Software Foundation, Inc.
      *>  Written by Chuck Haatvedt, Simon Sobisch
      *>
      *>  This file is part of GnuCOBOL.
      *>
      *>  The GnuCOBOL compiler is free software: you can redistribute
      *>  it and/or modify it under the terms of the GNU General Public
      *>  License as published by the Free Software Foundation, either
      *>  version 3 of the License, or (at your option) any later
      *>  version.
      *>
      *>  GnuCOBOL is distributed in the hope that it will be useful,
      *>  but WITHOUT ANY WARRANTY; without even the implied warranty of
      *>  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
      *>  GNU General Public License for more details.
      *>
      *>  You should have received a copy of the GNU General Public
      *>  License along with GnuCOBOL.
      *>  If not, see <https://www.gnu.org/licenses/>.

      *>* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *>                                                              *
      *>   THIS COMMUNICATION AREA IS USED TO RETRIEVE                *
      *>   INFORMATION ABOUT THE WINDOWS WHICH HAVE BEEN CREATED      *
      *>   BY THE CBL_GC_WINDOW_NEW FUNCTION. IT WILL REPORT ON       *
      *>   ALL OF THE WINDOWS BOTH ACTIVE AND HIDDEN. THE ACTIVE      *
      *>   WINDOWS WILL BE IN ORDER OF THEIR DEPTH ON THE STACK       *
      *>   OF WINDOWS. AN ENTRY OF ALL LOW VALUES WILL REPRESENT      *
      *>   THE NON EXISTANCE OF ANY MORE WINDOWS.                     *
      *>                                                              *
      *>* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

       01  WL-WINDOW-LIST.
           05  WL-WINDOW           OCCURS 20 TIMES
                                   INDEXED BY WL-INDEX.
               10  WL-DEPTH                PIC S9(4)   COMP-5.
               10  WL-WINDOW-NUMBER        PIC S9(4)   COMP-5.
               10  WL-WINDOW-POSITION-ROW  PIC S9(4)   COMP-5.
               10  WL-WINDOW-POSITION-COL  PIC S9(4)   COMP-5.
               10  WL-WINDOW-SIZE-ROW      PIC S9(4)   COMP-5.
               10  WL-WINDOW-SIZE-COL      PIC S9(4)   COMP-5.
               10  WL-WINDOW-FG-COLOR      PIC S9(4)   COMP-5.
               10  WL-WINDOW-BG-COLOR      PIC S9(4)   COMP-5.
               10  WL-WINDOW-HIDDEN        PIC S9(4)   COMP-5.

      *>* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *>                                                              *
      *>   This copy member is used to call the Multiple Window       *
      *>   CBL_GC_WINDOW functions. Note that the return value        *
      *>   is passed via the COBOL RETURN-CODE; also note that these  *
      *>   functions only work when in extended i/o mode.             *
      *>   See the Programmers Guide for more information on the use  *
      *>   of these functions.                                        *
      *>                                                              *
      *>* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

       01  WP-WINDOW-PARMS.
           05  WP-WINDOW-MODE              PIC 9(09) COMP-5.
               88  WP-MODE-NEW-WINDOW          VALUE 1.
               88  WP-MODE-DELETE-WINDOW       VALUE 2.
               88  WP-MODE-MOVE-WINDOW         VALUE 3.
               88  WP-MODE-SHOW-WINDOW         VALUE 4.
               88  WP-MODE-HIDE-WINDOW         VALUE 5.
               88  WP-MODE-TOP-WINDOW          VALUE 6.
               88  WP-MODE-BOTTOM-WINDOW       VALUE 7.
      *>       88  WP-MODE-LIST-WINDOW         VALUE 8.
           05  WP-WINDOW-NUMBER            PIC 9(09) COMP-5.
           05  WP-WINDOW-START-ROW         PIC 9(09) COMP-5.
           05  WP-WINDOW-START-COL         PIC 9(09) COMP-5.
           05  WP-WINDOW-ROWS              PIC 9(09) COMP-5.
           05  WP-WINDOW-COLUMNS           PIC 9(09) COMP-5.
           05  WP-WINDOW-FG-COLOR          PIC 9(09) COMP-5.
           05  WP-WINDOW-BG-COLOR          PIC 9(09) COMP-5.
           05  WP-WINDOW-HIDDEN            PIC 9(09) COMP-5.

      *> RETURN-CODE value
       01  WP-WINDOW-RETURN-CODE       PIC 9(09) COMP-5.
               88  WP-UNSUPPORTED              VALUE -1.
               88  WP-DISABLED                 VALUE -2.
               88  WP-BAD-PARAMETER-LEN        VALUE -8.
               88  WP-OK                       VALUE 0.
               88  WP-START-Y-EXCEEDS-MAX      VALUE 2.
               88  WP-START-X-EXCEEDS-MAX      VALUE 4.
               88  WP-WIN-ROWS-EXCEEDS-MAX     VALUE 6.
               88  WP-WIN-COLS-EXCEEDS-MAX     VALUE 7.
               88  WP-INVALID-BG-COLOR         VALUE 8.
               88  WP-INVALID-FG-COLOR         VALUE 9.
               88  WP-WIN-NUMBER-INVALID       VALUE 10.
               88  WP-FG-BG-COLORS-ARE-SAME    VALUE 11.
               88  WP-WIN-HIDDEN-NOT-1         VALUE 12.
               88  WP-WIN-HIDDEN-NOT-0         VALUE 14.
               88  WP-TOP-WIN-NBR-INVALID      VALUE 16.
               88  WP-BOTTOM-WIN-NBR-INVALID   VALUE 18.
               88  WP-NO-MORE-WINDOWS-LEFT     VALUE 20.
               88  WP-NEWWIN-FAILED            VALUE 22.
               88  WP-NEW-PANEL-FAILED         VALUE 24.
               88  WP-PAN-USRPTR-FAILED        VALUE 26.
               88  WP-WINDOW-NOT-FOUND         VALUE 28.
               88  WP-MOVE-WINDOW-FAILED       VALUE 29.
               88  WP-SHOW-WINDOW-FAILED       VALUE 30.
               88  WP-HIDE-WINDOW-FAILED       VALUE 32.
               88  WP-TOP-WINDOW-FAILED        VALUE 34.
               88  WP-BOTTOM-WINDOW-FAILED     VALUE 35.
               88  WP-DELETE-WINDOW-FAILED     VALUE 36.
               88  WP-DELETE-PANEL-FAILED      VALUE 38.
               88  WP-WINDOW-IS-HIDDEN         VALUE 40.
               88  WP-WINDOW-IS-NOT-HIDDEN     VALUE 42.
               88  WP-WINDOW-MODE-IS-INVALID   VALUE 44.
