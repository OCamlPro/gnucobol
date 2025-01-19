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

      *> Modus operandi

      *> put the window on the top of the deck of windows
       78  WP-WINDOW-TOP                    VALUE 'T'.
      *> put the window on the bottom of the deck of windows
       78  WP-WINDOW-BOTTOM                 VALUE 'B'.

      *> create a new window and place it on the top of the deck of windows
       78  WP-WINDOW-NEW                    VALUE 'N'.
      *> physically delete the window from memory
       78  WP-WINDOW-DELETE                 VALUE 'D'.

      *> get the attributes (position / size / default color) of a window
       78  WP-WINDOW-GET-ATTRIBUTES         VALUE 'G'.
      *> change the window's attributes (partially implemented)
       78  WP-WINDOW-SET-ATTRIBUTES         VALUE 'S'.

      *> the following *MAY* be dropped in favor of [LIST/GET+] SET
      *> move the window
       78  WP-WINDOW-MOVE                   VALUE 'M'.
      *> make the window invisible / hide it
       78  WP-WINDOW-HIDE                   VALUE 'I'.
      *> make a previously hidden window visible
       78  WP-WINDOW-SHOW                   VALUE 'V'.

      *> get list of windows and their attributes
       78  WP-WINDOW-LIST                   VALUE 'L'.


      *>   Optional flags for the windows
       78  WP-FLAG-HIDDEN                   VALUE h'01'.
       78  WP-FLAG-NOSCROLL                 VALUE h'02'.
       78  WP-FLAG-NOWRAP                   VALUE h'04'.

      *> secondary parameter block for all commands
      *> but the listingcontaining definitions
       01  WP-WINDOW-PARMS.
           03  WP-WINDOW-NUMBER             USAGE BINARY-SHORT.
           03  WP-WINDOW-ATTRIBUTES.
               05  WP-WINDOW-START-LINE     USAGE BINARY-SHORT.
               05  WP-WINDOW-START-COL      USAGE BINARY-SHORT.
               05  WP-WINDOW-LINES          USAGE BINARY-SHORT.
               05  WP-WINDOW-COLUMNS        USAGE BINARY-SHORT.
               05  WP-WINDOW-FG-COLOR       USAGE BINARY-SHORT.
               05  WP-WINDOW-BG-COLOR       USAGE BINARY-SHORT.
               05  WP-WINDOW-FLAGS          USAGE BINARY-SHORT.

      *> convenience variable for possible call after SET WP-MODE-...
       01  WP-WINDOW-MODE                   PIC X.
           88  WP-MODE-NEW                  VALUE WP-WINDOW-NEW.
           88  WP-MODE-DELETE               VALUE WP-WINDOW-DELETE.
           88  WP-MODE-MOVE                 VALUE WP-WINDOW-MOVE.
           88  WP-MODE-SHOW                 VALUE WP-WINDOW-SHOW.
           88  WP-MODE-HIDE                 VALUE WP-WINDOW-HIDE.
           88  WP-MODE-TOP                  VALUE WP-WINDOW-TOP.
           88  WP-MODE-BOTTOM               VALUE WP-WINDOW-BOTTOM.
           88  WP-MODE-LIST                 VALUE WP-WINDOW-LIST.

      *> RETURN-CODE value convenience variable allowing named checks
       01  WP-WINDOW-RETURN-CODE            USAGE BINARY-INT.
           88  WP-UNSUPPORTED               VALUE -1.
           88  WP-DISABLED                  VALUE -2.
           88  WP-BAD-PARAMETER-LEN         VALUE -8.
           88  WP-OK                        VALUE 0.
           88  WP-LINE-EXCEEDS-MAX          VALUE 2.
           88  WP-COL-EXCEEDS-MAX           VALUE 4.
           88  WP-WIN-LINES-EXCEEDS-MAX     VALUE 6.
           88  WP-WIN-COLS-EXCEEDS-MAX      VALUE 7.
           88  WP-INVALID-BG-COLOR          VALUE 8.
           88  WP-INVALID-FG-COLOR          VALUE 9.
           88  WP-WIN-NUMBER-INVALID        VALUE 10.
           88  WP-FG-BG-COLORS-ARE-SAME     VALUE 11.
           88  WP-NO-MORE-WINDOWS-LEFT      VALUE 20.
           88  WP-NEWWIN-FAILED             VALUE 22.
           88  WP-NEW-PANEL-FAILED          VALUE 24.
           88  WP-PAN-USRPTR-FAILED         VALUE 26.
           88  WP-WINDOW-NOT-FOUND          VALUE 28.
           88  WP-MOVE-WINDOW-FAILED        VALUE 29.
           88  WP-SHOW-WINDOW-FAILED        VALUE 30.
           88  WP-HIDE-WINDOW-FAILED        VALUE 32.
           88  WP-TOP-WINDOW-FAILED         VALUE 34.
           88  WP-BOTTOM-WINDOW-FAILED      VALUE 35.
           88  WP-DELETE-WINDOW-FAILED      VALUE 36.
           88  WP-DELETE-PANEL-FAILED       VALUE 38.
           88  WP-WINDOW-IS-HIDDEN          VALUE 40.
           88  WP-WINDOW-IS-NOT-HIDDEN      VALUE 42.
           88  WP-WINDOW-MODE-IS-INVALID    VALUE 44.

      *> communication area to retrieve information about all windows created,
      *> including the windows currently hidden; the visible/active ones will
      *> be in order of their depth within the visible window-stack;
      *> elements that are complete empty (low values) represent the end of
      *> used elements
       01  WL-WINDOW-LIST.
           05  WL-WINDOW           OCCURS 20 TIMES
                                   INDEXED BY WL-INDEX.
             88 WL-WINDOW-UNUSED   VALUE ALL LOW-VALUE.
               10  WL-DEPTH                USAGE BINARY-SHORT.
               10  WL-WINDOW-NUMBER        USAGE BINARY-SHORT.
               10  WL-WINDOW-POSITION-LINE USAGE BINARY-SHORT.
               10  WL-WINDOW-POSITION-COL  USAGE BINARY-SHORT.
               10  WL-WINDOW-SIZE-LINES    USAGE BINARY-SHORT.
               10  WL-WINDOW-SIZE-COLS     USAGE BINARY-SHORT.
               10  WL-WINDOW-FG-COLOR      USAGE BINARY-SHORT.
               10  WL-WINDOW-BG-COLOR      USAGE BINARY-SHORT.
               10  WL-WINDOW-FLAGS         USAGE BINARY-SHORT.
