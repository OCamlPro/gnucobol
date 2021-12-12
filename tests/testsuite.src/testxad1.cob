       identification division.
       program-id. testxad1.
       data division.
       working-storage section.

       78 ATTR-NORM	value 0.
       78 ATTR-ACS	value 33.

       01 l    pic 9(2) value 0.
       01 c    pic 9(2) value 0.
       01 c1   pic 9 value 0.
       01 c2   pic 9(2) value 0.
       01 c2r redefines c2.
         05 c21   pic 9.
         05 c22   pic 9.

       01 spos pic 9(4) value 0.
       01 spos-r redefines spos.
         05 spos-y  pic 9(2).
         05 spos-x  pic 9(2).

       01 xpos.
         05 xpos-l  pic x comp-x.
         05 xpos-c  pic x comp-x.

       01 attr pic 9(3) value 0.

       01 abits0.
         05 abits pic x(20) value spaces.

       01 d.
         05 d1 pic x value "<".
         05 d24 pic 9(3) value 0.
         05 d5 pic x value ">".

       01 x pic x.

       procedure division.

       perform varying c1 from 1 by 1 until c1 > 8
       	 compute spos = 100 + c1 * 10
         display c1 at spos
         perform varying c2 from 1 by 1 until c2 > 10
	   compute spos = 200 + (c1 - 1) * 10 + c2
	   display c22 at spos
         end-perform
       end-perform

       perform varying c2 from 3 by 1 until c2 > 24
       	 compute spos = c2 * 100 + 1
         display c2 at spos
       end-perform

       call "CBL_XAD_SETATTRBYTE" using by value ATTR-ACS

       display "l" at 0303

       perform varying c2 from 4 by 1 until c2 > 80
       	 compute spos = 300 + c2
       	 display "q" at spos
       end-perform

       perform varying c2 from 4 by 1 until c2 > 24
       	 compute spos = c2 * 100 + 3
       	 display "x" at spos
       end-perform

       call "CBL_XAD_SETATTRBYTE" using by value ATTR-NORM
      * perform Get-Key

       call "CBL_WRITE_SCR_N_ATTR" using
         by value 0303,
         by value x"01",
         by value 75
       end-call
      * perform Get-Key

       call "CBL_WRITE_SCR_N_ATTR" using
         by value 0403,
         by value x"00",
         by value 75
       end-call
      * perform Get-Key

       call "CBL_WRITE_SCR_N_ATTR" using
         by value 0503,
         by value x"11",
         by value 75
       end-call
      * perform Get-Key

       perform varying c2 from 3 by 1 until c2 > 14
         move c2 to xpos-l
         move 4 to xpos-c
         call "CBL_WRITE_SCR_CHARS" using
           xpos,
           by value "abcdefghijklmnopqrstuvwxyz0123456789",
           by value 36
         end-call
       end-perform
       perform Get-Key

       display "abcdefghijklmnopqrstuvwxyz0123456789" at 1604
       perform Get-Key

       call "CBL_WRITE_SCR_N_ATTR" using
         by value 0303,
         by value x"01",
         by value 75
       end-call
       perform Get-Key
       perform Refresh-Screen
       perform Get-Key

       call "CBL_WRITE_SCR_N_ATTR" using
         by value 0403,
         by value x"30",
         by value 75
       end-call
       perform Get-Key
       perform Refresh-Screen
       perform Get-Key

       call "CBL_WRITE_SCR_N_ATTR" using
         by value 0503,
         by value x"0f",
         by value 75
       end-call
       perform Get-Key
       perform Refresh-Screen
       perform Get-Key

       display "." at 2480
       perform Get-Key
       perform Refresh-Screen
       perform Get-Key

       stop run

       perform varying l from 1 by 1 until l > 16
         perform varying c from 1 by 1 until c > 16
           compute spos = l * 100 + (c - 1) * 5 + 1 + 300
           compute attr = (l - 1) * 16 + (c - 1)
           move attr to d24
           call "CBL_XAD_SETATTRBYTE" using attr
           display d at spos
         end-perform
       end-perform

       perform Get-Key

       call "CBL_XAD_SETATTRBYTE" using by value 0
       display "abcdefghijklmnopqrstuvwxyz01234" at 1901

       perform Get-Key
       call "CBL_XAD_SETATTRBYTE" using by value 32
       display "abcdefghijklmnopqrstuvwxyz01234" at 2001
       display "ABCDEFGHIJKLMNOPQRSTUVWXYZ56789" at 2101

       perform Get-Key
       call "CBL_XAD_SETATTRBYTE" using by value 0
       display "ABCDEFGHIJKLMNOPQRSTUVWXYZ56789" at 2201

       perform Get-Key
       move all "*" to abits
       call "CBL_XAD_GETATTRBITS" using abits
       display "<                                >" at 1941
      *move all "_" to abits
       display abits at 1942
      *display "<", abits, ">" at 2022
       display " " at 2401
       perform Get-Key
       stop run
      -.


       Get-Key.
         call "CBL_READ_KBD_CHAR" using x
         display x at 0180 with reverse
      -.


       Refresh-Screen.
         call "CBL_XAD_REDRAW_SCR" using by value 0
      -.
