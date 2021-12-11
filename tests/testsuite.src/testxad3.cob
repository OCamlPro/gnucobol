       identification division.
       program-id. testxad3.
       data division.
       working-storage section.

       01 return-value          pic 9 value 0.

       01 i                     pic 9(2) value 0.

       01 testcase              pic x(50) value spaces.
       01 test-result           pic x(10) value spaces.
       01 test-line             pic x(30) value spaces.

       01 spos                  pic 9(4) value 0.
       01 spos-r                redefines spos.
         05 spos-y              pic 9(2).
         05 spos-x              pic 9(2).

       01 xpos.
         05 xpos-l              pic x comp-x.
         05 xpos-c              pic x comp-x.


       01 pattern.
         05 pattern-01 pic x(30) value "+----------------------------+".
         05 pattern-02 pic x(30) value "!*******.......*******.......!".
         05 pattern-03 pic x(30) value "!#######.......#######.......!".
         05 pattern-04 pic x(30) value "!XXXXXXX.......XXXXXXX.......!".
         05 pattern-05 pic x(30) value "!@@@@@@@.......@@@@@@@.......!".
         05 pattern-06 pic x(30) value "!.......*******.......*******!".
         05 pattern-07 pic x(30) value "!.......#######.......#######!".
         05 pattern-08 pic x(30) value "!.......XXXXXXX.......XXXXXXX!".
         05 pattern-09 pic x(30) value "!.......@@@@@@@.......@@@@@@@!".
         05 pattern-10 pic x(30) value "+----------------------------+".

       01 pattern-lines redefines pattern.
         05 pattern-line pic x(30) occurs 10 times.

       01 pattern-item.
         05 pattern-item-lines occurs 10 times.
           10 pattern-item-line pic x(30).
           10 filler pic x(50).

       01 frames.
       05 frame-top pic x(32) value " vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv ".
       05 frame-row pic x(32) value ">                              <".
       05 frame-bot pic x(32) value " ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ".


       screen section.

       01 check-screen.
         05 check-frame-s-t using frame-top, column 39, line 04.
         05 check-frame-s-01 using frame-row, column 39, line + 1.
         05 check-frame-s-02 using frame-row, column 39, line + 1.
         05 check-frame-s-03 using frame-row, column 39, line + 1.
         05 check-frame-s-04 using frame-row, column 39, line + 1.
         05 check-frame-s-05 using frame-row, column 39, line + 1.
         05 check-frame-s-06 using frame-row, column 39, line + 1.
         05 check-frame-s-07 using frame-row, column 39, line + 1.
         05 check-frame-s-08 using frame-row, column 39, line + 1.
         05 check-frame-s-09 using frame-row, column 39, line + 1.
         05 check-frame-s-10 using frame-row, column 39, line + 1.
         05 check-frame-s-b using frame-bot, column 39, line + 1.

         05 check-screen-01 using pattern-01, column 40, line 05.
         05 check-screen-02 using pattern-02, column 40, line + 1.
         05 check-screen-03 using pattern-03, column 40, line + 1.
         05 check-screen-04 using pattern-04, column 40, line + 1.
         05 check-screen-05 using pattern-05, column 40, line + 1.
         05 check-screen-06 using pattern-06, column 40, line + 1.
         05 check-screen-07 using pattern-07, column 40, line + 1.
         05 check-screen-08 using pattern-08, column 40, line + 1.
         05 check-screen-09 using pattern-09, column 40, line + 1.
         05 check-screen-10 using pattern-10, column 40, line + 1.

         05 check-frame-t using frame-top, column 02, line 04.
         05 check-frame-l-01 value ">", column 02, line + 1.
         05 check-frame-l-02 value ">", column 02, line + 1.
         05 check-frame-l-03 value ">", column 02, line + 1.
         05 check-frame-l-04 value ">", column 02, line + 1.
         05 check-frame-l-05 value ">", column 02, line + 1.
         05 check-frame-l-06 value ">", column 02, line + 1.
         05 check-frame-l-07 value ">", column 02, line + 1.
         05 check-frame-l-08 value ">", column 02, line + 1.
         05 check-frame-l-09 value ">", column 02, line + 1.
         05 check-frame-l-10 value ">", column 02, line + 1.
         05 check-frame-r-01 value "<", column 33, line 05.
         05 check-frame-r-02 value "<", column 33, line + 1.
         05 check-frame-r-03 value "<", column 33, line + 1.
         05 check-frame-r-04 value "<", column 33, line + 1.
         05 check-frame-r-05 value "<", column 33, line + 1.
         05 check-frame-r-06 value "<", column 33, line + 1.
         05 check-frame-r-07 value "<", column 33, line + 1.
         05 check-frame-r-08 value ">", column 33, line + 1.
         05 check-frame-r-09 value "<", column 33, line + 1.
         05 check-frame-r-10 value "<", column 33, line + 1.
         05 check-frame-b using frame-bot, column 02, line + 1.


      *-------------------
       procedure division.
      *-------------------

       accept testcase from command-line

       move spaces to pattern-item.
       perform varying i from 1 by 1 until i > 10
         move pattern-line(i) to pattern-item-line(i)
       end-perform

       display spaces at 0101

       evaluate testcase
       when "display-check-screen"
         display check-screen

       when "display-pattern-item"
       	 display "Expect: Pattern within frame" at 0101
         display pattern-item at 0503

       when "display-pattern-lines"
       	 display "Expect: Pattern within frame" at 0101
         perform varying i from 1 by 1 until i > 10
           compute spos = i * 100 + 403
           display pattern-line(i) at spos
         end-perform

       when "write-scr-chars-pattern-item"
       	 display "Expect: Pattern within frame" at 0101
         move 4 to xpos-l
         move 2 to xpos-c
         call "CBL_WRITE_SCR_CHARS"
           using xpos,
           pattern-item,
           800
         end-call

       when "write-scr-chars-pattern-lines"
       	 display "Expect: Pattern within frame" at 0101
         perform varying i from 1 by 1 until i > 10
           compute xpos-l = i + 3
           move 2 to xpos-c
           call "CBL_WRITE_SCR_CHARS"
             using xpos,
             pattern-line(i),
             30
           end-call
         end-perform

       when other
         move "unknown testcase" to testcase
       end-evaluate

       display check-screen

       perform check-pattern-on-screen

       display testcase at 2401
       display test-result at 2450

       if test-result = "passed"
         move 0 to return-value
       else
         move 1 to return-value
       end-if

       goback returning return-value
       .


      *------------------------
       check-pattern-on-screen.
      *------------------------
       move spaces to test-result

       perform varying i from 1 by 1 until i > 10
         compute xpos-l = i + 3
         move 2 to xpos-c
         call "CBL_READ_SCR_CHARS"
           using xpos,
           test-line,
           30
         end-call

         if test-line <> pattern-line(i)
           move "failed:" to test-result
           move i to test-result(8:2)
           exit perform
         end-if
       end-perform

       if test-result = spaces
         move "passed" to test-result
       end-if
       .
