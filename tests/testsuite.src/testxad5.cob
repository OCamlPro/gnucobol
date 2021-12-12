       identification division.
       program-id. testxad5.
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

       01 txt                   pic x(3) value spaces.
       01 ctl                   pic x(80) value spaces.

       01 fgc                   pic 9 value 7.
       01 bgc                   pic 9 value 0.


      *---------------
       screen section.
      *---------------


      *-------------------
       procedure division.
      *-------------------

       accept testcase from command-line

       display spaces at 0101

       evaluate testcase
       when "colors-display-ctrl"
       	 display "Expect: Colors" at 0101
       	 perform colors-display-ctrl

       when other
         move "unknown testcase" to testcase
       end-evaluate

       perform display-color-frame

       display testcase at 2401
       display test-result at 2450

       if test-result = "passed"
         move 0 to return-value
       else
         move 1 to return-value
       end-if

       goback returning return-value
       .


      *--------------------
       display-color-frame.
      *--------------------
       perform varying bgc from 0 by 1 until bgc > 7
         string
          " ", bgc, " "
          delimited by size into txt

         compute spos-y = bgc + 2 + 2
         compute spos-x = 1

         display txt(1:3) at spos with background-color bgc

         add 40 to spos-x
         display txt(1:3) at spos with background-color bgc

         compute spos-y = 2
         compute spos-x = bgc * 4 + 1 + 5

         display txt(1:3) at spos with background-color bgc

         add 40 to spos-x
         display txt(1:3) at spos with background-color bgc
       end-perform
       .


      *--------------------
       colors-display-ctrl.
      *--------------------
       perform varying bgc from 0 by 1 until bgc > 7
         perform varying fgc from 0 by 1 until fgc > 7
           move spaces to ctl
           string
            "background-color is ", bgc, " ",
            "foreground-color is ", fgc, " "
            delimited by size into ctl

           move spaces to txt
           string
            fgc, "-", bgc
            delimited by size into txt

           compute spos-y = bgc + 2 + 2
           compute spos-x = fgc * 4 + 1 + 5

           display txt at spos, control ctl

           add 40 to spos-x
           display txt at spos, with reverse-video, control ctl
         end-perform
       end-perform
       .
