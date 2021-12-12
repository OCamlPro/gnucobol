       identification division.
       program-id. testxad4.
       data division.
       working-storage section.


       01 ctl pic x(80).
       01 dta-mask.
         05 dta pic x(40).

       procedure division.

       move "reverse-video underline" to ctl
       perform with test after until ctl = spaces
         display "Control:" at 0101
         display ctl at 0110
         accept ctl at 0110

      *   if ctl <> spaces
           move "Hello World" to dta

           display dta-mask at 0301 with control ctl
           display dta-mask at 0501 with control ctl
           accept dta-mask at 0501 with control ctl
      *   end-if
       end-perform

       stop run
       .
