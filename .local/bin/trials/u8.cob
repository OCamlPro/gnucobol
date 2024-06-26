       identification division.
       program-id. hello.
       author. ahmed maher.
       date-written. 2/3/2024.
       date-compiled. 2/3/2024.
       security.  non confidential.

      *this is an optional division to define the machine dependent details 
       environment division.
       configuration section.
       source-computer. ubuntu-22.
       object-computer. ubuntu-22.
    

       data division.
           working-storage section.
           01 x pic x(3) value '€'. 
       procedure division.
           display "hello world".
           DISPLAY "UTF8: -" FUNCTION HEX-OF (x) "-".
       stop run.

