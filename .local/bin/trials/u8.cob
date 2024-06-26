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
           01 x pic 9(2) value 5.
           01 y pic 9(2) value 10.
           01 z pic 9(2) value 0.
           01 ahmed pic u(3) value 'foo'. 
       procedure division.
           compute z = x + y.
           display "hello world".
           DISPLAY "UTF8: -" FUNCTION HEX-OF (ahmed) "-".
       stop run.

