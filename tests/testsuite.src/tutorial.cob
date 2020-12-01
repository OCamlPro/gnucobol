
      *> This is in most part the tutorial code from
      *> MicroFocus "external file handler" documentation.
      *>
      *> "Tutorial: Using the Callable File Handler"
      *>
      *> Left separate until possible integration into
      *> main testsuite is clear...
      *>
      $SET SOURCEFORMAT "VARIABLE"
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      tutorial.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  opcode                       pic x(2).
         78  OP-QUERY-FILE              value x"0006".
         78  OP-OPEN-INPUT              value x"fa00".
         78  OP-OPEN-OUTPUT             value x"fa01".
         78  OP-OPEN-I-O                value x"fa02".
         78  OP-WRITE                   value x"faf3".
         78  OP-RELEASE                 value x"faf3".
         78  OP-REWRITE                 value x"faf4".
         78  OP-READ-NEXT               value x"faf5".
         78  OP-START-EQUAL             value x"fae9".
         78  OP-CLOSE                   value x"fa80".


       01  FCD.
          copy 'xfhfcd3.cpy'.


       01 ex-filename                  pic x(260) value "idxfile.dat".
       01 ex-index-name	               pic x(100).  *> not used in different formats


       01 ex-keydef.                               
          47 key2length                pic 9(4)  comp-x.
          47 key-version               pic 9(2) comp-x value 2. 
          47 filler                    pic 9(6)  comp-x. *> reserved
          47 key-count                 pic 9(4)  comp-x.
          47 filler                    pic 9(13) comp-x. *> reserved

      *  key-specification is repeated for the number of keys defined by
      *  key-count
          47 key-specification. 
             49 component-count        pic 9(4) comp-x.
      * The offset for the component-specification for this key
             49 component-defs         pic 9(4) comp-x. 
             49 key-flags              pic 9(2) comp-x.
                  78 KEY2KEYFLAG-DUPS-IN-ORDER        value h"40".
                  78 KEY2KEYFLAG-PRIME                value h"10".
                  78 KEY2KEYFLAG-SPARSE-KEY           value h"02".
             49 key-compression        pic 9(2) comp-x.
                  78 KEY2COMPRESS-TRAILING-NULLS      value h"08".
                  78 KEY2COMPRESS-TRAILING-SPACES     value h"04".
                  78 KEY2COMPRESS-IDENTICAL-CHARS     value h"02".
                  78 KEY2COMPRESS-FOLLOWING-DUP       value h"01".
                  78 KEY2COMPRESS-NO-COMPRESSION      value h"00".
                  78 KEY2COMPRESS-DEFAULT        value KEY2COMPRESS-NO-COMPRESSION.
             49 sparse-characters      pic x(2).
             49 filler                 pic x(8). *> reserved


      *  component-specifications for all keys follows after the key-specifications
      *  for all the keys.
          47 component-specification.
             49 component-flags        pic 9(2) comp-x.
             49 component-type         pic 9(2) comp-x.  
                  78 KEY2PARTTYP-NUMERIC              value h"80".
                  78 KEY2PARTTYP-SIGNED               value h"40".
                  78 KEY2PARTTYP-COMP                 value h"20".
                  78 KEY2PARTTYP-COMP-3               value h"21".
                  78 KEY2PARTTYP-COMP-X               value h"22".
                  78 KEY2PARTTYP-COMP-5               value h"23".
                  78 KEY2PARTTYP-FLOAT                value h"24".
                  78 KEY2PARTTYP-COMP-6               value h"25".
                  78 KEY2PARTTYP-DISPLAY              value h"00".
                  78 KEY2PARTTYP-SIGN-TRAIL-INCL      value h"00".
                  78 KEY2PARTTYP-SIGN-TRAIL-SEP       value h"01".
                  78 KEY2PARTTYP-SIGN-LEAD-INCL       value h"02".
                  78 KEY2PARTTYP-SIGN-LEAD-SEP        value h"03".
                  78 KEY2PARTTYP-SIGN-LEAD-FLOAT      value h"04".
             49 component-offset       pic 9(9) comp-x.
             49 component-length       pic 9(9) comp-x.


      * storage for record
       01 ex-record.
               03 record-key           pic 9(5). 
               03 record-data          pic x(95).
             
       PROCEDURE        DIVISION.

      *>
      *> invoke part I
      *>

      *> Create an indexed file
      *>   open output an indexed file call "idxfile.dat"
           display "Create new indexed file"
           perform set-fcd
           move OP-OPEN-OUTPUT to opcode
           perform call-file-handler
           perform display-file-status.
           
      *> Write 5 records increasing record length by 1 each time
           move all "A" to record-data
           move 0 to record-key
           move 5 to fcd-current-rec-len
           move OP-WRITE to opcode
           perform 5 times
              add 1 to record-key
              add 1 to fcd-current-rec-len
              perform call-file-handler
           end-perform.
           
      *> Now close the file
           move OP-CLOSE to opcode
           perform call-file-handler
           perform display-file-status
           display "file closed".

      *>
      *> invoke part II
      *>
           
      *> Query the file to retrieve file information 
           move low-values to fcd
           set  fcd-filename-address  to address of ex-filename
           move 80                    to fcd-name-length
           move fcd--determine-org    to fcd-organization
           move fcd--version-number   to fcd-version
           set fcd-filename-address   to address of ex-filename
           set fcd-idxname-address    to address of ex-index-name
           set fcd-key-def-address    to address of ex-keydef
           set fcd-record-address     to address of ex-record
           move OP-QUERY-FILE         to opcode
              accept omitted
           perform call-file-handler
           perform display-file-status
           display "file open, ready to read"
           perform read-all-records
           perform rewrite-first-record.

      *> Now read all the records again
           perform read-all-records
           
           
           goback. 
           
      *>
      *> Part I starts here
      *>
           
       set-fcd section.
      *> Initially sets up FCD for OPEN op 
           move low-values to fcd
           move length of fcd to fcd-length
           move fcd--version-number    to fcd-version
           move fcd--indexed-org       to fcd-organization
           move fcd--dynamic-access    to fcd-access-mode
           move fcd--open-closed       to fcd-open-mode *> When opening a file this should be set to fcd--open-closed
           move fcd--recmode-variable  to fcd-recording-mode
           move fcd--format-big        to fcd-file-format
           move fcd--auto-lock-bit     to fcd-lock-mode
           move 12                     to fcd-name-length
           set fcd-filename-address    to address of ex-filename
           set fcd-idxname-address     to address of ex-index-name
           set fcd-key-def-address     to address of ex-keydef
           move 10                     to fcd-max-rec-length
           move 5                      to fcd-min-rec-length
           set fcd-record-address      to address of ex-record
           perform set-keydefinitions
           .

       set-keydefinitions section.
           move low-values to ex-keydef
           move length of ex-keydef to key2length
           move 1 to key-count
           set component-defs to length of key-specification
           add 14 to component-defs
           move 1 to component-count
           move 0 to component-offset *> start of key
           move 5 to component-length *> key length
           .

       call-file-handler section.
           call "EXTFH" using opcode, fcd
           .
           
       display-file-status section.
           if fcd-status-key-1 = "9"
              display "file-status = " fcd-status-key-1 "/" fcd-binary
           else
              display "file-status = " fcd-status-key-1 "/" fcd-status-key-2
           end-if
           .

      *>
      *> Part II starts here
      *>

       read-all-records section.
           display "Open the file and read all the records".    
           move OP-OPEN-INPUT         to opcode
           move fcd--open-closed      to fcd-open-mode
           perform call-file-handler
           if fcd-status-key-1 not = "0"
              display "Failed to open file"
              goback
           end-if
           move 1 to record-key
           move OP-START-EQUAL to opcode
           perform call-file-handler
           move OP-READ-NEXT to opcode
           perform until fcd-status-key-1 not = "0"
             perform call-file-handler
             if fcd-status-key-1 = "0"
                display ex-record(1:fcd-current-rec-len)
             end-if
           end-perform
           move OP-CLOSE to opcode
           perform call-file-handler
           if fcd-status-key-1 not = "0" 
              display "Close failed"
              goback   
           end-if
           .
           
       rewrite-first-record section.
           move OP-OPEN-I-O to opcode
           perform call-file-handler
           move 1 to record-key
           move OP-START-EQUAL to opcode
           perform call-file-handler
           move OP-READ-NEXT to opcode
           perform call-file-handler
           if fcd-status-key-1 = "0"    
              move "Fred" to record-data
              move 9 to fcd-current-rec-len
              move OP-REWRITE to opcode
              accept omitted
              perform call-file-handler
              if fcd-status-key-1  = "0"
               display "Record update : Success"
              else
               display "Record update : Failed"
             end-if
           end-if     
           move OP-CLOSE to opcode
           perform call-file-handler
           if fcd-status-key-1 not = "0" 
              display "Close failed"
              goback   
           end-if
           .
           
           
       end program tutorial.
