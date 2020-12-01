      *>
      *>* File control descriptor (FCD3)
      *>* used by the callable file handler
      *>
           40  FCD-FILE-STATUS.
              42 FCD-STATUS-KEY-1    pic x.
              42 FCD-STATUS-KEY-2    pic x.
              42 FCD-BINARY          redefines FCD-STATUS-KEY-2
                                     pic x comp-x.

           40  FCD-LENGTH            pic xx comp-x.
           40  FCD-VERSION           pic x comp-x.
              78 fcd--version-number           value 1.

           40  FCD-ORGANIZATION      pic x comp-x.
              78 fcd--line-sequential-org      value 0.
              78 fcd--sequential-org           value 1.
              78 fcd--indexed-org              value 2.
              78 fcd--relative-org             value 3.
      *>         see opcode 0006:
              78 fcd--determine-org            value 255.

           40  FCD-ACCESS-MODE       pic x comp-x.
              78 fcd--sequential-access        value 0.
              78 fcd--dup-prime-access         value 1.
              78 fcd--random-access            value 4.
              78 fcd--dynamic-access           value 8.
              78 fcd--status-defined           value h"80".

      *> open mode
           40  FCD-OPEN-MODE         pic x comp-x.
              78 fcd--open-input               value 0.
              78 fcd--open-output              value 1.
              78 fcd--open-i-o                 value 2.
              78 fcd--open-extend              value 3.
              78 fcd--open-max                 value 3.
              78 fcd--open-closed              value 128.

      *> recording mode
           40  FCD-RECORDING-MODE    pic x comp-x.
              78 fcd--recmode-fixed            value 0.
              78 fcd--recmode-variable         value 1.

           40  FCD-FILE-FORMAT       pic x comp-x.
              78 fcd--format-liiv1             value 0.
              78 fcd--format-cisam             value 1.
              78 fcd--format-liiv2             value 2.
              78 fcd--format-cobol2            value 3.
              78 fcd--format-idx4              value 4.
              78 fcd--format-btrieve-ansi      value 5.
              78 fcd--format-btrieve-non-ansi  value 6.
      *>      78 fcd--format-rlio              value 7.
              78 fcd--format-big               value 8.
              78 fcd--format-leafrec           value 9.
              78 fcd--format-cst               value 10.
              78 fcd--format-mvs-print         value 11.
      *>      78                               value 13.
              78 fcd--format-heap              value 14.
              78 fcd--format-esds              value 15.
              78 fcd--format-qsamv             value 255.
      *>         1 greater than max permissible format:
              78 fcd--max-file-format          value 16.

           40  FCD-DEVICE-FLAG       Pic x comp-x.
              78 fcd--dev-normal               value 0.
              78 fcd--dev-device               value 1.
              78 fcd--dev-stdin                value 2.
              78 fcd--dev-stdout               value 3.
              78 fcd--dev-stderr               value 4.
              78 fcd--dev-badname              value 5.
              78 fcd--dev-input-pipe           value 6.
              78 fcd--dev-output-pipe          value 7.
              78 fcd--dev-i-o-pipe             value 8.
              78 fcd--dev-library              value 9.
              78 fcd--dev-disk-file            value 10.
              78 fcd--dev-null                 value 11.
              78 fcd--dev-disk-redir           value 12.
              78 fcd--dev-no-map               value 13.

           40  FCD-LOCK-ACTION       pic x comp-x.
      *>      Used only in c-isam type calls...
              78 fcd--getlock                  value 1.
              78 fcd--nolock                   value 2.
              78 fcd--ignorelock               value 3.

           40  FCD-DATA-COMPRESS     pic x comp-x.

           40  FCD-BLOCKING          pic x comp-x.
           40  FCD-additional-status redefines FCD-blocking
                                     pic x    comp-x.

           40  FCD-IDXCACHE-SIZE     pic x comp-x.

           40  FCD-PERCENT           pic x comp-x.
           40  FCD-REC-COUNT-SET     redefines FCD-PERCENT
                                     pic x comp-x.

           40  FCD-BLOCK-SIZE        pic x comp-x.

           40  FCD-FLAGS-1           pic x comp-x.
              78 fcd--mainframe-compat         value h"80".
              78 fcd--ansi-line-adv            value h"40".
              78 fcd--return-key-only          value h"20".
              78 fcd--bypass-esds              value h"10".
              78 fcd--no-xfhname-mapping       value h"08".
              78 fcd--dont-call-xfhtrace       value h"04".
              78 fcd--call-xfhtrace            value h"02".
      *>         declaratives exist:
              78 fcd--fcd-decl                 value h"01".

           40  FCD-FLAGS-2           pic x comp-x.
              78  fcd--convert-dbspace         value h"01".

      *> MVS flag bits
           40  fcd-mvs-flags         pic x comp-x.
              78  fcd--file-is-syspunch        value h"10".
              78  fcd--file-is-indd            value h"08".
              78  fcd--file-is-outdd           value h"04".
              78  fcd--amode-31bit             value h"02".
              78  fcd--amode-24bit             value h"01".
              78  fcd--amode-bits              value
                  fcd--amode-31bit
                + fcd--amode-24bit.

           40  FCD-STATUS-TYPE       pic x comp-x.
              78 fcd--ans85-status             value h"80".
              78 fcd--no-space-fill            value h"40".
              78 fcd--no-strip-spaces          value h"20".
              78 fcd--no-expand-tabs           value h"10".
              78 fcd--rec-term-bit             value h"08".
              78 fcd--insert-tabs              value h"04".
              78 fcd--insert-nulls             value h"02".
              78 fcd--cr-delimiter             value h"01".
              78 fcd--modify-writes            value
                 fcd--insert-tabs
               + fcd--insert-nulls.

           40  FCD-OTHER-FLAGS       pic x comp-x.
              78 fcd--optional-file            value h"80".
              78 fcd--nodetectlock-input       value h"40".
              78 fcd--not-optional             value h"20".
              78 fcd--external-name            value h"10".
              78 fcd--get-info                 value h"08".
              78 fcd--nodetectlock             value h"04".
              78 fcd--multiple-reel            value h"02".
              78 fcd--line-advancing           value h"01".
              78 fcd--special-sequential       value
                 fcd--optional-file
               + fcd--multiple-reel
               + fcd--line-advancing.

           40  FCD-TRANS-LOG         pic x comp-x.
              78 fcd--open-input-shared        value h"80".
              78 fcd--allow-input-locks        value h"40".
              78 fcd--no-read-sema             value h"20".
              78 fcd--expand-positioning-bit   value h"10".

              78 fcd--no-seq-check             value h"08".
              78 fcd--dat-term-bit             value h"04".
              78 fcd--slow-read                value h"02".
              78 fcd--suppress-adv             value h"01".

           40  FCD-LOCKTYPES         pic x comp-x.
              78 fcd--interlang-locking        value h"80".
              78 fcd--allow-readers            value h"40".
              78 fcd--separate-lock-file       value h"20".
              78 fcd--single-open              value h"10".
              78 fcd--nfs-file-lock            value h"08".
              78 fcd--nfs-file-lock-hp         value h"04".
              78 fcd--nfs-file-locks           value
                 fcd--nfs-file-lock
               + fcd--nfs-file-lock-hp.

           40  FCD-FS-FLAGS          pic x comp-x.
              78 fcd--transaction-processing-bit  value h"80".
              78 fcd--recovery-run-b           value h"04".
              78 fcd--fs-server-bit            value h"02".

           40  FCD-CONFIG-FLAGS      pic x comp-x.
              78 fcd--writethru-bit            value h"80".
              78 fcd--relative-bit             value h"40".
              78 fcd--set-crp-bit              value h"20".
              78 fcd--bigfile-bit              value h"10".
      *>      78 fcd--return-percent           value h"08".
      *>      78 fcd--dont-call-xfhconv        value h"04".
              78 fcd--call-cobfstatconv        value h"02".
              78 fcd--ignorelock-bit           value h"01".

           40  FCD-MISC-FLAGS        pic x comp-x.
              78 fcd--mainframe-hostfd         value h"80".
              78 fcd--set-idxdatbuf            value h"40".
              78 fcd--load-onto-heap           value h"20".
              78 fcd--usage-unknown            value h"10".
              78 fcd--recmode-s                value h"08".
              78 fcd--recmode-u                value h"04".
              78 fcd--external-fcd             value h"02".
              78 fcd--closed-with-lock         value h"01".

           40  FCD-CONFIG-FLAGS2     pic x comp-x.
              78 fcd--file-is-ebcdic           value h"80".
              78 fcd--file-has-write-after     value h"40".
              78 fcd--file-has-write-before    value h"20".
              78 fcd--file-has-adv-specified   value h"10".
              78 fcd--no-min-len-check         value h"08".
              78 fcd--no-key-check             value h"04".
              78 fcd--convert-to-ascii         value h"02".
              78 fcd--rm-behaviour             value h"01".
              78 fcd--file-has-before-or-after value
                 fcd--file-has-write-before
               + fcd--file-has-write-after.

           40  FCD-LOCK-MODE         pic x comp-x.
              78 fcd--multilock-bit            value h"80".
              78 fcd--writelock-bit            value h"40".
              78 fcd--retry-open-bit           value h"20".
              78 fcd--skip-lock-bit            value h"10".
              78 fcd--retry-lock-bit           value h"08".
              78 fcd--manual-lock-bit          value h"04".
              78 fcd--auto-lock-bit            value h"02".
              78 fcd--exclusive-bit            value h"01".
              78 fcd--sharing-bits             value
                 fcd--manual-lock-bit
               + fcd--auto-lock-bit.


           40  FCD-SHR2              pic x comp-x.
              78 fcd--file-max-bit             value h"08".
              78 fcd--file-pointer-bit         value h"04".
              78 fcd--retry-time-bit           value h"02".
              78 fcd--start-unlock             value h"01".

           40  FCD-IDXCACHE-BUFFS    pic x comp-x.

           40  FCD-INTERNAL-FLAGS-1  pic x comp-x.
           40  FCD-INTERNAL-FLAGS-2  pic x comp-x.
           40                        pic x(15).

      *>   NLS id (else 0)
           40  FCD-NLS-ID            pic xx comp-x.

           40  FCD-FS-FILE-ID        pic xx comp-x.

           40  fcd-retry-open-count  pic xx comp-x.

           40  FCD-NAME-LENGTH       pic xx comp-x.

           40  fcd-idxname-length    pic xx comp-x.
           40  fcd-retry-count       pic xx comp-x.
      *> Indexed key identifier
           40  FCD-KEY-ID            pic xx comp-x.
      *> Line count (seq files)
           40  FCD-LINE-COUNT        pic xx comp-x.

           40  FCD-USE-FILES         pic x comp-x.
           40  FCD-GIVE-FILES        pic x comp-x.
      *> Effective key length
           40  FCD-KEY-LENGTH        pic xx comp-x.

           40                        pic x(20).

      *> Current record length
           40  FCD-CURRENT-REC-LEN   pic x(4) comp-x.
      *> Minimum record length
           40  FCD-MIN-REC-LENGTH    pic x(4) comp-x.
      *> Max record length
           40  FCD-MAX-REC-LENGTH    pic x(4) comp-x.

           40  FCD-SESSION-ID        pic x(4) comp-x.

           40                        pic x(24).

           40  FCD-RELADDR-OFFSET    pic x(8) comp-x.
           40  FCD-RELADDR           redefines FCD-RELADDR-OFFSET
                                     pic x(8) comp-x.
           40  FCD-RELADDR-BIG       redefines FCD-RELADDR-OFFSET
                                     pic x(8) comp-x.
           40  FCD-MAX-REL-KEY       pic x(8) comp-x.

           40  FCD-RELATIVE-KEY      pic x(8) comp-x.

           40  FCD-PTR-FILLER1       pic x(8).
           40  FCD-HANDLE            redefines FCD-PTR-FILLER1
                                     usage pointer.
           40  FCD-HANDLE-NUM        redefines FCD-PTR-FILLER1
                                     pic x(4) comp-x.
      *> Pointer to record area
           40  FCD-PTR-FILLER2       pic x(8).
           40  FCD-RECORD-ADDRESS    redefines FCD-PTR-FILLER2
                                     usage pointer.
      *> Pointer to file name
           40  FCD-PTR-FILLER3       pic x(8).
           40  FCD-FILENAME-ADDRESS  redefines FCD-PTR-FILLER3
                                     usage pointer.
      *> Pointer to index name (applies only if separate index file exists)
           40  FCD-PTR-FILLER4       pic x(8).
           40  FCD-IDXNAME-ADDRESS   redefines FCD-PTR-FILLER4
                                     usage pointer.
           40  FCD-INDEX-NAME        redefines FCD-PTR-FILLER4
                                     usage pointer.
      *> Pointer to key def block
           40  FCD-PTR-FILLER5       pic x(8).
           40  FCD-KEY-DEF-ADDRESS   redefines FCD-PTR-FILLER5
                                     usage pointer.
      *> Pointer to collating seq
           40  FCD-PTR-FILLER6       pic x(8).
           40  FCD-COL-SEQ-ADDRESS   redefines FCD-PTR-FILLER6
                                     usage pointer.
      *> Pointer to using list
           40  FCD-PTR-FILLER7       pic x(8).
           40  FCD-FILDEF-ADDRESS    redefines FCD-PTR-FILLER7
                                     usage pointer.

           40  FCD-PTR-FILLER8       pic x(8).
           40  FCD-DFSORT-ADDRESS    redefines FCD-PTR-FILLER8
                                     usage pointer.
