       identification division.
       program-id. testxad2.
       data division.
       working-storage section.


       01 x1 pic x(40).
       01 x2 pic x(40).

       procedure division.

       SET ENVIRONMENT "KEYSTROKE" TO "EXCEPTION=055,Left kl"

       display "A1:" at 0101
       accept x1 at 0110

       call "CBL_XAD_READ_KEYMAPFILE" using
         by value "./keymap2.map"

       display "A2:" at 0201
       accept x2 at 0210

       call "CBL_XAD_RESET_KEYMAP"

       display "A3:" at 0301
       accept x1 at 0310

       call "CBL_XAD_ADD_KEYMAPPING" using
         by value "x",
      *   by value "y"
         by value """Ypsilon"",COB_SCR_F1,""Zett"""
       end-call

       display "A4:" at 0401
       accept x2 at 0410

       display "A5:" at 0501
       accept x1 at 0510

       display "Hallo" at 1301
       stop run
      -.
