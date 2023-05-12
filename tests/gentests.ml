
(* Current problems:
IX:
AT_CHECK([./IX119A], [1], [], [libcob: IX119A.SUB:408: error: stack overflow, possible PERFORM depth exceeded

RL:
AT_CHECK([./RL207A], [1], [], [libcob: RL207A.SUB:456: error: 'XRECORD-NUMBER' (Type: NUMERIC DISPLAY) not numeric: '\000\000\000\000\000\000'
*)

let tests = [
  ( "DB", [
        "DB101A" ;
        "DB102A" ;
        "DB103M" ;
        "DB104A" ;
        "DB105A" ;
        "DB201A" ;
        "DB202A" ;
        "DB203A" ;
        "DB204A" ;
        "DB205A" ;
        "DB301M" ;
        "DB302M" ;
        "DB303M" ;
        "DB304M" ;
        "DB305M" ;
      ]);
  ( "IC", [
        "IC102A" ;
        "IC104A" ;
        "IC105A" ;
        "IC107A" ;
        "IC109A" ;
        "IC110A" ;
        "IC111A" ;
        "IC113A" ;
        "IC115A" ;
        "IC117M" ;
        "IC118M" ;
        "IC202A" ;
        "IC204A" ;
        "IC205A" ;
        "IC206A" ;
        "IC208A" ;
        "IC210A" ;
        "IC211A" ;
        "IC212A" ;
        "IC214A" ;
        "IC215A" ;
        "IC217A" ;
        "IC101A" ;
        "IC103A" ;
        "IC106A" ;
        "IC108A" ;
        "IC112A" ;
        "IC114A" ;
        "IC116M" ;
        "IC201A" ;
        "IC203A" ;
        "IC207A" ;
        "IC209A" ;
        "IC213A" ;
        "IC216A" ;
        "IC222A" ;
        "IC223A" ;
        "IC224A" ;
        "IC225A" ;
        "IC226A" ;
        "IC227A" ;
        "IC228A" ;
        "IC233A" ;
        "IC234A" ;
        "IC235A" ;
        "IC237A" ;
        "IC401M" ;
      ]);
  ( "IF", [
        "IF101A" ;
        "IF102A" ;
        "IF103A" ;
        "IF104A" ;
        "IF105A" ;
        "IF106A" ;
        "IF107A" ;
        "IF108A" ;
        "IF109A" ;
        "IF110A" ;
        "IF111A" ;
        "IF112A" ;
        "IF113A" ;
        "IF114A" ;
        "IF115A" ;
        "IF116A" ;
        "IF117A" ;
        "IF118A" ;
        "IF119A" ;
        "IF120A" ;
        "IF121A" ;
        "IF122A" ;
        "IF123A" ;
        "IF124A" ;
        "IF125A" ;
        "IF126A" ;
        "IF127A" ;
        "IF128A" ;
        "IF129A" ;
        "IF130A" ;
        "IF131A" ;
        "IF132A" ;
        "IF133A" ;
        "IF134A" ;
        "IF135A" ;
        "IF136A" ;
        "IF137A" ;
        "IF138A" ;
        "IF139A" ;
        "IF140A" ;
        "IF141A" ;
        "IF142A" ;
        "IF401M" ;
        "IF402M" ;
        "IF403M" ;
      ]);
  ( "IX", [
        "IX101A" ;
        "IX102A" ;
        "IX103A" ;
        "IX104A" ;
        "IX105A" ;
        "IX106A" ;
        "IX107A" ;
        "IX108A" ;
        "IX109A" ;
        "IX110A" ;
        "IX111A" ;
        "IX112A" ;
        "IX113A" ;
        "IX114A" ;
        "IX115A" ;
        "IX116A" ;
        "IX117A" ;
        "IX118A" ;
        "IX119A" ;
        "IX120A" ;
        "IX121A" ;
        "IX201A" ;
        "IX202A" ;
        "IX203A" ;
        "IX204A" ;
        "IX205A" ;
        "IX206A" ;
        "IX207A" ;
        "IX208A" ;
        "IX209A" ;
        "IX210A" ;
        "IX211A" ;
        "IX212A" ;
        "IX213A" ;
        "IX214A" ;
        "IX215A" ;
        "IX216A" ;
        "IX217A" ;
        "IX218A" ;
        "IX301M" ;
        "IX302M" ;
        "IX401M" ;
      ]);
  ( "NC", [
        "NC101A" ;
        "NC102A" ;
        "NC103A" ;
        "NC104A" ;
        "NC105A" ;
        "NC106A" ;
        "NC107A" ;
        "NC108M" ;
        "NC109M" ;
        "NC110M" ;
        "NC111A" ;
        "NC112A" ;
        "NC113M" ;
        "NC114M" ;
        "NC115A" ;
        "NC116A" ;
        "NC117A" ;
        "NC118A" ;
        "NC119A" ;
        "NC120A" ;
        "NC121M" ;
        "NC122A" ;
        "NC123A" ;
        "NC124A" ;
        "NC125A" ;
        "NC126A" ;
        "NC127A" ;
        "NC131A" ;
        "NC132A" ;
        "NC133A" ;
        "NC134A" ;
        "NC135A" ;
        "NC136A" ;
        "NC137A" ;
        "NC138A" ;
        "NC139A" ;
        "NC140A" ;
        "NC141A" ;
        "NC170A" ;
        "NC171A" ;
        "NC172A" ;
        "NC173A" ;
        "NC174A" ;
        "NC175A" ;
        "NC176A" ;
        "NC177A" ;
        "NC201A" ;
        "NC202A" ;
        "NC203A" ;
        "NC204M" ;
        "NC205A" ;
        "NC206A" ;
        "NC207A" ;
        "NC208A" ;
        "NC209A" ;
        "NC210A" ;
        "NC211A" ;
        "NC214M" ;
        "NC215A" ;
        "NC216A" ;
        "NC217A" ;
        "NC218A" ;
        "NC219A" ;
        "NC220M" ;
        "NC221A" ;
        "NC222A" ;
        "NC223A" ;
        "NC224A" ;
        "NC225A" ;
        "NC231A" ;
        "NC232A" ;
        "NC233A" ;
        "NC234A" ;
        "NC235A" ;
        "NC236A" ;
        "NC237A" ;
        "NC238A" ;
        "NC239A" ;
        "NC240A" ;
        "NC241A" ;
        "NC242A" ;
        "NC243A" ;
        "NC244A" ;
        "NC245A" ;
        "NC246A" ;
        "NC247A" ;
        "NC248A" ;
        "NC250A" ;
        "NC251A" ;
        "NC252A" ;
        "NC253A" ;
        "NC254A" ;
        "NC302M" ;
        "NC303M" ;
        "NC401M" ;
      ]);
  ( "OB", [
        "OBIC2A" ;
        "OBIC3A" ;
        "OBIC1A" ;
        "OBNC1M" ;
        "OBNC2M" ;
        "OBSQ1A" ;
        "OBSQ3A" ;
        "OBSQ4A" ;
        "OBSQ5A" ;
      ]);
  ( "RL", [
        "RL101A" ;
        "RL102A" ;
        "RL103A" ;
        "RL104A" ;
        "RL105A" ;
        "RL106A" ;
        "RL107A" ;
        "RL108A" ;
        "RL109A" ;
        "RL110A" ;
        "RL111A" ;
        "RL112A" ;
        "RL113A" ;
        "RL114A" ;
        "RL115A" ;
        "RL116A" ;
        "RL117A" ;
        "RL118A" ;
        "RL119A" ;
        "RL201A" ;
        "RL202A" ;
        "RL203A" ;
        "RL204A" ;
        "RL205A" ;
        "RL206A" ;
        "RL207A" ;
        "RL208A" ;
        "RL209A" ;
        "RL210A" ;
        "RL211A" ;
        "RL212A" ;
        "RL213A" ;
        "RL301M" ;
        "RL302M" ;
        "RL401M" ;
      ]);
  ( "RW", [
        "RW101A" ;
        "RW102A" ;
        "RW103A" ;
        "RW104A" ;
        "RW301M" ;
        "RW302M" ;
      ]);
  ( "SG", [
        "SG101A" ;
        "SG102A" ;
        "SG103A" ;
        "SG104A" ;
        "SG105A" ;
        "SG106A" ;
        "SG201A" ;
        "SG202A" ;
        "SG203A" ;
        "SG204A" ;
        "SG302M" ;
        "SG303M" ;
        "SG401M" ;
      ]);
  ( "SM", [
        "SM101A" ;
        "SM102A" ;
        "SM103A" ;
        "SM104A" ;
        "SM105A" ;
        "SM106A" ;
        "SM107A" ;
        "SM201A" ;
        "SM202A" ;
        "SM203A" ;
        "SM204A" ;
        "SM205A" ;
        "SM206A" ;
        "SM207A" ;
        "SM208A" ;
        "SM301M" ;
        "SM401M" ;
      ]);
  ( "SQ", [
        "SQ101M" ;
        "SQ102A" ;
        "SQ103A" ;
        "SQ104A" ;
        "SQ105A" ;
        "SQ106A" ;
        "SQ107A" ;
        "SQ108A" ;
        "SQ109M" ;
        "SQ110M" ;
        "SQ111A" ;
        "SQ112A" ;
        "SQ113A" ;
        "SQ114A" ;
        "SQ115A" ;
        "SQ116A" ;
        "SQ117A" ;
        "SQ121A" ;
        "SQ122A" ;
        "SQ123A" ;
        "SQ124A" ;
        "SQ125A" ;
        "SQ126A" ;
        "SQ127A" ;
        "SQ128A" ;
        "SQ129A" ;
        "SQ130A" ;
        "SQ131A" ;
        "SQ132A" ;
        "SQ133A" ;
        "SQ134A" ;
        "SQ135A" ;
        "SQ136A" ;
        "SQ137A" ;
        "SQ138A" ;
        "SQ139A" ;
        "SQ140A" ;
        "SQ141A" ;
        "SQ142A" ;
        "SQ143A" ;
        "SQ144A" ;
        "SQ146A" ;
        "SQ147A" ;
        "SQ148A" ;
        "SQ149A" ;
        "SQ150A" ;
        "SQ151A" ;
        "SQ152A" ;
        "SQ153A" ;
        "SQ154A" ;
        "SQ155A" ;
        "SQ156A" ;
        "SQ201M" ;
        "SQ202A" ;
        "SQ203A" ;
        "SQ204A" ;
        "SQ205A" ;
        "SQ206A" ;
        "SQ207M" ;
        "SQ208M" ;
        "SQ209M" ;
        "SQ210M" ;
        "SQ211A" ;
        "SQ212A" ;
        "SQ213A" ;
        "SQ214A" ;
        "SQ215A" ;
        "SQ216A" ;
        "SQ217A" ;
        "SQ218A" ;
        "SQ219A" ;
        "SQ220A" ;
        "SQ221A" ;
        "SQ222A" ;
        "SQ223A" ;
        "SQ224A" ;
        "SQ225A" ;
        "SQ226A" ;
        "SQ227A" ;
        "SQ228A" ;
        "SQ229A" ;
        "SQ230A" ;
        "SQ302M" ;
        "SQ303M" ;
        "SQ401M" ;
      ]);
  ( "ST", [
        "ST101A" ;
        "ST102A" ;
        "ST103A" ;
        "ST104A" ;
        "ST105A" ;
        "ST106A" ;
        "ST107A" ;
        "ST108A" ;
        "ST109A" ;
        "ST110A" ;
        "ST111A" ;
        "ST112M" ;
        "ST113M" ;
        "ST114M" ;
        "ST115A" ;
        "ST116A" ;
        "ST117A" ;
        "ST118A" ;
        "ST119A" ;
        "ST120A" ;
        "ST121A" ;
        "ST122A" ;
        "ST123A" ;
        "ST124A" ;
        "ST125A" ;
        "ST126A" ;
        "ST127A" ;
        "ST131A" ;
        "ST132A" ;
        "ST133A" ;
        "ST134A" ;
        "ST135A" ;
        "ST136A" ;
        "ST137A" ;
        "ST139A" ;
        "ST140A" ;
        "ST144A" ;
        "ST146A" ;
        "ST147A" ;
        "ST301M" ;
      ]);
]

let () =

  let dir = "nistrun.src" in
  List.iter (fun (ext, tests) ->


      let nist_dir = Filename.concat "../_build/tests/cobol85" ext in

      let find_file ext test =
        let file = test ^ ".CBL" in
        if Sys.file_exists ( Filename.concat nist_dir file ) then
          file, file
        else
          let lib_file = Filename.concat "lib" file in
          if Sys.file_exists ( Filename.concat nist_dir lib_file ) then
            lib_file, file
          else
            let file = test ^ ".SUB" in
            if Sys.file_exists ( Filename.concat nist_dir file ) then
              file, file
            else
              let lib_file = Filename.concat "lib" file in
              if Sys.file_exists ( Filename.concat nist_dir lib_file ) then
                lib_file, file
              else
                Printf.kprintf failwith "no file for test %s" test
      in

      (*
      assert (0 =
              Printf.kprintf Sys.command "grep -v promoted %s/run_%s.at > %s/run_%s.at.old" dir ext dir ext);
*)
      let file = Printf.sprintf "%s/run_%s.at" dir ext in
      let oc = open_out file in

      List.iter (fun test ->

          let lib_file, test_file = find_file ext test in

          Printf.fprintf oc "AT_SETUP([%s])\n" test;
          Printf.fprintf oc "AT_KEYWORDS([%s %s])\n" ext test;


          if ext = "SM" then begin
            Printf.fprintf oc "AT_CHECK([mkdir -p copy], [0], [], [])\n";
            Printf.fprintf oc "AT_CHECK([cp -f ${abs_builddir}/cobol85/copy/* copy/], [0], [], [])\n";
          end;

          let modules, generators =
            match test with

            | "IC101A" -> [ "IC102A" ], []
            | "IC103A" -> [ "IC104A" ; "IC105A" ], []
            | "IC106A" -> [ "IC107A" ], []
            | "IC108A" -> [ "IC109A" ; "IC110A" ; "IC111A" ], []
            | "IC112A" -> [ "IC113A" ], []
            | "IC114A" -> [ "IC115A" ], []
            | "IC116M" -> [ "IC117M" ; "IC118M" ], []
            | "IC201A" -> [ "IC202A" ], []
            | "IC203A" -> [ "IC204A" ; "IC205A" ; "IC206A" ], []
            | "IC207A" -> [ "IC208A" ], []
            | "IC209A" -> [ "IC210A" ; "IC211A" ; "IC212A" ], []
            | "IC213A" -> [ "IC214A" ; "IC215A" ], []
            | "IC216A" -> [ "IC217A" ], []

            | "IX102A"
            | "IX119A"
            | "IX120A"
              -> [], [ "IX101A.CBL" ] (* XXXXX024 *)
            | "IX202A"
            | "IX203A" -> [], [ "IX201A.CBL" ] (* XXXXX024 *)

            | "OBIC1A" -> [ "OBIC2A" ; "OBIC3A" ], []
            | "OBSQ4A" -> [], [ "OBSQ3A.CBL" ] (* XXXXX004 *)
            | "OBSQ5A" -> [], [ "OBSQ3A.CBL" ] (* XXXXX009 *)
(*
run_IX.at 358:AT_CHECK([./IX119A], [1], [], [libcob: IX119A.SUB:408: error: stack overflow, possible PERFORM depth exceeded
run_OB.at 35:AT_CHECK([./OBIC1A], [1], [], [libcob: OBIC1A.CBL:60: error: module 'OBIC2A' not found
run_RL.at 522:AT_CHECK([./RL207A], [1], [], [libcob: RL207A.SUB:456: error: 'XRECORD-NUMBER' (Type: NUMERIC DISPLAY) not numeric: '\000\000\000\000\000\000'
*)
            | "RL102A"
            | "RL103A"
              -> [], [ "RL101A.CBL" ] (* XXXXX021 *)
            | "RL109A"
            | "RL110A"
              -> [], [ "RL108A.CBL" ] (* XXXXX061 *)
            | "RL202A"
            | "RL203A"
              -> [], [ "RL201A.CBL" ] (* XXXXX021 *)
            | "RL207A"
            | "RL208A"
              -> [], [ "RL206A.CBL" ] (* XXXXX021 *)


(*
run_SM.at 80:AT_CHECK([./SM102A], [1], [], [libcob: SM102A.SUB:307: error: file does not exist (status = 35) for file TEST-FILE ('XXXXX001') on OPEN
run_SM.at 158:AT_CHECK([./SM104A], [1], [], [libcob: SM104A.SUB:311: error: file does not exist (status = 35) for file TEST-FILE ('XXXXX001') on OPEN
run_SM.at 316:AT_CHECK([./SM202A], [1], [], [libcob: SM202A.SUB:338: error: file does not exist (status = 35) for file TEST-FILE ('XXXXX001') on OPEN
run_SM.at 375:AT_CHECK([./SM204A], [1], [], [libcob: SM204A.SUB:310: error: file does not exist (status = 35) for file TEST-FILE ('XXXXX002') on OPEN
*)
            | "SM102A"
              -> [], [ "SM101A.CBL" ] (* XXXXX001 *)
            | "SM104A"
              -> [], [ "SM103A.CBL" ] (* XXXXX001 *)
            | "SM202A"
              -> [], [ "SM201A.CBL" ] (* XXXXX001 *)
            | "SM204A"
              -> [], [ "SM203A.CBL" ] (* XXXXX002 *)

            | "ST103A"
              -> [], [ "ST102A.SUB" ] (* XXXXX002 *)
            | "ST107A"
              -> [], [ "ST106A.CBL" ] (* XXXXX001 *)
            | "ST111A"
              -> [], [ "ST110A.SUB" ] (* XXXXX002 *)
(*
run_ST.at 226:AT_CHECK([./ST114M], [1], [], [libcob: ST114M.SUB:317: error: file does not exist (status = 35) for file SORTIN-1N ('XXXXX001') on OPEN
*)
            | "ST114M"
              -> [], [ "ST113M.SUB" ] (* XXXXX001 *)
            | "ST117A"
              -> [], [ "ST116A.SUB" ] (* XXXXX002 *)
            | "ST121A"
              -> [], [ "ST120A.SUB" ] (* XXXXX002 *)
            | "ST124A"
              -> [], [ "ST123A.SUB" ] (* XXXXX002 *)

(*
run_ST.at 430:AT_CHECK([./ST126A], [1], [], [libcob: ST126A.SUB:345: error: file does not exist (status = 35) for file SORTIN-1G ('XXXXX001') on OPEN
*)
            | "ST126A"
              -> [], [ "ST125A.CBL" ] (* XXXXX001 *)

            | _ -> [], []
          in
          List.iter (fun m ->
              Printf.fprintf oc {|
AT_CHECK([cp -f ${abs_builddir}/cobol85/%s/lib/%s.CBL .], [0], [], [])
AT_CHECK([$COMPILE_MODULE85 %s.CBL], [0], [], [])
|} ext m m
            ) modules;

          List.iter (fun m ->
              Printf.fprintf oc {|
AT_CHECK([cp -f ${abs_builddir}/cobol85/%s/%s .], [0], [], [])
AT_CHECK([$COMPILE85 %s], [0], [], [])
AT_CHECK([./%s], [0], [], [])
|} ext m m (Filename.chop_suffix m ".CBL")
            ) generators;

          Printf.fprintf oc "AT_CHECK([cp -f ${abs_builddir}/cobol85/%s/%s .], [0], [], [])\n" ext lib_file  ;

          let data_file = test ^ ".DAT" in
          let has_data_file =
            Sys.file_exists ( Filename.concat nist_dir data_file ) in
          if has_data_file then begin
            Printf.fprintf oc "AT_CHECK([cp -f ${abs_builddir}/cobol85/%s/%s .], [0], [], [])\n" ext data_file  ;
          end;

          let has_data_file =
            has_data_file ||
            (match test with
             | "NC302M" ->
               Printf.fprintf oc "AT_DATA([NC302M.DAT], [\n])\n";
               true
             | "OBNC1M" ->
               Printf.fprintf oc {|
AT_CHECK([printf '\n\n\n\n\n\n\n\n\003' > OBNC1M.DAT], [0], [], [])
|};
               true
             | _ -> false
            )
          in

          let execute =
            match test with
              "IX301M" | "IX401M"
            | "ST301M"
            | "RL301M"
            | "RL401M"
            | "RW301M"
            | "RW302M"
            | "IC401M"
            | "DB205A"
            | "NC401M"
            | "SQ303M"
            | "SQ401M"

              -> false
            | _ -> true

          in
          if lib_file = test_file then begin
            Printf.fprintf oc "AT_CHECK([$COMPILE85 %s], [0], [], [])\n" test_file ;
            if not execute then
              Printf.fprintf oc "# ";

            if has_data_file then
              Printf.fprintf oc "AT_CHECK([./%s < %s], [0], [], [])\n"
                test data_file
            else
              Printf.fprintf oc "AT_CHECK([./%s], [0], [], [])\n" test;
          end else
            Printf.fprintf oc "AT_CHECK([$COMPILE_MODULE85 %s], [0], [], [])\n" test_file ;

          if execute then begin
            let check_report =
              match test with
              | "NC110M"
              | "NC214M"
              | "OBSQ3A"
              | "ST102A"
              | "ST109A"
              | "ST110A"
              | "ST112M"
              | "ST113M"
              | "ST115A"
              | "ST116A"
              | "ST120A"
              | "ST122A"
              | "ST123A"
              | "DB301M"
              | "DB302M"
              | "DB303M"
              | "DB305M"
              | "IF402M"
                -> false

              | "NC107A" ->
                Printf.fprintf oc "AT_CHECK([$GREP INFORMATION REPORT], [0], [], [])\n";
                true

              | "NC113M" ->
                Printf.fprintf oc "AT_CHECK([$GREP MARGIN REPORT], [0], [], [])\n";
                Printf.fprintf oc "AT_CHECK([$GREP VISUAL REPORT | tr -d ' '], [0], [], [])\n";
                false

              | "NC135A" ->
                Printf.fprintf oc "AT_CHECK([$GREP -A 15 SPACES REPORT | tr -d ' '], [0], [], [])\n";
                true
              | "NC121M" | "NC220M" ->
                Printf.fprintf oc "AT_CHECK([$GREP INFORMATION REPORT | tr -d ' '], [0], [], [])\n";
                true
              | _ ->
                true
            in
            if check_report then
              let report_log = Filename.concat nist_dir (test ^ ".log") in
              if Sys.file_exists report_log then begin
                (* assert ( 0 = Printf.kprintf Sys.command "cp -f %s nistrun.src/%s.REF" report_log test ); *)
                Printf.fprintf oc "AT_CHECK([$GREP -A 3 SUCCESSFULLY REPORT | tr -d ' '], [0], [], [])\n";
              end ;

          end;
          Printf.fprintf oc "AT_CLEANUP\n\n";
        ) tests ;

      close_out oc ;

      (*
      assert (0 =
              Printf.kprintf Sys.command "diff -w -b -B %s/run_%s.at.old %s/run_%s.at" dir ext dir ext);
*)
    ) tests
