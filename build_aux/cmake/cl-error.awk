#
# Transform output of CL.EXE to be less verbose and match
# gcc format for reporting errors and warnings.  If used as
#
#    cl.exe foo.c | awk -f cl-error.awk
#
# many tools will recognize the error as one coming from a "normal"
# compiler.
#

BEGIN {
    "echo %cd%" | getline
    home = $0 "\\"
    gsub(/\\/, "\\\\", home)
    gsub(/[Cc]:/, "C:", home)
    print "home at ", home
    exit_status = 0
}

#
# Remove the prompt lines and $PWD from the filename. 
#
{
    gsub(/[[].+\n/, "")
    gsub(home, "")
}

#
# Replace "foo(line,column):" with "foo:line:column:"
#
/[\(][0-9]+,/ {
    match($0, /[\(][0-9]+/)
    #rint
    #rint "beg:  " substr($0, 1, RSTART-1) ":"
    #rint "line: " substr($0, RSTART+1, RLENGTH-1) ":"
    #rint "end:  " substr($0, RSTART + RLENGTH +1, 256) 
    
    line = substr($0, 1, RSTART-1) ":" \
	   substr($0, RSTART+1, RLENGTH-1) ":"	\
           substr($0, RSTART + RLENGTH + 1, 256) 

    gsub(/[\)]: /, ": ", line)

    $0 = line    
}

#
# If an error is found, exit with nonzero status.
#
/ error [A-Z]/ {
    exit_status = 1
}

{
    print
}

END {
    exit(exit_status)
}
