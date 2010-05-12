@echo off

echo deleting emacs backups
del/q *~ > nul 2>&1

echo deleting compiler intermediates
del/q *.ali
del/q *.o

echo deleting ayacc and aflex reports
del/q parser.verbose

echo deleting ayacc and aflex output 
del/q parser.a
del/q parser_goto.a
del/q parser_shift_reduce.a
del/q parser_tokens.a
del/q scanner.a
del/q scanner_dfa.a
del/q scanner_io.a

if "%1"=="" (

echo deleting gnatchop of ayacc and aflex
del/q parse-go_to.ads
del/q parse-shift_reduce.ads
del/q parse-tokens.ads
del/q parse.adb
del/q parse.ads
del/q scan-dfa.adb
del/q scan-dfa.ads
del/q scan-io.adb
del/q scan-io.ads
del/q scan.adb
del/q scan.ads

)
