echo OFF
cd NOBN
dir
echo *** Compile DNA for Bane NOR (NOBN)? (press Ctrl-C to abort)
pause *** Compile DNA for Bane NOR (NOBN)? (press Ctrl-C to abort)
call compileAndCopyDNA_NOBN.bat
cd ..
echo *** Done.
pause
echo .