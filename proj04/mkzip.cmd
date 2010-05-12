set v=14
del *.zip
pushd ..
zip -r MIEDs%v% MIEDs
move MIEDs%v%.zip MIEDs
popd
set v=
