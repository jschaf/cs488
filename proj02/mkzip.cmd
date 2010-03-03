del build\*.* /y
set v=10
del *.zip
pushd ..
zip -r project2-%v% 02
move project2-%v%.zip 02
popd
set v=
