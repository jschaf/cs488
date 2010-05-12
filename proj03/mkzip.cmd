set v=v10
del *.zip
pushd ..
set name=03
zip -r %name%%v% %name%
move %name%%v%.zip %name%
popd
set v=
