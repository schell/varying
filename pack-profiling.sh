name=`date | tr ' ' -`
mv varying-example.prof $name.prof 
mv varying-example.hp $name.hp 
hp2ps -e8in -c $name.hp 
open -e $name.prof
open $name.ps
