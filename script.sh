cd /Users/emanuelecavalleri/Desktop/Progetto
fsharpc -a IntSort.fsi IntSorti.fs | echo 'Please wait...'
echo -ne 'List length ?= '
fsharpi intSort.fsx > out.txt
cat out.txt
echo
echo '*** Output stored as "out.txt" ***'