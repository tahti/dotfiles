TMPDIR=$(mktemp -d -t gitXXXX)
#Install brightness
cd $TMPDIR
git clone https://github.com/multiplexd/brightlight.git 
cd $TMPDIR/brightlight
sudo apt install libbsd-dev
make
cp brightlight ~/bin
cd
rm -Rf $TMPDIR
