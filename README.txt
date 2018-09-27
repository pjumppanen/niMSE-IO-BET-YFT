Due to file size limitations of github repositories the example conditioned
model data files are not included. To obtain the necessary data files go to,

https://cloudstor.aarnet.edu.au/plus/index.php/s/j9CkRGRoVu4PiJk

then download and store the BET.zip and YFT.zip files in the OMconditioning
folder of the niMSE-IO-BET-YFT github source tree.

To run the niMSE-IO-BET-YFT example models first requires that the conditioned
model data is recreated in the correct hierarchy. To extract the data run the
setup-examples.bat (windows) or setup-examples.sh (linux / Mac) script in the
OMconditioning folder. To run the example models run a 64 bit version of R,
change directory to the root folder for the project, then run the MSE's with
the command,

source('RScripts/YFTandBET_demo_ script.R', print.eval=TRUE)

Note that to run on Mac OS X or Linux it is necessary to build the niMseom
shared library (libniMseom.so) and copy it to the lib/linux sub-directory.
64 bit and 32 bit Windows dlls are provided with this distribution.

To build the shared library on OS X, Linux, Cygwin or mingWin you must have
automake and libtool installed. To build the niMseom shared library for
OS X or Linux go to the niMseom sub-directory at the command prompt and
then run,

chmod -R 755 ./*
./init_config.sh

then,

./configure
make clean
make

then copy the libniMseom.so file to the lib/linux directory under the
niMSE-IO-BET-YFT project...
