#!/bin/bash
#
# This script is used for installing the dependencies required for
# building opencog on openSUSE.The script has been tested using docker
# image opensuse:13.2
# It is provided for those on 32-bit system or don't want to use
# If you encounter an issue don't hesitate to supply a patch on github.

# NOTE: Script functionality verified for openSUSE dependency installation

# trap errors
set -e

# Environment Variables
SELF_NAME=$(basename $0)

# Some tools
PACKAGES_TOOLS="
		git \
		python-pip \
		wget \
		"

# Packages for building opencog
# NOTE: cxxtest and tbb packages may need alternative installation method
# Alternative packages: cxxtest-devel, libtbb-devel
PACKAGES_BUILD="
		gcc \
		make \
		cmake \
		cxxtest \
		rlwrap \
		guile \
		libicu-devel \
		libzip2 \
		python-Cython \
		python-devel \
		python-pyzmq \
		python-simplejson \
		boost-devel \
		libzmq3 \
		zeromq-devel \
		binutils-devel \
		libgsl0 gsl-devel \
		unixodbc-devel \
		uuid-devel \
		libprotobuf-c-devel \
		libSDL_gfx-devel \
		libssl27 \
		tcl  \
		tcsh \
		freetype2-devel \
		libatlas3 \
		gcc-fortran \
		"

# Packages required for integrating opencog with other services
PACKAGES_RUNTIME="
		unixODBC-devel \
		psqlODBC \
		postgresql \
		"

# Template for messages printed.
message() {
echo -e "\e[1;34m[$SELF_NAME] $MESSAGE\e[0m"
}

# Install  json-spirit (4.05)
install_json_spirit(){
MESSAGE="Installing json-spirit library...." ; message
cd /tmp
export BOOST_ROOT=/usr/include/boost/
wget http://http.debian.net/debian/pool/main/j/json-spirit/json-spirit_4.05.orig.tar.gz
tar -xvf json-spirit_4.05.orig.tar.gz
cd json_spirit_v4_05
mkdir build
cd build/
cmake ..
make -j$(nproc)
sudo make install
cd ../..
rm -rf json-spirit_4.05.orig.tar.gz json_spirit_v4_05
}

# Install cogutil
install_cogutil(){
MESSAGE="Installing cogutil...." ; message
cd /tmp/
wget https://github.com/opencog/cogutil/archive/master.tar.gz
tar -xvf master.tar.gz
cd cogutil-master/
mkdir build
cd build/
cmake ..
make -j$(nproc)
sudo make install
cd ../..
rm -rf master.tar.gz cogutil-master/
}

# Install Python Packages
install_python_packages(){
MESSAGE="Installing python packages...." ; message
cd /tmp
wget https://raw.githubusercontent.com/opencog/opencog/master/opencog/python/requirements.txt
sudo pip install -U -r /tmp/requirements.txt
rm requirements.txt
}

# Install AtomSpace
install_atomspace(){
MESSAGE="Installing atomspace...." ; message
cd /tmp/
wget https://github.com/opencog/atomspace/archive/master.tar.gz
tar -xvf master.tar.gz
cd atomspace-master/
mkdir build
cd build/
cmake ..
make -j$(nproc)
sudo make install
cd ../..
rm -rf master.tar.gz atomspace-master/
}

# Function for installing all required dependenceis for building OpenCog,
# as well as dependencies required for running opencog with other services.
install_dependencies() {
MESSAGE="Installing OpenCog build dependencies...." ; message
# NOTE: Package installation may fail on specific packages - checking individual packages
# Using --no-recommends to reduce potential conflicts
if ! (zypper --no-refresh install --no-recommends $PACKAGES_BUILD $PACKAGES_RUNTIME $PACKAGES_TOOLS);
then
  MESSAGE="Error installing some dependencies. Attempting individual package installation..." ; message
  # Try installing packages individually to identify problematic ones
  for pkg in $PACKAGES_BUILD $PACKAGES_RUNTIME $PACKAGES_TOOLS; do
    if ! zypper --no-refresh install --no-recommends $pkg; then
      MESSAGE="Failed to install package: $pkg (continuing with others)" ; message
    fi
  done
fi
install_json_spirit
install_python_packages
install_cogutil
install_atomspace
}

# Main Program
install_dependencies
