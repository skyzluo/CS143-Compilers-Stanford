# COOL Compiler
Programming Assignments of CS143 Compilers - C++ version

This repository contains all my solutions to the MOOC Compilers on Coursera:

[https://www.coursera.org/course/compilers](https://www.coursera.org/course/compilers)

You can also find it on Stanford Online:

[http://online.stanford.edu/course/compilers-0](http://online.stanford.edu/course/compilers-0)

### How to set up the assignments
The following steps are copied from the "Installing Directly on Linux" page on the course website.

Steps:

* Install packages (If you only intend to use the C++ version, you don't need the jdk). For Ubuntu:

        sudo apt-get install flex bison build-essential csh openjdk-6-jdk libxaw7-dev

* Make the `/usr/class` directory:

        sudo mkdir /usr/class

* Make the directory owned by you:

        sudo chown $USER /usr/class

* Go to `/usr/class` and download the tarball:

        cd /usr/class
        wget http://spark-university.s3.amazonaws.com/stanford-compilers/vm/student-dist.tar.gz

* Untar:

        tar -xf student-dist.tar.gz

* If you want things exactly like the VM:

  Add a symlink to your home directory:

        ln -s /usr/class/cs143/cool ~/cool

* Add the bin directory to your `$PATH` environment variable. If you are using bash, add to your `.profile` (or `.bash_profile`, etc. depending on your configuration; note that in Ubuntu have to log out and back in for this to take effect):

        PATH=/usr/class/cs143/cool/bin:$PATH

For each assignment, you need to set up the starter code according to the assignment specification.

Listed below are (only) the files I modified in each assignment.

### Assignment 1
> score: 63 / 63

`/PA2/cool.flex`

### Assignment 2
> score: 70 / 70

`/PA3/cool.y`

#### Important note on assignment 2

According to the assignment spec, you will need to make one slight change to the starter code before it will link.

Please comment out line `29` of the file `parser-phase.cc` (which you should not otherwise modify), so that it looks like:

    //int curr_lineno;               // needed for lexical analyzer

### Assignment 3
> score: 74 / 74

`/PA4/semant.cc`

`/PA4/semant.h`

`/PA4/cool-tree.h`

### Assignment 4
> score: 63 / 63

`/PA5/cgen.cc`

`/PA5/cgen.h`

`/PA5/cool-tree.h`

`/PA5/cool-tree.handcode.h`

#### Important note on assignment 4

I used some C++11 features in this assignment. In order compile the code, you will have to set up a flag for `g++`. Go to `/PA5/Makefile`, and find the line starting with `CFLAGS=...` (which should be line `29`).

Add `-std=c++11` at the end of that line. Now that line should be something like:

    CFLAGS=-g -Wall -Wno-unused -Wno-write-strings -Wno-deprecated ${CPPINCLUDE} -DDEBUG -std=c++11
