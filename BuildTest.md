# Introduction #

This page describes how to download and build sx-gcc and sx-binutils.

NOTE: you will need standard C libs (libc, startup files etc.) and headers delivered with the SX in order to build running SX executables, as we don't yet have a glibc/newlib port for SX.

The installation prefix for the toolchain installation is denoted as PREFIX in this text.

# Details #

## Download the sources ##

Check out repositories:
```
svn co https://source.tree.se/sx/sx-binutils/svn/trunk sx-binutils
svn co https://source.tree.se/sx/sx-gcc/svn/trunk sx-gcc
```

`https://source.tree.se/sx/sx-binutils/svn/trunk` will point you at the current development version of SX port of binutils 2.19. if this version causes you problems, you can try the 2.18 branch from `https://source.tree.se/sx/sx-binutils/svn/branches/sx-binutils-2.18/` or one of the [released tarballs](http://code.google.com/p/sx-gcc/downloads/list)

## Set up the environment ##

You need access to an SX machine or SX cross compilation kit in order to copy SX headers and libs (FLOAT0, 64-bit libs, see the note below!) into the gcc installation prefix, in the **sys-include** dir (which is used for vendor OS headers).

First the headers:
```
mkdir -p PREFIX/sx8-nec-superux
cd PREFIX/sx8-nec-superux
cp -r /path/to/SX/include ./sys-include
```

If you're doing a fresh install from scratch (binutils and gcc), make sure that PREFIX/sx8-nec-superux/include is **empty**! If you're just reinstalling gcc, leave it be.

<a href='Hidden comment: 
(no patching of headers should be needed anymore since r280 and the above described use of sys-include)
After setting them up, patch the headers with the following patch:
```
--- sx8-nec-superux/include/stdio.h.old    2003-08-27 08:55:32.000000000 +0200
+++ sx8-nec-superux/include/stdio.h    2009-02-23 12:54:53.000000000 +0100
@@ -143,8 +143,8 @@
 #   endif
 #endif

-#if (defined(_XOPEN_SOURCE) && !defined(__VA_LIST)) \
-       || (defined(_SX) && __STDC__ - 0 == 0 && !defined(__VA_LIST))
+#if !defined(__GNUC__) && ((defined(_XOPEN_SOURCE) && !defined(__VA_LIST)) \
+       || (defined(_SX) && __STDC__ - 0 == 0 && !defined(__VA_LIST)))
        /* for compatibility */
 #   define __VA_LIST
        typedef _VA_LIST        va_list;
```
'></a>

Now the libs:
```
mkdir -p PREFIX/sx8-nec-superux
cd PREFIX/sx8-nec-superux
cp -r SX_LIBS ./lib/
```

SX\_LIBS should at minimum include the following SX libraries:
  * crt1.o, crt2.o, crt3.o, crt4.o, crt5.o, crtn.o
  * mcrt1.o, mcrt2.o, mcrt3.o, mcrt4.o, mcrt5.o
  * libm.a
  * libc.a
  * libcpp.a
  * libpthread.a
  * libpthread\_s.a
  * libt.a
  * libt\_s.a
  * values-Xa.o, values-Xc.o, values-Xsvr42.o, values-Xt.o
  * libg.a

**NOTE:** even though binutils can handle all three floating point flavours (FLOAT0,1,2) and 32- or 64-bit size\_t (actually, most of binutils except from the assembler are completely agnostic of all these details), gcc implicitly #defines `_SIZE_T64` and uses FLOAT0 floating point flavour (IEEE). thus, you should copy **the 64-bit FLOAT0 libraries** from an SX system (found in /usr/lib0/lib64/ on a native SX system, or in /SX/usr/lib0/lib64/ on a cross-compilation linux host like a1).

## Build binutils ##

```
cd sx-binutils 
./configure --target=sx8-nec-superux --enable-languages=c --prefix=PREFIX
make
make install
cd ..
```

Both GNU assembler and GNU linker are working pretty well and you can safely use both of them.

<a href='Hidden comment: 
You can also opt to use the assembler and linker from SX crosskit. To this end, you will need to use scripts from

https://source.tree.se/sx/sx-gcc/svn/tools/testbench/sx-exe/

Download the scripts to your system, chmod them to 0755, remove as and ld installed by the SX port of binutils and created softlinks pointing to just downloaded sx[as|ld].sh in their stead.
'></a>

## Build gcc ##

```
mkdir sx-build
cd sx-build
../sx-gcc/configure  --target=sx8-nec-superux --enable-languages=c \
  --prefix=PREFIX --disable-libssp --disable-threads
make
make install
```

<a href='Hidden comment: 
If GCC build fails during cross compilation of libgcc2 or other libraries for the SX cross environment, we need you to do the following with each bug encountered at this stage:
* move to the directory where the build failed (make will spit out "Leaving directory xyz" when it fails),
* reproduce the whole command line that failed (it will most likely begin with "/home/jaka/dev/sx/gcc/build/./gcc/xgcc"), and add "-save-temps -v" to it. run it!
* now attach the whole output of this run to the bug report, as well as the intermediate files (preprocessed C source and generated assembler source, if they were generated)
'></a>

# Testing #

## Setting up DejaGNU ##

For running the GCC automated testing from the cross-compile machine you must have access to a SX machine either through ssh or rsh. Let's suppose your SX is called **v00** and you can access it though ssh from the compile machine. The simplest configuration is:

  1. In your home directory create the file ~/.dejagnurc containing
```
set boards_dir ~/.dejagnu_boards
```
  1. Create the ~/.dejagnu\_boards directory and in it a file named **v00.exp** containing the description how to access the remote node:
```
load_generic_config "unix";
set_board_info hostname v00;
set_board_info username efocht;
set_board_info shell_prompt    "$ ";
set_board_info rsh_prog /usr/bin/ssh;
set_board_info rcp_prog /usr/bin/scp;
```

To run the tests on a SX-6 machine add
```
set_board_info ldflags  "-msx6"
```
to the end of the .exp file. This will generate the correct magic number of
the linked executable. If the SX-8 magic number is used the execution on SX-6 will fail.

## Running tests ##

First: add sx/bin to your PATH environment variable, such that the commands sx8-nec-superux-**can be found.**

From inside the GCC build directory you can now run tests by calling:
```
env RUNTESTFLAGS="--target_board=v00" make check
```

It is possible to limit the range of tests, for example:
```
env RUNTESTFLAGS="--target_board=v00 compile.exp" make check
env RUNTESTFLAGS="--target_board=v00 execute.exp" make check
env RUNTESTFLAGS="--target_board=v00 builtins.exp" make check
```

If you need more verbosity, you can add several **-v** to the RUNTESTFLAGS.

The default output of the tests is limited and mainly shows failures. The full output is stored in the file gcc/testsuite/gcc/gcc.log .

## Building a C++ cross-compiler ##

Just use --enable-languages=c,c++ option to configure. Note that the C++ compiler is not yet fully ported and while it will compile all sources, code will often not be correct.