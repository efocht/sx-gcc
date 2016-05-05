# Getting started #

There are many overviews on GCC internals on the web, just google for them. Some people who have ported the GCC backend to some new architectures have documented their work, and some of the presentations or papers are interesting for getting an overview:
  * http://gcc.cookys.org/studio/cookys/reference/pgccp.ppt
  * http://sslab.cs.ccu.edu.tw/course/toolchain/toolchain_2005/2004/porting_gcc.pdf
  * [ftp://ftp.axis.se/pub/users/hp/pgccfd/pgccfd-0.5.pdf](ftp://ftp.axis.se/pub/users/hp/pgccfd/pgccfd-0.5.pdf)
  * http://archiv.tu-chemnitz.de/pub/2004/0107/data/thesis.ps

Several interesting papers for later parts of this project with regard to vectorization may be found at the [GCC-Summit pages](http://www.gccsummit.org). The proceedings of the 2007 summit can be obtained [here](http://ols.108.redhat.com/2007/GCC-Reprints/GCC2007-Proceedings.pdf).

  * Vladimir N. Makarov, "The Integrated register allocator for GCC", GCC-Summit, 2007, p. 77
  * Roberto Costa, Andrea C. Ornstein and Erven Rohou, "CLI Back-Endin GCC", GCC-Summit 2007, p. 111

The next summit is June 17-19, 2008 in Ottawa, Canada.

# GCC documentation #

The gcc documentation can be found at [http://gcc.gnu.org](http://gcc.gnu.org/)
and the most interesting manual is the **GCC Internals Manual** [gccint](http://gcc.gnu.org/onlinedocs/gccint). A pdf version is available at the same site.

The relevant topics for implementing the SX backend are RTLs, machine and target descriptions.



---


# Own GCC docs #

[Page with own GCC docs](GCCDocs.md)