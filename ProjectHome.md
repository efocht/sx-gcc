This project aims at porting GCC to support the NEC SX vector CPU (SX6 - SX9). Targets of the project are:
  * Document the SX assembler instructions, scalar CPU registers and existing calling conventions by using information from existing and openly available reference guides. This step is a prerequisite for such an open source effort.
  * Write Machine description, expander definitions and insn patterns for SX CPU.
  * Get GCC to produce assembler code for the SX, focus on scalar instructions.

This project doesn't aim to produce a competitive vectorizing compiler for the SX, the knowhow for that is far too special and the effort too high. Instead it aims at a compiler producing correct scalar code, as basis for further open-source projects for the SX architecture.

## Wiki entry points: ##

  * [Status](StatusPage.md)
  * [SVN repository](SVNrepository.md) access
  * [Building and Testing](BuildTest.md)
  * [Documentation to read](DocsToRead.md) to get started with the development
  * [SX Architecture](SXArch.md) introduction
  * [FAQ](QuestionsAndAnswers.md)


## Related projects ##

  * [SX-Linux](https://sx-linux.hlrs.de/trac.cgi/sx-linux), an effort to investigate, design and implement Linux to the NEC SX architecture (accessible for registered users).


---


<sub>Last updated: Erich Focht, September 2008</sub>