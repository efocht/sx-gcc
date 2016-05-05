# Status #

There already exists a framework within the regular GCC 4.2.2 tree that allows to compile an SX cross compiler on i386 / x86\_64 architectures (others have not been tested!).

We do regular tests and the current results can be found at :
http://source.tree.se/sx/sx-gcc/testrun/

## binutils port ##

| **tool**      | **status** | **ticket list** | **notes** |
|:--------------|:-----------|:----------------|:----------|
| ld            | ported     | [issues](http://code.google.com/p/sx-gcc/issues/list?q=label:ld) |           |
| as            | ported     | [issues](http://code.google.com/p/sx-gcc/issues/list?q=label:gas) |           |
| objdump       | ported     | [issues](http://code.google.com/p/sx-gcc/issues/list?q=label:objdump) |           |
| addr2line     | ported     | [issues](http://code.google.com/p/sx-gcc/issues/list?q=label:addr2line) |           |
| ar            | ported     | [issues](http://code.google.com/p/sx-gcc/issues/list?q=label:ar) |           |
| c++filt       | ported     | TODO            |           |
| gprof         | TODO       | TODO            |           |
| nlmconv       | TODO       | TODO            |           |
| nm            | ported     | [issues](http://code.google.com/p/sx-gcc/issues/list?q=label:nm) |           |
| objcopy       | ported     | [issues](http://code.google.com/p/sx-gcc/issues/list?q=label:objcopy) |           |
| ranlib        | ported     | TODO            |           |
| readelf       | TODO       | TODO            | when building for COFF/SX target, we don't need this one |
| size          | ported     | TODO            |           |
| strings       | ported     | TODO            |           |
| strip         | ported     | [issues](http://code.google.com/p/sx-gcc/issues/list?q=label:strip) |           |
| dlltool       | TODO       | TODO            |           |
| windmc        | TODO       | TODO            | we don't really want this one ;) |
| windres       | TODO       | TODO            | we don't really want this one ;) |

## gcc port ##

| **tool**      | **status** | **ticket list** | **notes** |
|:--------------|:-----------|:----------------|:----------|
| gcc           | ported     | [issues](http://code.google.com/p/sx-gcc/issues/list?q=label:gcc) | done      |
| g++           | ported     | [issues](http://code.google.com/p/sx-gcc/issues/list?q=label:g++) | done      |


Latest test suite results, 20091217, binutils [r214](https://code.google.com/p/sx-gcc/source/detail?r=214), gcc [r309](https://code.google.com/p/sx-gcc/source/detail?r=309)

```
                === g++ Summary ===

# of expected passes            12211
# of unexpected failures        3
# of unexpected successes       2
# of expected failures          67
# of unsupported tests          198

                === gcc Summary ===

# of expected passes            39310
# of unexpected failures        2
# of unexpected successes       2
# of expected failures          78
# of unresolved testcases       3
# of untested testcases         28
# of unsupported tests          512

                === libstdc++ Summary ===

# of expected passes            3625
# of unexpected failures        104
# of expected failures          13
# of unsupported tests          328
```

Currently the largest number of the tests that fail are in the "libstdc++" group. They can roughly be categorized in 4 groups:
  1. 16 test cases fail because of the [issue #138](https://code.google.com/p/sx-gcc/issues/detail?id=#138) in the sx-gcc bugtracker. We could fix them if we started using the SX format for quad precision floating point numbers instead of IEEE format.
  1. 4 test cases fail because of the [issue #141](https://code.google.com/p/sx-gcc/issues/detail?id=#141) in the sx-gcc bugtracker. These test cases fail only if the "-ffunction-sections -fdata-sections" switches are used when compiling the code.
  1. 39 test cases fail because of the [issue #139](https://code.google.com/p/sx-gcc/issues/detail?id=#139) in the sx-gcc bugtracker. These test cases fail to compile only if the "-g" switch is used when compiling the code (i.e. debugging info is enabled).
  1. the cause of failure for the rest of the test cases is unknown. Some more time would have to be spent to identify them.

# Checking out and building the source #

Instructions are on the [Building and Testing](BuildTest.md) wiki page.