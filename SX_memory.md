# General #
  * Addressroom: 40bits=1TB
  * Pagesizes: 32k, 4m, 16m/64m '''Page Frames'''
  * Small pages only in lower 32GB (HW convention)/4GB (SW convention)
  * Byte address space, but natural aligned
  * '''Partial Space''' has 256 '''Page Frames''' (8M-16GB depending on page size)
  * ''observation'': In Super-UX, a smallpage executable can not allocate more than 5 '''Partial Spaces'''
  * Cache is logical addressed, software has to flush cache when changing address space

# ATB #
  * HW caching of '''partial spaces''' using '''ATB'''(address translation buffer), with 40 entries for 40 ''''partial spaces'''
  * if partial space is not in ATB, a '''missing space exception''' is raised.
  * no replacement policy implemented, has to be done in software
  * no usage information, so which partial space to remove?

# PTE (usage in Super-UX) #
  * see ''/include/sys/immu.h''
  * Page Table Entry has 32 bits
    * 20 bits PB (page base)
    * 1 bit for locking page in memory
    * 3 bits unused
    * 1 bit unused (hw convention)
    * 3 bits page attributes
    * 1 bit cache bypass flag
    * 1 bit write protection
    * 1 bit privileged bit
    * 1 bit unvailability

# PTE discussion #
  * attributes not yet clear
  * no referenced bit (minor problem)
  * no modified bit (major problem)
  * VAX has no modified bit either