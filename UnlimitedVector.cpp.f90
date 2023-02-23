# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/src/v1/UnlimitedVector.F90"
module gFTL1_UnlimitedVector

# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/types/unlimitedPoly.inc" 1
# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/extern/gFTL/include/v1/types/../templates/header.m4"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------








# 1 "stdin"






# 4 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/src/v1/UnlimitedVector.F90" 2



# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector.inc" 1
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/tmplbase.inc" 1
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------







! The Intel compiler requires a flag to use F2003 allocate-on-assignment
! semantics.  To avoid assuming that users have that flag set,
! we use the more verbose option here.  Unfortunately, gfortran does not
! support this variant for arrays, so we do use the allocate-on-assignment
! for that compiler.













! 64 bit integers are necessary to support containers with > 2**32 items.
! While F2008 makes INT64 standard, vendors are permitted to give it a negative
! value and not support integers of that kind.   We switch to 32 bit
! integers in that case.
      implicit none

      integer, parameter :: SIZE_KIND =                                        &
     &           max(kind(1),selected_int_kind(18))

      ! Private type used to force keyword access for 
      ! optional arguments.
      type Unusable
      end type Unusable

! Assume 64 bit is supported by default








# 11 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector.inc" 2
      private




















      public :: UnlimitedVector
      public :: UnlimitedVectorIterator
      public :: UnlimitedVectorRIterator
      public :: swap

# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/type_set_use_tokens.inc" 1
# 1 "stdin"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------














# 27


# 33




# 36

# 36

# 36






# 37






# 38






# 39






# 40






# 41






# 42


# 43

# 43

# 43






# 44







# 46






# 47







# 49






# 50






# 51







# 53






# 54






# 55

# 38 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector.inc" 2

# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector_decl.inc" 1
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

! Main container file must define
!   UnlimitedVector
!   v_
! Maiy also define
!   UnlimitedVectorIterator
!   UnlimitedVectorRIterator

# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_template_macros.inc" 1
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/type_template_macros.inc" 1
# 1 "stdin"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------









! Althoug the code below could be compressed to some degree,
! maintainability is enhanced by isolating each complication in
! its own decision block.
!
! User settable tokens:
!    (a)  class(*)
!    (b)  __use_rank  and __use_extents
!    (c)  __use_string and __use_string_deferred
!    (d)  __use_logical
!    (e)  __use_pointer
!    (f)  
!    (g)  __use_procedure (not complete)
!
!    __USE_EQ
!
! Output macros
!
!    (a)  __TYPE_ASSIGN(dest,src)  ! Set dest to have value src
!    (b)  __TYPE_MOVE(dest,src)    ! If allocatable, move memory for src to variable dest
!                               ! otherwise, behave as __TYPE_ASSIGN(dest,src)
!    (c)  __TYPE_FREE(x)      ! Release memory associated with x (if allocatable)
!
! Output tokens
!
!    (a) __type_declare_component
!    (b) __type_declare_target
!    (c) __type_declare_dummy
!    (d) __type_declare_result
!    (e) __type_interface ! unused - keeping for later use with procedure pointers
!
! Other tokens are for internal use in this file and should be undefined at the end.

!-------------------------------------------------------------------------------
! 1) Declared type



















!-------------------------------------------------------------------------------
! 2) Dimensions
!    (a) There are two cases to consider: deferred shape and non-deferred shape.
!    (b) Return pointers are always deferred shape.





























!-------------------------------------------------------------------------------
! 3) Does the type need to be wrapped













!-------------------------------------------------------------------------------
! 4) Attributes for component declaration



















! macros for testing equality
















!    Array support















! macros for comparing order
! User can specify (or override):










! In most cases, we can provide a compare operator.  Not recommended for vector,
! but useful for set and keys for map:











!-------------------------------------------------------------------------------
! 5) Attributes for target and dummy declaration















!-------------------------------------------------------------------------------
! 6) Attributes for function result declaration
!    Always used deferred shape here as pointer cannot work 
!    with non-deferred shape.





!-------------------------------------------------------------------------------
! 8) Assembly







!-------------------------------------------------------------------------------
! 9) Macros that manipulate storage













































# 11 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_template_macros.inc" 2
# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/key_template_macros.inc" 1
# 1 "stdin"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------









! Althoug the code below could be compressed to some degree,
! maintainability is enhanced by isolating each complication in
! its own decision block.
!
! User settable tokens:
!    (a)  __use_key
!    (b)  __use_key_rank  and __use_key_extents
!    (c)  __use_key_string and __use_key_string_deferred
!    (d)  __use_key_logical
!    (e)  __use_key_pointer
!    (f)  __use_key_allocatable
!    (g)  __use_key_procedure (not complete)
!
!    __USE_KEY_EQ
!
! Output macros
!
!    (a)  __KEY_ASSIGN(dest,src)  ! Set dest to have value src
!    (b)  __KEY_MOVE(dest,src)    ! If allocatable, move memory for src to variable dest
!                               ! otherwise, behave as __KEY_ASSIGN(dest,src)
!    (c)  __KEY_FREE(x)      ! Release memory associated with x (if allocatable)
!
! Output tokens
!
!    (a) __key_declare_component
!    (b) __key_declare_target
!    (c) __key_declare_dummy
!    (d) __key_declare_result
!    (e) __key_interface ! unused - keeping for later use with procedure pointers
!
! Other tokens are for internal use in this file and should be undefined at the end.

!-------------------------------------------------------------------------------
! 1) Declared type



















!-------------------------------------------------------------------------------
! 2) Dimensions
!    (a) There are two cases to consider: deferred shape and non-deferred shape.
!    (b) Return pointers are always deferred shape.





























!-------------------------------------------------------------------------------
! 3) Does the type need to be wrapped













!-------------------------------------------------------------------------------
! 4) Attributes for component declaration



















! macros for testing equality
















!    Array support















! macros for comparing order
! User can specify (or override):










! In most cases, we can provide a compare operator.  Not recommended for vector,
! but useful for set and keys for map:











!-------------------------------------------------------------------------------
! 5) Attributes for target and dummy declaration















!-------------------------------------------------------------------------------
! 6) Attributes for function result declaration
!    Always used deferred shape here as pointer cannot work 
!    with non-deferred shape.





!-------------------------------------------------------------------------------
! 8) Assembly







!-------------------------------------------------------------------------------
! 9) Macros that manipulate storage













































# 12 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_template_macros.inc" 2
# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/value_template_macros.inc" 1
# 1 "stdin"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------









! Althoug the code below could be compressed to some degree,
! maintainability is enhanced by isolating each complication in
! its own decision block.
!
! User settable tokens:
!    (a)  __use_value
!    (b)  __use_value_rank  and __use_value_extents
!    (c)  __use_value_string and __use_value_string_deferred
!    (d)  __use_value_logical
!    (e)  __use_value_pointer
!    (f)  __use_value_allocatable
!    (g)  __use_value_procedure (not complete)
!
!    __USE_VALUE_EQ
!
! Output macros
!
!    (a)  __VALUE_ASSIGN(dest,src)  ! Set dest to have value src
!    (b)  __VALUE_MOVE(dest,src)    ! If allocatable, move memory for src to variable dest
!                               ! otherwise, behave as __VALUE_ASSIGN(dest,src)
!    (c)  __VALUE_FREE(x)      ! Release memory associated with x (if allocatable)
!
! Output tokens
!
!    (a) __value_declare_component
!    (b) __value_declare_target
!    (c) __value_declare_dummy
!    (d) __value_declare_result
!    (e) __value_interface ! unused - keeping for later use with procedure pointers
!
! Other tokens are for internal use in this file and should be undefined at the end.

!-------------------------------------------------------------------------------
! 1) Declared type



















!-------------------------------------------------------------------------------
! 2) Dimensions
!    (a) There are two cases to consider: deferred shape and non-deferred shape.
!    (b) Return pointers are always deferred shape.





























!-------------------------------------------------------------------------------
! 3) Does the type need to be wrapped













!-------------------------------------------------------------------------------
! 4) Attributes for component declaration



















! macros for testing equality
















!    Array support















! macros for comparing order
! User can specify (or override):










! In most cases, we can provide a compare operator.  Not recommended for vector,
! but useful for set and keys for map:











!-------------------------------------------------------------------------------
! 5) Attributes for target and dummy declaration















!-------------------------------------------------------------------------------
! 6) Attributes for function result declaration
!    Always used deferred shape here as pointer cannot work 
!    with non-deferred shape.





!-------------------------------------------------------------------------------
! 8) Assembly







!-------------------------------------------------------------------------------
! 9) Macros that manipulate storage













































# 13 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_template_macros.inc" 2
# 18 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector_decl.inc" 2



      type :: v_Wrapper
         class(*) , allocatable :: item
      end type v_Wrapper





      type :: UnlimitedVector
         private
         type(v_Wrapper), allocatable :: elements(:)
         integer(kind=SIZE_KIND) :: vsize = 0
      contains
         procedure :: size => v_size
         procedure :: capacity => v_capacity
         procedure :: empty => v_empty

         procedure :: at_size_kind => v_at_size_kind
         generic :: at => at_size_kind

         procedure :: at_32 => v_at_32
         generic :: at => at_32


         procedure :: of => v_of
         procedure :: get_size_kind => v_get_size_kind
         generic :: get => get_size_kind

         procedure :: get_32 => v_get_32
         generic :: get => get_32





         procedure :: back => v_back
         procedure :: front => v_front


         procedure :: set_size_kind => v_set_size_kind
         generic :: set => set_size_kind

         procedure :: set_32 => v_set_32
         generic :: set => set_32








         procedure :: push_back => v_push_back
         procedure :: pop_back => v_pop_back
         procedure :: insert_size_kind => v_insert_size_kind
         generic :: insert => insert_size_kind

         procedure :: insert_32 => v_insert_32
         generic :: insert => insert_32


         procedure :: resize_size => v_resize_size
         generic :: resize => resize_size

         procedure :: resize_32 => v_resize_32
         generic :: resize => resize_32

         procedure :: clear => v_clear
         procedure :: shrink_to_fit => v_shrink_to_fit

         procedure :: v_erase_one
         procedure :: v_erase_range
         generic :: erase => v_erase_one, v_erase_range


         procedure :: reserve_size_kind => v_reserve_size_kind
         generic :: reserve => reserve_size_kind

         procedure :: reserve_32 => v_reserve_32
         generic :: reserve => reserve_32



         procedure :: swap => v_swap
         procedure :: reset => v_reset























         ! Iterator constructors
         procedure :: begin => v_begin
         procedure :: end => v_end

         

         procedure :: rbegin => v_rbegin
         procedure :: rend => v_rend


         procedure, private :: set_capacity => v_set_capacity
         procedure, private :: grow_to => v_grow_to
         procedure, private :: downsize=>v_downsize
      end type UnlimitedVector
      

      interface UnlimitedVector
         module procedure v_new_empty
      end interface UnlimitedVector

      interface swap
         module procedure v_swap
      end interface swap


# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vectorIterator_decl.inc" 1
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

      type UnlimitedVectorIterator
 !        private

         type(v_Wrapper), dimension(:), pointer :: elements
         integer(kind=SIZE_KIND) :: currentIndex = -1 ! intentionally invalid value
         
      contains
         
         procedure :: get => v_iter_get
         procedure :: next => v_iter_next
         procedure :: previous => v_iter_previous
         procedure :: v_iter_atDefault
         generic :: at => v_iter_atDefault
         procedure :: v_iter_atOffset
         generic :: at => v_iter_atOffset

         procedure :: v_iter_atOffset_32
         generic :: at => v_iter_atOffset_32

         
         procedure :: v_iter_equal
         procedure ::  v_not_iter_equal
         generic :: operator(==) => v_iter_equal
         generic :: operator(/=) => v_not_iter_equal
         
         procedure :: v_iter_less
         procedure :: v_iter_less_equal
         procedure :: v_iter_greater
         procedure :: v_iter_greater_equal
         generic :: operator(<) => v_iter_less
         generic :: operator(<=) => v_iter_less_equal
         generic :: operator(>) => v_iter_greater
         generic :: operator(>=) => v_iter_greater_equal
         
         procedure :: v_iter_add
         procedure :: v_iter_subtract
         generic :: operator(+) => v_iter_add
         generic :: operator(-) => v_iter_subtract


         procedure :: v_iter_add_32
         procedure :: v_iter_subtract_32
         generic :: operator(+) => v_iter_add_32
         generic :: operator(-) => v_iter_subtract_32

         
      end type UnlimitedVectorIterator
# 155 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector_decl.inc" 2


# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vectorRIterator_decl.inc" 1
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

      type UnlimitedVectorRIterator
         private

         type(v_Wrapper), dimension(:), pointer :: elements
         integer(kind=SIZE_KIND) :: currentIndex = -1 ! intentionally invalid value

      contains
         
         procedure :: get => v_riter_get
         procedure :: next => v_riter_next
         procedure :: previous => v_riter_previous
         
         procedure :: v_riter_atDefault
         generic :: at => v_riter_atDefault
         procedure :: v_riter_atOffset
         generic :: at => v_riter_atOffset

         procedure :: v_riter_atOffset_32
         generic :: at => v_riter_atOffset_32

         
         
         procedure :: v_riter_equal
         procedure :: v_riter_not_equal
         generic :: operator(==) => v_riter_equal
         generic :: operator(/=) => v_riter_not_equal
         
         procedure :: v_riter_less
         procedure :: v_riter_less_equal
         procedure :: v_riter_greater
         procedure :: v_riter_greater_equal
         generic :: operator(<) => v_riter_less
         generic :: operator(<=) => v_riter_less_equal
         generic :: operator(>) => v_riter_greater
         generic :: operator(>=) => v_riter_greater_equal
         
         procedure :: v_riter_add
         procedure :: v_riter_subtract
         generic :: operator(+) => v_riter_add
         generic :: operator(-) => v_riter_subtract


         procedure :: v_riter_add_32
         procedure :: v_riter_subtract_32
         generic :: operator(+) => v_riter_add_32
         generic :: operator(-) => v_riter_subtract_32

      end type UnlimitedVectorRIterator
      

# 158 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector_decl.inc" 2



# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_template_macros_undefs.inc" 1
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/type_template_macros_undefs.inc" 1
# 1 "stdin"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------




































# 11 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_template_macros_undefs.inc" 2
# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/key_template_macros_undefs.inc" 1
# 1 "stdin"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------




































# 12 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_template_macros_undefs.inc" 2
# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/value_template_macros_undefs.inc" 1
# 1 "stdin"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------




































# 13 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_template_macros_undefs.inc" 2
# 162 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector_decl.inc" 2
# 40 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector.inc" 2

# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/unused.inc" 1
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------


# 42 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector.inc" 2
# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/error_codes.inc" 1
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

      ! Error codes
      integer, parameter :: SUCCESS = 0
      integer, parameter :: OUT_OF_RANGE = 1
      integer, parameter :: BAD_ALLOC = 2
      integer, parameter :: ILLEGAL_INPUT = 3

      ! private type for separating RC
      type :: KeywordEnforcer
      end type KeywordEnforcer

      

# 43 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector.inc" 2

      contains

# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector_impl.inc" 1
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

! Main container file must define
!   UnlimitedVector
!   UnlimitedVectorIterator
!   UnlimitedVectorRIterator
!   v_

# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_template_macros.inc" 1
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/type_template_macros.inc" 1
# 1 "stdin"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------









! Althoug the code below could be compressed to some degree,
! maintainability is enhanced by isolating each complication in
! its own decision block.
!
! User settable tokens:
!    (a)  class(*)
!    (b)  __use_rank  and __use_extents
!    (c)  __use_string and __use_string_deferred
!    (d)  __use_logical
!    (e)  __use_pointer
!    (f)  
!    (g)  __use_procedure (not complete)
!
!    __USE_EQ
!
! Output macros
!
!    (a)  __TYPE_ASSIGN(dest,src)  ! Set dest to have value src
!    (b)  __TYPE_MOVE(dest,src)    ! If allocatable, move memory for src to variable dest
!                               ! otherwise, behave as __TYPE_ASSIGN(dest,src)
!    (c)  __TYPE_FREE(x)      ! Release memory associated with x (if allocatable)
!
! Output tokens
!
!    (a) __type_declare_type __type_component_attrs
!    (b) __type_declare_target
!    (c) __type_declare_dummy
!    (d) __type_declare_result
!    (e) __type_interface ! unused - keeping for later use with procedure pointers
!
! Other tokens are for internal use in this file and should be undefined at the end.

!-------------------------------------------------------------------------------
! 1) Declared type



















!-------------------------------------------------------------------------------
! 2) Dimensions
!    (a) There are two cases to consider: deferred shape and non-deferred shape.
!    (b) Return pointers are always deferred shape.





























!-------------------------------------------------------------------------------
! 3) Does the type need to be wrapped













!-------------------------------------------------------------------------------
! 4) Attributes for component declaration



















! macros for testing equality
















!    Array support















! macros for comparing order
! User can specify (or override):










! In most cases, we can provide a compare operator.  Not recommended for vector,
! but useful for set and keys for map:











!-------------------------------------------------------------------------------
! 5) Attributes for target and dummy declaration















!-------------------------------------------------------------------------------
! 6) Attributes for function result declaration
!    Always used deferred shape here as pointer cannot work 
!    with non-deferred shape.





!-------------------------------------------------------------------------------
! 8) Assembly







!-------------------------------------------------------------------------------
! 9) Macros that manipulate storage













































# 11 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_template_macros.inc" 2
# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/key_template_macros.inc" 1
# 1 "stdin"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------









! Althoug the code below could be compressed to some degree,
! maintainability is enhanced by isolating each complication in
! its own decision block.
!
! User settable tokens:
!    (a)  __use_key
!    (b)  __use_key_rank  and __use_key_extents
!    (c)  __use_key_string and __use_key_string_deferred
!    (d)  __use_key_logical
!    (e)  __use_key_pointer
!    (f)  __use_key_allocatable
!    (g)  __use_key_procedure (not complete)
!
!    __USE_KEY_EQ
!
! Output macros
!
!    (a)  __KEY_ASSIGN(dest,src)  ! Set dest to have value src
!    (b)  __KEY_MOVE(dest,src)    ! If allocatable, move memory for src to variable dest
!                               ! otherwise, behave as __KEY_ASSIGN(dest,src)
!    (c)  __KEY_FREE(x)      ! Release memory associated with x (if allocatable)
!
! Output tokens
!
!    (a) __key_declare_type __key_component_attrs
!    (b) __key_declare_target
!    (c) __key_declare_dummy
!    (d) __key_declare_result
!    (e) __key_interface ! unused - keeping for later use with procedure pointers
!
! Other tokens are for internal use in this file and should be undefined at the end.

!-------------------------------------------------------------------------------
! 1) Declared type



















!-------------------------------------------------------------------------------
! 2) Dimensions
!    (a) There are two cases to consider: deferred shape and non-deferred shape.
!    (b) Return pointers are always deferred shape.





























!-------------------------------------------------------------------------------
! 3) Does the type need to be wrapped













!-------------------------------------------------------------------------------
! 4) Attributes for component declaration



















! macros for testing equality
















!    Array support















! macros for comparing order
! User can specify (or override):










! In most cases, we can provide a compare operator.  Not recommended for vector,
! but useful for set and keys for map:











!-------------------------------------------------------------------------------
! 5) Attributes for target and dummy declaration















!-------------------------------------------------------------------------------
! 6) Attributes for function result declaration
!    Always used deferred shape here as pointer cannot work 
!    with non-deferred shape.





!-------------------------------------------------------------------------------
! 8) Assembly







!-------------------------------------------------------------------------------
! 9) Macros that manipulate storage













































# 12 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_template_macros.inc" 2
# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/value_template_macros.inc" 1
# 1 "stdin"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------









! Althoug the code below could be compressed to some degree,
! maintainability is enhanced by isolating each complication in
! its own decision block.
!
! User settable tokens:
!    (a)  __use_value
!    (b)  __use_value_rank  and __use_value_extents
!    (c)  __use_value_string and __use_value_string_deferred
!    (d)  __use_value_logical
!    (e)  __use_value_pointer
!    (f)  __use_value_allocatable
!    (g)  __use_value_procedure (not complete)
!
!    __USE_VALUE_EQ
!
! Output macros
!
!    (a)  __VALUE_ASSIGN(dest,src)  ! Set dest to have value src
!    (b)  __VALUE_MOVE(dest,src)    ! If allocatable, move memory for src to variable dest
!                               ! otherwise, behave as __VALUE_ASSIGN(dest,src)
!    (c)  __VALUE_FREE(x)      ! Release memory associated with x (if allocatable)
!
! Output tokens
!
!    (a) __value_declare_type __value_component_attrs
!    (b) __value_declare_target
!    (c) __value_declare_dummy
!    (d) __value_declare_result
!    (e) __value_interface ! unused - keeping for later use with procedure pointers
!
! Other tokens are for internal use in this file and should be undefined at the end.

!-------------------------------------------------------------------------------
! 1) Declared type



















!-------------------------------------------------------------------------------
! 2) Dimensions
!    (a) There are two cases to consider: deferred shape and non-deferred shape.
!    (b) Return pointers are always deferred shape.





























!-------------------------------------------------------------------------------
! 3) Does the type need to be wrapped













!-------------------------------------------------------------------------------
! 4) Attributes for component declaration



















! macros for testing equality
















!    Array support















! macros for comparing order
! User can specify (or override):










! In most cases, we can provide a compare operator.  Not recommended for vector,
! but useful for set and keys for map:











!-------------------------------------------------------------------------------
! 5) Attributes for target and dummy declaration















!-------------------------------------------------------------------------------
! 6) Attributes for function result declaration
!    Always used deferred shape here as pointer cannot work 
!    with non-deferred shape.





!-------------------------------------------------------------------------------
! 8) Assembly







!-------------------------------------------------------------------------------
! 9) Macros that manipulate storage













































# 13 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_template_macros.inc" 2
# 17 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector_impl.inc" 2












      function v_new_empty() result(v)
         type (UnlimitedVector) :: v
         logical, parameter :: flag = .false.
         if (flag) print*,shape(v) ! avoid warning about unused return value
         return
      end function v_new_empty
      
      
      ! =======================
      !  size
      ! =======================
      pure function v_size(this) result(res)
         class(UnlimitedVector), intent(in) :: this
         integer(kind=SIZE_KIND) :: res
         
         res=this%vsize
         return
      end function v_size
      
      ! =======================
      !  capacity
      ! =======================
      pure function v_capacity(this) result(capacity)
         integer(kind=SIZE_KIND) :: capacity
         class (UnlimitedVector), intent(in) :: this
         
         if (allocated(this%elements)) then
            capacity = size(this%elements)
         else
            capacity = 0
         end if
         
      end function v_capacity
      
      
      ! =======================
      !  empty
      ! =======================
      pure logical function v_empty(this) result(empty)
         class(UnlimitedVector), intent(in) :: this
         
         empty = this%vsize==0
         
      end function v_empty
      
      
      ! =======================
      !  at
      ! =======================
      function v_at_size_kind(this, i, unused, rc) result(res)
         class(UnlimitedVector), target, intent(in) :: this
         integer(KIND=SIZE_KIND), intent(in) :: i
         type (KeywordEnforcer), optional, intent(in) :: unused
         integer, optional, intent(out) :: rc
         class(*) , pointer :: res

         if (.false.) print*,shape(unused)

         if ((i<=0).or.(i>this%vsize)) then
            if (present(rc)) rc = OUT_OF_RANGE
            return
         end if

         res=>this%elements(i)%item
         return
      end function v_at_size_kind


      function v_at_32(this, i, unused, rc) result(res)
         class(UnlimitedVector), target, intent(in) :: this
         integer, intent(in) :: i
         class(*) , pointer :: res
         type (KeywordEnforcer), optional, intent(in) :: unused
         integer, optional, intent(out) :: rc

         if (.false.) print*,shape(unused)

         res => this%at_size_kind(int(i,kind=SIZE_KIND),rc=rc)

      end function v_at_32



      ! =======================
      !  of
      ! =======================
      function v_of(this, i) result(res)
         class(UnlimitedVector), target, intent(in) :: this
         integer, intent(in) :: i
         class(*) , pointer :: res

         res=>this%elements(i)%item
         return
      end function v_of


      ! =======================
      !  get
      ! =======================
      function v_get_size_kind(this, i) result(res)
         class(UnlimitedVector), target, intent(in) :: this
         integer(kind=SIZE_KIND), intent(in) :: i
         class(*) , allocatable :: res
         integer(kind=SIZE_KIND) :: idx

         idx=merge(i, this%vsize+i, i>0)
         allocate(res, source= this%elements(idx)%item)

      end function v_get_size_kind


      function v_get_32(this, i) result(res)
         class(UnlimitedVector), target, intent(in) :: this
         integer, intent(in) :: i
         class(*) , allocatable :: res

! This should call get_size_kind(), but there is an ICE for
! gfortran 5.1
         integer(kind=SIZE_KIND) :: idx
         integer(kind=SIZE_KIND) :: i64
         i64 = int(i,kind=SIZE_KIND)
         idx=merge(i64, this%vsize+i64, i64>0)
         allocate(res, source= this%elements(idx)%item)


      end function v_get_32
















      
      ! =======================
      !  back
      ! =======================
      function v_back(this) result(res)
         class(UnlimitedVector), target, intent(in) :: this
         class(*) , pointer :: res
         
         res=>this%elements(this%vsize)%item
         return
      end function v_back
      
      ! =======================
      !  front
      ! =======================
      function v_front(this) result(res)
         class(UnlimitedVector), target, intent(in) :: this
         class(*) , pointer :: res
         
         res=>this%elements(1)%item
         return
      end function v_front
      
      
      ! =======================
      !  set
      ! =======================
      subroutine v_set_size_kind(this, i, value)
         class(UnlimitedVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: i
         class(*) , intent(in) :: value

         integer(kind=SIZE_KIND) :: idx

         idx=merge(i, this%vsize+i, i>0)
         
         deallocate(this%elements(idx)%item)
         allocate(this%elements(idx)%item, source= value)
         return
      end subroutine v_set_size_kind

      subroutine v_set_32(this, i, value)
         class(UnlimitedVector), intent(inout) :: this
         integer, intent(in) :: i
         class(*) , intent(in) :: value

         call this%set(int(i,kind=SIZE_KIND), value)

      end subroutine v_set_32
      
      
      ! =======================
      !  reset
      ! =======================
      subroutine v_reset(this)
         class(UnlimitedVector), intent(inout) :: this
         
         if (allocated(this%elements)) then
            deallocate(this%elements)
         end if
         this%vsize=0
         return
      end subroutine v_reset
      
      



















      
      


































































































































      

      
      ! =======================
      !  push_back
      ! =======================
      subroutine v_push_back(this, value, unused, rc)
         class(UnlimitedVector), intent(inout) :: this
         class(*) , intent(in) :: value
         type (KeywordEnforcer), optional, intent(in) :: unused
         integer, optional, intent(out) :: rc

         if (.false.) print*,shape(unused)

         call this%grow_to(this%vsize+1)
         call this%resize(this%vsize+1, value, rc=rc)
         return

      end subroutine v_push_back
      
      
      ! =======================
      !  pop_back
      ! =======================
      subroutine v_pop_back(this)
         class(UnlimitedVector), intent(inout) :: this
         
         call this%downsize(this%vsize-1)
         return
      end subroutine v_pop_back
      
      
      ! =======================
      !  insert
      ! =======================
      subroutine v_insert_size_kind(                                                 &
     &   this, index, value, unused, rc)
         class(UnlimitedVector), target, intent(inout) :: this

         integer(kind=SIZE_KIND), intent(in) :: index

         class(*) , intent(in) :: value
         type (KeywordEnforcer), optional, intent(in) :: unused
         integer, optional, intent(out) :: rc

         class(*) , allocatable :: temp
         integer(kind=SIZE_KIND) :: i, n

         if (.false.) print*,shape(unused)

         n = this%vsize
         if (index==n+1) then
            call this%push_back(value)
            ! Workaround for NAG -6221 - temp needs some status
            allocate(temp, source=value)
            return
         endif
         call this%grow_to(this%vsize+1)
         allocate(temp, source=this%elements(n)%item)
         call this%resize(n+1, temp, rc=rc)
         
         do i = n, index, -1
          call move_alloc(from=this%elements(i)%item, to=this%elements(i+1)%item)
         end do

         allocate(this%elements(index)%item, source= value)
         
         return
      end subroutine v_insert_size_kind

      subroutine v_insert_32(this, index, value)
         class(UnlimitedVector), intent(inout) :: this
         integer, intent(in) :: index
         class(*) , intent(in) :: value
         
         call this%insert(int(index,kind=SIZE_KIND), value)
      end subroutine v_insert_32

      
      ! =======================
      !  resize
      ! =======================
      subroutine v_resize_size(this, newsize, value, unused, rc)
         class(UnlimitedVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: newsize
         class(*) , optional, intent(in) :: value
         type (KeywordEnforcer), optional, intent(in) :: unused
         integer, optional, intent(out) :: rc

         integer(kind=SIZE_KIND) :: oldSize
         integer(kind=SIZE_KIND) :: i

         if (.false.) print*,shape(unused)

         if (newSize == this%vsize) return
         if (newSize < 0) then
            if (present(rc)) rc = ILLEGAL_INPUT
            return
         end if
         oldSize=this%vsize
         
         call this%reserve(newSize)
         this%vsize = newSize

         do i = newSize + 1, oldSize
            deallocate(this%elements(i)%item)
         end do
         
         if (present(value) .and. (newsize>oldsize)) then
            do i = oldSize + 1, newSize
               allocate(this%elements(i)%item, source= value)
            end do
         endif
         return
      end subroutine v_resize_size


      subroutine v_resize_32(this, newsize, value, unused, rc)
         class(UnlimitedVector), intent(inout) :: this
         integer, intent(in) :: newsize
         class(*) , optional, intent(in) :: value
         type (KeywordEnforcer), optional, intent(in) :: unused
         integer, optional, intent(out) :: rc

         if (.false.) print*,shape(unused)
         
         call this%resize(int(newsize,kind=SIZE_KIND), value, rc=rc)

      end subroutine v_resize_32



      ! =======================
      !  downsize
      ! =======================
      subroutine v_downsize(this, newsize)
         class(UnlimitedVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: newsize  ! assumes newsize<=size()
         integer(kind=SIZE_KIND) :: i

         if (newsize<this%vsize) then
           do i=newsize+1, this%vsize
              deallocate(this%elements(i)%item)
           end do
           this%vsize=newsize
         endif

         return
      end subroutine v_downsize
      
      
      ! =======================
      !  clear
      ! =======================
      subroutine v_clear(this)
         class(UnlimitedVector), intent(inout) :: this
         
         call this%downsize(0_SIZE_KIND)
         
         return
      end subroutine v_clear
      
      
      ! =======================
      !  shrink_to_fit
      ! =======================

      subroutine v_shrink_to_fit(this)
         class(UnlimitedVector), intent(inout) :: this

         if (this%vsize<this%capacity()) then
           call this%set_capacity(this%vsize)
         endif
         return
      end subroutine v_shrink_to_fit
      


      ! =======================
      !  erase_one
      ! =======================
      subroutine v_erase_one(this, position)
         class(UnlimitedVector), target, intent(inout) :: this
         type (UnlimitedVectorIterator), intent(in)  :: position
         
         call this%erase(position, position+1)
         
         return
      end subroutine v_erase_one
      
      
      ! =======================
      !  erase_range
      ! =======================
      subroutine v_erase_range(this, first, last)
         class(UnlimitedVector), target, intent(inout) :: this
         type (UnlimitedVectorIterator), intent(in)  :: first
         type (UnlimitedVectorIterator), intent(in)  :: last

         integer :: i, d

         d = last%currentindex-first%currentindex
         do i=last%currentIndex, this%vsize
            ! Workaround for Intel 2021.1
            ! Defect report has been submitted to Intel.
            ! Indentation is critical here.
         call move_alloc(from=this%elements(i)%item, to=this%elements(i-d)%item)
         end do
         do i = this%vsize - d + 1, last%currentIndex - 1
            deallocate(this%elements(i)%item)
         end do
         this%vsize=this%vsize-d

         return
      end subroutine v_erase_range

      
      ! =======================
      !  reserve
      ! =======================
      subroutine v_reserve_size_kind(this, capacity)
         class(UnlimitedVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: capacity

         if (capacity>this%capacity()) then
           call this%set_capacity(capacity)
         endif
         
         return
      end subroutine v_reserve_size_kind

      subroutine v_reserve_32(this, capacity)
         class(UnlimitedVector), intent(inout) :: this
         integer, intent(in) :: capacity
         
         call this%reserve(int(capacity,kind=SIZE_KIND))
         return
      end subroutine v_reserve_32

      
      
      ! =======================
      !  set_capacity
      ! =======================
      subroutine v_set_capacity(this, capacity)
         class(UnlimitedVector), target, intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: capacity  ! capacity must be >=0
         type(v_Wrapper),dimension(:),allocatable,target :: temp
         integer(kind=SIZE_KIND) :: i

         if (capacity>0) then                     ! capacity>0
           if (.not.allocated(this%elements)) then   ! not allocated
             allocate(this%elements(capacity))
           else                                      ! allocated
             allocate(temp(capacity))
             do i=1, this%vsize
               call move_alloc(from= this%elements(i)%item, to=temp(i)%item)
             end do
             deallocate(this%elements)
             call move_alloc(temp, this%elements)
           endif
         else if (allocated(this%elements)) then  ! capacity==0
            ! Note: vsize must be 0 to reach this point.
            deallocate(this%elements)
         endif

         return
      end subroutine v_set_capacity

      ! =======================
      !  grow_to
      ! =======================
      subroutine v_grow_to(this, capacity)
         class(UnlimitedVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: capacity

         if (capacity>this%capacity()) then
           call this%set_capacity(max(2*this%vsize, capacity)) ! gives O(n) algorithm for growing vector with push.
         endif
         return
      end subroutine v_grow_to

      ! =======================
      !  swap
      ! =======================
      subroutine v_swap(this, other)
         class(UnlimitedVector), target, intent(inout) :: this
         type(UnlimitedVector), target, intent(inout) :: other
         type(v_Wrapper),                                                                &
     &           dimension(:), allocatable :: tmpelementsfer
         integer :: tmpsize
         
         call move_alloc(this%elements, tmpelementsfer)
         call move_alloc(other%elements, this%elements)
         call move_alloc(tmpelementsfer, other%elements)
         tmpsize=this%vsize
         this%vsize=other%vsize
         other%vsize=tmpsize
         return
      end subroutine v_swap

      

      ! =======================
      !  begin - create an iterator
      ! =======================
      function v_begin(this) result(iter)
         type (UnlimitedVectorIterator) :: iter
         class (UnlimitedVector), target, intent(in) :: this
         
         iter%currentIndex = 1
         if (allocated(this%elements)) then
            iter%elements => this%elements
         else
            iter%elements => null()
         end if
         
      end function v_begin
      
      
      ! =======================
      !  end_
      !  Construct  forward iterator, initially set to just
      !  after last element of vector.
      ! =======================
      function v_end(this) result(iter)
         class (UnlimitedVector), target, intent(in) :: this
         type (UnlimitedVectorIterator) :: iter
         
         iter%currentIndex = this%size() + 1 ! past the end
         if (allocated(this%elements)) then
            iter%elements => this%elements
         else
            iter%elements => null()
         end if
         
      end function v_end

      

      ! =======================
      !  rbegin - create a reverse iterator
      ! =======================
      function v_rbegin(this) result(iter)
         type (UnlimitedVectorRIterator) :: iter
         class (UnlimitedVector), target, intent(in) :: this
         
         iter%currentIndex = this%vsize
         if (allocated(this%elements)) then
            iter%elements => this%elements
         else
            iter%elements => null()
         end if
         
      end function v_rbegin
      
      
      ! =======================
      !  rend
      !  Construct  reverse iterator, initially set to just
      !  before first element of vector
      ! =======================
      function v_rend(this) result(iter)
         class (UnlimitedVector), target, intent(in) :: this
         type (UnlimitedVectorRIterator) :: iter
         
         iter%currentIndex = 0 ! before beginning
         if (allocated(this%elements)) then
            iter%elements => this%elements
         else
            iter%elements => null()
         end if
         
      end function v_rend



















# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vectorIterator_impl.inc" 1
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

      function v_iter_get(this) result (item)
         class (UnlimitedVectorIterator), intent(in) :: this

         class(*) , pointer :: item
         item => this%elements(this%currentIndex)%item

      end function v_iter_get


      subroutine v_iter_next(this)
         class (UnlimitedVectorIterator), intent(inout) :: this
         this%currentIndex = this%currentIndex + 1
      end subroutine v_iter_next


      subroutine v_iter_previous(this)
         class (UnlimitedVectorIterator), intent(inout) :: this
         this%currentIndex = this%currentIndex - 1
      end subroutine v_iter_previous


      logical function v_iter_equal(this, other) result(eq)
         class (UnlimitedVectorIterator), intent(in) :: this
         class (UnlimitedVectorIterator), intent(in) :: other

         eq = (this%currentIndex == other%currentIndex)

      end function v_iter_equal


      logical function v_not_iter_equal(this, other) result(ne)
         class (UnlimitedVectorIterator), intent(in) :: this
         class (UnlimitedVectorIterator), intent(in) :: other

         ne = .not. (this == other)

      end function v_not_iter_equal


      ! Illegal to use these unless both arguments reference the
      ! same vector.
      logical function v_iter_less(this, other) result(less)
         class (UnlimitedVectorIterator), intent(in) :: this
         class (UnlimitedVectorIterator), intent(in) :: other
         less = (this%currentIndex < other%currentIndex)
      end function v_iter_less


      function v_iter_less_equal(this,other) result(le)
         logical :: le
         class (UnlimitedVectorIterator), intent(in) :: this
         class (UnlimitedVectorIterator), intent(in) :: other
         le = (this%currentIndex <= other%currentIndex)
      end function v_iter_less_equal

      ! =======================
      !  greaterThanIter
      ! =======================
      logical function v_iter_greater(this, other) result(gt)
         class (UnlimitedVectorIterator), intent(in) :: this
         class (UnlimitedVectorIterator), intent(in) :: other
         gt = (this%currentIndex > other%currentIndex)
      end function v_iter_greater

      function v_iter_greater_equal(this,other) result(gte)
         logical :: gte
         class (UnlimitedVectorIterator), intent(in) :: this
         class (UnlimitedVectorIterator), intent(in) :: other
         gte = (this%currentIndex >= other%currentIndex)
      end function v_iter_greater_equal


      function v_iter_atDefault(this) result(ptr)
         class(*) , pointer :: ptr
         class (UnlimitedVectorIterator), intent(in) :: this

         ptr => this%elements(this%currentIndex)%item

      end function v_iter_atDefault


      function v_iter_atOffset(this, i) result(ptr)
         class(*) , pointer :: ptr
         class (UnlimitedVectorIterator), intent(in) :: this
         integer(kind=SIZE_KIND), intent(in) :: i

         ptr => this%elements(this%currentIndex + i)%item

      end function v_iter_atOffset

      function v_iter_atOffset_32(this, i) result(ptr)
         class(*) , pointer :: ptr
         class (UnlimitedVectorIterator), intent(in) :: this
         integer, intent(in) :: i

!!$         ptr => this%at(int(i,kind=SIZE_KIND))
         ! workaround for ifort 15.0.3 - no reproducer submitted
         ptr => this%elements(this%currentIndex + i)%item

      end function v_iter_atOffset_32


      function v_iter_add(this, n) result(newIter)
         type (UnlimitedVectorIterator) :: newIter
         class (UnlimitedVectorIterator), intent(in) :: this
         integer(kind=SIZE_KIND), intent(in) :: n

         newIter%currentIndex = this%currentIndex + n
         newIter%elements => this%elements

      end function v_iter_add

      function v_iter_subtract(this, n) result(newIter)
         type (UnlimitedVectorIterator) :: newIter
         class (UnlimitedVectorIterator), intent(in) :: this
         integer(kind=SIZE_KIND), intent(in) :: n

         newIter%currentIndex = this%currentIndex - n
         newIter%elements => this%elements

      end function v_iter_subtract


      function v_iter_add_32(this, n) result(newIter)
         type (UnlimitedVectorIterator) :: newIter
         class (UnlimitedVectorIterator), intent(in) :: this
         integer, intent(in) :: n

         newIter = this + int(n,kind=SIZE_KIND)

      end function v_iter_add_32

      function v_iter_subtract_32(this, n) result(newIter)
         type (UnlimitedVectorIterator) :: newIter
         class (UnlimitedVectorIterator), intent(in) :: this
         integer, intent(in) :: n

         newIter = this - int(n,kind=SIZE_KIND)

      end function v_iter_subtract_32


# 781 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector_impl.inc" 2


# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vectorRiterator_impl.inc" 1
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------


      function v_riter_get(this) result (item)
         class (UnlimitedVectorRIterator), intent(in) :: this
         class(*) , pointer :: item
         item => this%elements(this%currentIndex)%item

      end function v_riter_get


      subroutine v_riter_next(this)
         class (UnlimitedVectorRIterator), intent(inout) :: this
         this%currentIndex = this%currentIndex - 1
      end subroutine v_riter_next


      subroutine v_riter_previous(this)
         class (UnlimitedVectorRIterator), intent(inout) :: this
         this%currentIndex = this%currentIndex + 1
      end subroutine v_riter_previous


      logical function v_riter_equal(this, other) result(eq)
         class (UnlimitedVectorRIterator), intent(in) :: this
         class (UnlimitedVectorRIterator), intent(in) :: other

         eq = (this%currentIndex == other%currentIndex)

      end function v_riter_equal


      logical function v_riter_not_equal(this,other)result(neq)
         class (UnlimitedVectorRIterator), intent(in) :: this
         class (UnlimitedVectorRIterator), intent(in) :: other

         neq = .not. (this == other)

      end function v_riter_not_equal


      ! same vector.
      function v_riter_less(this, other) result(lt)
         logical :: lt
         class (UnlimitedVectorRIterator), intent(in) :: this
         class (UnlimitedVectorRIterator), intent(in) :: other
         lt = (this%currentIndex > other%currentIndex)
      end function v_riter_less


      function v_riter_less_equal(this, other) result (lte)
         logical :: lte
         class (UnlimitedVectorRIterator), intent(in) :: this
         class (UnlimitedVectorRIterator), intent(in) :: other
         lte = (this%currentIndex >= other%currentIndex)
      end function v_riter_less_equal


      function v_riter_greater(this, other) result(gt)
         logical :: gt
         class (UnlimitedVectorRIterator), intent(in) :: this
         class (UnlimitedVectorRIterator), intent(in) :: other
         gt = (this%currentIndex < other%currentIndex)
      end function v_riter_greater

      function v_riter_greater_equal(this, other) result(gte)
         logical :: gte
         class (UnlimitedVectorRIterator), intent(in) :: this
         class (UnlimitedVectorRIterator), intent(in) :: other
         gte = (this%currentIndex <= other%currentIndex)
      end function v_riter_greater_equal


      function v_riter_atDefault(this) result(ptr)
         class(*) , pointer :: ptr
         class (UnlimitedVectorRIterator), intent(in) :: this

         ptr => this%elements(this%currentIndex)%item

      end function v_riter_atDefault


      function v_riter_atOffset(this, i) result(ptr)
         class(*) , pointer :: ptr
         class (UnlimitedVectorRIterator), intent(in) :: this
         integer(kind=SIZE_KIND), intent(in) :: i

         ptr => this%elements(this%currentIndex - i)%item

      end function v_riter_atOffset

      function v_riter_atOffset_32(this, i) result(ptr)
         class(*) , pointer :: ptr
         class (UnlimitedVectorRIterator), intent(in) :: this
         integer, intent(in) :: i

         ptr => this%elements(this%currentIndex - i)%item

      end function v_riter_atOffset_32


      function v_riter_add(this, n) result(newIter)
         type (UnlimitedVectorRIterator) :: newIter
         class (UnlimitedVectorRIterator), intent(in) :: this
         integer(kind=SIZE_KIND), intent(in) :: n

         newIter%currentIndex = this%currentIndex - n
         newIter%elements => this%elements

      end function v_riter_add

      function v_riter_subtract(this, n) result(newIter)
         type (UnlimitedVectorRIterator) :: newIter
         class (UnlimitedVectorRIterator), intent(in) :: this
         integer(kind=SIZE_KIND), intent(in) :: n

         newIter%currentIndex = this%currentIndex + n
         newIter%elements => this%elements

      end function v_riter_subtract


      function v_riter_add_32(this, n) result(newIter)
         type (UnlimitedVectorRIterator) :: newIter
         class (UnlimitedVectorRIterator), intent(in) :: this
         integer, intent(in) :: n

         newIter = this + int(n, kind=SIZE_KIND)

      end function v_riter_add_32

      function v_riter_subtract_32(this, n) result(newIter)
         type (UnlimitedVectorRIterator) :: newIter
         class (UnlimitedVectorRIterator), intent(in) :: this
         integer, intent(in) :: n

         newIter = this - int(n, kind=SIZE_KIND)

      end function v_riter_subtract_32



# 784 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector_impl.inc" 2




# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_template_macros_undefs.inc" 1
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/type_template_macros_undefs.inc" 1
# 1 "stdin"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------




































# 11 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_template_macros_undefs.inc" 2
# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/key_template_macros_undefs.inc" 1
# 1 "stdin"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------




































# 12 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_template_macros_undefs.inc" 2
# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/value_template_macros_undefs.inc" 1
# 1 "stdin"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------




































# 13 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_template_macros_undefs.inc" 2
# 789 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector_impl.inc" 2
# 47 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector.inc" 2

# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/type_use_tokens_undef.inc" 1
# 1 "stdin"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------














# 27


# 33




# 36

# 36

# 36






# 37






# 38






# 39






# 40






# 41






# 42


# 43

# 43

# 43






# 44







# 46






# 47







# 49






# 50






# 51







# 53






# 54






# 55


# 49 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector.inc" 2






# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/tmpltail.inc" 1
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------









# 56 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector.inc" 2
# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_macros_undefs.inc" 1
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/type_macros_undefs.inc" 1
# 1 "stdin"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/extern/gFTL/include/v1/templates/../templates/header.m4"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------








# 10 "stdin"


# 16



# 18

# 18

# 18






# 19






# 20






# 21






# 22






# 23






# 24


# 25

# 25

# 25






# 26







# 28






# 29






# 30






# 31






# 32






# 33


# 11 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_macros_undefs.inc" 2
# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/key_macros_undefs.inc" 1
# 1 "stdin"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/extern/gFTL/include/v1/templates/../templates/header.m4"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------








# 10 "stdin"


# 16







# 18






# 19






# 20






# 21






# 22






# 23






# 24






# 25






# 26







# 28






# 29






# 30






# 31






# 32






# 33


# 12 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_macros_undefs.inc" 2
# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/value_macros_undefs.inc" 1
# 1 "stdin"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------

# 1 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/extern/gFTL/include/v1/templates/../templates/header.m4"
!--------------------------------------------------------------------
! Copyright © 2017 United States Government as represented by the   |
! Administrator of the National Aeronautics and Space               |
! Administration. No copyright is claimed in the United States      |
! under Title 17, U.S. Code. All Other Rights Reserved.             |
!                                                                   |
! Licensed under the Apache License, Version 2.0.                   |
!--------------------------------------------------------------------








# 10 "stdin"


# 16







# 18






# 19






# 20






# 21






# 22






# 23






# 24






# 25






# 26







# 28






# 29






# 30






# 31






# 32






# 33


# 13 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/all_macros_undefs.inc" 2
   
# 57 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/build/extern/gFTL/include/v1/templates/vector.inc" 2

# 8 "/OFS/users/xb80/source_code/jedi/hpc-stack/pkg/gftl-shared-v1.3.0/src/v1/UnlimitedVector.F90" 2





end module gFTL1_UnlimitedVector

module gFTL_UnlimitedVector
   use gFTL1_UnlimitedVector
end module gFTL_UnlimitedVector
