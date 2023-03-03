
### Error log

```
+ mach=fx1000
+ rm -rf fy_arraywrapper.mod fy_string.mod gftl1_unlimitedvector.mod gftl_unlimitedvector.mod ArrayWrapper.o String.o UnlimitedVector.cpp.o ArrayWrapper.lst String.lst UnlimitedUtilities.lst UnlimitedVector.cpp.lst '*.x'
+ [[ fx1000 == \f\x\1\0\0\0 ]]
+ comp='frt -Ec -Nlst=a,lst=d,lst=i,lst=p,lst=t,lst=x -Nalloc_assign -Cpp'
+ lib_path=hpc_stack/fujitsu-1.2.33
+ gftl_lib=/users/xb80/opt/hpc_stack/fujitsu-1.2.33/gftl-shared/v1.3.0
+ use_gftl=false
+ false
+ frt -Ec -Nlst=a,lst=d,lst=i,lst=p,lst=t,lst=x -Nalloc_assign -Cpp -c String.F90
Fortran diagnostic messages: program name(fy_String)
 Module subprogram name(newString_str)
  jwd2881i-i  "String.F90", line 65: The allocatable assignment of the Fortran 2003 or later standard is operated. 
 Module subprogram name(toString_self)
  jwd2881i-i  "String.F90", line 73: The allocatable assignment of the Fortran 2003 or later standard is operated. 
 Module subprogram name(copyFromString)
  jwd2881i-i  "String.F90", line 97: The allocatable assignment of the Fortran 2003 or later standard is operated. 
 Module subprogram name(copyToString)
  jwd2881i-i  "String.F90", line 106: The allocatable assignment of the Fortran 2003 or later standard is operated. 
+ frt -Ec -Nlst=a,lst=d,lst=i,lst=p,lst=t,lst=x -Nalloc_assign -Cpp -c ArrayWrapper.F90
+ false
+ frt -Ec -Nlst=a,lst=d,lst=i,lst=p,lst=t,lst=x -Nalloc_assign -Cpp -c UnlimitedVector.cpp.f90
+ frt -Ec -Nlst=a,lst=d,lst=i,lst=p,lst=t,lst=x -Nalloc_assign -Cpp -c UnlimitedUtilities.F90
Fortran diagnostic messages: program name(fy_UnlimitedUtilities)
 Module subprogram name(is_logical_scalar)
  jwd2529i-s  "UnlimitedUtilities.F90", line 52, column 21: Derived type actual argument must have same type name, and type parameter values of actual argument must agree with the corresponding ones of the dummy argument that is not assumed or deferred, and same structure components as those of dummy argument 'is_logical_scalar'.
```

### Support Team 

*【Answer】*

We have found out that the output of jwd2529i-s during translation of yafyaml-v0.5.1 is a problem in Fujitsu Fortran Compiler.
The occurrence condition for this issue is still being investigated.
We will let you know as soon as we find out.
It cannot be avoided by options, etc., so please execute the workaround using the following program correction.

*【Workaround】*

Please execute all of these.

*	Copy the definition (37th~60th lines) of is_logical_scalar function of UnlimitedUtilities.F90, 
  Then change the function name in the destination to is_logical_scalar_temp.

*	Change the is_logical_scalar function reference on line 50 of UnlimitedUtilities.F90
  To the reference of is_logical_scalar_temp function.

