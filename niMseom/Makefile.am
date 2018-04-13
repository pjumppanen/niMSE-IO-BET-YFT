# niMseom makefile
dist_doc_DATA = INSTALL README ChangeLog

lib_LTLIBRARIES = libniMseom.la

libniMseom_la_LDFLAGS = -no-undefined

libniMseom_la_SOURCES = src/niMseom.cpp src/D_niMseom.cpp src/niMseomMin.cpp src/D_niMseomMin.cpp src/niMseom_registration.cpp
libniMseom_la_LIBADD = -L$(libdir) -lADLib -lADLibR -L$(R_LIB) -lR -lblas -lpthread

BUILT_SOURCES = src/D_niMseom.cpp src/D_niMseomMin.cpp

src/D_niMseom.cpp : src/niMseom.mk
	src/run_mk niMseom.mk

src/D_niMseomMin.cpp : src/niMseom.mk
	src/run_mk niMseom.mk

clean-local:
 -rm -f src/niMseom.dch && \
  rm -f src/OmB_R_interface.r && \
  rm -f src/Om_R_interface.r && \
  rm -f src/include/niMseom_reg_*.hpp && \
  rm -f src/include/OmB_array_plans.hpp && \
  rm -f src/include/OmB_array_plans_copy.hpp && \
  rm -f src/include/OmB_array_plans_init.hpp && \
  rm -f src/include/OmB_constructor_args.hpp && \
  rm -f src/include/OmB_constructor_arrays_phase_1.hpp && \
  rm -f src/include/OmB_constructor_arrays_phase_2.hpp && \
  rm -f src/include/OmB_constructor_call_args.hpp && \
  rm -f src/include/OmB_constructor_locals.hpp && \
  rm -f src/include/OmB_constructor_scalars_phase_1.hpp && \
  rm -f src/include/OmB_constructor_scalars_phase_2.hpp && \
  rm -f src/include/OmB_decl_lib_interface_constructor.hpp && \
  rm -f src/include/OmB_decl_lib_interface_globals.hpp && \
  rm -f src/include/OmB_decl_lib_interface_methods.hpp && \
  rm -f src/include/OmB_impl_lib_interface_constructor.hpp && \
  rm -f src/include/OmB_impl_lib_interface_globals.hpp && \
  rm -f src/include/OmB_impl_lib_interface_methods.hpp && \
  rm -f src/include/OmB_impl_lib_registration.hpp && \
  rm -f src/include/Om_array_plans.hpp && \
  rm -f src/include/Om_array_plans_copy.hpp && \
  rm -f src/include/Om_array_plans_init.hpp && \
  rm -f src/include/Om_constructor_args.hpp && \
  rm -f src/include/Om_constructor_arrays_phase_1.hpp && \
  rm -f src/include/Om_constructor_call_args.hpp && \
  rm -f src/include/Om_constructor_locals.hpp && \
  rm -f src/include/Om_constructor_scalars_phase_1.hpp && \
  rm -f src/include/Om_decl_lib_interface_constructor.hpp && \
  rm -f src/include/Om_decl_lib_interface_globals.hpp && \
  rm -f src/include/Om_decl_lib_interface_methods.hpp && \
  rm -f src/include/Om_impl_lib_interface_constructor.hpp && \
  rm -f src/include/Om_impl_lib_interface_globals.hpp && \
  rm -f src/include/Om_impl_lib_interface_methods.hpp && \
  rm -f src/include/Om_impl_lib_registration.hpp
