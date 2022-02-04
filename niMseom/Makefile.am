# niMseom makefile
dist_doc_DATA = INSTALL README ChangeLog

lib_LTLIBRARIES = libniMseom.la

libniMseom_la_LDFLAGS = -no-undefined

libniMseom_la_SOURCES = src/niMseom.cpp src/D_niMseom.cpp src/niMseomMin.cpp src/D_niMseomMin.cpp src/niMseom_registration.cpp
libniMseom_la_LIBADD = -L$(libdir) -lADLib -lADLibR -L$(R_LIB) -lR -lblas -lpthread

