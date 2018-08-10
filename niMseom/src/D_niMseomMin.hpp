//  ----------------------------------------------------------------------------
//  ADT generated header defining class D_OperatingModelMin
//  ----------------------------------------------------------------------------


#ifndef __D_OperatingModelMin_HPP
#define __D_OperatingModelMin_HPP


#include "niMseomMin.hpp"


class D_OperatingModelMin  : public OperatingModelMin
{
public: 
  
  #include "include/Om_decl_lib_interface_methods.hpp"
  D_OperatingModelMin(int arg_npop, int arg_nages, int arg_nsubyears, int arg_nareas, int arg_nfleets, const ARRAY_1I arg_Recsubyr);
  D_OperatingModelMin(const D_OperatingModelMin& rCopy);
  virtual ~D_OperatingModelMin();
  
};


#include "include/Om_decl_lib_interface_globals.hpp"
#include "include/Om_decl_lib_interface_constructor.hpp"


#endif //__D_OperatingModelMin_HPP

