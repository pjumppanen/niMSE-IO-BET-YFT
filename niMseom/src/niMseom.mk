AD BEGIN
  WORKING DIRECTORY: "./work/";
  INCLUDE DIRECTORY: "./include/";
  BLACKBOX: niMseom.bbdef;
  SWITCHES: "rebuild","Oarray";
  CPP OPTIONS FILE: cpp_macros.txt;
  FORTRAN INCLUDE FILES: stdlib.f;

  CLASS D_OperatingModelBase(OperatingModelBase) SOURCE FILE: niMseom.cpp OUTPUT FILES: D_niMseom.cpp D_niMseom.hpp
  BEGIN
    FUNCTION=MSYrefs_objective OUTVAR=MSYrefs_objective VAR=par MODE=f;
    FUNCTION=popdyn_projection_objective OUTVAR=popdyn_projection_objective VAR=par MODE=r PRAGMAS='PushPopDisable';
  END

  CLASS D_OperatingModelMin(OperatingModelMin) SOURCE FILE: niMseomMin.cpp OUTPUT FILES: D_niMseomMin.cpp D_niMseomMin.hpp
  BEGIN

  END
END
