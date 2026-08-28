# Executables
add_executable(eigenvalue eigenvalue.f mistst.f eigtst.f realeig_2.f realeig_3.f realeig_4.f radioss_eigen.f radioss_arpack.f consistent_mass.f)
target_link_libraries(eigenvalue eigen_help eigen_cha eigen_allg engeln-reutter arpack lapack blas)

add_executable(eigtst_exe main_eigtst.f)
target_link_libraries(eigtst_exe eigen_help eigen_cha eigen_allg engeln-reutter lapack blas)

add_executable(mistst_exe main_mistst.f)
target_link_libraries(mistst_exe eigen_help eigen_cha eigen_allg engeln-reutter lapack blas)

add_executable(realeig_2_exe main_realeig_2.f)
target_link_libraries(realeig_2_exe eigen_help eigen_cha eigen_allg engeln-reutter lapack blas)

add_executable(test_implicit_solver test_implicit_solver.f)
target_link_libraries(test_implicit_solver lapack blas)

add_executable(test_eigen_solver test_eigen_solver.f consistent_mass.f)
target_link_libraries(test_eigen_solver lapack blas)

# Ensure data files are available in the build directory
configure_file(test.dat test.dat COPYONLY)
configure_file(test_sym.dat test_sym.dat COPYONLY)
configure_file(HEINIT.DAT HEINIT.DAT COPYONLY)
