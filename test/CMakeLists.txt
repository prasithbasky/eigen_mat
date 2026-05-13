# Executables
add_executable(eigenvalue eigenvalue.f mistst.f eigtst.f realeig_2.f realeig_3.f)
target_link_libraries(eigenvalue eigen_help eigen_cha eigen_allg engeln-reutter lapack blas)

add_executable(eigtst_exe main_eigtst.f)
target_link_libraries(eigtst_exe eigen_help eigen_cha eigen_allg engeln-reutter lapack blas)

add_executable(mistst_exe main_mistst.f)
target_link_libraries(mistst_exe eigen_help eigen_cha eigen_allg engeln-reutter lapack blas)

add_executable(realeig_2_exe main_realeig_2.f)
target_link_libraries(realeig_2_exe eigen_help eigen_cha eigen_allg engeln-reutter lapack blas)

# Ensure data files are available in the build directory
configure_file(test.dat test.dat COPYONLY)
configure_file(HEINIT.DAT HEINIT.DAT COPYONLY)
