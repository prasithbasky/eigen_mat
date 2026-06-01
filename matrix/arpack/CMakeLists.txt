set(ARPACKLIB arpack)

set(ARPACK_SRC
    # Non-symmetric double precision (DNAUPD/DNEUPD)
    dnaupd.f  dneupd.f  dnaup2.f  dnaitr.f  dnapps.f
    dneigh.f  dngets.f  dnconv.f  dstatn.f
    # Symmetric double precision (DSAUPD/DSEUPD)
    dsaupd.f  dseupd.f  dsaup2.f  dsaitr.f  dsapps.f
    dseigt.f  dsgets.f  dsesrt.f  dstqrb.f  dsconv.f
    dstats.f
    # Shared
    dgetv0.f  dsortr.f  dsortc.f
    # Utilities
    dvout.f   dmout.f   ivout.f   second_NONE.f
)

add_library(${ARPACKLIB}_obj OBJECT ${ARPACK_SRC})
set_target_properties(${ARPACKLIB}_obj PROPERTIES POSITION_INDEPENDENT_CODE ON)
target_include_directories(${ARPACKLIB}_obj PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

add_library(${ARPACKLIB} STATIC $<TARGET_OBJECTS:${ARPACKLIB}_obj>)
target_link_libraries(${ARPACKLIB} PRIVATE lapack blas)
