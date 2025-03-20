! ! #ifndef PARALLEL
! #define num_images() 1
! #define this_image() 1
! ! #endif

submodule(nf_parallel) nf_parallel_submodule
  implicit none
contains

  pure module function tile_indices(dims) result(res)
    integer, intent(in) :: dims
    integer :: res(2)
    integer :: tile_size

    ! No parallel execution, so always assume a single image
    tile_size = dims

    ! Start and end indices cover the full range
    res(1) = 1
    res(2) = dims

  end function tile_indices

end submodule nf_parallel_submodule

