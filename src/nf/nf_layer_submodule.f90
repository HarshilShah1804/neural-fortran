submodule(nf_layer) nf_layer_submodule

  use iso_fortran_env, only: stderr => error_unit
  ! use nf_conv2d_layer, only: conv2d_layer
  ! use nf_dense_layer, only: dense_layer
  ! use nf_flatten_layer, only: flatten_layer
  use nf_input1d_layer, only: input1d_layer
  ! use nf_input3d_layer, only: input3d_layer
  ! use nf_maxpool2d_layer, only: maxpool2d_layer
  ! use nf_reshape_layer, only: reshape3d_layer
  ! use nf_optimizers, only: optimizer_base_type

contains

  pure module subroutine get_output_1d(self, output)
    implicit none
    class(layer), intent(in) :: self
    real, allocatable, intent(out) :: output(:)

    select type(this_layer => self % p)

      type is(input1d_layer)
        allocate(output, source=this_layer % output)
      class default
        error stop '1-d output can only be read from an input1d, dense, or flatten layer.'

    end select

  end subroutine get_output_1d

  impure elemental module subroutine print_info(self)
    implicit none
    class(layer), intent(in) :: self
    print '("Layer: ", a)', self % name
    print '(60("-"))'
    if (.not. self % name == 'input') &
      print '("Input shape: ", *(i0, 1x))', self % input_layer_shape
    print '("Output shape: ", *(i0, 1x))', self % layer_shape
    print '("Parameters: ", i0)', self % get_num_params()
    if (.not. self % name == 'input') &
      print '("Activation: ", a)', self % activation
    print *
  end subroutine print_info


  elemental module function get_num_params(self) result(num_params)
    implicit none
    class(layer), intent(in) :: self
    integer :: num_params

    select type (this_layer => self % p)
      type is (input1d_layer)
        num_params = 0
      class default
        error stop 'Unknown layer type.'
    end select

  end function get_num_params

  module function get_params(self) result(params)
    class(layer), intent(in) :: self
    real, allocatable :: params(:)

    select type (this_layer => self % p)
      type is (input1d_layer)
         ! No parameters to get.
      class default
        error stop 'Unknown layer type.'
    end select

  end function get_params

  module function get_gradients(self) result(gradients)
    class(layer), intent(in) :: self
    real, allocatable :: gradients(:)

    select type (this_layer => self % p)
      type is (input1d_layer)
        ! No gradients to get.
      class default
        error stop 'Unknown layer type.'
    end select

  end function get_gradients

  module subroutine set_params(self, params)
    class(layer), intent(in out) :: self
    real, intent(in) :: params(:)

    ! Check that the number of parameters is correct.
    ! This check will still pass if the size(params) == 0 and the layer is a
    ! non-zero parameter layer; if so, we will warn the user about it below.
    if (size(params) /= self % get_num_params()) then
      error stop 'layer % set_params: number of parameters does not match.'
    end if

    ! When layer % set_params() is called from network % set_params,
    ! zero-parameter layers such as input, flatten, reshape, and maxpool layers
    ! will not be reached because we are guarding against calling
    ! layer % set_params() with zero-size parameters there.
    ! However, a user is allowed to call layer % set_params() on a
    ! zero-parameter layer and pass to it parameters of non-zero size.
    ! If that happens, we will warn about it here.
    select type (this_layer => self % p)

      type is (input1d_layer)
        ! No parameters to set.
        write(stderr, '(a)') 'Warning: calling set_params() ' &
          // 'on a zero-parameter layer; nothing to do.'

          class default
        error stop 'Unknown layer type.'
    end select

  end subroutine set_params

end submodule nf_layer_submodule
