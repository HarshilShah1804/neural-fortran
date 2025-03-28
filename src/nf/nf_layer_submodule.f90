submodule(nf_layer) nf_layer_submodule

  use iso_fortran_env, only: stderr => error_unit
  ! use nf_conv2d_layer, only: conv2d_layer
  ! use nf_dense_layer, only: dense_layer
  ! use nf_flatten_layer, only: flatten_layer
  ! use nf_input1d_layer, only: input1d_layer
  use nf_input3d_layer, only: input3d_layer
  use nf_maxpool2d_layer, only: maxpool2d_layer
  ! use nf_reshape_layer, only: reshape3d_layer
  ! use nf_optimizers, only: optimizer_base_type

contains

  pure module subroutine backward_3d(self, previous, gradient)
    implicit none
    class(layer), intent(in out) :: self
    class(layer), intent(in) :: previous
    real, intent(in) :: gradient(:,:,:)

    ! Backward pass from a 3-d layer downstream currently implemented
    ! only for conv2d and reshape3d layers
    select type(this_layer => self % p)

      type is(maxpool2d_layer)

        ! Upstream layers permitted: conv2d, input3d, maxpool2d, reshape3d
        select type(prev_layer => previous % p)
          type is(maxpool2d_layer)
            call this_layer % backward(prev_layer % output, gradient)
          type is(input3d_layer)
            call this_layer % backward(prev_layer % output, gradient)
        end select

    end select

  end subroutine backward_3d


  pure module subroutine forward(self, input)
    implicit none
    class(layer), intent(in out) :: self
    class(layer), intent(in) :: input

    select type(this_layer => self % p)

      type is(maxpool2d_layer)

        ! Upstream layers permitted: input3d, conv2d, maxpool2d, reshape3d
        select type(prev_layer => input % p)
          type is(input3d_layer)
            call this_layer % forward(prev_layer % output)
          type is(maxpool2d_layer)
            call this_layer % forward(prev_layer % output)
        end select

    end select

  end subroutine forward

  pure module subroutine get_output_3d(self, output)
    implicit none
    class(layer), intent(in) :: self
    real, allocatable, intent(out) :: output(:,:,:)

    select type(this_layer => self % p)

      type is(input3d_layer)
        allocate(output, source=this_layer % output)

      type is(maxpool2d_layer)
        allocate(output, source=this_layer % output)

      class default
        error stop '3-d output can only be read from a conv2d, input3d, maxpool2d, or reshape3d layer.'

    end select

  end subroutine get_output_3d


  impure elemental module subroutine init(self, input)
    implicit none
    class(layer), intent(in out) :: self
    class(layer), intent(in) :: input

    if (self % initialized) &
      error stop self % name // ' layer is already initialized.'

    select type(this_layer => self % p); class default
      call this_layer % init(input % layer_shape)
    end select

    ! The shape of conv2d, maxpool2d, or flatten layers is not known
    ! until we receive an input layer.
    select type(this_layer => self % p)

      type is(maxpool2d_layer)
        self % layer_shape = shape(this_layer % output)

    end select

    self % input_layer_shape = input % layer_shape 
    self % initialized = .true.

  end subroutine init


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

      type is (input3d_layer)
        num_params = 0

      type is (maxpool2d_layer)
        num_params = 0

      class default
        error stop 'Unknown layer type.'
    end select

  end function get_num_params

  module function get_params(self) result(params)
    class(layer), intent(in) :: self
    real, allocatable :: params(:)

    select type (this_layer => self % p)

      type is (input3d_layer)
         ! No parameters to get.

      type is (maxpool2d_layer)
        ! No parameters to get.

      class default
        error stop 'Unknown layer type.'
    end select

  end function get_params

  module function get_gradients(self) result(gradients)
    class(layer), intent(in) :: self
    real, allocatable :: gradients(:)

    select type (this_layer => self % p)

      type is (input3d_layer)
        ! No gradients to get.

      type is (maxpool2d_layer)
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



      type is (input3d_layer)
        ! No parameters to set.
        write(stderr, '(a)') 'Warning: calling set_params() ' &
          // 'on a zero-parameter layer; nothing to do.'


      type is (maxpool2d_layer)
        ! No parameters to set.
        write(stderr, '(a)') 'Warning: calling set_params() ' &
          // 'on a zero-parameter layer; nothing to do.'


          class default
        error stop 'Unknown layer type.'
    end select

  end subroutine set_params

end submodule nf_layer_submodule
