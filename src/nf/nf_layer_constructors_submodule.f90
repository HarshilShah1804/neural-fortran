submodule(nf_layer_constructors) nf_layer_constructors_submodule

  use nf_layer, only: layer
  ! use nf_conv2d_layer, only: conv2d_layer
  ! use nf_dense_layer, only: dense_layer
  ! use nf_flatten_layer, only: flatten_layer
  ! use nf_input1d_layer, only: input1d_layer
  use nf_input3d_layer, only: input3d_layer
  use nf_maxpool2d_layer, only: maxpool2d_layer
  ! use nf_reshape_layer, only: reshape3d_layer
  ! use nf_activation, only: activation_function, relu, sigmoid

  implicit none

contains

  module function input3d(layer_shape) result(res)
    integer, intent(in) :: layer_shape(3)
    type(layer) :: res
    res % name = 'input'
    res % layer_shape = layer_shape
    res % input_layer_shape = [integer ::]
    allocate(res % p, source=input3d_layer(layer_shape))
    res % initialized = .true.
  end function input3d

  module function maxpool2d(pool_size, stride) result(res)
    integer, intent(in) :: pool_size
    integer, intent(in), optional :: stride
    integer :: stride_
    type(layer) :: res

    if (pool_size < 2) &
      error stop 'pool_size must be >= 2 in a maxpool2d layer'

    ! Stride defaults to pool_size if not provided
    if (present(stride)) then
      stride_ = stride
    else
      stride_ = pool_size
    end if

    if (stride_ < 1) &
      error stop 'stride must be >= 1 in a maxpool2d layer'

    res % name = 'maxpool2d'

    allocate( &
      res % p, &
      source=maxpool2d_layer(pool_size, stride_) &
    )

  end function maxpool2d

end submodule nf_layer_constructors_submodule
