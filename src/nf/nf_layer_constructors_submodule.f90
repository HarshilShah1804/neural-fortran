submodule(nf_layer_constructors) nf_layer_constructors_submodule

  use nf_layer, only: layer
  use nf_input1d_layer, only: input1d_layer
  use nf_input3d_layer, only: input3d_layer
  use nf_reshape_layer, only: reshape3d_layer

  implicit none

contains

  module function input1d(layer_size) result(res)
    integer, intent(in) :: layer_size
    type(layer) :: res
    res % name = 'input'
    res % layer_shape = [layer_size]
    res % input_layer_shape = [integer ::]
    allocate(res % p, source=input1d_layer(layer_size))
    res % initialized = .true.
  end function input1d


  module function input3d(layer_shape) result(res)
    integer, intent(in) :: layer_shape(3)
    type(layer) :: res
    res % name = 'input'
    res % layer_shape = layer_shape
    res % input_layer_shape = [integer ::]
    allocate(res % p, source=input3d_layer(layer_shape))
    res % initialized = .true.
  end function input3d

  module function reshape(output_shape) result(res)
    integer, intent(in) :: output_shape(:)
    type(layer) :: res

    res % name = 'reshape'
    res % layer_shape = output_shape

    if (size(output_shape) == 3) then
      allocate(res % p, source=reshape3d_layer(output_shape))
    else
      error stop 'size(output_shape) of the reshape layer must == 3'
    end if

  end function reshape

end submodule nf_layer_constructors_submodule
