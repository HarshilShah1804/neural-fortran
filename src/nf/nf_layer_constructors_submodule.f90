submodule(nf_layer_constructors) nf_layer_constructors_submodule
  use nf_input1d_layer, only: input1d_layer
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

end submodule nf_layer_constructors_submodule
