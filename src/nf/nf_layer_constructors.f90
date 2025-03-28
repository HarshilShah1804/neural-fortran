module nf_layer_constructors

  !! This module provides the functions to instantiate specific layers.

  use nf_layer, only: layer
  ! use nf_activation, only : activation_function

  implicit none

  private
  public ::  input, maxpool2d

  interface input

    module function input3d(layer_shape) result(res)
      !! 3-d input layer constructor.
      !!
      !! This layer is for inputting 3-d data to the network.
      !! Currently, this layer must be followed by a conv2d layer.
      !! An input layer must be the first layer in the network.
      !!
      !! This is a specific function that is available
      !! under a generic name `input`.
      !!
      !! Example:
      !!
      !! ```
      !! use nf, only :: input, layer
      !! type(layer) :: input_layer
      !! input_layer = input([28, 28, 1])
      !! ```
      integer, intent(in) :: layer_shape(3)
        !! Shape of the input layer
      type(layer) :: res
        !! Resulting layer instance
    end function input3d

  end interface input

  interface

    module function maxpool2d(pool_size, stride) result(res)
      !! 2-d maxpooling layer constructor.
      !!
      !! This layer is for downscaling other layers, typically `conv2d`.
      !!
      !! Example:
      !!
      !! ```
      !! use nf, only :: maxpool2d, layer
      !! type(layer) :: maxpool2d_layer
      !! maxpool2d_layer = maxpool2d(pool_size=2)
      !! maxpool2d_layer = maxpool2d(pool_size=2, stride=3)
      !! ```
      integer, intent(in) :: pool_size
        !! Width of the pooling window, commonly 2
      integer, intent(in), optional :: stride
        !! Stride of the pooling window, commonly equal to `pool_size`;
        !! Defaults to `pool_size` if omitted.
      type(layer) :: res
        !! Resulting layer instance
    end function maxpool2d

  end interface

end module nf_layer_constructors
