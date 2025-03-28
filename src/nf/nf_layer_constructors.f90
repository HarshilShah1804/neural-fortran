module nf_layer_constructors

  !! This module provides the functions to instantiate specific layers.

  use nf_layer, only: layer

  implicit none

  private
  public ::  input, reshape

  interface input

    module function input1d(layer_size) result(res)
      !! 1-d input layer constructor.
      !!
      !! This layer is for inputting 1-d data to the network.
      !! Currently, this layer must be followed by a dense layer.
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
      !! input_layer = input(768)
      !! ```
      integer, intent(in) :: layer_size
        !! Size of the input layer
      type(layer) :: res
        !! Resulting layer instance
    end function input1d

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

    module function reshape(output_shape) result(res)
      !! Rank-1 to rank-any reshape layer constructor.
      !! Currently implemented is only rank-3 for the output of the reshape.
      !!
      !! This layer is for connecting 1-d inputs to conv2d or similar layers.
      integer, intent(in) :: output_shape(:)
        !! Shape of the output
      type(layer) :: res
        !! Resulting layer instance
    end function reshape

  end interface

end module nf_layer_constructors
