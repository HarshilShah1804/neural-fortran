module nf_layer_constructors

  !! This module provides the functions to instantiate specific layers.

  use nf_layer, only: layer

  implicit none

  private
  public :: input

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

  end interface input

end module nf_layer_constructors
