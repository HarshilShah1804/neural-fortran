submodule(nf_network) nf_network_submodule

  use nf_input1d_layer, only: input1d_layer
  use nf_input3d_layer, only: input3d_layer
  use nf_reshape_layer, only: reshape3d_layer
  use nf_layer, only: layer
  use nf_layer_constructors, only:  input, reshape
  use nf_loss, only: quadratic
  use nf_parallel, only: tile_indices
  use nf_optimizers, only: optimizer_base_type, sgd


  implicit none

contains

  module function evaluate_batch_1d(self, input_data, output_data, metric) result(res)
    class(network), intent(in out) :: self
    real, intent(in) :: input_data(:,:)
    real, intent(in) :: output_data(:,:)
    class(metric_type), intent(in), optional :: metric
    real, allocatable :: res(:,:)

    integer :: i, n
    real, allocatable :: output(:,:)

    ! output = self % predict(input_data)

    n = 1
    if (present(metric)) n = n + 1

    allocate(res(size(output, dim=1), n))

    do i = 1, size(output, dim=1)
      res(i,1) = self % loss % eval(output_data(i,:), output(i,:))
    end do

    if (.not. present(metric)) return

    do i = 1, size(output, dim=1)
      res(i,2) = metric % eval(output_data(i,:), output(i,:))
    end do

  end function evaluate_batch_1d


  module subroutine forward_1d(self, input)
    class(network), intent(in out) :: self
    real, intent(in) :: input(:)
    integer :: n

    ! Set the input array into the input layer
    select type(input_layer => self % layers(1) % p); type is(input1d_layer)
      call input_layer % set(input)
    end select

    do n = 2, size(self % layers)
      call self % layers(n) % forward(self % layers(n - 1))
    end do

  end subroutine forward_1d


  module subroutine forward_3d(self, input)
    class(network), intent(in out) :: self
    real, intent(in) :: input(:,:,:)
    integer :: n

    ! Set the input array into the input layer
    select type(input_layer => self % layers(1) % p); type is(input3d_layer)
      call input_layer % set(input)
    end select

    do n = 2, size(self % layers)
      call self % layers(n) % forward(self % layers(n - 1))
    end do

  end subroutine forward_3d

  module subroutine print_info(self)
    class(network), intent(in) :: self
    call self % layers % print_info()
  end subroutine print_info


  module function get_num_params(self)
    class(network), intent(in) :: self
    integer :: get_num_params

    get_num_params = sum(self % layers % get_num_params())

  end function get_num_params


  module function get_params(self) result(params)
    class(network), intent(in) :: self
    real, allocatable :: params(:)
    integer :: n, nstart, nend

    allocate(params(self % get_num_params()))

    nstart = 1
    do n = 1, size(self % layers)

      if (self % layers(n) % get_num_params() < 1) cycle

      nend = nstart + self % layers(n) % get_num_params() - 1
      params(nstart:nend) = self % layers(n) % get_params()
      nstart = nend + 1
    end do

  end function get_params


  module function get_gradients(self) result(gradients)
    class(network), intent(in) :: self
    real, allocatable :: gradients(:)
    integer :: n, nstart, nend

    allocate(gradients(self % get_num_params()))

    nstart = 1
    do n = 1, size(self % layers)

      if (self % layers(n) % get_num_params() < 1) cycle

      nend = nstart + self % layers(n) % get_num_params() - 1
      gradients(nstart:nend) = self % layers(n) % get_gradients()
      nstart = nend + 1
    end do

  end function get_gradients


  module subroutine set_params(self, params)
    class(network), intent(in out) :: self
    real, intent(in) :: params(:)
    integer :: n, nstart, nend

    ! Check that the number of parameters is correct.
    if (size(params) /= self % get_num_params()) then
      error stop 'network % set_params: number of parameters does not match.'
    end if

    nstart = 1
    do n = 1, size(self % layers)
      nend = nstart + self % layers(n) % get_num_params() - 1
      if (nend - nstart < 1) cycle
      call self % layers(n) % set_params(params(nstart:nend))
      nstart = nend + 1
    end do

  end subroutine set_params


  module subroutine train(self, input_data, output_data, batch_size, &
                          epochs, optimizer, loss)
    class(network), intent(in out) :: self
    real, intent(in) :: input_data(:,:)
    real, intent(in) :: output_data(:,:)
    integer, intent(in) :: batch_size
    integer, intent(in) :: epochs
    class(optimizer_base_type), intent(in), optional :: optimizer
    class(loss_type), intent(in), optional :: loss

    real :: pos
    integer :: dataset_size
    integer :: batch_start
    integer :: i, j, n
    integer :: istart, iend, indices(2)

    ! Passing the optimizer instance is optional.
    ! If not provided, we default to SGD with its default settings.
    if (present(optimizer)) then
      self % optimizer = optimizer
    else
      self % optimizer = sgd()
    end if

    call self % optimizer % init(self % get_num_params())

    ! Passing the loss instance is optional.
    ! If not provided, we default to quadratic().
    if (present(loss)) then
      self % loss = loss
    else
      self % loss = quadratic()
    end if

    dataset_size = size(output_data, dim=2)

    epoch_loop: do n = 1, epochs
      batch_loop: do i = 1, dataset_size / batch_size

        ! Pull a random mini-batch from the dataset
        call random_number(pos)
        batch_start = int(pos * (dataset_size - batch_size + 1)) + 1

! #ifdef PARALLEL
!         ! FIXME shuffle in a way that doesn't require co_broadcast
!         call co_broadcast(batch_start, 1)
! #endif

        ! Distribute the batch in nearly equal pieces to all images
        indices = tile_indices(batch_size)
        istart = indices(1) + batch_start - 1
        iend = indices(2) + batch_start - 1

        do j = istart, iend
          call self % forward(input_data(:,j))
          ! call self % backward(output_data(:,j))
        end do

        ! call self % update(batch_size=batch_size)

      end do batch_loop
    end do epoch_loop

  end subroutine train

end submodule nf_network_submodule