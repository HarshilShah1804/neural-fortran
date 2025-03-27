module nf
  !! User API: everything an application needs to reference directly

  use nf_loss, only: mse, quadratic
  use nf_metrics, only: corr, maxabs

end module nf
