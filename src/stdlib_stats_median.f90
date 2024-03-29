
submodule (stdlib_stats) stdlib_stats_median

  use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
  use stdlib_error, only: error_stop
  use stdlib_optval, only: optval
  ! Use "ord_sort" rather than "sort" because the former can be much faster for arrays
  ! that are already partly sorted. While it is slightly slower for random arrays,
  ! ord_sort seems a better overall choice.
  use stdlib_sorting, only: sort => ord_sort
  implicit none

contains

      module function median_all_1_iint8_dp (x, mask) result(res)
        integer(int8), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int8), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if


        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum( real(x_tmp(c:c+1), kind=dp) ) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_1_iint8_dp
      module function median_all_2_iint8_dp (x, mask) result(res)
        integer(int8), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int8), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if


        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum( real(x_tmp(c:c+1), kind=dp) ) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_2_iint8_dp
      module function median_all_3_iint8_dp (x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int8), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if


        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum( real(x_tmp(c:c+1), kind=dp) ) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_3_iint8_dp
      module function median_all_4_iint8_dp (x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int8), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if


        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum( real(x_tmp(c:c+1), kind=dp) ) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_4_iint8_dp
      module function median_all_1_iint16_dp (x, mask) result(res)
        integer(int16), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int16), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if


        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum( real(x_tmp(c:c+1), kind=dp) ) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_1_iint16_dp
      module function median_all_2_iint16_dp (x, mask) result(res)
        integer(int16), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int16), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if


        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum( real(x_tmp(c:c+1), kind=dp) ) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_2_iint16_dp
      module function median_all_3_iint16_dp (x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int16), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if


        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum( real(x_tmp(c:c+1), kind=dp) ) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_3_iint16_dp
      module function median_all_4_iint16_dp (x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int16), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if


        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum( real(x_tmp(c:c+1), kind=dp) ) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_4_iint16_dp
      module function median_all_1_iint32_dp (x, mask) result(res)
        integer(int32), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int32), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if


        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum( real(x_tmp(c:c+1), kind=dp) ) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_1_iint32_dp
      module function median_all_2_iint32_dp (x, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int32), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if


        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum( real(x_tmp(c:c+1), kind=dp) ) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_2_iint32_dp
      module function median_all_3_iint32_dp (x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int32), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if


        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum( real(x_tmp(c:c+1), kind=dp) ) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_3_iint32_dp
      module function median_all_4_iint32_dp (x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int32), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if


        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum( real(x_tmp(c:c+1), kind=dp) ) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_4_iint32_dp
      module function median_all_1_iint64_dp (x, mask) result(res)
        integer(int64), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int64), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if


        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum( real(x_tmp(c:c+1), kind=dp) ) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_1_iint64_dp
      module function median_all_2_iint64_dp (x, mask) result(res)
        integer(int64), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int64), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if


        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum( real(x_tmp(c:c+1), kind=dp) ) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_2_iint64_dp
      module function median_all_3_iint64_dp (x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int64), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if


        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum( real(x_tmp(c:c+1), kind=dp) ) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_3_iint64_dp
      module function median_all_4_iint64_dp (x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int64), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if


        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum( real(x_tmp(c:c+1), kind=dp) ) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_4_iint64_dp
      module function median_all_1_rsp_sp (x, mask) result(res)
        real(sp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        integer(kind = int64) :: c, n
        real(sp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._sp, ieee_quiet_nan)
            return
          end if

        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._sp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._sp
        else
            res = x_tmp(c)
        end if

      end function median_all_1_rsp_sp
      module function median_all_2_rsp_sp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        integer(kind = int64) :: c, n
        real(sp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._sp, ieee_quiet_nan)
            return
          end if

        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._sp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._sp
        else
            res = x_tmp(c)
        end if

      end function median_all_2_rsp_sp
      module function median_all_3_rsp_sp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        integer(kind = int64) :: c, n
        real(sp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._sp, ieee_quiet_nan)
            return
          end if

        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._sp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._sp
        else
            res = x_tmp(c)
        end if

      end function median_all_3_rsp_sp
      module function median_all_4_rsp_sp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        integer(kind = int64) :: c, n
        real(sp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._sp, ieee_quiet_nan)
            return
          end if

        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._sp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._sp
        else
            res = x_tmp(c)
        end if

      end function median_all_4_rsp_sp
      module function median_all_1_rdp_dp (x, mask) result(res)
        real(dp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        real(dp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._dp, ieee_quiet_nan)
            return
          end if

        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_1_rdp_dp
      module function median_all_2_rdp_dp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        real(dp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._dp, ieee_quiet_nan)
            return
          end if

        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_2_rdp_dp
      module function median_all_3_rdp_dp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        real(dp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._dp, ieee_quiet_nan)
            return
          end if

        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_3_rdp_dp
      module function median_all_4_rdp_dp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer(kind = int64) :: c, n
        real(dp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._dp, ieee_quiet_nan)
            return
          end if

        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._dp
        else
            res = x_tmp(c)
        end if

      end function median_all_4_rdp_dp
      module function median_all_1_rqp_qp (x, mask) result(res)
        real(qp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        integer(kind = int64) :: c, n
        real(qp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._qp, ieee_quiet_nan)
            return
          end if

        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._qp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._qp
        else
            res = x_tmp(c)
        end if

      end function median_all_1_rqp_qp
      module function median_all_2_rqp_qp (x, mask) result(res)
        real(qp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        integer(kind = int64) :: c, n
        real(qp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._qp, ieee_quiet_nan)
            return
          end if

        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._qp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._qp
        else
            res = x_tmp(c)
        end if

      end function median_all_2_rqp_qp
      module function median_all_3_rqp_qp (x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        integer(kind = int64) :: c, n
        real(qp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._qp, ieee_quiet_nan)
            return
          end if

        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._qp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._qp
        else
            res = x_tmp(c)
        end if

      end function median_all_3_rqp_qp
      module function median_all_4_rqp_qp (x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        integer(kind = int64) :: c, n
        real(qp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._qp, ieee_quiet_nan)
            return
          end if

        n = size(x, kind=int64)
        c = floor( (n + 1) / 2._qp, kind=int64 )

        x_tmp = reshape(x, [n])

        call sort(x_tmp)

        if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._qp
        else
            res = x_tmp(c)
        end if

      end function median_all_4_rqp_qp

      module function median_1_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer :: c, n
        integer(int8), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
                x_tmp(:) = x(:)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res = x_tmp(c)
                end if
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_1_iint8_dp
      module function median_2_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        integer(int8), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
                x_tmp(:) = x(:, j2)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j2) = x_tmp(c)
                end if
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
                x_tmp(:) = x(j1, :)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1) = x_tmp(c)
                end if
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_2_iint8_dp
      module function median_3_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer(int8), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(:, j2, j3)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j2, j3) = x_tmp(c)
                end if
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(j1, :, j3)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j3) = x_tmp(c)
                end if
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
                x_tmp(:) = x(j1, j2, :)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j2) = x_tmp(c)
                end if
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_3_iint8_dp
      module function median_4_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer :: j4
        integer(int8), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(:, j2, j3, j4)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2, j3, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j2, j3, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(j1, :, j3, j4)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j3, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j3, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(j1, j2, :, j4)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j2, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(4)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(j1, j2, j3, :)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j2, j3) = x_tmp(c)
                end if
              end do
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_4_iint8_dp
      module function median_1_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer :: c, n
        integer(int16), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
                x_tmp(:) = x(:)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res = x_tmp(c)
                end if
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_1_iint16_dp
      module function median_2_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        integer(int16), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
                x_tmp(:) = x(:, j2)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j2) = x_tmp(c)
                end if
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
                x_tmp(:) = x(j1, :)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1) = x_tmp(c)
                end if
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_2_iint16_dp
      module function median_3_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer(int16), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(:, j2, j3)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j2, j3) = x_tmp(c)
                end if
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(j1, :, j3)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j3) = x_tmp(c)
                end if
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
                x_tmp(:) = x(j1, j2, :)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j2) = x_tmp(c)
                end if
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_3_iint16_dp
      module function median_4_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer :: j4
        integer(int16), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(:, j2, j3, j4)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2, j3, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j2, j3, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(j1, :, j3, j4)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j3, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j3, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(j1, j2, :, j4)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j2, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(4)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(j1, j2, j3, :)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j2, j3) = x_tmp(c)
                end if
              end do
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_4_iint16_dp
      module function median_1_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer :: c, n
        integer(int32), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
                x_tmp(:) = x(:)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res = x_tmp(c)
                end if
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_1_iint32_dp
      module function median_2_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        integer(int32), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
                x_tmp(:) = x(:, j2)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j2) = x_tmp(c)
                end if
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
                x_tmp(:) = x(j1, :)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1) = x_tmp(c)
                end if
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_2_iint32_dp
      module function median_3_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer(int32), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(:, j2, j3)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j2, j3) = x_tmp(c)
                end if
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(j1, :, j3)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j3) = x_tmp(c)
                end if
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
                x_tmp(:) = x(j1, j2, :)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j2) = x_tmp(c)
                end if
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_3_iint32_dp
      module function median_4_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer :: j4
        integer(int32), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(:, j2, j3, j4)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2, j3, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j2, j3, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(j1, :, j3, j4)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j3, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j3, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(j1, j2, :, j4)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j2, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(4)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(j1, j2, j3, :)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j2, j3) = x_tmp(c)
                end if
              end do
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_4_iint32_dp
      module function median_1_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer :: c, n
        integer(int64), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
                x_tmp(:) = x(:)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res = x_tmp(c)
                end if
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_1_iint64_dp
      module function median_2_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        integer(int64), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
                x_tmp(:) = x(:, j2)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j2) = x_tmp(c)
                end if
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
                x_tmp(:) = x(j1, :)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1) = x_tmp(c)
                end if
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_2_iint64_dp
      module function median_3_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer(int64), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(:, j2, j3)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j2, j3) = x_tmp(c)
                end if
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(j1, :, j3)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j3) = x_tmp(c)
                end if
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
                x_tmp(:) = x(j1, j2, :)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j2) = x_tmp(c)
                end if
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_3_iint64_dp
      module function median_4_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer :: j4
        integer(int64), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(:, j2, j3, j4)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2, j3, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j2, j3, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(j1, :, j3, j4)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j3, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j3, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(j1, j2, :, j4)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j2, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(4)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(j1, j2, j3, :)


                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp) ) / 2._dp
                else
                    res(j1, j2, j3) = x_tmp(c)
                end if
              end do
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_4_iint64_dp
      module function median_1_rsp_sp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res

        integer :: c, n
        real(sp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._sp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
                x_tmp(:) = x(:)

                  if (any(ieee_is_nan(x_tmp))) then
                    res = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      return
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else
                    res = x_tmp(c)
                end if
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_1_rsp_sp
      module function median_2_rsp_sp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        real(sp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._sp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
                x_tmp(:) = x(:, j2)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j2) = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      return
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2) = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else
                    res(j2) = x_tmp(c)
                end if
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
                x_tmp(:) = x(j1, :)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1) = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1) = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else
                    res(j1) = x_tmp(c)
                end if
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_2_rsp_sp
      module function median_3_rsp_sp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        real(sp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._sp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(:, j2, j3)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j2, j3) = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      return
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2, j3) = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else
                    res(j2, j3) = x_tmp(c)
                end if
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(j1, :, j3)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j3) = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j3) = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else
                    res(j1, j3) = x_tmp(c)
                end if
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
                x_tmp(:) = x(j1, j2, :)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j2) = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2) = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else
                    res(j1, j2) = x_tmp(c)
                end if
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_3_rsp_sp
      module function median_4_rsp_sp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer :: j4
        real(sp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._sp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(:, j2, j3, j4)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j2, j3, j4) = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      return
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2, j3, j4) = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else
                    res(j2, j3, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(j1, :, j3, j4)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j3, j4) = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j3, j4) = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else
                    res(j1, j3, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(j1, j2, :, j4)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j2, j4) = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2, j4) = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else
                    res(j1, j2, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(4)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(j1, j2, j3, :)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j2, j3) = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2, j3) = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else
                    res(j1, j2, j3) = x_tmp(c)
                end if
              end do
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_4_rsp_sp
      module function median_1_rdp_dp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer :: c, n
        real(dp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
                x_tmp(:) = x(:)

                  if (any(ieee_is_nan(x_tmp))) then
                    res = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      return
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else
                    res = x_tmp(c)
                end if
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_1_rdp_dp
      module function median_2_rdp_dp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        real(dp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
                x_tmp(:) = x(:, j2)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j2) = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      return
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2) = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else
                    res(j2) = x_tmp(c)
                end if
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
                x_tmp(:) = x(j1, :)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1) = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1) = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else
                    res(j1) = x_tmp(c)
                end if
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_2_rdp_dp
      module function median_3_rdp_dp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        real(dp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(:, j2, j3)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j2, j3) = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      return
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2, j3) = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else
                    res(j2, j3) = x_tmp(c)
                end if
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(j1, :, j3)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j3) = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j3) = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else
                    res(j1, j3) = x_tmp(c)
                end if
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
                x_tmp(:) = x(j1, j2, :)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j2) = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2) = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else
                    res(j1, j2) = x_tmp(c)
                end if
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_3_rdp_dp
      module function median_4_rdp_dp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer :: j4
        real(dp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._dp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(:, j2, j3, j4)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j2, j3, j4) = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      return
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2, j3, j4) = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else
                    res(j2, j3, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(j1, :, j3, j4)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j3, j4) = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j3, j4) = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else
                    res(j1, j3, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(j1, j2, :, j4)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j2, j4) = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2, j4) = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else
                    res(j1, j2, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(4)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(j1, j2, j3, :)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j2, j3) = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2, j3) = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else
                    res(j1, j2, j3) = x_tmp(c)
                end if
              end do
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_4_rdp_dp
      module function median_1_rqp_qp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res

        integer :: c, n
        real(qp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._qp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
                x_tmp(:) = x(:)

                  if (any(ieee_is_nan(x_tmp))) then
                    res = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      return
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else
                    res = x_tmp(c)
                end if
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_1_rqp_qp
      module function median_2_rqp_qp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        real(qp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._qp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
                x_tmp(:) = x(:, j2)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j2) = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      return
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2) = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else
                    res(j2) = x_tmp(c)
                end if
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
                x_tmp(:) = x(j1, :)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1) = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1) = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else
                    res(j1) = x_tmp(c)
                end if
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_2_rqp_qp
      module function median_3_rqp_qp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        real(qp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._qp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(:, j2, j3)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j2, j3) = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      return
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2, j3) = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else
                    res(j2, j3) = x_tmp(c)
                end if
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(j1, :, j3)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j3) = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j3) = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else
                    res(j1, j3) = x_tmp(c)
                end if
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
                x_tmp(:) = x(j1, j2, :)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j2) = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2) = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else
                    res(j1, j2) = x_tmp(c)
                end if
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_3_rqp_qp
      module function median_4_rqp_qp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer :: j4
        real(qp), allocatable :: x_tmp(:)

        if (.not.optval(mask, .true.) .or. size(x) == 0) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = size(x, dim)
        c = floor( (n + 1) / 2._qp )

        allocate(x_tmp(n))

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(:, j2, j3, j4)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j2, j3, j4) = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      return
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j2, j3, j4) = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else
                    res(j2, j3, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(j1, :, j3, j4)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j3, j4) = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j3, j4) = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else
                    res(j1, j3, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j4 = 1, size(x, 4)
                x_tmp(:) = x(j1, j2, :, j4)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j2, j4) = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2, j4) = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else
                    res(j1, j2, j4) = x_tmp(c)
                end if
              end do
              end do
              end do
          case(4)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp(:) = x(j1, j2, j3, :)

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j2, j3) = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                if (mod(n, 2) == 0) then
                    res(j1, j2, j3) = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else
                    res(j1, j2, j3) = x_tmp(c)
                end if
              end do
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_4_rqp_qp


      module function median_all_mask_1_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int8), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if


        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_1_iint8_dp
      module function median_all_mask_2_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int8), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if


        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_2_iint8_dp
      module function median_all_mask_3_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int8), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if


        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_3_iint8_dp
      module function median_all_mask_4_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int8), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if


        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_4_iint8_dp
      module function median_all_mask_1_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int16), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if


        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_1_iint16_dp
      module function median_all_mask_2_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int16), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if


        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_2_iint16_dp
      module function median_all_mask_3_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int16), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if


        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_3_iint16_dp
      module function median_all_mask_4_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int16), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if


        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_4_iint16_dp
      module function median_all_mask_1_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int32), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if


        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_1_iint32_dp
      module function median_all_mask_2_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int32), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if


        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_2_iint32_dp
      module function median_all_mask_3_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int32), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if


        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_3_iint32_dp
      module function median_all_mask_4_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int32), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if


        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_4_iint32_dp
      module function median_all_mask_1_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int64), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if


        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_1_iint64_dp
      module function median_all_mask_2_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int64), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if


        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_2_iint64_dp
      module function median_all_mask_3_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int64), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if


        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_3_iint64_dp
      module function median_all_mask_4_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int64), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if


        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_4_iint64_dp
      module function median_all_mask_1_rsp_sp(x, mask) result(res)
        real(sp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(sp) :: res

        integer(kind = int64) :: c, n
        real(sp), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._sp, ieee_quiet_nan)
            return
          end if

        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._sp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._sp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._sp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_1_rsp_sp
      module function median_all_mask_2_rsp_sp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(sp) :: res

        integer(kind = int64) :: c, n
        real(sp), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._sp, ieee_quiet_nan)
            return
          end if

        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._sp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._sp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._sp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_2_rsp_sp
      module function median_all_mask_3_rsp_sp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(sp) :: res

        integer(kind = int64) :: c, n
        real(sp), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._sp, ieee_quiet_nan)
            return
          end if

        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._sp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._sp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._sp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_3_rsp_sp
      module function median_all_mask_4_rsp_sp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(sp) :: res

        integer(kind = int64) :: c, n
        real(sp), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._sp, ieee_quiet_nan)
            return
          end if

        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._sp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._sp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._sp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_4_rsp_sp
      module function median_all_mask_1_rdp_dp(x, mask) result(res)
        real(dp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        real(dp), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._dp, ieee_quiet_nan)
            return
          end if

        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_1_rdp_dp
      module function median_all_mask_2_rdp_dp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        real(dp), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._dp, ieee_quiet_nan)
            return
          end if

        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_2_rdp_dp
      module function median_all_mask_3_rdp_dp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        real(dp), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._dp, ieee_quiet_nan)
            return
          end if

        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_3_rdp_dp
      module function median_all_mask_4_rdp_dp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        real(dp), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._dp, ieee_quiet_nan)
            return
          end if

        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._dp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._dp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_4_rdp_dp
      module function median_all_mask_1_rqp_qp(x, mask) result(res)
        real(qp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(qp) :: res

        integer(kind = int64) :: c, n
        real(qp), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._qp, ieee_quiet_nan)
            return
          end if

        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._qp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._qp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._qp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_1_rqp_qp
      module function median_all_mask_2_rqp_qp(x, mask) result(res)
        real(qp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(qp) :: res

        integer(kind = int64) :: c, n
        real(qp), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._qp, ieee_quiet_nan)
            return
          end if

        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._qp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._qp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._qp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_2_rqp_qp
      module function median_all_mask_3_rqp_qp(x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(qp) :: res

        integer(kind = int64) :: c, n
        real(qp), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._qp, ieee_quiet_nan)
            return
          end if

        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._qp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._qp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._qp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_3_rqp_qp
      module function median_all_mask_4_rqp_qp(x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(qp) :: res

        integer(kind = int64) :: c, n
        real(qp), allocatable   :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

          if (any(ieee_is_nan(x))) then
            res = ieee_value(1._qp, ieee_quiet_nan)
            return
          end if

        x_tmp = pack(x, mask)

        call sort(x_tmp)

        n = size(x_tmp, kind=int64)
        c = floor( (n + 1) / 2._qp, kind=int64)

        if (n == 0) then
            res = ieee_value(1._qp, ieee_quiet_nan)
        else if (mod(n, 2_int64) == 0) then
            res = sum(x_tmp(c:c+1)) / 2._qp
        else if (mod(n, 2_int64) == 1) then
            res = x_tmp(c)
        end if

      end function median_all_mask_4_rqp_qp

      module function median_mask_1_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int8), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
                x_tmp = pack(x(:), &
                              mask(:))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res = x_tmp(c)
                end if

                deallocate(x_tmp)
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_1_iint8_dp
      module function median_mask_2_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        integer(int8), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
                x_tmp = pack(x(:, j2), &
                              mask(:, j2))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j2) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j2) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
                x_tmp = pack(x(j1, :), &
                              mask(j1, :))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_2_iint8_dp
      module function median_mask_3_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer(int8), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(:, j2, j3), &
                              mask(:, j2, j3))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j2, j3) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j2, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(j1, :, j3), &
                              mask(j1, :, j3))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j3) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
                x_tmp = pack(x(j1, j2, :), &
                              mask(j1, j2, :))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j2) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_3_iint8_dp
      module function median_mask_4_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer :: j4
        integer(int8), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(:, j2, j3, j4), &
                              mask(:, j2, j3, j4))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j2, j3, j4) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2, j3, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j2, j3, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(j1, :, j3, j4), &
                              mask(j1, :, j3, j4))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j3, j4) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j3, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j3, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(j1, j2, :, j4), &
                              mask(j1, j2, :, j4))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j2, j4) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(4)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(j1, j2, j3, :), &
                              mask(j1, j2, j3, :))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j2, j3) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_4_iint8_dp
      module function median_mask_1_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int16), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
                x_tmp = pack(x(:), &
                              mask(:))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res = x_tmp(c)
                end if

                deallocate(x_tmp)
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_1_iint16_dp
      module function median_mask_2_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        integer(int16), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
                x_tmp = pack(x(:, j2), &
                              mask(:, j2))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j2) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j2) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
                x_tmp = pack(x(j1, :), &
                              mask(j1, :))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_2_iint16_dp
      module function median_mask_3_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer(int16), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(:, j2, j3), &
                              mask(:, j2, j3))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j2, j3) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j2, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(j1, :, j3), &
                              mask(j1, :, j3))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j3) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
                x_tmp = pack(x(j1, j2, :), &
                              mask(j1, j2, :))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j2) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_3_iint16_dp
      module function median_mask_4_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer :: j4
        integer(int16), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(:, j2, j3, j4), &
                              mask(:, j2, j3, j4))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j2, j3, j4) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2, j3, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j2, j3, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(j1, :, j3, j4), &
                              mask(j1, :, j3, j4))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j3, j4) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j3, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j3, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(j1, j2, :, j4), &
                              mask(j1, j2, :, j4))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j2, j4) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(4)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(j1, j2, j3, :), &
                              mask(j1, j2, j3, :))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j2, j3) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_4_iint16_dp
      module function median_mask_1_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int32), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
                x_tmp = pack(x(:), &
                              mask(:))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res = x_tmp(c)
                end if

                deallocate(x_tmp)
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_1_iint32_dp
      module function median_mask_2_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        integer(int32), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
                x_tmp = pack(x(:, j2), &
                              mask(:, j2))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j2) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j2) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
                x_tmp = pack(x(j1, :), &
                              mask(j1, :))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_2_iint32_dp
      module function median_mask_3_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer(int32), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(:, j2, j3), &
                              mask(:, j2, j3))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j2, j3) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j2, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(j1, :, j3), &
                              mask(j1, :, j3))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j3) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
                x_tmp = pack(x(j1, j2, :), &
                              mask(j1, j2, :))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j2) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_3_iint32_dp
      module function median_mask_4_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer :: j4
        integer(int32), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(:, j2, j3, j4), &
                              mask(:, j2, j3, j4))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j2, j3, j4) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2, j3, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j2, j3, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(j1, :, j3, j4), &
                              mask(j1, :, j3, j4))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j3, j4) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j3, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j3, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(j1, j2, :, j4), &
                              mask(j1, j2, :, j4))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j2, j4) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(4)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(j1, j2, j3, :), &
                              mask(j1, j2, j3, :))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j2, j3) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_4_iint32_dp
      module function median_mask_1_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        integer(int64), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
                x_tmp = pack(x(:), &
                              mask(:))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res = x_tmp(c)
                end if

                deallocate(x_tmp)
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_1_iint64_dp
      module function median_mask_2_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        integer(int64), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
                x_tmp = pack(x(:, j2), &
                              mask(:, j2))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j2) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j2) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
                x_tmp = pack(x(j1, :), &
                              mask(j1, :))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_2_iint64_dp
      module function median_mask_3_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer(int64), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(:, j2, j3), &
                              mask(:, j2, j3))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j2, j3) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j2, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(j1, :, j3), &
                              mask(j1, :, j3))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j3) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
                x_tmp = pack(x(j1, j2, :), &
                              mask(j1, j2, :))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j2) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_3_iint64_dp
      module function median_mask_4_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer :: j4
        integer(int64), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(:, j2, j3, j4), &
                              mask(:, j2, j3, j4))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j2, j3, j4) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2, j3, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j2, j3, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(j1, :, j3, j4), &
                              mask(j1, :, j3, j4))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j3, j4) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j3, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j3, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(j1, j2, :, j4), &
                              mask(j1, j2, :, j4))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j2, j4) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2, j4) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(4)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(j1, j2, j3, :), &
                              mask(j1, j2, j3, :))


                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j2, j3) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2, j3) = &
                        sum(real(x_tmp(c:c+1), kind=dp)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_4_iint64_dp
      module function median_mask_1_rsp_sp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(sp) :: res

        integer(kind = int64) :: c, n
        real(sp), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
                x_tmp = pack(x(:), &
                              mask(:))

                  if (any(ieee_is_nan(x_tmp))) then
                    res = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      return
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._sp, kind=int64 )

                if (n == 0) then
                    res = &
                        ieee_value(1._sp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else if (mod(n, 2_int64) == 1) then
                    res = x_tmp(c)
                end if

                deallocate(x_tmp)
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_1_rsp_sp
      module function median_mask_2_rsp_sp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        real(sp), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
                x_tmp = pack(x(:, j2), &
                              mask(:, j2))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j2) = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._sp, kind=int64 )

                if (n == 0) then
                    res(j2) = &
                        ieee_value(1._sp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2) = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else if (mod(n, 2_int64) == 1) then
                    res(j2) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
                x_tmp = pack(x(j1, :), &
                              mask(j1, :))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1) = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._sp, kind=int64 )

                if (n == 0) then
                    res(j1) = &
                        ieee_value(1._sp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1) = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else if (mod(n, 2_int64) == 1) then
                    res(j1) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_2_rsp_sp
      module function median_mask_3_rsp_sp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        real(sp), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(:, j2, j3), &
                              mask(:, j2, j3))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j2, j3) = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._sp, kind=int64 )

                if (n == 0) then
                    res(j2, j3) = &
                        ieee_value(1._sp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2, j3) = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else if (mod(n, 2_int64) == 1) then
                    res(j2, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(j1, :, j3), &
                              mask(j1, :, j3))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j3) = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._sp, kind=int64 )

                if (n == 0) then
                    res(j1, j3) = &
                        ieee_value(1._sp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j3) = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
                x_tmp = pack(x(j1, j2, :), &
                              mask(j1, j2, :))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j2) = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._sp, kind=int64 )

                if (n == 0) then
                    res(j1, j2) = &
                        ieee_value(1._sp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2) = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_3_rsp_sp
      module function median_mask_4_rsp_sp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer :: j4
        real(sp), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(:, j2, j3, j4), &
                              mask(:, j2, j3, j4))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j2, j3, j4) = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._sp, kind=int64 )

                if (n == 0) then
                    res(j2, j3, j4) = &
                        ieee_value(1._sp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2, j3, j4) = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else if (mod(n, 2_int64) == 1) then
                    res(j2, j3, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(j1, :, j3, j4), &
                              mask(j1, :, j3, j4))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j3, j4) = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._sp, kind=int64 )

                if (n == 0) then
                    res(j1, j3, j4) = &
                        ieee_value(1._sp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j3, j4) = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j3, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(j1, j2, :, j4), &
                              mask(j1, j2, :, j4))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j2, j4) = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._sp, kind=int64 )

                if (n == 0) then
                    res(j1, j2, j4) = &
                        ieee_value(1._sp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2, j4) = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(4)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(j1, j2, j3, :), &
                              mask(j1, j2, j3, :))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j2, j3) = &
                      ieee_value(1._sp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._sp, kind=int64 )

                if (n == 0) then
                    res(j1, j2, j3) = &
                        ieee_value(1._sp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2, j3) = &
                        sum(x_tmp(c:c+1)) / 2._sp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_4_rsp_sp
      module function median_mask_1_rdp_dp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res

        integer(kind = int64) :: c, n
        real(dp), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
                x_tmp = pack(x(:), &
                              mask(:))

                  if (any(ieee_is_nan(x_tmp))) then
                    res = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      return
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res = x_tmp(c)
                end if

                deallocate(x_tmp)
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_1_rdp_dp
      module function median_mask_2_rdp_dp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        real(dp), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
                x_tmp = pack(x(:, j2), &
                              mask(:, j2))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j2) = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j2) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2) = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j2) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
                x_tmp = pack(x(j1, :), &
                              mask(j1, :))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1) = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1) = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_2_rdp_dp
      module function median_mask_3_rdp_dp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        real(dp), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(:, j2, j3), &
                              mask(:, j2, j3))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j2, j3) = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j2, j3) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2, j3) = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j2, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(j1, :, j3), &
                              mask(j1, :, j3))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j3) = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j3) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j3) = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
                x_tmp = pack(x(j1, j2, :), &
                              mask(j1, j2, :))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j2) = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j2) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2) = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_3_rdp_dp
      module function median_mask_4_rdp_dp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer :: j4
        real(dp), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(:, j2, j3, j4), &
                              mask(:, j2, j3, j4))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j2, j3, j4) = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j2, j3, j4) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2, j3, j4) = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j2, j3, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(j1, :, j3, j4), &
                              mask(j1, :, j3, j4))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j3, j4) = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j3, j4) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j3, j4) = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j3, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(j1, j2, :, j4), &
                              mask(j1, j2, :, j4))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j2, j4) = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j2, j4) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2, j4) = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(4)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(j1, j2, j3, :), &
                              mask(j1, j2, j3, :))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j2, j3) = &
                      ieee_value(1._dp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._dp, kind=int64 )

                if (n == 0) then
                    res(j1, j2, j3) = &
                        ieee_value(1._dp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2, j3) = &
                        sum(x_tmp(c:c+1)) / 2._dp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_4_rdp_dp
      module function median_mask_1_rqp_qp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(qp) :: res

        integer(kind = int64) :: c, n
        real(qp), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
                x_tmp = pack(x(:), &
                              mask(:))

                  if (any(ieee_is_nan(x_tmp))) then
                    res = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      return
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._qp, kind=int64 )

                if (n == 0) then
                    res = &
                        ieee_value(1._qp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else if (mod(n, 2_int64) == 1) then
                    res = x_tmp(c)
                end if

                deallocate(x_tmp)
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_1_rqp_qp
      module function median_mask_2_rqp_qp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        real(qp), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
                x_tmp = pack(x(:, j2), &
                              mask(:, j2))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j2) = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._qp, kind=int64 )

                if (n == 0) then
                    res(j2) = &
                        ieee_value(1._qp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2) = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else if (mod(n, 2_int64) == 1) then
                    res(j2) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
                x_tmp = pack(x(j1, :), &
                              mask(j1, :))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1) = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._qp, kind=int64 )

                if (n == 0) then
                    res(j1) = &
                        ieee_value(1._qp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1) = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else if (mod(n, 2_int64) == 1) then
                    res(j1) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_2_rqp_qp
      module function median_mask_3_rqp_qp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        real(qp), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(:, j2, j3), &
                              mask(:, j2, j3))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j2, j3) = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._qp, kind=int64 )

                if (n == 0) then
                    res(j2, j3) = &
                        ieee_value(1._qp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2, j3) = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else if (mod(n, 2_int64) == 1) then
                    res(j2, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(j1, :, j3), &
                              mask(j1, :, j3))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j3) = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._qp, kind=int64 )

                if (n == 0) then
                    res(j1, j3) = &
                        ieee_value(1._qp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j3) = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
                x_tmp = pack(x(j1, j2, :), &
                              mask(j1, j2, :))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j2) = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._qp, kind=int64 )

                if (n == 0) then
                    res(j1, j2) = &
                        ieee_value(1._qp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2) = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_3_rqp_qp
      module function median_mask_4_rqp_qp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer(kind = int64) :: c, n
        integer :: j1
        integer :: j2
        integer :: j3
        integer :: j4
        real(qp), allocatable :: x_tmp(:)

        if (any(shape(x) .ne. shape(mask))) then
            call error_stop("ERROR (median): shapes of x and mask are different")
        end if

        select case(dim)
          case(1)
            ! Loop over every dimension of the array except "dim"
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(:, j2, j3, j4), &
                              mask(:, j2, j3, j4))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j2, j3, j4) = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._qp, kind=int64 )

                if (n == 0) then
                    res(j2, j3, j4) = &
                        ieee_value(1._qp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j2, j3, j4) = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else if (mod(n, 2_int64) == 1) then
                    res(j2, j3, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(2)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j3 = 1, size(x, 3)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(j1, :, j3, j4), &
                              mask(j1, :, j3, j4))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j3, j4) = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._qp, kind=int64 )

                if (n == 0) then
                    res(j1, j3, j4) = &
                        ieee_value(1._qp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j3, j4) = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j3, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(3)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j4 = 1, size(x, 4)
                x_tmp = pack(x(j1, j2, :, j4), &
                              mask(j1, j2, :, j4))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j2, j4) = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._qp, kind=int64 )

                if (n == 0) then
                    res(j1, j2, j4) = &
                        ieee_value(1._qp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2, j4) = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2, j4) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case(4)
            ! Loop over every dimension of the array except "dim"
              do j1 = 1, size(x, 1)
              do j2 = 1, size(x, 2)
              do j3 = 1, size(x, 3)
                x_tmp = pack(x(j1, j2, j3, :), &
                              mask(j1, j2, j3, :))

                  if (any(ieee_is_nan(x_tmp))) then
                    res(j1, j2, j3) = &
                      ieee_value(1._qp, ieee_quiet_nan)
                      cycle
                  end if

                call sort(x_tmp)

                n = size(x_tmp, kind=int64)
                c = floor( (n + 1) / 2._qp, kind=int64 )

                if (n == 0) then
                    res(j1, j2, j3) = &
                        ieee_value(1._qp, ieee_quiet_nan)
                else if (mod(n, 2_int64) == 0) then
                    res(j1, j2, j3) = &
                        sum(x_tmp(c:c+1)) / 2._qp
                else if (mod(n, 2_int64) == 1) then
                    res(j1, j2, j3) = x_tmp(c)
                end if

                deallocate(x_tmp)
              end do
              end do
              end do
          case default
            call error_stop("ERROR (median): wrong dimension")
        end select

      end function median_mask_4_rqp_qp

end submodule
