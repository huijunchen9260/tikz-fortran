program example

    use iso_Fortran_env, only: wp => real64
    use tikz_module

    integer :: i
    real(wp) :: x(5)
    real(wp) :: y(5, 4)
    real(wp) :: z(5)


    x = [1, 2, 3, 4, 5]
    do i = 1, 4, 1
        y(:, i) = x + i
    enddo
    z = x * i

    !! example: simplest syntax for 1D array
    call tikz(z)

    !! example: plot 2D array with names
    call tikz(x, y, name = "tikzplot_4.tex")

    !! example: plot 1D with legend
    call tikz(z, legend = "Example", name = "tikzplot_1_le.tex")

    !! example: plot 2D with multiple legend
    call tikz(x, y, legend = "$+1$; $+2$; $+3$; $+4$ ", name = "tikzplot_4_le.tex")

end program example
