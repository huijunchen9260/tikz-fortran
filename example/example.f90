program example

    use iso_Fortran_env, only: wp => real64
    use tikz_module

    integer :: i
    real(wp) :: x(5)
    real(wp) :: y(5, 4)
    real(wp) :: z(5)
    real(wp) :: w(5, 20)
    character(len=255) :: val
    integer            :: length, rc


    x = [1, 2, 3, 4, 5]
    do i = 1, 4, 1
        y(:, i) = x + i
    enddo
    z = x * i

    do i = 1, 20, 1
        w(:, i) = x * i
    enddo

    !! example: simplest syntax for 1D array, will be deleted after closing window
    call tikz(z)

    !! example: plot 2D array with names
    call tikz(x, y, name = "./example/tikzplot_4.tex")

    !! example: plot 1D with legend
    call tikz(z, legend = "Example", name = "./example/tikzplot_1_le.tex")

    !! example: plot 2D with multiple legend
    call tikz(x, y, legend = "$+1$; $+2$; $+3$; $+4$ ", name = "./example/tikzplot_4_le.tex")

    !! example: plot 2D with predetermined color
    call tikz(x, y, name = './example/tikzplot_4_col.tex', &
        options = 'color: gray, blue, orange, yellow')

    !! example: plot 2D with legend in the box and box at north east
    call tikz(x, y, name = './example/tikzplot_4_le_box.tex', &
        options = 'legend: box, north east')

    !! example: plot 2D with legend in the box and box at north east
    call tikz(x, y, name = './example/tikzplot_4_col_box.tex', &
        options = 'color: gray, blue, orange, yellow; &
                  legend: box, north east')

    !! example: plot 2D with marker
    call tikz(x, y, name = './example/tikzplot_4_col_mark.tex', &
        options = 'color: gray, blue, orange, yellow; &
                  marker: true')

    !! example: groupplot
    call groupplot(2, 1)
    call tikz(x)
    call tikz(z)
    call groupplot

end program example
