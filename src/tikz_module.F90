module tikz_module

!! Not sure whether ifport library is necessary
!#ifdef __INTEL_COMPILER
!use ifport
!#endif

use, intrinsic :: iso_Fortran_env, only : wp => real64, int32, int64, real32, real64, OUTPUT_UNIT, INPUT_UNIT, ERROR_UNIT
!use omp_lib
!Suggestion from fortran-lang to remove strong dependency on openmp:
!https://fortran-lang.discourse.group/t/using-tikz-to-plot-for-fortran/8166/13?u=fish830911
!$omp_lib
implicit none
private
public :: tikz

integer, parameter :: OS_UNKNOWN = 0
integer, parameter :: OS_LINUX   = 1
integer, parameter :: OS_MACOS   = 2
integer, parameter :: OS_WINDOWS = 3
integer, parameter :: OS_CYGWIN  = 4
integer, parameter :: OS_SOLARIS = 5
integer, parameter :: OS_FREEBSD = 6
integer, parameter :: OS_OPENBSD = 7

interface tikz
    module procedure tikz_plot_y, tikz_plot_y2, tikz_plot_xy, tikz_plot_xy2
end interface tikz


! convert int32, int64, real32, real64 into string
interface num2str
    module procedure num2str_int32
    module procedure num2str_int64
    module procedure num2str_real32
    module procedure num2str_real64
end interface num2str

character(len=255) :: opener

contains

subroutine tikz_plot_y(y, title, xlabel, ylabel, legend, name, options)

    !input
    real(wp), intent(in) :: y(:)
    character(len=*), intent(in), optional :: title, xlabel, ylabel, legend, name, options

    call tikz_plot_xy2(&
        linspace(minval(y), maxval(y), size(y, 1)), &
        reshape(y,[size(y),1]), &
        title, xlabel, ylabel, legend, name, options)
end subroutine tikz_plot_y

subroutine tikz_plot_y2(y, title, xlabel, ylabel, legend, name, options)

    !input
    real(wp), intent(in) :: y(:, :)
    character(len=*), intent(in), optional :: title, xlabel, ylabel, legend, name, options

    call tikz_plot_xy2(&
        linspace(minval(y), maxval(y), size(y, 1)), &
        y, &
        title, xlabel, ylabel, legend, name, options)
end subroutine tikz_plot_y2

subroutine tikz_plot_xy(x, y, title, xlabel, ylabel, legend, name, options)
    !input
    real(wp), intent(in) :: x(:), y(:)
    character(len=*), intent(in), optional :: title, xlabel, ylabel, legend, name, options

    call tikz_plot_xy2(x, reshape(y,[size(y),1]), title, xlabel, ylabel, legend, name, options)
end subroutine tikz_plot_xy

subroutine tikz_plot_xy2(x, y, title, xlabel, ylabel, legend, name, options)
    !input
    real(wp), intent(in) :: x(:), y(:, :)
    character(len=*), intent(in), optional :: title, xlabel, ylabel, legend, name, options

    !local
    integer :: i, j
    integer :: unitno                                               ! thread save unit number
    integer :: thread_id
    integer :: ni, nj                                               ! dimension of x and y
    character(len=5) :: tmpChar5                                    ! tmp file name based on thread
    character(len=:), allocatable :: xlb                            ! local x label
    character(len=:), allocatable :: ylb                            ! local y label
    character(len=:), allocatable :: le                             ! local legend
    character(len=:), allocatable :: fname                          ! name of the tex file
    character(len=:), allocatable :: fpath                          ! path of the tex file
    character(len=3) :: ftype                                       ! file type
    character(len=:), allocatable :: dat                            ! data file
    character(len=:), allocatable :: tikz                           ! plotting tex file
    real(wp) :: xmin, xmax, ymin, ymax                              ! boundary of the figure
    real(wp) :: xticsdist, yticsdist                                ! tics distances
    character(len=:), allocatable :: palette                        ! color palette
    character(len=:), allocatable :: cnames                         ! names of the color palette
    character(len=:), allocatable :: styleset                       ! line style sets
    character(len=:), allocatable :: markerset                      ! marker style sets
    character(len=:), dimension(:), allocatable :: colorvec, colorname, optionvec, stylevec
    character(len=:), dimension(:), allocatable :: markervec
    character(len=:), dimension(:), allocatable :: optmp
    character(len=30), dimension(:), allocatable :: legendvec, opcolor, oplinestyle, oplegend
    character(len=30), dimension(:), allocatable :: opmarker
    character(len=:), allocatable :: legend_type, legend_loc
    logical :: isTmp

    ! ---------------------- !
    ! Check inputs dimension !
    ! ---------------------- !

    ni = size(x)
    nj = size(y,2)
    if (size(y,1)/=ni) STOP 'sub_plot_x_y2: x and y dims don''t agree'

    ! ------------------ !
    ! Set default values !
    ! ------------------ !

    if (present(xlabel)) then
        xlb = xlabel
    else
        xlb = '$x$'
    endif

    if (present(ylabel)) then
        ylb = ylabel
    else
        ylb = '$y$'
    endif


    if (present(legend)) then
        le = legend
        legendvec = decompose_str(le, genLoc(le, ';'))
        if (size(legendvec) .ne. nj) then
            error stop "legend error: numbers of legends does not equal to numbers of columns on y"
        endif
    else
        allocate(legendvec(nj))
        do j = 1, nj, 1
            legendvec(j) = "data" // num2str(j)
        enddo
    endif

    ! -------------------- !
    ! Set default colormap !
    ! -------------------- !

    ! matlab colormap
    ! Source: http://gnuplotting.org/matlab-colorbar-parula-with-gnuplot/index.html
    palette = '0072BD; ' // & ! blue
              'D95319; ' // & ! orange
              'EDB120; ' // & ! yellow
              '7E2F8E; ' // & ! purple
              '77AC30; ' // & ! green
              '4DBEEE; ' // & ! light-blue
              'A2142F '       ! red

    cnames = "blue; orange; yellow; purple; green; light-blue; red"

    colorvec = decompose_str(palette, genLoc(palette, ';'))
    colorname = decompose_str(cnames, genLoc(cnames, ';'))

    ! ------------------ !
    ! Default line style !
    ! ------------------ !

    styleset = 'solid; ' // &
               'dashdotdotted; ' // &
               'densely dashdotdotted; ' // &
               'densely dotted; ' // &
               'densely dashed; ' // &
               'dotted; ' // &
               'dashed; ' // &
               'loosely dotted; ' // &
               'loosely dashed; ' // &
               'dashdotted; ' // &
               'densely dashdotted; ' // &
               'loosely dashdotted; ' // &
               'loosely dashdotdotted '

    stylevec = decompose_str(styleset, genLoc(styleset, ';'))

    ! ------------------ !
    ! Default marker set !
    ! ------------------ !

    markerset = 'x;' // &
                'o; ' // &
                'star;' // &
                'square;' // &
                'triangle;' // &
                'diamond;' // &
                'pentagon;' // &
                'asterisk'

    markervec = decompose_str(markerset, genLoc(markerset, ';'))

    call set_opener()

    ! ----------------- !
    ! Decompose options !
    ! ----------------- !

    if (present(options)) then
        optionvec = decompose_str(options, genLoc(options, ';'))
        do i = 1, size(optionvec), 1
            optmp = decompose_str(optionvec(i), genLoc(optionvec(i), ':'))
            select case (trim(optmp(1)))
                case ('legend')
                    oplegend = decompose_str(optmp(2),  genLoc(optmp(2), ','))
                case ('le') ! shorthand for legend
                    oplegend = decompose_str(optmp(2),  genLoc(optmp(2), ','))
                case ('color')
                    opcolor = decompose_str(optmp(2),  genLoc(optmp(2), ','))
                    if (size(opcolor) .ne. nj) then
                        error stop "options error: numbers of color do not equal to numbers of columns on y"
                    endif
                case ('c') ! shorthand for color
                    opcolor = decompose_str(optmp(2),  genLoc(optmp(2), ','))
                    if (size(opcolor) .ne. nj) then
                        error stop "options error: numbers of color do not equal to numbers of columns on y"
                    endif
                case ('linestyle')
                    oplinestyle = decompose_str(optmp(2),  genLoc(optmp(2), ','))
                    if (size(oplinestyle) .ne. nj) then
                        error stop "options error: numbers of line style do not equal to numbers of columns on y"
                    endif
                case ('ls') ! shorthand for linestyle
                    oplinestyle = decompose_str(optmp(2),  genLoc(optmp(2), ','))
                    if (size(oplinestyle) .ne. nj) then
                        error stop "options error: numbers of line style do not equal to numbers of columns on y"
                    endif
                case ('marker')
                    opmarker = decompose_str(optmp(2),  genLoc(optmp(2), ','))
                    if (size(opmarker) .ne. nj) then
                        error stop "options error: numbers of marker do not equal to numbers of columns on y"
                    endif
            end select
            deallocate(optmp)
       enddo
    endif

    ! ------------------------------------- !
    ! Assign oplegend to type and location  !
    ! ------------------------------------- !

    if (allocated(oplegend)) then
        do j = 1, size(oplegend), 1
            if (trim(oplegend(j)) == 'line' .or. trim(oplegend(j)) == 'box') then
                legend_type = oplegend(j)
            else
                legend_loc = oplegend(j)
            endif
        enddo
    endif

    ! ---------------- !
    ! default oplegend !
    ! ---------------- !

    if (.not. allocated(legend_loc)) legend_loc = 'south east'
    if (.not. allocated(legend_type)) legend_type = 'line'

    ! ------------------- !
    ! default oplinestyle !
    ! ------------------- !

    if (.not. allocated(oplinestyle)) then
        allocate(oplinestyle(nj))
        do j = 1, nj, 1
            i = mod(j, size(stylevec))
            i = merge(size(stylevec), i, i .eq. 0)
            oplinestyle(j) = stylevec(i)
        enddo
    endif

    ! ---------------- !
    ! default opmarker !
    ! ---------------- !

    if (.not. allocated(opmarker)) then
        allocate(opmarker(nj))
        do j = 1, nj, 1
            i = mod(j, size(markervec))
            i = merge(size(markervec), i, i .eq. 0)
            opmarker(j) = markervec(i)
        enddo
    endif

    ! --------------------------------------------------- !
    ! if color not set by options, use palette and repeat !
    ! --------------------------------------------------- !

    if (.not. allocated(opcolor)) then
        allocate(opcolor(nj))
        do j = 1, nj, 1
            i = mod(j, size(colorname))
            i = merge(size(colorname), i, i .eq. 0)
            opcolor(j) = colorname(i)
        enddo
    endif

    ! ------------------------------------- !
    ! thread safe external file unit number !
    ! ------------------------------------- !

    !Suggestion from fortran-lang to remove strong dependency on openmp:
    !https://fortran-lang.discourse.group/t/using-tikz-to-plot-for-fortran/8166/13?u=fish830911
    thread_id = 0
    !$thread_id = omp_get_thread_num()
    write(tmpChar5,'(i0)') thread_id
    unitno = 726+thread_id

    ! ------------------- !
    ! decompose file name !
    ! ------------------- !

    if (present(name)) then
        call decompose_name(fname, fpath, ftype, name)
        dat = trim(fname) // '.dat'
        tikz = trim(fname) // '.tex'
        isTmp = .false.
    else
        fpath = './'
        fname = 'tikzplot' // trim(tmpChar5)
        ftype = ''
        dat = 'tikzdata' // trim(tmpChar5) // '.dat'
        tikz = 'tikzplot' // trim(tmpChar5) // '.tex'
        isTmp = .true.
    endif

    ! ------------------ !
    ! creating data file !
    ! ------------------ !

    call write_dat(x, y, ni, nj, unitno, fpath, dat)

    xmin = minval(x)
    xmax = maxval(x)
    ymin = minval(y)
    ymax = maxval(y)

    xticsdist = (xmax - xmin) / 12.0_wp
    yticsdist = (ymax - ymin) / 12.0_wp

    call write_tikz(unitno, fpath, tikz, dat, nj, xmin, xmax, ymin, ymax, xticsdist, yticsdist, &
        colorvec, colorname, title, xlb, ylb, legendvec, legend_type, legend_loc, &
        opcolor, oplinestyle, opmarker)

    call typeset(fpath, fname, tikz, .true.)

    if (isTmp) then
        call execute_command_line(trim(opener) // ' ' // trim(fpath) // trim(fname) // '.pdf > /dev/null 2>&1;')
    else
        call execute_command_line(trim(opener) // ' ' // trim(fpath) // trim(fname) // '.pdf > /dev/null 2>&1 &')
    endif

    if (isTmp) call execute_command_line(&
        'rm ' // trim(fpath) // trim(fname) // '.pdf; ' // &
        'rm ' // trim(fpath) // trim(tikz) // '; ' // &
        'rm ' // trim(fpath) // trim(dat) // '; ')


end subroutine tikz_plot_xy2

subroutine write_tikz(unitno, fpath, tikz, dat, nj, xmin, xmax, ymin, ymax, xticsdist, yticsdist, &
        colorvec, colorname, title, xlb, ylb, legendvec, legend_type, legend_loc, &
        opcolor, oplinestyle, opmarker)

    integer, intent(in) :: unitno, nj
    character(len=*), intent(in) :: fpath, tikz, dat
    character(len=*), intent(in) :: title, xlb, ylb
    character(len=:), dimension(:), allocatable, intent(in) :: colorvec, colorname
    character(len=*), intent(in) :: legend_type, legend_loc
    character(len=*), dimension(:), intent(in) :: legendvec, opcolor, oplinestyle, opmarker
    real(wp), intent(in) :: xmin, xmax, ymin, ymax, xticsdist, yticsdist
    integer :: j


    open(unitno, file=trim(fpath) // trim(tikz),status='unknown')
       write(unitno, *) "\documentclass[tikz]{standalone}"

       write(unitno, *) "\usepackage{tikz}"
       write(unitno, *) "\usepackage{pgfplots}"
       write(unitno, *) "\usetikzlibrary{decorations}"
       write(unitno, *) "\usetikzlibrary{decorations.pathreplacing, intersections, fillbetween}"
       write(unitno, *) "\usetikzlibrary{calc,positioning}"
       write(unitno, *) "\usetikzlibrary{plotmarks}"
       write(unitno, *) "\pgfplotsset{compat=newest, scale only axis, width = 13cm, height = 6cm}"
       write(unitno, *) "\pgfplotsset{sciclean/.style={axis lines=left,"
       write(unitno, *) "        axis x line shift=0.5em,"
       write(unitno, *) "        axis y line shift=0.5em,"
       write(unitno, *) "        axis line style={-,very thin},"
       write(unitno, *) "        axis background/.style={draw,ultra thin,gray},"
       write(unitno, *) "        tick align=outside,"
       write(unitno, *) "        xtick distance=" // num2str(xticsdist) // ","
       write(unitno, *) "        ytick distance=" // num2str(yticsdist) // ","
       write(unitno, *) "        xticklabel style={"
       write(unitno, *) "            /pgf/number format/fixed,"
       write(unitno, *) "            /pgf/number format/precision=2"
       write(unitno, *) "        },"
       write(unitno, *) "        yticklabel style={"
       write(unitno, *) "            /pgf/number format/fixed,"
       write(unitno, *) "            /pgf/number format/precision=2"
       write(unitno, *) "        },"
       write(unitno, *) "        scaled y ticks=false,"
       write(unitno, *) "        major tick length=2pt}}"
       write(unitno, *) ""
       write(unitno, *) "% Create fake \onslide and other commands for standalone picture"
       write(unitno, *) "\usepackage{xparse}"
       write(unitno, *) "\NewDocumentCommand{\onslide}{s t+ d<>}{}"
       write(unitno, *) "\NewDocumentCommand{\only}{d<>}{}"
       write(unitno, *) "\NewDocumentCommand{\uncover}{d<>}{}"
       write(unitno, *) "\NewDocumentCommand{\visible}{d<>}{}"
       write(unitno, *) "\NewDocumentCommand{\invisible}{d<>}{}"
       write(unitno, *) ""
       write(unitno, *) "\makeatletter"
       write(unitno, *) "\tikzset{"
       write(unitno, *) "Sloped/.code = {"
       write(unitno, *) "\iftikz@fullytransformed% tikz.code.tex"
       write(unitno, *) "    \tikzset{sloped}"
       write(unitno, *) "\else"
       write(unitno, *) "    \pgfgettransformentries{\mya}{\myb}{\myc}{\myd}{\mys}{\myt}%"
       write(unitno, *) "    \tikzset{sloped, transform shape, rotate = {atan2(\myb,\mya)}}%"
       write(unitno, *) "\fi"
       write(unitno, *) "}"
       write(unitno, *) "}"
       write(unitno, *) "\makeatother"
       write(unitno, *) ""
       write(unitno, *) "% ---------------------------------------------------------------------"
       write(unitno, *) "% Coordinate extraction"
       write(unitno, *) "% #1: node name"
       write(unitno, *) "% #2: output macro name: x coordinate"
       write(unitno, *) "% #3: output macro name: y coordinate"
       write(unitno, *) "\newcommand{\Getxycoords}[3]{%"
       write(unitno, *) "    \pgfplotsextra{%"
       write(unitno, *) "        % using `\pgfplotspointgetcoordinates' stores the (axis)"
       write(unitno, *) "        % coordinates in `data point' which then can be called by"
       write(unitno, *) "        % `\pgfkeysvalueof' or `\pgfkeysgetvalue'"
       write(unitno, *) "        \pgfplotspointgetcoordinates{(#1)}%"
       write(unitno, *) "        % `\global' (a TeX macro and not a TikZ/PGFPlots one) allows to"
       write(unitno, *) "        % store the values globally"
       write(unitno, *) "         \global\pgfkeysgetvalue{/data point/x}{#2}%"
       write(unitno, *) "         \global\pgfkeysgetvalue{/data point/y}{#3}%"
       write(unitno, *) "     }%"
       write(unitno, *) "}"
       write(unitno, *) "% ---------------------------------------------------------------------"
       write(unitno, *) ""
       do j = 1, size(colorvec), 1
           write(unitno, *) "\definecolor{" // trim(colorname(j)) // "}{HTML}{" // trim(colorvec(j)) // "}"
       enddo
       write(unitno, *) ""
       write(unitno, *) "\begin{document}"
       write(unitno, *) ""
       write(unitno, *) "\begin{tikzpicture}"
       write(unitno, *) ""
       write(unitno, *) ""
       write(unitno, *) "\begin{axis}["
       write(unitno, *) "    sciclean,"
       write(unitno, *) "    xlabel = {" // xlb // "},"
       write(unitno, *) "    ylabel = {" // ylb // "},"
       write(unitno, *) "    xmin = " // num2str(xmin) // ","
       write(unitno, *) "    xmax = " // num2str(xmax) // ","
       write(unitno, *) "    ymin = " // num2str(ymin) // ","
       write(unitno, *) "    ymax = " // num2str(ymax) // ","
       write(unitno, *) "    legend cell align = left,"
       write(unitno, *) "    legend pos = " // legend_loc // ","
       write(unitno, *) "    title = {" // title // "}]"
       write(unitno, *) ""
       write(unitno, *) ""
       call plotting(unitno, nj, colorvec, colorname, fpath, dat, legendvec, &
           legend_type, opcolor, oplinestyle, opmarker)
       write(unitno, *) ""
       write(unitno, *) ""
       write(unitno, *) "\end{axis}"
       write(unitno, *) ""
       write(unitno, *) ""
       write(unitno, *) "\end{tikzpicture}"
       write(unitno, *) ""
       write(unitno, *) "\end{document}"
    close(unitno)


end subroutine write_tikz

subroutine plotting(unitno, nj, colorvec, colorname, fpath, dat, legendvec, legend_type, &
        opcolor, oplinestyle, opmarker)
    integer, intent(in) :: unitno, nj
    character(len=*), intent(in) :: fpath, dat
    character(len=:), dimension(:), allocatable, intent(in) :: colorvec, colorname
    character(len=*), dimension(:), intent(in) :: legendvec, opcolor, oplinestyle, opmarker
    character(len=*), intent(in) :: legend_type

    integer :: j

    do j = 1, nj, 1
        select case (trim(legend_type))
        case ('line')
            write(unitno, *) "\addplot[name path = " // num2str(j) // &
                ", thick, " // trim(oplinestyle(j)) // ", color=" // trim(opcolor(j)) // &
                ", mark=" // trim(opmarker(j)) // &
                "] table [x expr=\thisrowno{0}, y expr=\thisrowno{" // num2str(j) // &
                "}]{" // dat // "} node[" // trim(opcolor(j)) // ", pos=0.5, above, Sloped]{" // trim(legendvec(j)) // "};"
        case ('box')
            write(unitno, *) "\addplot[name path = " // num2str(j) // &
                ", thick, " // trim(oplinestyle(j)) // ", color=" // trim(opcolor(j)) // &
                ", mark=" // trim(opmarker(j)) // &
                "] table [x expr=\thisrowno{0}, y expr=\thisrowno{" // num2str(j) // &
                "}]{" // dat // "};"
            write(unitno, *) "\addlegendentry{" // trim(legendvec(j)) // "}"
        end select
    enddo

end subroutine plotting

subroutine write_dat(x, y, ni, nj, unitno, fpath, dat)
    real(wp), intent(in) :: x(:), y(:, :)
    integer, intent(in) :: unitno, ni, nj
    character(len=*), intent(in) :: fpath, dat
    integer :: i, j

    ! Write data to a temporary file
    open(unitno,file=trim(fpath) // trim(dat),status='unknown')
    do i = 1,ni
        write(unitno,'(1PE24.15E3)',advance='no') x(i)
        do j = 1,nj
            write(unitno,'(1PE24.15E3)',advance='no') y(i,j)
        end do
        write(unitno,*)
    end do
    close(unitno)
end subroutine write_dat

subroutine decompose_name(fname, fpath, ftype, name)

    character(len=*), intent(in) :: name
    character(len=3), intent(out) :: ftype
    character(len=:), intent(out), allocatable :: fname, fpath
    integer :: i, nf

    nf = len_trim(name)
    ftype = name(nf-2:nf)
    do i = nf, 1, -1
        if (name(i:i) .eq. '/') exit
    enddo
    fname = name(i+1:nf-4)

    if (len_trim(name(1:i)) .eq. 0) then
        fpath = "./"
    else
        fpath = name(1:i)
    endif

end subroutine decompose_name

! ------------------------------------ !
! decompose string into string vectors !
! ------------------------------------ !
function decompose_str(str, chloc) result(strvec)
    character(len=*), intent(in) :: str
    integer, dimension(:), intent(in) :: chloc
    character(len=len_trim(str)), dimension(:), allocatable :: strvec
    integer :: j, nk, k, lastj, nstr

    nstr = len_trim(str)
    nk = sum(chloc)

    allocate(strvec(nk+1))

    ! decompose str into array
    k = 0
    lastj = 1
    do j = 1, nstr, 1
        if (chloc(j) == 1) then
            k = k + 1
            strvec(k) = str(lastj:j-1)
            lastj = j+1
        endif
        if (j .eq. nstr) then
            k = k + 1
            strvec(k) = str(lastj:j)
        endif
    enddo

    do k = 1, nk+1, 1
        strvec(k) = trim(adjustl(strvec(k)))
    enddo

end function decompose_str

!function paired_str(str, delim1, delim2) result(pairvec)
!
!    character(len=*), intent(in) :: str, delim1, delim2
!    character(len=len_trim(str)), dimension(:, :), allocatable :: strvec
!    character(len=len_trim(str)), dimension(:), allocatable :: tmp
!
!
!    tmp = decompose_str(str, genLoc(str, delim1))
!
!
!
!end function paired_str

! -------------------------------------------------------- !
! generate an integer array chloc to record the appearance !
! of character ch in string s1                             !
! -------------------------------------------------------- !

function genLoc(s1, ch) result(chloc)
    character(len=*), intent(in) :: s1
    character(len=1), intent(in) :: ch
    character(len=len_trim(s1)) :: s
    integer :: i, k
    integer, dimension(:), allocatable :: chloc

    s = s1

    k = 0

    allocate(chloc(len_trim(s1)), source = 0)

    do i = 1, len_trim(s)-1, 1
        if (s(i:i) == ch) chloc(i) = 1
    enddo
end function genLoc

! Acts just like the colon in Matlab
pure function colon(a,b)
    implicit none
    integer, intent(in) :: a,b
    integer, dimension(1:b-a+1) :: colon
    integer :: i
    do i = a,b
        colon(i-a+1) = i
    end do
end function colon

! linspace(a,b,n) (of course) constructs a grid of n linearly spaced points between
! a and b. If n==1, then the grid is just "a", consistent with Matlab. If n<=0, the routine
! stops with an error.
function linspace(x1,x2,n) result(grid)
    implicit none
    real(wp), intent(in) :: x1,x2
    integer, intent(in) :: n
    real(wp), dimension(1:n) :: grid
    !local
    integer :: i

    if (n>1) then
        grid(1) = x1
        do i=2,n-1
            grid(i) = x1 + (x2-x1)*(real(i,wp)-1.0_wp)/(real(n,wp)-1.0_wp)
        end do
        grid(n) = x2
    elseif (n==1) then
        grid(1) = x1
    else
        STOP 'linspace: ERROR: grid has size <= 0'
    end if
end function linspace

function num2str_int32(num)
    integer(int32), intent(in)      :: num
    character(len=:), allocatable   :: num2str_int32
    character(len=range(num))       :: str

    write(unit=str, fmt='(I0)') num
    num2str_int32 = trim(str)
end function num2str_int32

function num2str_int64(num)
    integer(int64), intent(in)      :: num
    character(len=:), allocatable   :: num2str_int64
    character(len=range(num))       :: str

    write(unit=str, fmt='(I0)') num
    num2str_int64 = trim(str)
end function num2str_int64

function num2str_real32(num, strfmt)
    real(real32), intent(in)        :: num
    character(len=*), optional      :: strfmt
    character(len=:), allocatable   :: num2str_real32
    character(len=range(num))       :: str

    if (present(strfmt)) then
        write(unit=str, fmt= '('//trim(strfmt)//')' ) num
    else
        write(unit=str, fmt='(G0)') num
    end if

    num2str_real32 = trim(str)
end function num2str_real32

function num2str_real64(num, strfmt)
    real(real64), intent(in)        :: num
    character(len=*), optional      :: strfmt
    character(len=:), allocatable   :: num2str_real64
    character(len=range(num))       :: str

    if (present(strfmt)) then
        write(unit=str, fmt= '('//trim(strfmt)//')' ) num
    else
        write(unit=str, fmt='(G0)') num
    end if

    num2str_real64 = trim(str)
end function num2str_real64

function round(val, n)
    implicit none
    real(wp) :: val, round
    integer :: n
    round = anint(val*10.0_wp**n)/10.0_wp**n
end function round

subroutine typeset(fpath, fname, tikz, isSaveFile)

    character(len=*), intent(in) :: fname, fpath, tikz
    logical, intent(in) :: isSaveFile
    character(len=:), allocatable :: plotfile
    integer :: unitno, ierr

    write(*, *) trim(fpath) // trim(fname)

    call execute_command_line('cwd=$PWD; cd ' // trim(fpath) // &
        '; pdflatex ' // trim(tikz) // ' > /dev/null 2>&1; cd "$cwd"',exitstat=ierr)

    !! debug
    !call execute_command_line('cwd=$PWD; cd ' // trim(fpath) // &
    !    '; pdflatex ' // trim(tikz) // '; cd "$cwd"',exitstat=ierr)

    if (ierr/=0) print*,'sub_plot_x_y2: WARNING: error in calling gnuplot'

    call execute_command_line('cwd=$PWD; ' // &
        '[ -f ' // trim(fpath) // trim(fname) // '-eps-converted-to.pdf'   // ' ] && rm ' // trim(fpath) // trim(fname) // '-eps-converted-to.pdf;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.aux'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.aux;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.log'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.log;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.4tc'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.4tc;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.xref'                   // ' ] && rm ' // trim(fpath) // trim(fname) // '.xref;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.tmp'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.tmp;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.pyc'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.pyc;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.pyg'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.pyg;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.pyo'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.pyo;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.fls'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.fls;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.vrb'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.vrb;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.fdb_latexmk'            // ' ] && rm ' // trim(fpath) // trim(fname) // '.fdb_latexmk;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.bak'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.bak;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.swp'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.swp;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.aux'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.aux;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.log'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.log;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.lof'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.lof;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.lot'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.lot;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.maf'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.maf;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.idx'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.idx;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.mtc'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.mtc;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.mtc0'                   // ' ] && rm ' // trim(fpath) // trim(fname) // '.mtc0;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.nav'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.nav;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.out'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.out;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.snm'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.snm;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.toc'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.toc;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.bcf'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.bcf;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.run.xml'                // ' ] && rm ' // trim(fpath) // trim(fname) // '.run.xml;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.synctex.gz'             // ' ] && rm ' // trim(fpath) // trim(fname) // '.synctex.gz;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.blg'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.blg;' // &
        '[ -f ' // trim(fpath) // trim(fname) // '.bbl'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.bbl;' // &
        'cd "$cwd"',exitstat=ierr)



end subroutine typeset

subroutine set_opener

    integer :: OS

    OS = get_os_type()

    select case (OS)
        case (OS_UNKNOWN)
            opener = 'xdg-open'
        case (OS_LINUX)
            opener = 'xdg-open'
        case (OS_MACOS)
            opener = 'open'
    end select

end subroutine set_opener

!! Source: fpm
integer function get_os_type() result(r)
    !!
    !! Returns one of OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, OS_CYGWIN,
    !! OS_SOLARIS, OS_FREEBSD, OS_OPENBSD.
    !!
    !! At first, the environment variable `OS` is checked, which is usually
    !! found on Windows. Then, `OSTYPE` is read in and compared with common
    !! names. If this fails too, check the existence of files that can be
    !! found on specific system types only.
    !!
    !! Returns OS_UNKNOWN if the operating system cannot be determined.
    character(len=255) :: val
    integer            :: length, rc
    logical            :: file_exists
    logical, save      :: first_run = .true.
    integer, save      :: ret = OS_UNKNOWN
    !$omp threadprivate(ret, first_run)

    if (.not. first_run) then
        r = ret
        return
    end if

    first_run = .false.
    r = OS_UNKNOWN

    ! Check environment variable `OSTYPE`.
    call get_environment_variable('OSTYPE', val, length, rc)

    if (rc == 0 .and. length > 0) then
        ! Linux
        if (index(val, 'linux') > 0) then
            r = OS_LINUX
            ret = r
            return
        end if

        ! macOS
        if (index(val, 'darwin') > 0) then
            r = OS_MACOS
            ret = r
            return
        end if

        ! Windows, MSYS, MinGW, Git Bash
        if (index(val, 'win') > 0 .or. index(val, 'msys') > 0) then
            r = OS_WINDOWS
            ret = r
            return
        end if

        ! Cygwin
        if (index(val, 'cygwin') > 0) then
            r = OS_CYGWIN
            ret = r
            return
        end if

        ! Solaris, OpenIndiana, ...
        if (index(val, 'SunOS') > 0 .or. index(val, 'solaris') > 0) then
            r = OS_SOLARIS
            ret = r
            return
        end if

        ! FreeBSD
        if (index(val, 'FreeBSD') > 0 .or. index(val, 'freebsd') > 0) then
            r = OS_FREEBSD
            ret = r
            return
        end if

        ! OpenBSD
        if (index(val, 'OpenBSD') > 0 .or. index(val, 'openbsd') > 0) then
            r = OS_OPENBSD
            ret = r
            return
        end if
    end if

    ! Check environment variable `OS`.
    call get_environment_variable('OS', val, length, rc)

    if (rc == 0 .and. length > 0 .and. index(val, 'Windows_NT') > 0) then
        r = OS_WINDOWS
        ret = r
        return
    end if

    ! Linux
    inquire (file='/etc/os-release', exist=file_exists)

    if (file_exists) then
        r = OS_LINUX
        ret = r
        return
    end if

    ! macOS
    inquire (file='/usr/bin/sw_vers', exist=file_exists)

    if (file_exists) then
        r = OS_MACOS
        ret = r
        return
    end if

    ! FreeBSD
    inquire (file='/bin/freebsd-version', exist=file_exists)

    if (file_exists) then
        r = OS_FREEBSD
        ret = r
        return
    end if
end function get_os_type



end module tikz_module
