module tikz_module

!! Not sure whether ifport library is necessary
!#ifdef __INTEL_COMPILER
!use ifport
!#endif

use, intrinsic :: iso_Fortran_env, only : wp => real64, int32, int64, real32, real64, OUTPUT_UNIT, INPUT_UNIT, ERROR_UNIT
!use omp_lib
!Suggestion from fortran-lang to remove strong dependency on openmp:
!https://fortran-lang.discourse.group/t/using-tikz-to-plot-for-fortran/8166/13?u=fish830911
!!$omp_lib
implicit none
private
! public :: tikz, meshgrid, groupplot, surf
! Add to the top of the module
public :: tikz, meshgrid, groupplot, surf, tikz_yy
public :: TikzBase, TikzPlot, TikzSurf


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

interface groupplot
    module procedure groupplot_start, groupplot_stop
end interface groupplot

interface surf
    module procedure surf_plot_xy2
end interface surf

interface tikz_yy
    module procedure tikz_plot_yy
    module procedure tikz_plot_yy_vec       ! x(:), y1(:), y2(:)
    module procedure tikz_plot_yy_mat_vec   ! x(:), y1(:,:), y2(:)
    module procedure tikz_plot_yy_vec_mat   ! x(:), y1(:), y2(:,:)
    module procedure tikz_plot_yy_auto      ! y1(:), y2(:) (Auto X generation)
end interface tikz_yy


! convert int32, int64, real32, real64 into string
interface num2str
    module procedure num2str_int32
    module procedure num2str_int64
    module procedure num2str_real32
    module procedure num2str_real64
end interface num2str

! 1. BASE CLASS (Parent)
type, public :: TikzBase
    ! Internal storage renamed with trailing underscore to allow method names
    character(len=:), allocatable :: filename_
    character(len=:), allocatable :: title_, xlabel_, ylabel_
contains
    procedure :: init => init_base
    ! Setter methods (bound to simple names)
    procedure :: filename => set_filename
    procedure :: title    => set_title
    procedure :: xlabel   => set_xlabel
    procedure :: ylabel   => set_ylabel
    procedure :: destroy  => destroy_obj
end type TikzBase

! 2. 2D PLOT CLASS (Child)
type, public, extends(TikzBase) :: TikzPlot
    ! Internals always stored as matrices (Rank 2)
    real(wp), allocatable :: x(:), y(:, :)
    real(wp), allocatable :: y2(:, :)

    character(len=:), allocatable :: legend_, color_, mark_
    character(len=:), allocatable :: ylabel_right_
    character(len=:), allocatable :: legend_pos_
    real(wp), allocatable :: xlim_(:)
    real(wp), allocatable :: ylim_(:)
    real(wp), allocatable :: y2lim_(:)
    logical :: is_dual = .false.

contains
    ! GENERIC INTERFACES: This is the magic that allows flexibility
    generic :: set_data => set_data_vec, set_data_mat
    generic :: set_dual_data => set_dual_data_vv, set_dual_data_mm

    ! Specific implementations (hidden from user)
    procedure, private :: set_data_vec
    procedure, private :: set_data_mat
    procedure, private :: set_dual_data_vv
    procedure, private :: set_dual_data_mm

    procedure :: set_style
    procedure :: save => render_2d

    ! Simple Setters
    procedure :: legend       => set_legend
    procedure :: legend_pos   => set_legend_pos_2d
    procedure :: color        => set_color
    procedure :: mark         => set_mark
    procedure :: ylabel_right => set_ylabel_right
    procedure :: y2label      => set_ylabel_right
    procedure :: xlim         => set_xlim
    procedure :: ylim         => set_ylim
    procedure :: y2lim         => set_y2lim
end type TikzPlot

type, public, extends(TikzBase) :: TikzSurf
    real(wp), allocatable :: x(:), y(:), z(:,:)
    character(len=:), allocatable :: zlabel_
    integer :: view_(2) = [ -999, -999 ]

    ! ADD THIS: Duplicate the storage here since it's not in Base
    character(len=:), allocatable :: legend_pos_
    ! NEW: Axis Limits [min, max]
    real(wp), allocatable :: xlim_(:)
    real(wp), allocatable :: ylim_(:)
    real(wp), allocatable :: zlim_(:) ! Unique to Surface
contains
    procedure :: set_data => set_3d_data
    procedure :: save => render_3d
    procedure :: zlabel => set_zlabel
    procedure :: set_view

    ! ADD THIS: Bind the 3D setter
    procedure :: legend_pos => set_legend_pos_3d
    procedure :: xlim => set_surf_xlim
    procedure :: ylim => set_surf_ylim
    procedure :: zlim => set_surf_zlim
end type TikzSurf


character(len=255) :: opener

logical :: isGroupPlot = .false.
integer :: row_group = 0, col_group = 0
integer :: fig_count, fig_max
character(len=:), allocatable :: groupName, groupPath, groupFName, groupType

!! the unit of main_memory in texmf.cnf is a "word"? 1 word is 4 bytes
real(wp) :: total_mem = 3000000.0_wp/4.0_wp

contains

! ==================================================================
! OOP IMPLEMENTATIONS
! ==================================================================

! --- Base Init ---
subroutine init_base(self, filename, title, xlabel, ylabel)
    class(TikzBase), intent(inout) :: self
    character(len=*), intent(in), optional :: filename
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: xlabel
    character(len=*), intent(in), optional :: ylabel

    ! 1. Handle Filename
    if (present(filename)) then
        self%filename_ = filename
    else
        self%filename_ = 'output.tex'
    end if

    ! 2. Handle Labels
    if (present(title))  self%title_  = title
    if (present(xlabel)) self%xlabel_ = xlabel
    if (present(ylabel)) self%ylabel_ = ylabel
end subroutine init_base

subroutine destroy_obj(self)
    class(TikzBase), intent(inout) :: self

    if (allocated(self%filename_)) deallocate(self%filename_)
    if (allocated(self%title_   )) deallocate(self%title_)
    if (allocated(self%xlabel_  )) deallocate(self%xlabel_)
    if (allocated(self%ylabel_  )) deallocate(self%ylabel_)

    select type (self)
    type is (TikzPlot)
        if (allocated(self%x))            deallocate(self%x)
        if (allocated(self%y))            deallocate(self%y)
        if (allocated(self%y2))           deallocate(self%y2)

        if (allocated(self%legend_))       deallocate(self%legend_)
        if (allocated(self%color_))        deallocate(self%color_)
        if (allocated(self%mark_))         deallocate(self%mark_)
        if (allocated(self%ylabel_right_)) deallocate(self%ylabel_right_)
        if (allocated(self%legend_pos_))   deallocate(self%legend_pos_)

        if (allocated(self%xlim_))         deallocate(self%xlim_)
        if (allocated(self%ylim_))         deallocate(self%ylim_)
        if (allocated(self%y2lim_))        deallocate(self%y2lim_)

        self%is_dual = .false.

    type is (TikzSurf)
        if (allocated(self%x))            deallocate(self%x)
        if (allocated(self%y))            deallocate(self%y)
        if (allocated(self%z))            deallocate(self%z)

        if (allocated(self%zlabel_))      deallocate(self%zlabel_)
        if (allocated(self%legend_pos_))  deallocate(self%legend_pos_)

        if (allocated(self%xlim_))        deallocate(self%xlim_)
        if (allocated(self%ylim_))        deallocate(self%ylim_)
        if (allocated(self%zlim_))        deallocate(self%zlim_)

        self%view_ = [ -999, -999 ]
    end select
end subroutine destroy_obj

! --- Base Setters ---
subroutine set_filename(self, val)
    class(TikzBase), intent(inout) :: self
    character(len=*), intent(in) :: val
    self%filename_ = val
end subroutine set_filename

subroutine set_title(self, val)
    class(TikzBase), intent(inout) :: self
    character(len=*), intent(in) :: val
    self%title_ = val
end subroutine set_title

subroutine set_xlabel(self, val)
    class(TikzBase), intent(inout) :: self
    character(len=*), intent(in) :: val
    self%xlabel_ = val
end subroutine set_xlabel

subroutine set_ylabel(self, val)
    class(TikzBase), intent(inout) :: self
    character(len=*), intent(in) :: val
    self%ylabel_ = val
end subroutine set_ylabel

! --- Set X Limits ---
subroutine set_xlim(self, min_val, max_val)
    class(TikzPlot), intent(inout) :: self
    real(wp), intent(in) :: min_val, max_val
    self%xlim_ = [min_val, max_val]
end subroutine set_xlim

! --- Set Y Limits ---
subroutine set_ylim(self, min_val, max_val)
    class(TikzPlot), intent(inout) :: self
    real(wp), intent(in) :: min_val, max_val
    self%ylim_ = [min_val, max_val]
end subroutine set_ylim

! --- Set Y2 Limits ---
subroutine set_y2lim(self, min_val, max_val)
    class(TikzPlot), intent(inout) :: self
    real(wp), intent(in) :: min_val, max_val
    self%y2lim_ = [min_val, max_val]
end subroutine set_y2lim

! ------------------------------------------------------------------
! SET DATA: Vector Wrapper (Flexible Input)
! ------------------------------------------------------------------
subroutine set_data_vec(self, x, y, legend)
    class(TikzPlot), intent(inout) :: self
    real(wp), intent(in) :: x(:), y(:) ! Input is Vector
    character(len=*), intent(in), optional :: legend

    self%x = x
    ! Convert Vector -> Matrix (Col Vector)
    self%y = reshape(y, [size(y), 1])

    if (present(legend)) self%legend_ = legend
    self%is_dual = .false.
end subroutine set_data_vec

! ------------------------------------------------------------------
! SET DATA: Matrix Implementation (Standard)
! ------------------------------------------------------------------
subroutine set_data_mat(self, x, y, legend)
    class(TikzPlot), intent(inout) :: self
    real(wp), intent(in) :: x(:), y(:, :) ! Input is Matrix
    character(len=*), intent(in), optional :: legend

    self%x = x
    self%y = y

    if (present(legend)) self%legend_ = legend
    self%is_dual = .false.
end subroutine set_data_mat

! ------------------------------------------------------------------
! DUAL DATA: Vector-Vector Wrapper
! ------------------------------------------------------------------
subroutine set_dual_data_vv(self, x, y_left, y_right)
    class(TikzPlot), intent(inout) :: self
    real(wp), intent(in) :: x(:)
    real(wp), intent(in) :: y_left(:), y_right(:) ! Vectors

    self%x = x
    self%y  = reshape(y_left,  [size(y_left),  1])
    self%y2 = reshape(y_right, [size(y_right), 1])

    self%is_dual = .true.
end subroutine set_dual_data_vv

! ------------------------------------------------------------------
! DUAL DATA: Matrix-Matrix Wrapper
! ------------------------------------------------------------------
subroutine set_dual_data_mm(self, x, y_left, y_right)
    class(TikzPlot), intent(inout) :: self
    real(wp), intent(in) :: x(:)
    real(wp), intent(in) :: y_left(:,:), y_right(:,:) ! Matrices

    self%x  = x
    self%y  = y_left
    self%y2 = y_right

    self%is_dual = .true.
end subroutine set_dual_data_mm

subroutine set_style(self, color, mark)
    class(TikzPlot), intent(inout) :: self
    character(len=*), intent(in), optional :: color, mark
    if (present(color)) self%color_ = color
    if (present(mark))  self%mark_  = mark
end subroutine set_style

! --- 2D Setters ---
subroutine set_legend(self, val)
    class(TikzPlot), intent(inout) :: self
    character(len=*), intent(in) :: val
    self%legend_ = val
end subroutine set_legend
! --- 2D Setter ---
subroutine set_legend_pos_2d(self, pos)
    class(TikzPlot), intent(inout) :: self
    character(len=*), intent(in) :: pos
    self%legend_pos_ = pos
end subroutine set_legend_pos_2d

! --- 3D Setter ---
subroutine set_legend_pos_3d(self, pos)
    class(TikzSurf), intent(inout) :: self
    character(len=*), intent(in) :: pos
    self%legend_pos_ = pos
end subroutine set_legend_pos_3d

subroutine set_color(self, val)
    class(TikzPlot), intent(inout) :: self
    character(len=*), intent(in) :: val
    self%color_ = val
end subroutine set_color

subroutine set_mark(self, val)
    class(TikzPlot), intent(inout) :: self
    character(len=*), intent(in) :: val
    self%mark_ = val
end subroutine set_mark

subroutine set_ylabel_right(self, val)
    class(TikzPlot), intent(inout) :: self
    character(len=*), intent(in) :: val
    self%ylabel_right_ = val
end subroutine set_ylabel_right

subroutine render_2d(self)
    class(TikzPlot), intent(inout) :: self

    ! Local variables for processing
    character(len=500) :: opts_str
    character(len=:), allocatable :: legends_array(:)
    character(len=500) :: leg1, leg2
    integer :: nL, nR, j
    logical :: y1_is_vec, y2_is_vec

    ! --- 1. Build Options String (e.g. Legend Position) ---
    opts_str = ""
    if (allocated(self%legend_pos_)) then
        opts_str = "legend: " // self%legend_pos_
    end if

    ! 2. X Limits
    if (allocated(self%xlim_)) then
        if (len_trim(opts_str) > 0) opts_str = trim(opts_str) // "; "
        opts_str = trim(opts_str) // "xmin: " // num2str(self%xlim_(1)) // &
                   "; xmax: " // num2str(self%xlim_(2))
    end if

    ! 3. Y Limits
    if (allocated(self%ylim_)) then
        if (len_trim(opts_str) > 0) opts_str = trim(opts_str) // "; "
        opts_str = trim(opts_str) // "ymin: " // num2str(self%ylim_(1)) // &
                   "; ymax: " // num2str(self%ylim_(2))
    end if

    ! 4. Y2 Limits
    if (allocated(self%y2lim_)) then
        if (len_trim(opts_str) > 0) opts_str = trim(opts_str) // "; "
        opts_str = trim(opts_str) // "y2min: " // num2str(self%y2lim_(1)) // &
                   "; y2max: " // num2str(self%y2lim_(2))
    end if


    ! --- 2. Process Legends (Split "A;B" -> "A", "B") ---
    leg1 = ""; leg2 = ""
    if (allocated(self%legend_)) then

        if (self%is_dual) then
            ! Dual-axis: allow either
            !   (i)  explicit split by '|'   ->  "L1;L2 | R1;R2"
            !   (ii) flat list by ';'        ->  first nL labels for left axis, remaining nR for right axis
            nL = size(self%y,  2)
            nR = size(self%y2, 2)

            if (index(self%legend_, '|') > 0) then
                legends_array = decompose_str(self%legend_, genLoc(self%legend_, '|'))
                if (size(legends_array) >= 1) leg1 = legends_array(1)
                if (size(legends_array) >= 2) leg2 = legends_array(2)

            else
                legends_array = decompose_str(self%legend_, genLoc(self%legend_, ';'))

                if (size(legends_array) == 1) then
                    leg1 = legends_array(1)

                else if (size(legends_array) == 2 .and. (nL == 1 .and. nR == 1)) then
                    ! Backward-compatible: 1 line on each axis
                    leg1 = legends_array(1)
                    leg2 = legends_array(2)

                else
                    ! Pack first nL entries into label1
                    leg1 = ""
                    do j = 1, min(nL, size(legends_array))
                        if (j > 1) leg1 = trim(leg1) // "; "
                        leg1 = trim(leg1) // trim(legends_array(j))
                    end do

                    ! Pack next nR entries into label2
                    leg2 = ""
                    do j = nL + 1, min(nL + nR, size(legends_array))
                        if (j > nL + 1) leg2 = trim(leg2) // "; "
                        leg2 = trim(leg2) // trim(legends_array(j))
                    end do
                end if
            end if

        else
            ! Single axis: legend string is handled directly in tikz_plot_xy2
        end if
    end if


    ! --- 3. Render Logic ---
    if (self%is_dual) then
        ! Check if stored matrices are actually vectors (1 column)
        y1_is_vec = (size(self%y,  2) == 1)
        y2_is_vec = (size(self%y2, 2) == 1)

        ! Logic: If it's a vector (N,1), pass slice (:,1) to match procedural interface
        if (y1_is_vec .and. y2_is_vec) then
            ! Case: Vec - Vec
            call tikz_yy(self%x, self%y(:,1), self%y2(:,1), &
                         name    = self%filename_, &
                         title   = self%title_, &
                         xlabel  = self%xlabel_, &
                         ylabel1 = self%ylabel_, &
                         ylabel2 = self%ylabel_right_, &
                         label1  = trim(leg1), &
                         label2  = trim(leg2), &
                         options = trim(opts_str))

        else if (.not. y1_is_vec .and. y2_is_vec) then
            ! Case: Mat - Vec
            call tikz_yy(self%x, self%y, self%y2(:,1), &
                         name    = self%filename_, &
                         title   = self%title_, &
                         xlabel  = self%xlabel_, &
                         ylabel1 = self%ylabel_, &
                         ylabel2 = self%ylabel_right_, &
                         label1  = trim(leg1), &
                         label2  = trim(leg2), &
                         options = trim(opts_str))

        else if (y1_is_vec .and. .not. y2_is_vec) then
             ! Case: Vec - Mat
             call tikz_yy(self%x, self%y(:,1), self%y2, &
                         name    = self%filename_, &
                         title   = self%title_, &
                         xlabel  = self%xlabel_, &
                         ylabel1 = self%ylabel_, &
                         ylabel2 = self%ylabel_right_, &
                         label1  = trim(leg1), &
                         label2  = trim(leg2), &
                         options = trim(opts_str))
        else
             ! Case: Mat - Mat (Requires generic tikz_yy support for this)
             call tikz_yy(self%x, self%y, self%y2, &
                         name    = self%filename_, &
                         title   = self%title_, &
                         xlabel  = self%xlabel_, &
                         ylabel1 = self%ylabel_, &
                         ylabel2 = self%ylabel_right_, &
                         label1  = trim(leg1), &
                         label2  = trim(leg2), &
                         options = trim(opts_str))
        end if

    else
        ! Single Axis Plot (Standard)
        call tikz_plot_xy2(self%x, self%y, &
                  name    = self%filename_, &
                  title   = self%title_, &
                  xlabel  = self%xlabel_, &
                  ylabel  = self%ylabel_, &
                  legend  = self%legend_, &
                  options = trim(opts_str))
    end if

end subroutine render_2d


! --- 3D Methods ---
subroutine set_3d_data(self, x, y, z, zlabel)
    class(TikzSurf), intent(inout) :: self
    real(wp), intent(in) :: x(:), y(:), z(:,:)
    character(len=*), intent(in), optional :: zlabel

    self%x = x
    self%y = y
    self%z = z
    if (present(zlabel)) self%zlabel_ = zlabel
end subroutine set_3d_data

! --- 3D Setters ---
subroutine set_zlabel(self, val)
    class(TikzSurf), intent(inout) :: self
    character(len=*), intent(in) :: val
    self%zlabel_ = val
end subroutine set_zlabel

subroutine set_view(self, view_angle)
    class(TikzSurf), intent(inout) :: self
    integer, intent(in) :: view_angle(2)
    self%view_ = view_angle
end subroutine set_view

! --- Set Surface X Limits ---
subroutine set_surf_xlim(self, min_val, max_val)
    class(TikzSurf), intent(inout) :: self
    real(wp), intent(in) :: min_val, max_val
    self%xlim_ = [min_val, max_val]
end subroutine set_surf_xlim

! --- Set Surface Y Limits ---
subroutine set_surf_ylim(self, min_val, max_val)
    class(TikzSurf), intent(inout) :: self
    real(wp), intent(in) :: min_val, max_val
    self%ylim_ = [min_val, max_val]
end subroutine set_surf_ylim

! --- Set Surface Z Limits ---
subroutine set_surf_zlim(self, min_val, max_val)
    class(TikzSurf), intent(inout) :: self
    real(wp), intent(in) :: min_val, max_val
    self%zlim_ = [min_val, max_val]
end subroutine set_surf_zlim

subroutine render_3d(self)
    class(TikzSurf), intent(inout) :: self
    character(len=500) :: opts_str

    ! --- 1. Build Options String ---
    opts_str = ""
    if (allocated(self%legend_pos_)) then
        opts_str = "legend: " // self%legend_pos_
    end if

    ! 2. X Limits
    if (allocated(self%xlim_)) then
        if (len_trim(opts_str) > 0) opts_str = trim(opts_str) // "; "
        opts_str = trim(opts_str) // "xmin: " // num2str(self%xlim_(1)) // &
                   "; xmax: " // num2str(self%xlim_(2))
    end if

    ! 3. Y Limits
    if (allocated(self%ylim_)) then
        if (len_trim(opts_str) > 0) opts_str = trim(opts_str) // "; "
        opts_str = trim(opts_str) // "ymin: " // num2str(self%ylim_(1)) // &
                   "; ymax: " // num2str(self%ylim_(2))
    end if

    ! 4. Z Limits (New!)
    if (allocated(self%zlim_)) then
        if (len_trim(opts_str) > 0) opts_str = trim(opts_str) // "; "
        opts_str = trim(opts_str) // "zmin: " // num2str(self%zlim_(1)) // &
                   "; zmax: " // num2str(self%zlim_(2))
    end if

    ! --- 2. Check View Settings ---
    if (self%view_(1) /= -999) then
        ! Call WITH view
        call surf_plot_xy2(self%x, self%y, self%z, &
                  name    = self%filename_, &
                  title   = self%title_, &
                  xlabel  = self%xlabel_, &
                  ylabel  = self%ylabel_, &
                  zlabel  = self%zlabel_, &
                  view    = self%view_, &
                  options = trim(opts_str))
    else
        ! Call WITHOUT view (Defaults)
        call surf_plot_xy2(self%x, self%y, self%z, &
                  name    = self%filename_, &
                  title   = self%title_, &
                  xlabel  = self%xlabel_, &
                  ylabel  = self%ylabel_, &
                  zlabel  = self%zlabel_, &
                  options = trim(opts_str))
    end if

end subroutine render_3d

subroutine groupplot_start(row, col)
    integer, intent(in) :: row, col

    row_group = row
    col_group = col
    fig_count = 1
    fig_max = row_group*col_group
    isGroupPlot = .true.
end subroutine groupplot_start

subroutine groupplot_stop
    row_group = 0
    col_group = 0
    fig_count = 1
    fig_max = 1
    isGroupPlot = .false.
end subroutine groupplot_stop

! ----------------------------------------------------------------------
! Wrapper: Takes 1D y1 and 1D y2, reshapes to matrices, calls base
! ----------------------------------------------------------------------
subroutine tikz_plot_yy_vec(x, y1, y2, title, xlabel, ylabel1, ylabel2, &
                            label1, label2, name, options)
    real(wp), intent(in) :: x(:), y1(:), y2(:)
    character(len=*), intent(in), optional :: title, xlabel, ylabel1, ylabel2
    character(len=*), intent(in), optional :: label1, label2
    character(len=*), intent(in), optional :: name, options

    ! Reshape 1D arrays to (N,1) matrices
    call tikz_plot_yy( &
        x, &
        reshape(y1, [size(y1), 1]), &
        reshape(y2, [size(y2), 1]), &
        title, xlabel, ylabel1, ylabel2, &
        label1, label2, name, options)

end subroutine tikz_plot_yy_vec

! ----------------------------------------------------------------------
! Wrapper: Takes Matrix y1 and 1D y2
! ----------------------------------------------------------------------
subroutine tikz_plot_yy_mat_vec(x, y1, y2, title, xlabel, ylabel1, ylabel2, &
                                label1, label2, name, options)
    real(wp), intent(in) :: x(:), y1(:,:), y2(:)
    character(len=*), intent(in), optional :: title, xlabel, ylabel1, ylabel2
    character(len=*), intent(in), optional :: label1, label2
    character(len=*), intent(in), optional :: name, options

    call tikz_plot_yy( &
        x, &
        y1, &
        reshape(y2, [size(y2), 1]), &
        title, xlabel, ylabel1, ylabel2, &
        label1, label2, name, options)

end subroutine tikz_plot_yy_mat_vec

! ----------------------------------------------------------------------
! Wrapper: Takes 1D y1 and Matrix y2
! ----------------------------------------------------------------------
subroutine tikz_plot_yy_vec_mat(x, y1, y2, title, xlabel, ylabel1, ylabel2, &
                                label1, label2, name, options)
    real(wp), intent(in) :: x(:), y1(:), y2(:,:)
    character(len=*), intent(in), optional :: title, xlabel, ylabel1, ylabel2
    character(len=*), intent(in), optional :: label1, label2
    character(len=*), intent(in), optional :: name, options

    call tikz_plot_yy( &
        x, &
        reshape(y1, [size(y1), 1]), &
        y2, &
        title, xlabel, ylabel1, ylabel2, &
        label1, label2, name, options)

end subroutine tikz_plot_yy_vec_mat

! ----------------------------------------------------------------------
! Wrapper: Takes y1 and y2 only (No X). Generates X = 1..N
! Matches functionality of 'tikz_plot_y'
! ----------------------------------------------------------------------
subroutine tikz_plot_yy_auto(y1, y2, title, xlabel, ylabel1, ylabel2, &
                             label1, label2, name, options)
    real(wp), intent(in) :: y1(:), y2(:)
    character(len=*), intent(in), optional :: title, xlabel, ylabel1, ylabel2
    character(len=*), intent(in), optional :: label1, label2
    character(len=*), intent(in), optional :: name, options

    real(wp), allocatable :: xgen(:)
    integer :: n, i

    n = size(y1)
    allocate(xgen(n))

    ! Generate X axis: 1.0, 2.0, ... N.0
    do i = 1, n
        xgen(i) = real(i, wp)
    end do

    call tikz_plot_yy( &
        xgen, &
        reshape(y1, [n, 1]), &
        reshape(y2, [size(y2), 1]), &
        title, xlabel, ylabel1, ylabel2, &
        label1, label2, name, options)

end subroutine tikz_plot_yy_auto


subroutine tikz_plot_yy(x, y1, y2, title, xlabel, ylabel1, ylabel2, &
                        label1, label2, name, options)
    ! Arguments
    real(wp), intent(in) :: x(:), y1(:,:), y2(:,:)
    character(len=*), intent(in), optional :: title, xlabel, ylabel1, ylabel2
    character(len=*), intent(in), optional :: label1, label2
    character(len=*), intent(in), optional :: name
    character(len=*), intent(in), optional :: options

    ! Local variables
    integer :: i, j, n, m1, m2, k, tot_lines, idx
    integer :: unitno, unitdat, thread_id
    character(len=5) :: tmpChar5
    character(len=:), allocatable :: fname, fpath, dat, tikz
    character(len=3) :: ftype
    character(len=:), allocatable :: xl, yl1, yl2, ti
    logical :: isTmp

    ! Labels variables
    character(len=100), dimension(:), allocatable :: l1_vec, l2_vec

    ! Boundary variables
    real(wp) :: xmin, xmax, ymin, ymax, y2min, y2max

    ! Palette and Style variables
    character(len=:), allocatable :: palette, cnames, styleset, markerset
    character(len=:), dimension(:), allocatable :: colorvec, colorname, stylevec, markervec

    ! Option Parsing Variables
    character(len=:), dimension(:), allocatable :: optionvec, optmp
    character(len=100), dimension(:), allocatable :: oplegend, opcolor, oplinestyle, opmarker
    character(len=:), allocatable :: legend_type, legend_pos

    logical :: set_xmin, set_xmax
    logical :: set_ymin, set_ymax
    logical :: set_y2min, set_y2max

    set_xmin=.false.
    set_xmax=.false.
    set_ymin=.false.
    set_ymax=.false.
    set_y2min=.false.
    set_y2max=.false.

    ! ---------------------- !
    ! Check inputs dimension !
    ! ---------------------- !
    n = size(x)
    m1 = size(y1, 2)
    m2 = size(y2, 2)
    tot_lines = m1 + m2

    if (size(y1, 1) /= n) error stop "plot_yy error: y1 row dimension does not match x"
    if (size(y2, 1) /= n) error stop "plot_yy error: y2 row dimension does not match x"

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
                    ! if (size(opcolor) .ne. nj) then
                    !     error stop "options error: numbers of color do not equal to numbers of columns on y"
                    ! endif
                case ('c') ! shorthand for color
                    opcolor = decompose_str(optmp(2),  genLoc(optmp(2), ','))
                    ! if (size(opcolor) .ne. nj) then
                    !     error stop "options error: numbers of color do not equal to numbers of columns on y"
                    ! endif
                case ('linestyle')
                    oplinestyle = decompose_str(optmp(2),  genLoc(optmp(2), ','))
                    ! if (size(oplinestyle) .ne. nj) then
                    !     error stop "options error: numbers of line style do not equal to numbers of columns on y"
                    ! endif
                case ('ls') ! shorthand for linestyle
                    oplinestyle = decompose_str(optmp(2),  genLoc(optmp(2), ','))
                    ! if (size(oplinestyle) .ne. nj) then
                    !     error stop "options error: numbers of line style do not equal to numbers of columns on y"
                    ! endif
                case ('marker')
                    opmarker = decompose_str(optmp(2),  genLoc(optmp(2), ','))
                case ('xmin')
                    read(optmp(2), *) xmin
                    set_xmin = .true.
                case ('xmax')
                    read(optmp(2), *) xmax
                    set_xmax = .true.
                case ('ymin')
                    read(optmp(2), *) ymin
                    set_ymin = .true.
                case ('ymax')
                    read(optmp(2), *) ymax
                    set_ymax = .true.
                case ('y2min')
                    read(optmp(2), *) y2min
                    set_y2min = .true.
                case ('y2max')
                    read(optmp(2), *) y2max
                    set_y2max = .true.
            end select
            deallocate(optmp)
       enddo
    endif

    ! ----------------------- !
    ! Calculate Boundaries    !
    ! ----------------------- !

    if (.not. set_xmin) xmin = minval(x)
    if (.not. set_xmax) xmax = maxval(x)

    if (.not. set_ymin) ymin = minval(y1)
    if (.not. set_ymax) ymax = maxval(y1) + 0.1_wp*maxval(y1)

    if (.not. set_y2min) y2min = minval(y2)
    if (.not. set_y2max) y2max = maxval(y2) + 0.3_wp*maxval(y2)


    ! ------------------ !
    ! Set default values !
    ! ------------------ !
    if (present(xlabel)) then; xl = xlabel; else; xl = '$X$'; endif
    if (present(ylabel1)) then; yl1 = ylabel1; else; yl1 = 'Data 1'; endif
    if (present(ylabel2)) then; yl2 = ylabel2; else; yl2 = 'Data 2'; endif
    if (present(title)) then; ti = title; else; ti = ''; endif

    ! Process Label 1 (Left Axis Legends)
    if (present(label1) .and. len_trim(label1) > 0) then
        l1_vec = decompose_str(label1, genLoc(label1, ';'))
        ! Handle mismatch if user provides fewer labels than columns
        if (size(l1_vec) < m1) then
            ! Ideally, resize or handle error. Here we assume user matches or we truncate logic later
        endif
    else
        allocate(l1_vec(m1))
        do j = 1, m1
            l1_vec(j) = 'Y1-' // trim(num2str(j))
        enddo
    endif

    ! Process Label 2 (Right Axis Legends)
    if (present(label2) .and. len_trim(label2) > 0) then
        l2_vec = decompose_str(label2, genLoc(label2, ';'))
    else
        allocate(l2_vec(m2))
        do j = 1, m2
            l2_vec(j) = 'Y2-' // trim(num2str(j))
        enddo
    endif

    ! ------------------------------------------ !
    ! Define Standard Palette & Styles           !
    ! ------------------------------------------ !
    palette = '0072BD; D95319; EDB120; 7E2F8E; 77AC30; 4DBEEE; A2142F'
    cnames = "blue; orange; yellow; purple; green; light-blue; red"
    colorvec = decompose_str(palette, genLoc(palette, ';'))
    colorname = decompose_str(cnames, genLoc(cnames, ';'))

    styleset = 'solid; dashdotdotted; densely dashdotdotted; densely dotted; densely dashed; dotted; dashed'
    stylevec = decompose_str(styleset, genLoc(styleset, ';'))

    markerset = 'o; square; pentagon; triangle; diamond; asterisk'
    markervec = decompose_str(markerset, genLoc(markerset, ';'))

    ! ----------------------- !
    ! Initialize Styles       !
    ! ----------------------- !
    ! We allocate these for TOTAL lines (m1 + m2)
    allocate(oplinestyle(tot_lines)); oplinestyle(:) = ''
    allocate(opmarker(tot_lines)); opmarker(:) = ''
    allocate(opcolor(tot_lines)); opcolor(:) = ''
    legend_pos = 'north east'
    legend_type = 'box'

    if (allocated(oplegend)) then
        do j = 1, size(oplegend), 1
            if (trim(oplegend(j)) == 'line' .or. trim(oplegend(j)) == 'box') then
                legend_type = oplegend(j)
            else
                legend_pos = oplegend(j)
            endif
        enddo
    endif

    ! Set default colors (cycling through palette)
    do j = 1, tot_lines
        idx = mod(j, size(colorname))
        idx = merge(size(colorname), idx, idx == 0)
        opcolor(j) = colorname(idx)
    enddo

    ! Set default linestyles
    do j = 1, tot_lines
        idx = mod(j, size(stylevec))
        idx = merge(size(stylevec), idx, idx == 0)
        oplinestyle(j) = stylevec(idx)
    enddo

    ! ------------------------------------- !
    ! thread safe external file unit number !
    ! ------------------------------------- !
    thread_id = 0
    write(tmpChar5,'(i0)') thread_id
    unitno = 726 + thread_id
    unitdat = 412 + thread_id

    ! ------------------- !
    ! decompose file name !
    ! ------------------- !
    if (present(name)) then
        call decompose_name(fname, fpath, ftype, name)
        dat = trim(fname) // '.dat'
        tikz = trim(fname) // '.tex'
        isTmp = .false.
    else
        ! (Assumed grouping logic same as original)
        fpath = './'
        fname = 'tikzplot' // trim(tmpChar5)
        ftype = ''
        dat = 'tikzdata' // trim(tmpChar5) // '_yy.dat'
        tikz = 'tikzplot' // trim(tmpChar5) // '.tex'
        isTmp = .true.
    endif

    call set_opener()

    ! ------------------ !
    ! creating data file !
    ! ------------------ !
    ! Structure: Col 0: X, Col 1..m1: Y1, Col m1+1..m1+m2: Y2
    open(unitdat, file=trim(fpath)//trim(dat), status='unknown')
    do i = 1, n
        ! Write X
        write(unitdat, '(g0.6)', advance='no') x(i)

        ! Write Y1 Columns
        do j = 1, m1
            write(unitdat, '(1x, g0.6)', advance='no') y1(i,j)
        end do

        ! Write Y2 Columns
        do j = 1, m2
            write(unitdat, '(1x, g0.6)', advance='no') y2(i,j)
        end do

        write(unitdat, *) ! Newline
    end do
    close(unitdat)

    ! ------------------- !
    ! writing TeX content !
    ! ------------------- !

    call write_preamble(colorvec, colorname, unitno, fpath, tikz)

    ! --- AXIS 1 (Left Y-Axis) ---
    write(unitno, '(a)') "\begin{axis}["
    write(unitno, '(a)') "    sciclean,"
    write(unitno, '(a)') "    scale only axis,"
    write(unitno, '(a)') "    axis y line*=left,"
    write(unitno, '(a)') "    tick pos=left,"

    ! Dashed top border
    ! write(unitno, '(a)') "    after end axis/.code={\draw[dashed, gray!30] (rel axis cs:0,1) -- (rel axis cs:1,1);},"

    ! Apply Boundaries (Axis 1)
    write(unitno, '(a,a,a)') "    xmin=", trim(num2str(xmin)), ","
    write(unitno, '(a,a,a)') "    xmax=", trim(num2str(xmax)), ","
    write(unitno, '(a,a,a)') "    ymin=", trim(num2str(ymin)), ","
    write(unitno, '(a,a,a)') "    ymax=", trim(num2str(ymax)), ","

    ! Legend Styling
    write(unitno, '(a,a)')     "    legend pos=", trim(legend_pos) // ","
    write(unitno, '(a)')       "    legend style={legend cell align=left, align=left, draw=white!15!black},"

    ! Axis Styling (Color based on first line of Y1 or black? Usually left axis takes color of first Y1)
    write(unitno, '(a,a,a)') "    y axis line style={", trim(opcolor(1)), "},"
    write(unitno, '(a,a,a)') "    yticklabel style={", trim(opcolor(1)), "},"
    write(unitno, '(a,a,a)') "    ylabel style={", trim(opcolor(1)), "},"
    write(unitno, '(a,a,a)') "    xlabel={", trim(xl), "},"
    write(unitno, '(a,a,a)') "    ylabel={", trim(yl1), "},"
    if (len_trim(ti) > 0) write(unitno, '(a,a,a)') "    title={", trim(ti), "},"
    write(unitno, '(a)') "]"

    ! --- PLOT LINES FOR Y1 (Left Axis) ---
    ! Indices 1 to m1 in our color/style arrays
    do j = 1, m1
        ! Check column index. TikZ table uses 0-based or 1-based?
        ! Typically 'x index=0'. Data file: X is col 0 (in PGFPlots speak if header=false, usually cols are 0,1,2...).
        ! Fortran write puts spaces. PGFPlots read:
        ! Col 0: x. Col 1: y1(1). Col j: y1(j).

        write(unitno, '(a)', advance='no') "    \addplot[thick, " // trim(opcolor(j))
        if (len_trim(oplinestyle(j)) > 0) write(unitno, '(a)', advance='no') "," // trim(oplinestyle(j))
        if (len_trim(opmarker(j)) > 0)    write(unitno, '(a)', advance='no') ",mark=" // trim(opmarker(j))

        ! x index = 0, y index = j
        write(unitno, '(a,i0,a,a,a)') "] table [x index=0, y index=", j, "] {", trim(dat), "};"

        ! Legend Entry
        if (j <= size(l1_vec)) then
             write(unitno, '(a,a,a)') "    \addlegendentry{", trim(l1_vec(j)), "}"
        endif
    enddo

    ! --- FAKE LEGEND ENTRIES FOR Y2 (Right Axis) ---
    ! We add \addlegendimage so they appear in the Left Axis legend box
    ! Indices m1+1 to m1+m2
    do j = 1, m2
        idx = m1 + j
        write(unitno, '(a)', advance='no') "    \addlegendimage{thick, " // trim(opcolor(idx))
        if (len_trim(oplinestyle(idx)) > 0) write(unitno, '(a)', advance='no') "," // trim(oplinestyle(idx))
        if (len_trim(opmarker(idx)) > 0)    write(unitno, '(a)', advance='no') ",mark=" // trim(opmarker(idx))
        write(unitno, '(a)') "}"

        if (j <= size(l2_vec)) then
            write(unitno, '(a,a,a)') "    \addlegendentry{", trim(l2_vec(j)), "}"
        endif
    enddo

    write(unitno, '(a)') "\end{axis}"

    ! --- AXIS 2 (Right Y-Axis) ---
    write(unitno, '(a)') "\begin{axis}["
    write(unitno, '(a)') "    sciclean,"
    write(unitno, '(a)') "    scale only axis,"
    write(unitno, '(a)') "    axis y line*=right,"
    write(unitno, '(a)') "    axis x line=none,"
    write(unitno, '(a)') "    grid=none,"
    write(unitno, '(a)') "    axis background/.style={fill=none},"
    write(unitno, '(a)') "    after end axis/.code={},"

    ! Apply Boundaries (Axis 2)
    write(unitno, '(a,a,a)') "    xmin=", trim(num2str(xmin)), ","
    write(unitno, '(a,a,a)') "    xmax=", trim(num2str(xmax)), ","
    write(unitno, '(a,a,a)') "    ymin=", trim(num2str(y2min)), ","
    write(unitno, '(a,a,a)') "    ymax=", trim(num2str(y2max)), ","

    ! Use color of the first line of Y2 for axis styling
    write(unitno, '(a,a,a)') "    y axis line style={", trim(opcolor(m1+1)), "},"
    write(unitno, '(a,a,a)') "    yticklabel style={", trim(opcolor(m1+1)), "},"
    write(unitno, '(a,a,a)') "    ylabel style={", trim(opcolor(m1+1)), "},"
    write(unitno, '(a,a,a)') "    ylabel={", trim(yl2), "},"
    write(unitno, '(a)') "]"

    ! --- PLOT LINES FOR Y2 (Right Axis) ---
    do j = 1, m2
        idx = m1 + j
        ! y index in file is m1 + j

        write(unitno, '(a)', advance='no') "    \addplot[thick, " // trim(opcolor(idx))
        if (len_trim(oplinestyle(idx)) > 0) write(unitno, '(a)', advance='no') "," // trim(oplinestyle(idx))
        if (len_trim(opmarker(idx)) > 0)    write(unitno, '(a)', advance='no') ",mark=" // trim(opmarker(idx))

        write(unitno, '(a,i0,a,a,a)') "] table [x index=0, y index=", idx, "] {", trim(dat), "};"
    enddo

    write(unitno, '(a)') "\end{axis}"

    call write_finale(unitno)

    ! ------------------- !
    ! Compile and Execute !
    ! ------------------- !

    call typeset(fpath, fname, tikz, .true.)

    if (isTmp) then
        call execute_command_line(trim(opener) // ' ' // trim(fpath) // trim(fname) // '.pdf > /dev/null 2>&1;')
    else
        call execute_command_line(trim(opener) // ' ' // trim(fpath) // trim(fname) // '.pdf > /dev/null 2>&1 &')
    endif

end subroutine tikz_plot_yy


subroutine surf_plot_xy2(x, y, z, title, xlabel, ylabel, zlabel, colormap, view, name, options)
    !input
    real(wp), intent(in) :: x(:), y(:), z(:, :)
    integer, optional :: view(2)
    character(len=*), intent(in), optional :: title, xlabel, ylabel, zlabel, colormap, name, options

    !local
    integer :: i, j
    integer :: unitno                                               ! thread save unit number
    integer :: unitdat                                              ! thread save unit number
    integer :: thread_id
    integer :: nx, ny, nz1, nz2                                     ! dimension of x, y, and z
    character(len=5) :: tmpChar5                                    ! tmp file name based on thread
    character(len=:), allocatable :: xlb                            ! local x label
    character(len=:), allocatable :: ylb                            ! local y label
    character(len=:), allocatable :: zlb                            ! local z label
    character(len=:), allocatable :: le                             ! local legend
    character(len=:), allocatable :: fname                          ! name of the tex file
    character(len=:), allocatable :: fpath                          ! path of the tex file
    character(len=:), allocatable :: cmp                            ! colormap
    integer                       :: vview(2)                       ! view
    character(len=3) :: ftype                                       ! file type
    character(len=:), allocatable :: dat                            ! data file
    character(len=:), allocatable :: tikz                           ! plotting tex file
    real(wp) :: xmin, xmax, ymin, ymax, zmin, zmax                  ! boundary of the figure
    character(len=:), allocatable :: palette                        ! color palette
    character(len=:), allocatable :: cnames                         ! names of the color palette
    character(len=:), allocatable :: styleset                       ! line style sets
    character(len=:), allocatable :: markerset                      ! marker style sets
    character(len=:), dimension(:), allocatable :: colorvec, colorname, optionvec, stylevec
    character(len=:), dimension(:), allocatable :: markervec
    character(len=:), dimension(:), allocatable :: optmp
    character(len=100), dimension(:), allocatable :: legendvec, opcolor, oplinestyle, oplegend
    character(len=100), dimension(:), allocatable :: opmarker
    character(len=:), allocatable :: legend_type, legend_pos
    logical :: isTmp

    real(wp), dimension(:, :), allocatable :: xmesh, ymesh, data

    ! 1. Add Flags to track if user set them
    logical :: set_xmin, set_xmax
    logical :: set_ymin, set_ymax
    logical :: set_zmin, set_zmax

    set_xmin=.false.
    set_xmax=.false.
    set_ymin=.false.
    set_ymax=.false.
    set_zmin=.false.
    set_zmax=.false.


    nx = size(x)
    ny = size(y)
    nz1 = size(z, 1)
    nz2 = size(z, 2)

    !if (nx == nz2 .and. ny == nz1) error stop 'switch the grid of x and y'

    !if (nz1*nz2*8.0_wp*3.0_wp > total_mem) error stop 'the size of z is too big'

    allocate(xmesh(nx, ny), source = 0.0_wp)
    allocate(ymesh(nx, ny), source = 0.0_wp)
    allocate(data(nx*ny, 3), source = 0.0_wp)

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

    if (present(zlabel)) then
        zlb = zlabel
    else
        zlb = '$z$'
    endif

    if (present(colormap)) then
        cmp = colormap
    else
        !cmp = "viridis"
        cmp = "jet"
    endif

    if (present(view)) then
        vview = view
    else
        vview = [30, 50]
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

    markerset = 'o; ' // &
                'square;' // &
                'pentagon;' // &
                'square;' // &
                'triangle;' // &
                'diamond;' // &
                'pentagon;' // &
                'asterisk'

    markervec = decompose_str(markerset, genLoc(markerset, ';'))

    ! ------------------------------------- !
    ! thread safe external file unit number !
    ! ------------------------------------- !

    !Suggestion from fortran-lang to remove strong dependency on openmp:
    !https://fortran-lang.discourse.group/t/using-tikz-to-plot-for-fortran/8166/13?u=fish830911
    thread_id = 0
    !$thread_id = omp_get_thread_num()
    write(tmpChar5,'(i0)') thread_id
    unitno = 726+thread_id
    unitdat = 412+thread_id

    ! ------------------- !
    ! decompose file name !
    ! ------------------- !

    if (present(name)) then
        call decompose_name(fname, fpath, ftype, name)
        dat = trim(fname) // '.dat'
        tikz = trim(fname) // '.tex'
        isTmp = .false.
    else
        if (isGroupPlot) then
            fpath = './'
            fname = 'tikzplot' // trim(tmpChar5)
            ftype = ''
            dat = 'tikzdata' // trim(tmpChar5) // num2str(fig_count) // '.dat'
            tikz = 'tikzplot' // trim(tmpChar5) // '.tex'
            isTmp = .true.
        else
            fpath = './'
            fname = 'tikzplot' // trim(tmpChar5)
            ftype = ''
            dat = 'tikzdata' // trim(tmpChar5) // '.dat'
            tikz = 'tikzplot' // trim(tmpChar5) // '.tex'
            isTmp = .true.
        endif
    endif

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
                    opmarker = decompose_str(optmp(2),  genLoc(optmp(2), ','))
                case ('xmin')
                    read(optmp(2), *) xmin
                    set_xmin = .true.
                case ('xmax')
                    read(optmp(2), *) xmax
                    set_xmax = .true.
                case ('ymin')
                    read(optmp(2), *) ymin
                    set_ymin = .true.
                case ('ymax')
                    read(optmp(2), *) ymax
                    set_ymax = .true.
                case ('zmin')
                    read(optmp(2), *) zmin
                    set_zmin = .true.
                case ('zmax')
                    read(optmp(2), *) zmax
                    set_zmax = .true.
            end select
            deallocate(optmp)
       enddo
    endif

    call meshgrid(ymesh, xmesh, y, x)

    data(:, 1) = reshape(xmesh, [nx*ny])
    data(:, 2) = reshape(ymesh, [nx*ny])
    data(:, 3) = reshape(z, [nx*ny])

    ! ------------------ !
    ! creating data file !
    ! ------------------ !

    call write_surf_dat(data, unitdat, nx, fpath, dat)

    ! xmin = minval(x)
    ! xmax = maxval(x)
    ! ymin = minval(y)
    ! ymax = maxval(y) + 0.1_wp*maxval(y)

    if (.not. set_xmin) xmin = minval(x)
    if (.not. set_xmax) xmax = maxval(x)

    if (.not. set_ymin) ymin = minval(y)
    if (.not. set_ymax) ymax = maxval(y)

    if (.not. set_zmin) zmin = minval(z)
    if (.not. set_zmax) zmax = maxval(z) + 0.1_wp*maxval(z)


    if (isGroupPlot) then
        if (fig_count .eq. 1) then
            groupName = tikz
            groupPath = fpath
            groupFName = fname
            groupType = ftype
            write(*, *) 'Groupplot names: ', trim(fpath) // trim(fname)
            call write_preamble(colorvec, colorname, unitno, fpath, tikz)
            call multi_plot_start(unitno)
        endif
        call surf_multi_plot(unitno, fpath, tikz, dat, xmin, xmax, ymin, ymax, &
                vview, cmp, colorname, title, xlb, ylb, legendvec, legend_type, legend_pos, &
                opcolor, oplinestyle, opmarker)
        if (fig_count < fig_max) then
            fig_count = fig_count + 1
            return
        else
            call multi_plot_end(unitno)
        endif
    else
        call write_preamble(colorvec, colorname, unitno, fpath, tikz)
        call surf_single_plot(unitno, fpath, tikz, dat, xmin, xmax, ymin, ymax, &
                vview, cmp, colorname, title, xlb, ylb, legendvec, legend_type, legend_pos, &
                opcolor, oplinestyle, opmarker)
    endif


    call write_finale(unitno)

    if (isGroupPlot) then
        call typeset(groupPath, groupFName, groupName, .true.)
        if (isTmp) then
            call execute_command_line(trim(opener) // ' ' // trim(groupPath) // trim(groupFName) // '.pdf > /dev/null 2>&1;')
        else
            call execute_command_line(trim(opener) // ' ' // trim(groupPath) // trim(groupFName) // '.pdf > /dev/null 2>&1 &')
        endif
    else
        call typeset(fpath, fname, tikz, .true.)
        if (isTmp) then
            call execute_command_line(trim(opener) // ' ' // trim(fpath) // trim(fname) // '.pdf > /dev/null 2>&1;')
        else
            call execute_command_line(trim(opener) // ' ' // trim(fpath) // trim(fname) // '.pdf > /dev/null 2>&1 &')
        endif
    endif

end subroutine surf_plot_xy2

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
    integer :: unitdat                                              ! thread save unit number
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
    character(len=:), allocatable :: palette                        ! color palette
    character(len=:), allocatable :: cnames                         ! names of the color palette
    character(len=:), allocatable :: styleset                       ! line style sets
    character(len=:), allocatable :: markerset                      ! marker style sets
    character(len=:), dimension(:), allocatable :: colorvec, colorname, optionvec, stylevec
    character(len=:), dimension(:), allocatable :: markervec
    character(len=:), dimension(:), allocatable :: optmp
    character(len=100), dimension(:), allocatable :: legendvec, opcolor, oplinestyle, oplegend
    character(len=100), dimension(:), allocatable :: opmarker
    character(len=:), allocatable :: legend_type, legend_pos
    logical :: isTmp
    ! 1. Add Flags to track if user set them
    logical :: set_xmin, set_xmax
    logical :: set_ymin, set_ymax

    set_xmin = .false.
    set_xmax = .false.
    set_ymin = .false.
    set_ymax = .false.


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

    markerset = 'o; ' // &
                'square;' // &
                'pentagon;' // &
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
                case ('xmin')
                    read(optmp(2), *) xmin
                    set_xmin = .true.
                case ('xmax')
                    read(optmp(2), *) xmax
                    set_xmax = .true.
                case ('ymin')
                    read(optmp(2), *) ymin
                    set_ymin = .true.
                case ('ymax')
                    read(optmp(2), *) ymax
                    set_ymax = .true.
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
                legend_pos = oplegend(j)
            endif
        enddo
    endif

    ! ---------------- !
    ! default oplegend !
    ! ---------------- !

    if (.not. allocated(legend_pos)) legend_pos = 'south east'
    if (.not. allocated(legend_type)) legend_type = 'box'

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
            opmarker(j) = 'false'
        enddo
    else
        deallocate(opmarker)
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
    unitdat = 412+thread_id

    ! ------------------- !
    ! decompose file name !
    ! ------------------- !

    if (present(name)) then
        call decompose_name(fname, fpath, ftype, name)
        dat = trim(fname) // '.dat'
        tikz = trim(fname) // '.tex'
        isTmp = .false.
    else
        if (isGroupPlot) then
            fpath = './'
            fname = 'tikzplot' // trim(tmpChar5)
            ftype = ''
            dat = 'tikzdata' // trim(tmpChar5) // num2str(fig_count) // '.dat'
            tikz = 'tikzplot' // trim(tmpChar5) // '.tex'
            isTmp = .true.
        else
            fpath = './'
            fname = 'tikzplot' // trim(tmpChar5)
            ftype = ''
            dat = 'tikzdata' // trim(tmpChar5) // '.dat'
            tikz = 'tikzplot' // trim(tmpChar5) // '.tex'
            isTmp = .true.
        endif
    endif

    ! ------------------ !
    ! creating data file !
    ! ------------------ !

    call write_dat(x, y, ni, nj, unitdat, fpath, dat)

    if (.not. set_xmin) xmin = minval(x)
    if (.not. set_xmax) xmax = maxval(x)

    if (.not. set_ymin) ymin = minval(y)
    if (.not. set_ymax) ymax = maxval(y) + 0.1_wp*maxval(y)

    if (isGroupPlot) then
        if (fig_count .eq. 1) then
            groupName = tikz
            groupPath = fpath
            groupFName = fname
            groupType = ftype
            write(*, *) 'Groupplot names: ', trim(fpath) // trim(fname)
            call write_preamble(colorvec, colorname, unitno, fpath, tikz)
            call multi_plot_start(unitno)
        endif
        call multi_plot(unitno, fpath, tikz, dat, nj, xmin, xmax, ymin, ymax, &
                colorvec, colorname, title, xlb, ylb, legendvec, legend_type, legend_pos, &
                opcolor, oplinestyle, opmarker)
        if (fig_count < fig_max) then
            fig_count = fig_count + 1
            return
        else
            call multi_plot_end(unitno)
        endif
    else
        call write_preamble(colorvec, colorname, unitno, fpath, tikz)
        call single_plot(unitno, fpath, tikz, dat, nj, xmin, xmax, ymin, ymax, &
            colorvec, colorname, title, xlb, ylb, legendvec, legend_type, legend_pos, &
            opcolor, oplinestyle, opmarker)
    endif

    call write_finale(unitno)

    if (isGroupPlot) then
        call typeset(groupPath, groupFName, groupName, .true.)
        if (isTmp) then
            call execute_command_line(trim(opener) // ' ' // trim(groupPath) // trim(groupFName) // '.pdf > /dev/null 2>&1;')
        else
            call execute_command_line(trim(opener) // ' ' // trim(groupPath) // trim(groupFName) // '.pdf > /dev/null 2>&1 &')
        endif
    else
        call typeset(fpath, fname, tikz, .true.)
        if (isTmp) then
            call execute_command_line(trim(opener) // ' ' // trim(fpath) // trim(fname) // '.pdf > /dev/null 2>&1;')
        else
            call execute_command_line(trim(opener) // ' ' // trim(fpath) // trim(fname) // '.pdf > /dev/null 2>&1 &')
        endif
    endif

    !if (isTmp) then
    !    call execute_command_line(trim(opener) // ' ' // trim(fpath) // trim(fname) // '.pdf > /dev/null 2>&1;')
    !else
    !    call execute_command_line(trim(opener) // ' ' // trim(fpath) // trim(fname) // '.pdf > /dev/null 2>&1 &')
    !endif

    !if (isTmp) call execute_command_line(&
    !    'rm ' // trim(fpath) // trim(fname) // '.pdf; ' // &
    !    'rm ' // trim(fpath) // trim(tikz) // '; ' // &
    !    'rm ' // trim(fpath) // trim(dat) // '; ')


end subroutine tikz_plot_xy2

! subroutine write_preamble(colorvec, colorname, unitno, fpath, tikz)
!     integer, intent(in) :: unitno
!     character(len=:), dimension(:), allocatable, intent(in) :: colorvec, colorname
!     character(len=*), intent(in) :: fpath, tikz
!     integer :: j

!     open(unitno, file=trim(fpath) // trim(tikz),status='unknown')
!        write(unitno, *) "\documentclass[tikz]{standalone}"

!        write(unitno, *) "\usepackage{tikz}"
!        write(unitno, *) "\usepackage{pgfplots}"
!        write(unitno, *) "\usetikzlibrary{decorations}"
!        write(unitno, *) "\usetikzlibrary{decorations.pathreplacing, intersections, fillbetween}"
!        write(unitno, *) "\usetikzlibrary{calc,positioning}"
!        write(unitno, *) "\usetikzlibrary{plotmarks}"
!        write(unitno, *) "\usetikzlibrary{pgfplots.groupplots, pgfplots.colormaps}"
!        write(unitno, *) "\pgfplotsset{compat=newest, scale only axis, width = 9cm, height = 5cm}"

!        ! write(unitno, *) "\pgfplotsset{sciclean/.style={axis lines=left,"
!        ! write(unitno, *) "        grid=both,"
!        ! write(unitno, *) "        major grid style={line width=.2pt,draw=gray!50, dashed},"
!        ! write(unitno, *) "        axis x line shift=0.5em,"
!        ! write(unitno, *) "        axis y line shift=0.5em,"
!        ! write(unitno, *) "        axis line style={-,very thin},"
!        ! write(unitno, *) "        axis background/.style={draw,ultra thin,gray},"
!        ! write(unitno, *) "        tick align=outside,"
!        ! write(unitno, *) "        xticklabel style={"
!        ! write(unitno, *) "            /pgf/number format/fixed,"
!        ! write(unitno, *) "            /pgf/number format/precision=2"
!        ! write(unitno, *) "        },"
!        ! write(unitno, *) "        yticklabel style={"
!        ! write(unitno, *) "            /pgf/number format/fixed,"
!        ! write(unitno, *) "            /pgf/number format/precision=2"
!        ! write(unitno, *) "        },"
!        ! write(unitno, *) "        scaled y ticks=false,"
!        ! write(unitno, *) "        major tick length=2pt}}"


!         ! --- Style Definition: sciclean ---
!        write(unitno, *) "\pgfplotsset{"
!        write(unitno, *) "    sciclean/.style={"
!        write(unitno, *) "        font=\footnotesize,"
!        write(unitno, *) "        axis background/.style={fill=white},"
!        write(unitno, *) "        axis on top=false,"
!        write(unitno, *) "        axis lines=left,"
!        write(unitno, *) "        axis line style={-, black},"
!        write(unitno, *) "        tick align=inside,"
!        write(unitno, *) "        tick pos=both,"
!        write(unitno, *) "        major tick length=0.15cm,"
!        write(unitno, *) "        grid=major,"
!        write(unitno, *) "        grid style={dashed, gray!30},"
!        write(unitno, *) "        enlargelimits=false,"
!        write(unitno, *) "        title style={font=\small\bfseries, yshift=0.5em},"
!        write(unitno, *) "        label style={font=\footnotesize},"
!        write(unitno, *) "        tick label style={font=\scriptsize},"
!        write(unitno, *) "    }"
!        write(unitno, *) "}"

!        write(unitno, *) ""
!        write(unitno, *) "% Create fake \onslide and other commands for standalone picture"
!        write(unitno, *) "\usepackage{xparse}"
!        write(unitno, *) "\NewDocumentCommand{\onslide}{s t+ d<>}{}"
!        write(unitno, *) "\NewDocumentCommand{\only}{d<>}{}"
!        write(unitno, *) "\NewDocumentCommand{\uncover}{d<>}{}"
!        write(unitno, *) "\NewDocumentCommand{\visible}{d<>}{}"
!        write(unitno, *) "\NewDocumentCommand{\invisible}{d<>}{}"
!        write(unitno, *) ""
!        write(unitno, *) "% ---------------------------------------------------------------------"
!        write(unitno, *) "% Create sloped legend on the data line"
!        write(unitno, *) "\makeatletter"
!        write(unitno, *) "\tikzset{"
!        write(unitno, *) "Sloped/.code = {"
!        write(unitno, *) "\iftikz@fullytransformed% tikz.code.tex"
!        write(unitno, *) "    \tikzset{sloped}"
!        write(unitno, *) "\else"
!        write(unitno, *) "    \pgfgettransformentries{\mya}{\myb}{\myc}{\myd}{\mys}{\myt}%"
!        write(unitno, *) "    \tikzset{sloped, transform shape, rotate = {atan2(\myb,\mya)}}%"
!        write(unitno, *) "\fi"
!        write(unitno, *) "}"
!        write(unitno, *) "}"
!        write(unitno, *) "\makeatother"
!        write(unitno, *) "% ---------------------------------------------------------------------"
!        write(unitno, *) ""
!        write(unitno, *) "% ---------------------------------------------------------------------"
!        write(unitno, *) "% Coordinate extraction"
!        write(unitno, *) "% #1: node name"
!        write(unitno, *) "% #2: output macro name: x coordinate"
!        write(unitno, *) "% #3: output macro name: y coordinate"
!        write(unitno, *) "\newcommand{\Getxycoords}[3]{%"
!        write(unitno, *) "    \pgfplotsextra{%"
!        write(unitno, *) "        % using `\pgfplotspointgetcoordinates' stores the (axis)"
!        write(unitno, *) "        % coordinates in `data point' which then can be called by"
!        write(unitno, *) "        % `\pgfkeysvalueof' or `\pgfkeysgetvalue'"
!        write(unitno, *) "        \pgfplotspointgetcoordinates{(#1)}%"
!        write(unitno, *) "        % `\global' (a TeX macro and not a TikZ/PGFPlots one) allows to"
!        write(unitno, *) "        % store the values globally"
!        write(unitno, *) "         \global\pgfkeysgetvalue{/data point/x}{#2}%"
!        write(unitno, *) "         \global\pgfkeysgetvalue{/data point/y}{#3}%"
!        write(unitno, *) "     }%"
!        write(unitno, *) "}"
!        write(unitno, *) "% ---------------------------------------------------------------------"
!        write(unitno, *) ""
!        do j = 1, size(colorvec), 1
!            write(unitno, *) "\definecolor{" // trim(colorname(j)) // "}{HTML}{" // trim(colorvec(j)) // "}"
!        enddo
!        write(unitno, *) ""
!        write(unitno, *) "\begin{document}"
!        write(unitno, *) ""
!        write(unitno, *) "\begin{tikzpicture}"
!        write(unitno, *) ""
!        write(unitno, *) ""
! end subroutine write_preamble

subroutine write_preamble(colorvec, colorname, unitno, fpath, tikz)
        integer, intent(in) :: unitno
        character(len=:), dimension(:), allocatable, intent(in) :: colorvec, colorname
        character(len=*), intent(in) :: fpath, tikz
        integer :: j

        open(unitno, file=trim(fpath) // trim(tikz), status='unknown')

        ! Use '(a)' to ensure no leading spaces are inserted by the compiler
        write(unitno, '(a)') "\documentclass[tikz]{standalone}"
        write(unitno, '(a)') "\usepackage{tikz}"
        write(unitno, '(a)') "\usepackage{pgfplots}"
        write(unitno, '(a)') "\usetikzlibrary{decorations}"
        write(unitno, '(a)') "\usetikzlibrary{decorations.pathreplacing, intersections, fillbetween}"
        write(unitno, '(a)') "\usetikzlibrary{calc,positioning}"
        write(unitno, '(a)') "\usetikzlibrary{plotmarks}"
        write(unitno, '(a)') "\usetikzlibrary{pgfplots.groupplots, pgfplots.colormaps}"

        ! Fonts
        write(unitno, '(a)') "\usepackage[T1]{fontenc}"
        write(unitno, '(a)') "\usepackage[scaled]{helvet}"
        write(unitno, '(a)') "\renewcommand\familydefault{\sfdefault}"
        write(unitno, '(a)') "\usepackage{sfmath}"

        if (isGroupPlot) then
            write(unitno, '(a)') "\pgfplotsset{compat=newest, scale only axis, width = 5cm, height = 5cm}"
        else
            write(unitno, '(a)') "\pgfplotsset{compat=newest, scale only axis, width = 9cm, height = 5cm}"
        endif

        ! --- Style Definition: sciclean ---
        write(unitno, '(a)') "\pgfplotsset{"
        write(unitno, '(a)') "    sciclean/.style={"
        write(unitno, '(a)') "        font=\footnotesize,"
        write(unitno, '(a)') "        axis background/.style={fill=white},"
        write(unitno, '(a)') "        axis on top=false,"
        ! Note: axis lines=left combined with tick pos=both might hide top/right ticks
        ! If you want a full box, change 'left' to 'box' or 'std'
        write(unitno, '(a)') "        axis lines=left,"
        write(unitno, '(a)') "        axis line style={-, black},"
        write(unitno, '(a)') "        tick align=inside,"
        write(unitno, '(a)') "        major tick length=0.15cm,"
        write(unitno, '(a)') "        grid=major,"
        write(unitno, '(a)') "        grid style={dashed, gray!30},"
        write(unitno, '(a)') "        enlargelimits=false,"
        write(unitno, '(a)') "        title style={font=\small\bfseries, yshift=0.5em},"
        write(unitno, '(a)') "        label style={font=\footnotesize},"
        write(unitno, '(a)') "        tick label style={font=\scriptsize},"
        write(unitno, '(a)') "    }"
        write(unitno, '(a)') "}"

        write(unitno, '(a)') ""
        write(unitno, '(a)') "% Create fake \onslide and other commands for standalone picture"
        write(unitno, '(a)') "\usepackage{xparse}"
        write(unitno, '(a)') "\NewDocumentCommand{\onslide}{s t+ d<>}{}"
        write(unitno, '(a)') "\NewDocumentCommand{\only}{d<>}{}"
        write(unitno, '(a)') "\NewDocumentCommand{\uncover}{d<>}{}"
        write(unitno, '(a)') "\NewDocumentCommand{\visible}{d<>}{}"
        write(unitno, '(a)') "\NewDocumentCommand{\invisible}{d<>}{}"
        write(unitno, '(a)') ""
        write(unitno, '(a)') "% ---------------------------------------------------------------------"
        write(unitno, '(a)') "% Create sloped legend on the data line"
        write(unitno, '(a)') "\makeatletter"
        write(unitno, '(a)') "\tikzset{"
        write(unitno, '(a)') "Sloped/.code = {"
        write(unitno, '(a)') "\iftikz@fullytransformed% tikz.code.tex"
        write(unitno, '(a)') "    \tikzset{sloped}"
        write(unitno, '(a)') "\else"
        write(unitno, '(a)') "    \pgfgettransformentries{\mya}{\myb}{\myc}{\myd}{\mys}{\myt}%"
        write(unitno, '(a)') "    \tikzset{sloped, transform shape, rotate = {atan2(\myb,\mya)}}%"
        write(unitno, '(a)') "\fi"
        write(unitno, '(a)') "}"
        write(unitno, '(a)') "}"
        write(unitno, '(a)') "\makeatother"
        write(unitno, '(a)') "% ---------------------------------------------------------------------"
        write(unitno, '(a)') ""
        write(unitno, '(a)') "% ---------------------------------------------------------------------"
        write(unitno, '(a)') "% Coordinate extraction"
        write(unitno, '(a)') "\newcommand{\Getxycoords}[3]{%"
        write(unitno, '(a)') "    \pgfplotsextra{%"
        write(unitno, '(a)') "        \pgfplotspointgetcoordinates{(#1)}%"
        write(unitno, '(a)') "         \global\pgfkeysgetvalue{/data point/x}{#2}%"
        write(unitno, '(a)') "         \global\pgfkeysgetvalue{/data point/y}{#3}%"
        write(unitno, '(a)') "     }%"
        write(unitno, '(a)') "}"
        write(unitno, '(a)') "% ---------------------------------------------------------------------"
        write(unitno, '(a)') ""

        do j = 1, size(colorvec), 1
            write(unitno, '(a,a,a,a,a)') "\definecolor{", trim(colorname(j)), "}{HTML}{", trim(colorvec(j)), "}"
        enddo

        write(unitno, '(a)') ""
        write(unitno, '(a)') "\begin{document}"
        write(unitno, '(a)') ""
        write(unitno, '(a)') "\begin{tikzpicture}"
        write(unitno, '(a)') ""

    end subroutine write_preamble


! subroutine write_preamble(colorvec, colorname, unitno, fpath, tikz)
!         integer, intent(in) :: unitno
!         character(len=:), dimension(:), allocatable, intent(in) :: colorvec, colorname
!         character(len=*), intent(in) :: fpath, tikz
!         integer :: j

!         open(unitno, file=trim(fpath) // trim(tikz), status='unknown')

!         ! --- Standard Headers ---
!         write(unitno, '(a)') "\documentclass[tikz, margin=5pt]{standalone}"
!         write(unitno, '(a)') "\usepackage{pgfplots}"
!         write(unitno, '(a)') "\pgfplotsset{compat=newest}"
!         write(unitno, '(a)') "\usepgfplotslibrary{groupplots}"
!         ! Useful libraries typically needed by the module's other features
!         write(unitno, '(a)') "\usetikzlibrary{calc, positioning, plotmarks, decorations.pathreplacing}"

!         ! --- Fonts (New) ---
!         write(unitno, '(a)') "\usepackage[T1]{fontenc}"
!         write(unitno, '(a)') "\usepackage[scaled]{helvet}"
!         write(unitno, '(a)') "\renewcommand\familydefault{\sfdefault}"
!         write(unitno, '(a)') "\usepackage{sfmath}"

!         ! --- Style Definition: sciclean ---
!         write(unitno, '(a)') "\pgfplotsset{"
!         write(unitno, '(a)') "    sciclean/.style={"
!         write(unitno, '(a)') "        font=\footnotesize,"
!         write(unitno, '(a)') "        axis background/.style={fill=white},"
!         write(unitno, '(a)') "        axis on top=false,"
!         write(unitno, '(a)') "        axis lines=left,"
!         write(unitno, '(a)') "        axis line style={-, black},"
!         write(unitno, '(a)') "        tick align=inside,"
!         write(unitno, '(a)') "        tick pos=both,"
!         write(unitno, '(a)') "        major tick length=0.15cm,"
!         write(unitno, '(a)') "        grid=major,"
!         write(unitno, '(a)') "        grid style={dashed, gray!30},"
!         write(unitno, '(a)') "        enlargelimits=false,"
!         write(unitno, '(a)') "        title style={font=\small\bfseries, yshift=0.5em},"
!         write(unitno, '(a)') "        label style={font=\footnotesize},"
!         write(unitno, '(a)') "        tick label style={font=\scriptsize},"
!         write(unitno, '(a)') "    }"
!         write(unitno, '(a)') "}"

!         ! --- Helper Macros (Preserved from original code) ---
!         write(unitno, '(a)') ""
!         write(unitno, '(a)') "% --- Macros for compatibility and utilities ---"
!         write(unitno, '(a)') "\usepackage{xparse}"
!         write(unitno, '(a)') "\NewDocumentCommand{\onslide}{s t+ d<>}{}"
!         write(unitno, '(a)') "\NewDocumentCommand{\only}{d<>}{}"
!         write(unitno, '(a)') "\NewDocumentCommand{\uncover}{d<>}{}"
!         write(unitno, '(a)') "\NewDocumentCommand{\visible}{d<>}{}"
!         write(unitno, '(a)') "\NewDocumentCommand{\invisible}{d<>}{}"

!         write(unitno, '(a)') "\makeatletter"
!         write(unitno, '(a)') "\tikzset{Sloped/.code = {\iftikz@fullytransformed\tikzset{sloped}\else\pgfgettransformentries{\mya}{\myb}{\myc}{\myd}{\mys}{\myt}\tikzset{sloped, transform shape, rotate = {atan2(\myb,\mya)}}\fi}}"
!         write(unitno, '(a)') "\makeatother"

!         write(unitno, '(a)') "\newcommand{\Getxycoords}[3]{\pgfplotsextra{\pgfplotspointgetcoordinates{(#1)}\global\pgfkeysgetvalue{/data point/x}{#2}\global\pgfkeysgetvalue{/data point/y}{#3}}}"
!         write(unitno, '(a)') ""

!         ! --- Color Definitions ---
!         do j = 1, size(colorvec), 1
!             write(unitno, '(a,a,a,a,a)') "\definecolor{", trim(colorname(j)), "}{HTML}{", trim(colorvec(j)), "}"
!         enddo

!         write(unitno, '(a)') ""
!         write(unitno, '(a)') "\begin{document}"
!         write(unitno, '(a)') "\begin{tikzpicture}"

!     end subroutine write_preamble

subroutine write_finale(unitno)
    integer, intent(in) :: unitno
        write(unitno, *) ""
        write(unitno, *) ""
        write(unitno, *) "\end{tikzpicture}"
        write(unitno, *) ""
        write(unitno, *) "\end{document}"
    close(unitno)
end subroutine write_finale

subroutine multi_plot_start(unitno)
    integer, intent(in) :: unitno
        write(unitno, *) "\begin{groupplot}["
        write(unitno, *) "    group style={"
        write(unitno, *) "        group size=" // num2str(col_group) // &
                                        " by " // num2str(row_group) // ","
        write(unitno, *) "        horizontal sep=2cm,"
        write(unitno, *) "         vertical sep=2cm"
        write(unitno, *) "}]"
end subroutine multi_plot_start

subroutine multi_plot_end(unitno)
    integer, intent(in) :: unitno

        write(unitno, *) "\end{groupplot}"
end subroutine multi_plot_end

subroutine multi_plot(unitno, fpath, tikz, dat, nj, xmin, xmax, ymin, ymax, &
        colorvec, colorname, title, xlb, ylb, legendvec, legend_type, legend_pos, &
        opcolor, oplinestyle, opmarker)

    integer, intent(in) :: unitno, nj
    character(len=*), intent(in) :: fpath, tikz, dat
    character(len=*), intent(in) :: title, xlb, ylb
    character(len=:), dimension(:), allocatable, intent(in) :: colorvec, colorname
    character(len=*), intent(in) :: legend_type, legend_pos
    character(len=*), dimension(:), intent(in) :: legendvec, opcolor, oplinestyle, opmarker
    real(wp), intent(in) :: xmin, xmax, ymin, ymax
    integer :: j
    write(unitno, *) "\nextgroupplot["
    write(unitno, *) "    sciclean,"
    write(unitno, *) "    xlabel = {" // xlb // "},"
    write(unitno, *) "    ylabel = {" // ylb // "},"
    write(unitno, *) "    xmin = " // num2str(xmin) // ","
    write(unitno, *) "    xmax = " // num2str(xmax) // ","
    write(unitno, *) "    ymin = " // num2str(ymin) // ","
    write(unitno, *) "    ymax = " // num2str(ymax) // ","
    write(unitno, *) "    legend cell align = left,"
    write(unitno, *) "    legend pos = " // trim(legend_pos) // ","
    write(unitno, *) "    title = {" // title // "}]"
    write(unitno, *) ""
    call plotting(unitno, nj, colorvec, colorname, fpath, dat, legendvec, &
       legend_type, opcolor, oplinestyle, opmarker)
    write(unitno, *) ""
end subroutine multi_plot

subroutine single_plot(unitno, fpath, tikz, dat, nj, xmin, xmax, ymin, ymax, &
        colorvec, colorname, title, xlb, ylb, legendvec, legend_type, legend_pos, &
        opcolor, oplinestyle, opmarker)

    integer, intent(in) :: unitno, nj
    character(len=*), intent(in) :: fpath, tikz, dat
    character(len=*), intent(in) :: title, xlb, ylb
    character(len=:), dimension(:), allocatable, intent(in) :: colorvec, colorname
    character(len=*), intent(in) :: legend_type, legend_pos
    character(len=*), dimension(:), intent(in) :: legendvec, opcolor, oplinestyle, opmarker
    real(wp), intent(in) :: xmin, xmax, ymin, ymax
    integer :: j

    write(unitno, *) "\begin{axis}["
    write(unitno, *) "    sciclean,"
    write(unitno, *) "    xlabel = {" // xlb // "},"
    write(unitno, *) "    ylabel = {" // ylb // "},"
    write(unitno, *) "    xmin = " // num2str(xmin) // ","
    write(unitno, *) "    xmax = " // num2str(xmax) // ","
    write(unitno, *) "    ymin = " // num2str(ymin) // ","
    write(unitno, *) "    ymax = " // num2str(ymax) // ","
    write(unitno, *) "    legend cell align = left,"
    write(unitno, *) "    legend pos = " // trim(legend_pos) // ","
    write(unitno, *) "    title = {" // title // "}]"
    write(unitno, *) ""
    write(unitno, *) ""
    call plotting(unitno, nj, colorvec, colorname, fpath, dat, legendvec, &
       legend_type, opcolor, oplinestyle, opmarker)
    write(unitno, *) ""
    write(unitno, *) ""
    write(unitno, *) "\end{axis}"

end subroutine single_plot

subroutine surf_multi_plot(unitno, fpath, tikz, dat, xmin, xmax, ymin, ymax, &
        view, colormap, colorname, title, xlb, ylb, legendvec, legend_type, legend_pos, &
        opcolor, oplinestyle, opmarker)

    integer, intent(in) :: unitno, view(2)
    character(len=*), intent(in) :: fpath, tikz, dat
    character(len=*), intent(in) :: title, xlb, ylb, colormap
    character(len=:), dimension(:), allocatable, intent(in) :: colorname
    character(len=*), intent(in) :: legend_type, legend_pos
    character(len=*), dimension(:), intent(in) :: legendvec, opcolor, oplinestyle, opmarker
    real(wp), intent(in) :: xmin, xmax, ymin, ymax
    integer :: j

    write(unitno, *) "\nextgroupplot["
    write(unitno, *) "    sciclean,"
    write(unitno, *) "    xlabel = {" // xlb // "},"
    write(unitno, *) "    ylabel = {" // ylb // "},"
    write(unitno, *) "    ylabel style = {Sloped},"
    write(unitno, *) "    xlabel style = {Sloped},"
    write(unitno, *) "    colormap/" // colormap //","
    write(unitno, *) "    colorbar,"
    write(unitno, *) "    view={" // num2str(view(1)) // "}{" // num2str(view(2)) // "},"
    write(unitno, *) "    xmin = " // num2str(xmin) // ","
    write(unitno, *) "    xmax = " // num2str(xmax) // ","
    write(unitno, *) "    ymin = " // num2str(ymin) // ","
    write(unitno, *) "    ymax = " // num2str(ymax) // ","
    write(unitno, *) "    unbounded coords=jump,"
    write(unitno, *) "    z filter/.expression={z > 0.0 ? z : nan},"
    write(unitno, *) "    title = {" // title // "}]"
    write(unitno, *) ""
    write(unitno, *) "\addplot3 [surf, fill = white] table {" // dat // "};"
    write(unitno, *) ""
end subroutine surf_multi_plot

subroutine surf_single_plot(unitno, fpath, tikz, dat, xmin, xmax, ymin, ymax, &
        view, colormap, colorname, title, xlb, ylb, legendvec, legend_type, legend_pos, &
        opcolor, oplinestyle, opmarker)

    integer, intent(in) :: unitno, view(2)
    character(len=*), intent(in) :: fpath, tikz, dat
    character(len=*), intent(in) :: title, xlb, ylb, colormap
    character(len=:), dimension(:), allocatable, intent(in) :: colorname
    character(len=*), intent(in) :: legend_type, legend_pos
    character(len=*), dimension(:), intent(in) :: legendvec, opcolor, oplinestyle, opmarker
    real(wp), intent(in) :: xmin, xmax, ymin, ymax
    integer :: j

    write(unitno, *) "\begin{axis}["
    write(unitno, *) "    sciclean,"
    write(unitno, *) "    xlabel = {" // xlb // "},"
    write(unitno, *) "    ylabel = {" // ylb // "},"
    write(unitno, *) "    ylabel style = {Sloped},"
    write(unitno, *) "    xlabel style = {Sloped},"
    write(unitno, *) "    colormap/" // colormap //","
    write(unitno, *) "    colorbar,"
    write(unitno, *) "    view={" // num2str(view(1)) // "}{" // num2str(view(2)) // "},"
    write(unitno, *) "    xmin = " // num2str(xmin) // ","
    write(unitno, *) "    xmax = " // num2str(xmax) // ","
    write(unitno, *) "    ymin = " // num2str(ymin) // ","
    write(unitno, *) "    ymax = " // num2str(ymax) // ","
    write(unitno, *) "    unbounded coords=jump,"
    write(unitno, *) "    z filter/.expression={z > 0.0 ? z : nan},"
    write(unitno, *) "    title = {" // title // "}]"
    write(unitno, *) ""
    write(unitno, *) ""
    write(unitno, *) "\addplot3 [surf, fill = white] table {" // dat // "};"
    write(unitno, *) ""
    write(unitno, *) ""
    write(unitno, *) "\end{axis}"

end subroutine surf_single_plot

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
            if (opmarker(j) == 'false') then
                write(unitno, *) "\addplot[name path = " // num2str(j) // &
                    ", thick, " // trim(oplinestyle(j)) // ", color=" // trim(opcolor(j)) // &
                    "] table [x expr=\thisrowno{0}, y expr=\thisrowno{" // num2str(j) // &
                    "}]{" // dat // "} node[" // trim(opcolor(j)) // ", pos=0.5, above, Sloped]{" // trim(legendvec(j)) // "};"
            else
                write(unitno, *) "\addplot[name path = " // num2str(j) // &
                    ", thick, " // trim(oplinestyle(j)) // ", color=" // trim(opcolor(j)) // &
                    ", mark=" // trim(opmarker(j)) // &
                    "] table [x expr=\thisrowno{0}, y expr=\thisrowno{" // num2str(j) // &
                    "}]{" // dat // "} node[" // trim(opcolor(j)) // ", pos=0.5, above, Sloped]{" // trim(legendvec(j)) // "};"
            endif
        case ('box')
            if (opmarker(j) == 'false') then
                write(unitno, *) "\addplot[name path = " // num2str(j) // &
                    ", thick, " // trim(oplinestyle(j)) // ", color=" // trim(opcolor(j)) // &
                    "] table [x expr=\thisrowno{0}, y expr=\thisrowno{" // num2str(j) // &
                    "}]{" // dat // "};"
                write(unitno, *) "\addlegendentry{" // trim(legendvec(j)) // "}"
            else
                write(unitno, *) "\addplot[name path = " // num2str(j) // &
                    ", thick, " // trim(oplinestyle(j)) // ", color=" // trim(opcolor(j)) // &
                    ", mark=" // trim(opmarker(j)) // &
                    "] table [x expr=\thisrowno{0}, y expr=\thisrowno{" // num2str(j) // &
                    "}]{" // dat // "};"
                write(unitno, *) "\addlegendentry{" // trim(legendvec(j)) // "}"
            endif
        end select
    enddo

end subroutine plotting

subroutine write_dat(x, y, ni, nj, unitdat, fpath, dat)
    real(wp), intent(in) :: x(:), y(:, :)
    integer, intent(in) :: unitdat, ni, nj
    character(len=*), intent(in) :: fpath, dat
    integer :: i, j

    ! Write data to a temporary file
    open(unitdat,file=trim(fpath) // trim(dat),status='unknown')
    do i = 1,ni
        write(unitdat,'(1PE24.15E3)',advance='no') x(i)
        do j = 1,nj
            write(unitdat,'(1PE24.15E3)',advance='no') y(i,j)
        end do
        write(unitdat,*)
    end do
    close(unitdat)
end subroutine write_dat

subroutine write_surf_dat(data, unitdat, nx, fpath, dat)
    real(wp), intent(in) :: data(:, :)
    integer, intent(in) :: unitdat, nx
    character(len=*), intent(in) :: fpath, dat
    integer :: ni, i, j

    ni = size(data, 1)

    ! Write data to a temporary file
    open(unitdat,file=trim(fpath) // trim(dat),status='unknown')
    do i = 1, ni
        write(unitdat,'(F15.9)',advance='no') data(i, 1)
        write(unitdat,'(F15.9)',advance='no') data(i, 2)
        write(unitdat,'(F15.9)',advance='no') data(i, 3)
        write(unitdat,*)
        if (mod(i, nx) .eq. 0) then
            write(unitdat,*)
        endif
    end do
    close(unitdat)

    !! Write data to a temporary file
    !open(unitdat,file='',status='unknown')
    !do i = 1,bknum*knum
    !    write(unitdat,'(F15.9)',advance='no') data(i, 1)
    !    write(unitdat,'(F15.9)',advance='no') data(i, 2)
    !    write(unitdat,'(F15.9)',advance='no') data(i, 3)
    !    write(unitdat,*)
    !    if (mod(i, knum) .eq. 0) then
    !        write(unitdat,*)
    !    endif
    !end do
    !close(unitdat)


end subroutine write_surf_dat

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

!subroutine typeset(fpath, fname, tikz, isSaveFile)

!    character(len=*), intent(in) :: fname, fpath, tikz
!    logical, intent(in) :: isSaveFile
!    character(len=:), allocatable :: plotfile
!    integer :: unitno, ierr

!    write(*, *) 'tikz typeset names: ', trim(fpath) // trim(fname)

!    call execute_command_line('cwd=$PWD; cd ' // trim(fpath) // &
!        '; pdflatex ' // trim(tikz) // ' > /dev/null 2>&1; cd "$cwd"',exitstat=ierr)

!    !! debug
!    !call execute_command_line('cwd=$PWD; cd ' // trim(fpath) // &
!    !    '; pdflatex ' // trim(tikz) // '; cd "$cwd"',exitstat=ierr)

!    if (ierr/=0) print*,'sub_plot_x_y2: WARNING: error in calling gnuplot'

!    call execute_command_line('cwd=$PWD; ' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '-eps-converted-to.pdf'   // ' ] && rm ' // trim(fpath) // trim(fname) // '-eps-converted-to.pdf;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.aux'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.aux;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.log'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.log;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.4tc'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.4tc;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.xref'                   // ' ] && rm ' // trim(fpath) // trim(fname) // '.xref;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.tmp'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.tmp;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.pyc'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.pyc;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.pyg'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.pyg;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.pyo'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.pyo;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.fls'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.fls;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.vrb'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.vrb;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.fdb_latexmk'            // ' ] && rm ' // trim(fpath) // trim(fname) // '.fdb_latexmk;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.bak'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.bak;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.swp'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.swp;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.aux'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.aux;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.log'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.log;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.lof'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.lof;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.lot'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.lot;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.maf'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.maf;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.idx'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.idx;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.mtc'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.mtc;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.mtc0'                   // ' ] && rm ' // trim(fpath) // trim(fname) // '.mtc0;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.nav'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.nav;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.out'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.out;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.snm'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.snm;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.toc'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.toc;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.bcf'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.bcf;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.run.xml'                // ' ] && rm ' // trim(fpath) // trim(fname) // '.run.xml;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.synctex.gz'             // ' ] && rm ' // trim(fpath) // trim(fname) // '.synctex.gz;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.blg'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.blg;' // &
!        '[ -f ' // trim(fpath) // trim(fname) // '.bbl'                    // ' ] && rm ' // trim(fpath) // trim(fname) // '.bbl;' // &
!        'cd "$cwd"',exitstat=ierr)



!end subroutine typeset

! ------------------------------------------------------------------
! PORTABLE TYPESET SUBROUTINE (Windows/Linux/Mac)
! Handles spaces in filenames and cleans up all intermediate files
! ------------------------------------------------------------------
subroutine typeset(fpath, fname, tikz, isSaveFile)
    character(len=*), intent(in) :: fname      ! Name of the output file (e.g., 'plot')
    character(len=*), intent(in) :: fpath      ! Path to directory (e.g., './')
    character(len=*), intent(in) :: tikz       ! Full .tex filename (e.g., 'plot.tex')
    logical, intent(in)          :: isSaveFile ! .true. = keep .tex/.dat, .false. = delete all

    integer :: ierr, i
    character(len=:), allocatable :: cmd, cmd_sep, null_dev, cd_cmd, del_cmd
    character(len=:), allocatable :: clean_list
    character(len=10) :: win_check
    logical :: is_windows

    ! 1. THE "JUNK" LIST (Always delete these)
    character(len=32), parameter :: exts(*) = [ character(len=32) :: &
        "-eps-converted-to.pdf", ".aux", ".log", ".4tc", ".xref",    &
        ".tmp", ".pyc", ".pyg", ".pyo", ".fls", ".vrb",              &
        ".fdb_latexmk", ".bak", ".swp", ".lof", ".lot",              &
        ".maf", ".idx", ".mtc", ".mtc0", ".nav", ".out",             &
        ".snm", ".toc", ".bcf", ".run.xml", ".synctex.gz",           &
        ".blg", ".bbl" ]

    ! 2. RUNTIME OS DETECTION & SYNTAX SETUP
    call get_environment_variable("OS", win_check)
    is_windows = (index(win_check, "Windows") > 0)

    if (is_windows) then
        cmd_sep  = " & "
        null_dev = " > NUL"
        cd_cmd   = "cd /d "
    else
        cmd_sep  = " ; "
        null_dev = " > /dev/null 2>&1"
        cd_cmd   = "cd "
    end if

    ! 3. COMPILE PDF
    cmd = cd_cmd // '"' // trim(fpath) // '"' // cmd_sep // &
          "lualatex " // '"' // trim(tikz) // '"' // null_dev

    write(*, '(A)', advance='no') ' >> Compiling ' // trim(tikz) // '... '
    call execute_command_line(cmd, exitstat=ierr)

    if (ierr /= 0) then
        write(*, *) "[FAILED]"
        write(ERROR_UNIT, *) "Error: pdflatex failed. Check your LaTeX installation."
    else
        write(*, *) "[DONE]"
    end if

    ! 4. ALWAYS CLEAN JUNK (.aux, .log, etc.)
    clean_list = ""
    do i = 1, size(exts)
        clean_list = clean_list // ' "' // trim(fname) // trim(exts(i)) // '"'
    end do

    ! 5. CONDITIONALLY CLEAN SOURCE (.tex, .dat)
    ! Only delete these if we are NOT saving the file (isSaveFile = .false.)
    if (.not. isSaveFile) then
        clean_list = clean_list // ' "' // trim(fname) // ".tex" // '"'
        clean_list = clean_list // ' "' // trim(fname) // ".dat" // '"'
    end if

    ! 6. EXECUTE CLEANUP
    if (is_windows) then
        ! Windows: del /Q [files] 2>NUL
        del_cmd = "del /Q " // clean_list // " 2>NUL"
    else
        ! Linux/Mac: rm -f [files]
        del_cmd = "rm -f " // clean_list
    end if

    cmd = cd_cmd // '"' // trim(fpath) // '"' // cmd_sep // del_cmd
    call execute_command_line(cmd, exitstat=ierr)

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

!subroutine meshgrid(x,y,xgv,ygv, ierr)
!    !! taken from ogpf
!    !..............................................................................
!    !meshgrid generate mesh grid over a rectangular domain of [xmin xmax, ymin, ymax]
!    ! Inputs:
!    !     xgv, ygv are grid vectors in form of full grid data
!    ! Outputs:
!    !     X and Y are matrix each of size [ny by nx] contains the grid data.
!    !     The coordinates of point (i,j) is [X(i,j), Y(i,j)]
!    !     ierr: The error flag
!    !     """
!    !     # Example
!    !     # call meshgrid(X, Y, [0.,1.,2.,3.],[5.,6.,7.,8.])
!    !     # X
!    !     # [0.0, 1.0, 2.0, 3.0,
!    !     #  0.0, 1.0, 2.0, 3.0,
!    !     #  0.0, 1.0, 2.0, 3.0,
!    !     #  0.0, 1.0, 2.0, 3.0]
!    !     #
!    !     #Y
!    !     #[ 5.0, 5.0, 5.0, 5.0,
!    !     #  6.0, 6.0, 6.0, 6.0,
!    !     #  7.0, 7.0, 7.0, 7.0,
!    !     #  8.0, 8.0, 8.0, 8.0]
!    !..............................................................................
!    ! Rev 0.2, Feb 2018
!    ! New feature added: xgv and ygv as full grid vector are accepted now

!    ! Arguments
!    real(wp), intent(out), allocatable  :: x(:,:)
!    real(wp), intent(out), allocatable  :: y(:,:)
!    real(wp), intent(in)                :: xgv(:) ! x grid vector [start, stop, step] or [start, stop]
!    real(wp), intent(in),  optional     :: ygv(:) ! y grid vector [start, stop, step] or [start, stop]
!    integer,  intent(out), optional     :: ierr   ! the error value

!    ! Local variables
!    integer:: sv
!    integer:: nx
!    integer:: ny
!    logical:: only_xgv_available

!    ! Initial setting
!    only_xgv_available  = .false.
!    sv=0 !Assume no error

!    nx=size(xgv, dim=1)

!    if (present(ygv)) then
!        ny = size(ygv, dim=1)
!    else
!        only_xgv_available=.true.
!        ny=nx
!    end if

!    allocate(x(ny,nx),y(ny,nx),stat=sv)
!    if (sv /=0) then
!        print*, "allocataion erro in meshgrid"
!        stop
!    end if

!    x(1,:)    = xgv
!    x(2:ny,:) = spread(xgv, dim=1, ncopies=ny-1)

!    if (only_xgv_available) then
!        y=transpose(x)
!    else
!        y(:,1)    = ygv
!        y(:,2:nx) = spread(ygv,dim=2,ncopies=nx-1)
!    end if

!    if (present(ierr)) then
!        ierr=sv
!    end if

!end subroutine meshgrid

! ------------------------------------------------------------------
! Meshgrid (Standard Cartesian / Matlab Style)
! Maps x_vec -> x_mat (varying along dim 2 - columns)
! Maps y_vec -> y_mat (varying along dim 1 - rows)
! ------------------------------------------------------------------
subroutine meshgrid(x_mat, y_mat, x_vec, y_vec)
    real(wp), dimension(:,:), allocatable, intent(out) :: x_mat, y_mat
    real(wp), dimension(:), intent(in) :: x_vec, y_vec

    integer :: nx, ny

    nx = size(x_vec)
    ny = size(y_vec)

    ! Note: Dimensions are (ny, nx) if using Matlab convention
    ! but usually in Fortran we keep (nx, ny) and just transpose the fill.
    ! Let's assume you want output shape (ny, nx) to match standard meshgrid.

    allocate(x_mat(ny, nx))
    allocate(y_mat(ny, nx))

    ! X varies across columns (dim 2)
    ! SPREAD x_vec along dim 1 (rows) to copy it downwards
    x_mat = spread(x_vec, dim=1, ncopies=ny)

    ! Y varies down rows (dim 1)
    ! SPREAD y_vec along dim 2 (columns) to copy it across
    y_mat = spread(y_vec, dim=2, ncopies=nx)

end subroutine meshgrid

end module tikz_module
