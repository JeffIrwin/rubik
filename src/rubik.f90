
module rubik

implicit none

! This module contains subroutines and functions related to
! Rubik's cube.

! TO DO:
!
! - count OBTM moves (outer block turn metric)
!
! - display time as well as OBTM taken to solve
!
! - print warning when unrecognized input is given
!
! - shorten z axis in graphic
!
! - in game, add capabilities to display "help", to "undo n"
!   series of moves, save game and quit, continue
!   previously saved game, display title screen,
!   enter free play (starting from solved state),

type rubik_settings
	integer :: iseed
	logical :: seed
end type rubik_settings

contains

!=======================================================================

subroutine load_args(s, io)

	character :: argv*256

	integer :: i, io, argc

	type(rubik_settings) :: s

	io = 0
	argc = command_argument_count()

	s%seed = .false.

	i = 0
	do while (i < argc)
		i = i + 1
		call get_command_argument(i, argv)

		if (argv == "-s") then
			i = i + 1
			call get_command_argument(i, argv)

			s%seed = .true.

			! TODO:  check formatting
			read(argv, *) s%iseed

			!print *, 'iseed = ', s%iseed

		else
			! TODO:  unknown arg
		end if

	end do

end subroutine load_args

!=======================================================================

subroutine scramble(s, moves, n)

	use randmod

	integer, allocatable :: moves(:,:)
	integer :: i, n

	type(rubik_settings) :: s

	! This subroutine generates a series of n random moves in array
	! 'moves'.

	! Any one row of moves (a, b) represents a move that turns face a
	! (1 - 9) by amount b (0 - 3) where the faces and amounts are
	! defined as follows:

	! face 1: R (right)
	! face 2: U (up)
	! face 3: L (left)
	! face 4: D (down)
	! face 5: F (front)
	! face 6: B (back)

	! face 7: M (between R and L, same direction as L)
	! face 8: E (between U and D, same direction as D)
	! face 9: S (between F and B, same direction as F)

	! amount 0: 0 degrees (not used)
	! amount 1: 90 degrees anti-clockwise as if you were facing that
	!           side of the cube
	! amount 2: 180 degrees
	! amount 3: 270 degrees (90 degrees clockwise)

	allocate(moves(n, 2))

	do i = 1, n
		! radomly pick one of the faces
		moves(i, 1) = ceiling(random(s%seed, s%iseed) * 9)

		! turn the face 90, 180, or 270 degrees (but not 0)
		moves(i, 2) = ceiling(random(s%seed, s%iseed) * 3)
	end do

end subroutine scramble

!=======================================================================

subroutine render(moves)
	!character :: smoves*(*)
	integer, allocatable :: moves(:,:)
	integer :: i

	! This subroutine prints a numerical array of moves 'moves' as a
	! human-readable string 'smoves'.

	write(*,*)
	do i = 1, size(moves, 1)
		if (moves(i, 2) /= 0) then
			if (moves(i, 1) == 1) then ! smoves = smoves//'R'
				write(*, '(a1)', advance = 'no') 'R'
			else if (moves(i, 1) == 2) then ! smoves = smoves//'U'
				write(*, '(a1)', advance = 'no') 'U'
			else if (moves(i, 1) == 3) then ! smoves = smoves//'L'
				write(*, '(a1)', advance = 'no') 'L'
			else if (moves(i, 1) == 4) then ! smoves = smoves//'D'
				write(*, '(a1)', advance = 'no') 'D'
			else if (moves(i, 1) == 5) then ! smoves = smoves//'F'
				write(*, '(a1)', advance = 'no') 'F'
			else if (moves(i, 1) == 6) then ! smoves = smoves//'B'
				write(*, '(a1)', advance = 'no') 'B'
			else if (moves(i, 1) == 7) then
				write(*, '(a1)', advance = 'no') 'M'
			else if (moves(i, 1) == 8) then
				write(*, '(a1)', advance = 'no') 'E'
			else if (moves(i, 1) == 9) then
				write(*, '(a1)', advance = 'no') 'S'
			end if

			if (moves(i, 2) == 1) then ! smoves = smoves//"'"
				write(*, '(a1)', advance = 'no') "'"
			else if (moves(i, 2) == 2) then ! smoves = smoves//'2'
				write(*, '(a1)', advance = 'no') '2'
			end if

			!smoves = smoves//' '
			write(*, '(a1)', advance = 'no') ' '
		end if
	end do
	write(*,*)

	!print *, smoves

end subroutine render

!=======================================================================

	!  For example, the solved state would appear as follows:
	!
	!        w w w
	!        w w w
	!        w w w
	!
	! r r r  b b b  o o o  g g g
	! r r r  b b b  o o o  g g g
	! r r r  b b b  o o o  g g g
	!
	!        y y y
	!        y y y
	!        y y y

	! The faces appear as follows:
	!
	!         U
	!
	!      L  F  R  B
	!
	!         D
	!
	! Each of the above letters are short for:
	!
	!     U  Up
	!     L  Left
	!     F  Front
	!     R  Right
	!     B  Back
	!     D  Down
	!
	! Corresponding face numbers are shown below:
	!
	!         2
	!
	!      3  5  1  6
	!
	!         4

	! Edge position numbers are shown below (note that each unique
	! edge piece corresponds to two separate tiles):
	!
	!          2
	!        3   1
	!          4
	!
	!   3      4      1      2
	! 6   7  7   8  8   5  5   6
	!   11     12     9      10
	!
	!          12
	!        11  9
	!          10

	! Corner position numbers are shown below (note that each unique
	! corner piece corresponds to three separate tiles):
	!
	!        2   1
	!
	!        3   4
	!
	! 2   3  3   4  4   1  1   2
	!
	! 6   7  7   8  8   5  5   6
	!
	!        7   8
	!
	!        6   5

	! In addition to edge and corner pieces, there are also center
	! pieces.  These do not move relative to the body of the cube.

	! Colors are represented as follows:
	!
	! w: white
	! r: red
	! b: blue
	! o: orange
	! g: green
	! y: yellow

	! colors on right, up, left, down, front, and back faces
	! respectively:
	!
	!cmap = 'owrybg'

	! In the following functions, note that "state" refers to the
	! current configuration of the pieces of the cube.  Rotating the
	! cube as a whole will not change its state, but turning a single
	! face will.

subroutine cprint(cstate)

	integer :: i
	character :: cstate(54)

	! This subroutine prints the state of the cube in human readable
	! form.

	write(*,*)
	do i = 1, 54

		! leading spaces for up and down faces
		if (i ==  1 .or. i ==  4 .or. i ==  7 .or. &
			i == 46 .or. i == 49 .or. i == 52) then
			write(*, '(a7)', advance = 'no') '       '
		end if

		! extra space for front, right, and back faces
		if (i == 13 .or. i == 16 .or. i == 19 .or.  &
			i == 25 .or. i == 28 .or. i == 31 .or.  &
			i == 37 .or. i == 40 .or. i == 43) then
			write(*, '(a1)', advance = 'no') ' '
		end if

		write(*, '(a2)', advance = 'no') cstate(i)//' '

		! new line
		if (i ==  3 .or. i ==  6 .or. i == 21 .or. &
			i == 33 .or. i == 48 .or. i == 51) then
			write(*, '(a1)', advance = 'yes') ' '
		end if

		! double new line
		if (i == 9 .or. i == 45) then
			write(*, '(a1)', advance = 'yes') ' '
			write(*, '(a1)', advance = 'yes') ' '
		end if

	end do

	write(*,*)

end subroutine cprint

!=======================================================================

subroutine apply(moves, state)
	integer :: state(26,2), state0(26,2), i, j
	integer, allocatable :: moves(:,:)

	! This subroutine applies moves to state and returns the
	! transformed state.

	! copy original state to temporary array
	state0(:,:) = state(:,:)

	! apply each move in order
	do i = 1, size(moves, 1)

		! apply correct number of 90 degree anticlockwise twists
		do j = 1, moves(i,2)

			! branch each possible face
			if (moves(i,1) == 1) then

				! RIGHT FACE
				! edge peices
				state(8,:) = state0(1,:)
				state(9,:) = state0(8,:)
				state(5,:) = state0(9,:)
				state(1,:) = state0(5,:)
				! corner peices
				state(20,:) = state0(16,:)
				state(17,:) = state0(20,:)
				state(13,:) = state0(17,:)
				state(16,:) = state0(13,:)

			else if (moves(i,1) == 2) then

				! UP FACE
				! edge positions
				state(2,:) = state0(1,:)
				state(3,:) = state0(2,:)
				state(4,:) = state0(3,:)
				state(1,:) = state0(4,:)
				! edge orientations
				state(1,2) = modulo(state(1,2), 2) + 1
				state(2,2) = modulo(state(2,2), 2) + 1
				! corner positions
				state(14,:) = state0(13,:)
				state(15,:) = state0(14,:)
				state(16,:) = state0(15,:)
				state(13,:) = state0(16,:)
				! corner orientations
				state(13,2) = modulo(modulo(state(13,2), 3)+1, 3) + 1
				state(14,2) = modulo(modulo(state(14,2), 3)+1, 3) + 1
				state(16,2) = modulo(modulo(state(16,2), 3)+1, 3) + 1

			else if (moves(i,1) == 3) then

				! LEFT FACE
				! edge positions
				state(6,:) = state0(3,:)
				state(11,:) = state0(6,:)
				state(7,:) = state0(11,:)
				state(3,:) = state0(7,:)
				! edge orientations
				state(6,2) = modulo(state(6,2), 2) + 1
				state(3,2) = modulo(state(3,2), 2) + 1
				! corner positions
				state(18,:) = state0(14,:)
				state(19,:) = state0(18,:)
				state(15,:) = state0(19,:)
				state(14,:) = state0(15,:)
				! corner orientations
				state(18,2) = modulo(modulo(state(18,2), 3)+1, 3) + 1
				state(15,2) = modulo(modulo(state(15,2), 3)+1, 3) + 1
				state(14,2) = modulo(modulo(state(14,2), 3)+1, 3) + 1

			else if (moves(i,1) == 4) then

				! DOWN FACE
				! edge positions
				state(11,:) = state0(12,:)
				state(10,:) = state0(11,:)
				state(9, :) = state0(10,:)
				state(12,:) = state0(9 ,:)
				! edge orientations
				state(11,2) = modulo(state(11,2), 2) + 1
				state(10,2) = modulo(state(10,2), 2) + 1
				state(9 ,2) = modulo(state(9 ,2), 2) + 1
				state(12,2) = modulo(state(12,2), 2) + 1
				! corner positions
				state(19,:) = state0(20,:)
				state(18,:) = state0(19,:)
				state(17,:) = state0(18,:)
				state(20,:) = state0(17,:)
				! corner orientations
				state(19,2) = modulo(state(19,2), 3)+1
				state(18,2) = modulo(modulo(state(18,2), 3)+1, 3) + 1
				state(17,2) = modulo(state(17,2), 3)+1
				state(20,2) = modulo(modulo(state(20,2), 3)+1, 3) + 1

			else if (moves(i,1) == 5) then

				! FRONT FACE
				! edge positions
				state(7,:) = state0(4,:)
				state(12,:) = state0(7,:)
				state(8,:) = state0(12,:)
				state(4,:) = state0(8,:)
				! corner positions
				state(15,:) = state0(16,:)
				state(19,:) = state0(15,:)
				state(20,:) = state0(19,:)
				state(16,:) = state0(20,:)
				! corner orientations
				state(16,2) = modulo(modulo(state(16,2), 3)+1, 3) + 1
				state(20,2) = modulo(state(20,2), 3)+1


			else if (moves(i,1) == 6) then

				! BACK FACE
				! edge positions
				state(5,:) = state0(2,:)
				state(10,:) = state0(5,:)
				state(6,:) = state0(10,:)
				state(2,:) = state0(6,:)
				! corner positions
				state(13,:) = state0(14,:)
				state(17,:) = state0(13,:)
				state(18,:) = state0(17,:)
				state(14,:) = state0(18,:)
				! corner orientations
				state(17,2) = modulo(modulo(state(17,2), 3)+1, 3) + 1
				state(18,2) = modulo(state(18,2), 3)+1

			else if (moves(i,1) == 7) then

				! M SLICE
				! edge positions
				state(12,:) = state0(10,:)
				state(10,:) = state0(2 ,:)
				state(2 ,:) = state0(4 ,:)
				state(4 ,:) = state0(12,:)
				! edge orientations
				state(10,2) = modulo(state(10,2), 2) + 1
				state(2 ,2) = modulo(state(2 ,2), 2) + 1
				state(4 ,2) = modulo(state(4 ,2), 2) + 1
				state(12,2) = modulo(state(12,2), 2) + 1
				! center positions
				state(25,:) = state0(24,:)
				state(24,:) = state0(26,:)
				state(26,:) = state0(22,:)
				state(22,:) = state0(25,:)

			else if (moves(i,1) == 8) then

				! E SLICE
				! edge positions
				state(8,:) = state0(5,:)
				state(5,:) = state0(6,:)
				state(6,:) = state0(7,:)
				state(7,:) = state0(8,:)
				! edge orientations
				state(5,2) = modulo(state(5,2), 2) + 1
				state(6,2) = modulo(state(6,2), 2) + 1
				state(7,2) = modulo(state(7,2), 2) + 1
				state(8,2) = modulo(state(8,2), 2) + 1
				! center positions
				state(25,:) = state0(21,:)
				state(21,:) = state0(26,:)
				state(26,:) = state0(23,:)
				state(23,:) = state0(25,:)

			else if (moves(i,1) == 9) then

				! S SLICE
				! edge positions
				state(1 ,:) = state0(9 ,:)
				state(9 ,:) = state0(11,:)
				state(11,:) = state0(3 ,:)
				state(3 ,:) = state0(1 ,:)
				! edge orientations
				state(1 ,2) = modulo(state(1 ,2), 2) + 1
				state(9 ,2) = modulo(state(9 ,2), 2) + 1
				! center positions
				state(22,:) = state0(21,:)
				state(21,:) = state0(24,:)
				state(24,:) = state0(23,:)
				state(23,:) = state0(22,:)

			end if

			state0(:,:) = state(:,:)

		end do

	end do

end subroutine apply

!=======================================================================

subroutine reverse(moves, rmoves)
	integer, allocatable :: moves(:,:), rmoves(:,:)
	integer :: i, n, rev(4)

	! This subroutine reverses (undoes) a series of moves.

	n = size(moves, 1)
	allocate(rmoves(n, 2))

	! opposites of 0, 1, 2, and 3 quarter turns
	rev = (/ 0, 3, 2, 1 /)

	do i = 1, n
		rmoves(n - i + 1, 1) = moves(i,1)
		rmoves(n - i + 1, 2) = rev(moves(i,2) + 1)
	end do

end subroutine reverse

!=======================================================================

subroutine readState(cstate)

	use textmod
	character :: cstate(54), line*100
	integer :: i, j, k

	! This subroutine prompts the user to enter the colors on each
	! side of the cube.  The state is then deduced.

	write(*,*)
	write(*,*) 'Enter the colors on each face as shown in the'  &
	  //' pattern below.  Spaces and indentation are not read,' &
	  //' however new lines must be entered as in the example.'
	write(*,*)
	write(*,*) '       w w w'
	write(*,*) '       w w w'
	write(*,*) '       w w w'
	write(*,*)
	write(*,*) 'r r r  b b b  o o o  g g g'
	write(*,*) 'r r r  b b b  o o o  g g g'
	write(*,*) 'r r r  b b b  o o o  g g g'
	write(*,*)
	write(*,*) '       y y y'
	write(*,*) '       y y y'
	write(*,*) '       y y y'
	write(*,*)

	j = 1

	do k = 1, 11
		read(*, '(a)') line
		do i = 1, len(trim(line))
			if (isAlpha(line(i:i))) then
				cstate(j) = line(i:i)
				j = j + 1
			end if
		end do
	end do

	! square numbers
	!
	!           1  2  3
	!           4  5  6
	!           7  8  9
	!
	! 10 11 12  13 14 15  16 17 18  19 20 21
	! 22 23 24  25 26 27  28 29 30  31 32 33
	! 34 35 36  37 38 39  40 41 42  43 44 45
	!
	!           46 47 48
	!           49 50 51
	!           52 53 54

end subroutine readState

!=======================================================================

subroutine c2n(cstate, nstate, cmap)

	character :: cstate(54), cmap*6
	integer :: nstate(54), i

	! This subroutine transforms a color state to an integer state
	! using a colormap.

	do i = 1, 54
		nstate(i) = index(cmap, cstate(i))
	end do

end subroutine c2n

!=======================================================================

subroutine n2p(nstate, state)

	integer :: nstate(54), i, j, corners(8,3), edges(12,2), nc(8,3), &
		ne(12,2), edge(2), corner(3), state(26,2)

	logical*1 :: found

	! This subroutine transforms an integer state to a position state.

	! colors (1-6) on each corner position (1-8)
	corners(1,:) = (/ 1, 6, 2 /)
	corners(2,:) = (/ 2, 6, 3 /)
	corners(3,:) = (/ 2, 3, 5 /)
	corners(4,:) = (/ 1, 2, 5 /)
	corners(5,:) = (/ 1, 4, 6 /)
	corners(6,:) = (/ 3, 6, 4 /)
	corners(7,:) = (/ 3, 4, 5 /)
	corners(8,:) = (/ 1, 5, 4 /)

	! colors (1-6) on each edge position (1-12)
	edges(1 ,:) = (/ 1, 2 /)
	edges(2 ,:) = (/ 2, 6 /)
	edges(3 ,:) = (/ 2, 3 /)
	edges(4 ,:) = (/ 2, 5 /)
	edges(5 ,:) = (/ 1, 6 /)
	edges(6 ,:) = (/ 3, 6 /)
	edges(7 ,:) = (/ 3, 5 /)
	edges(8 ,:) = (/ 1, 5 /)
	edges(9 ,:) = (/ 1, 4 /)
	edges(10,:) = (/ 4, 6 /)
	edges(11,:) = (/ 3, 4 /)
	edges(12,:) = (/ 4, 5 /)

	! nc(i,:) = (/ a, b, c /) where a, b, and c are the square numbers
	! that make up corner peice i, same ordering as in 'corners'
	nc(1,:) = (/ 18, 19,  3 /)
	nc(2,:) = (/ 1 , 21, 10 /)
	nc(3,:) = (/ 7 , 12, 13 /)
	nc(4,:) = (/ 16,  9, 15 /)
	nc(5,:) = (/ 42, 54, 43 /)
	nc(6,:) = (/ 34, 45, 52 /)
	nc(7,:) = (/ 36, 46, 37 /)
	nc(8,:) = (/ 40, 39, 48 /)

	! ne(i,:) = (/ a, b /) where a and b are the square numbers that
	! make up edge peice i, same ordering as in 'edges'
	ne(1 ,:) = (/ 17,  6 /)
	ne(2 ,:) = (/ 2 , 20 /)
	ne(3 ,:) = (/ 4 , 11 /)
	ne(4 ,:) = (/ 8 , 14 /)
	ne(5 ,:) = (/ 30, 31 /)
	ne(6 ,:) = (/ 22, 33 /)
	ne(7 ,:) = (/ 24, 25 /)
	ne(8 ,:) = (/ 28, 27 /)
	ne(9 ,:) = (/ 41, 51 /)
	ne(10,:) = (/ 53, 44 /)
	ne(11,:) = (/ 35, 49 /)
	ne(12,:) = (/ 47, 38 /)

	! determine which peices are in which positions to assign a state
	! edges
	do i = 1, 12

		! colors (represented as integers) ocupying edge position i
		edge = (/ nstate(ne(i,1)), nstate(ne(i,2)) /)
		j = 0
		found = .false.

		do while (.not. found)
			j = j + 1
			if (edge(1) == edges(j,1) .and. edge(2) == edges(j,2)) then
				found = .true.
				state(i,:) = (/ j, 1 /)
			else if (edge(1)==edges(j,2) .and. edge(2)==edges(j,1)) then
				found = .true.
				state(i,:) = (/ j, 2 /)
			end if
		end do

	end do

	! corners
	do i = 1, 8

		! colors ocupying corner position i
		corner = (/ nstate(nc(i,1)), nstate(nc(i,2)), nstate(nc(i,3)) /)
		j = 0
		found = .false.

		do while (.not. found)
			j = j + 1
			if (corner(1) == corners(j,1) .and. &
			    corner(2) == corners(j,2) .and. &
			    corner(3) == corners(j,3)) then
				found = .true.
				state(i+12,:) = (/ j, 1 /)
			else if (corner(2) == corners(j,1) .and. &
			         corner(3) == corners(j,2) .and. &
			         corner(1) == corners(j,3)) then
				found = .true.
				state(i+12,:) = (/ j, 3 /)
			else if (corner(3) == corners(j,1) .and. &
			         corner(1) == corners(j,2) .and. &
			         corner(2) == corners(j,3)) then
				found = .true.
				state(i+12,:) = (/ j, 2 /)
			end if
		end do

	end do

	! centers
	state(21,:) = (/ nstate(29), 1 /)
	state(22,:) = (/ nstate(5 ), 1 /)
	state(23,:) = (/ nstate(23), 1 /)
	state(24,:) = (/ nstate(50), 1 /)
	state(25,:) = (/ nstate(26), 1 /)
	state(26,:) = (/ nstate(32), 1 /)

end subroutine n2p

!=======================================================================

subroutine p2n(state, nstate)

	integer :: nstate(54), i, corners(8,3), edges(12,2), nc(8,3), &
		ne(12,2), edge(2), corner(3), state(26,2)

	! This subroutine transforms a position state to an integer state.

	! faces (1-6) on each corner position (1-8)
	corners(1,:) = (/ 1, 6, 2 /)
	corners(2,:) = (/ 2, 6, 3 /)
	corners(3,:) = (/ 2, 3, 5 /)
	corners(4,:) = (/ 1, 2, 5 /)
	corners(5,:) = (/ 1, 4, 6 /)
	corners(6,:) = (/ 3, 6, 4 /)
	corners(7,:) = (/ 3, 4, 5 /)
	corners(8,:) = (/ 1, 5, 4 /)

	! faces (1-6) on each edge position (1-12)
	edges(1 ,:) = (/ 1, 2 /)
	edges(2 ,:) = (/ 2, 6 /)
	edges(3 ,:) = (/ 2, 3 /)
	edges(4 ,:) = (/ 2, 5 /)
	edges(5 ,:) = (/ 1, 6 /)
	edges(6 ,:) = (/ 3, 6 /)
	edges(7 ,:) = (/ 3, 5 /)
	edges(8 ,:) = (/ 1, 5 /)
	edges(9 ,:) = (/ 1, 4 /)
	edges(10,:) = (/ 4, 6 /)
	edges(11,:) = (/ 3, 4 /)
	edges(12,:) = (/ 4, 5 /)

	! nc(i,:) = (/ a, b, c /) where a, b, and c are the square numbers
	! that make up corner peice i, same ordering as in 'corners'
	nc(1,:) = (/ 18, 19,  3 /)
	nc(2,:) = (/ 1 , 21, 10 /)
	nc(3,:) = (/ 7 , 12, 13 /)
	nc(4,:) = (/ 16,  9, 15 /)
	nc(5,:) = (/ 42, 54, 43 /)
	nc(6,:) = (/ 34, 45, 52 /)
	nc(7,:) = (/ 36, 46, 37 /)
	nc(8,:) = (/ 40, 39, 48 /)

	! ne(i,:) = (/ a, b /) where a and b are the square numbers that
	! make up edge peice i, same ordering as in 'edges'
	ne(1 ,:) = (/ 17,  6 /)
	ne(2 ,:) = (/ 2 , 20 /)
	ne(3 ,:) = (/ 4 , 11 /)
	ne(4 ,:) = (/ 8 , 14 /)
	ne(5 ,:) = (/ 30, 31 /)
	ne(6 ,:) = (/ 22, 33 /)
	ne(7 ,:) = (/ 24, 25 /)
	ne(8 ,:) = (/ 28, 27 /)
	ne(9 ,:) = (/ 41, 51 /)
	ne(10,:) = (/ 53, 44 /)
	ne(11,:) = (/ 35, 49 /)
	ne(12,:) = (/ 47, 38 /)

	! assign each square (1-54) an integer (1-6)
	! edges
	do i = 1, 12
		edge = (/ edges(state(i,1), 1), edges(state(i,1), 2) /)
		if (state(i,2) == 1) then
			nstate(ne(i,1)) = edge(1)
			nstate(ne(i,2)) = edge(2)
		else
			nstate(ne(i,1)) = edge(2)
			nstate(ne(i,2)) = edge(1)
		end if
	end do

	! corners
	do i = 1, 8
		corner(1) = corners(state(i+12,1), 1)
		corner(2) = corners(state(i+12,1), 2)
		corner(3) = corners(state(i+12,1), 3)
		if (state(i+12,2) == 1) then
			nstate(nc(i,1)) = corner(1)
			nstate(nc(i,2)) = corner(2)
			nstate(nc(i,3)) = corner(3)
		else if (state(i+12,2) == 2) then
			nstate(nc(i,1)) = corner(2)
			nstate(nc(i,2)) = corner(3)
			nstate(nc(i,3)) = corner(1)
		else if (state(i+12,2) == 3) then
			nstate(nc(i,1)) = corner(3)
			nstate(nc(i,2)) = corner(1)
			nstate(nc(i,3)) = corner(2)
		end if
	end do

	! centers
	nstate(29) = state(21,1)
	nstate(5)  = state(22,1)
	nstate(23) = state(23,1)
	nstate(50) = state(24,1)
	nstate(26) = state(25,1)
	nstate(32) = state(26,1)

	!print *, nstate(:)

end subroutine p2n

!=======================================================================

subroutine n2c(nstate, cstate, cmap)
	character :: cstate(54), cmap*6
	integer :: nstate(54), i

	! This subroutine transforms a color state to an integer state
	! using a colormap.

	do i = 1, 54
		cstate(i) = cmap(nstate(i): nstate(i))
	end do

end subroutine n2c

!=======================================================================

subroutine readMoves(moves)
	use textmod
	character :: line*1000
	integer, allocatable :: moves(:,:)

	! This subroutine prompts the user to enter a series of moves.
	! These are converted to an array and stored in moves.

	write(*,*)
	write(*,*) "Enter the moves using the characters R, L, U, D, F," &
		//" B, ', and 2, separated by spaces:"
	read(*,'(a)') line
	write(*,*)

	call convert(line, moves)

end subroutine readMoves

!=======================================================================

subroutine convert(hmoves, moves)
	use textmod
	character :: hmoves*1000, fmap*6, tmap*3, spaces*1000
	integer, allocatable :: moves(:,:)
	integer :: i, j, n

	! This subroutine converts human readable moves to an array of
	! moves.

	! map face letters to numbers
	fmap = 'RULDFB'

	! map twist amount to numbers
	tmap = "'2 "

	call catx(' ', 1000, spaces)

	! determine number of moves
	n = 0
	do i = 1, len(trim(hmoves))
		if (isAlpha(hmoves(i:i))) then
			n = n + 1
		end if
	end do

	! convert human readable moves to array
	allocate(moves(n,2))
	j = 1
	do i = 1, n

		! skip to next move
		do while(.not. isAlpha(hmoves(j:j)))
			j = j + 1
		end do

		! determine which face is turned
		moves(i,1) = index(fmap, hmoves(j:j))

		if (moves(i,1) == 0) then
			write(*,*)
			write(*,'(a36,i4)')'ERROR: unrecognized input in column ',j
			write(*,*)
			write(*,*) trim(hmoves)
			write(*,*) spaces(1: j - 1)//'^'
			write(*,*)
			stop
		end if

		j = j + 1

		moves(i,2) = index(tmap, hmoves(j:j))

		if (moves(i,2) == 0) then
			write(*,*)
			write(*,'(a36,i4)')'ERROR: unrecognized input in column ',j
			write(*,*)
			write(*,*) trim(hmoves)
			write(*,*) spaces(1: j - 1)//'^'
			write(*,*)
			stop
		end if

		j = j + 1

	end do

end subroutine convert

!=======================================================================

subroutine game(s)

	integer :: state0(26,2), state(26,2), nstate(54), i, n, &
		orientedState(26,2)

	integer, allocatable :: moves(:,:)

	character :: cmap*6, cstate(54), sn*10

	type(rubik_settings) :: s

	! This subroutine scrambles a cube, displays the scrambled state,
	! and challenges the user to solve it by entering moves in text
	! mode.

	!print *, 'iseed = ', s%iseed

	! solved state
	state0( 1:12, 1) = (/ (i, i = 1, 12) /)
	state0(13:20, 1) = (/ (i, i = 1,  8) /)
	state0(21:26, 1) = (/ (i, i = 1,  6) /)
	state0(:,2) = 1
	state(:,:) = state0(:,:)

	! color map
	cmap = 'owrybg'

	call p2n(state, nstate)
	call n2c(nstate, cstate, cmap)
	!call cprint(cstate)
	call multiView(cstate)
	write(*,*)
	write(*,*)

	call scramble(s, moves, 50)
	call apply(moves, state)
	!call render(moves)
	call orient(state)
	orientedState(:,:) = state(:,:)
	call p2n(state, nstate)
	call n2c(nstate, cstate, cmap)
	!call cprint(cstate)
	call multiView(cstate)
	n = 0

	do while (any(orientedState /= state0))

		!! The following block provides basic functionality for turning
		!! individual faces, but does not allow cube rotations,
		!! "lowercase" moves, slice moves, etc.
		!deallocate(moves)
		!call readMoves(moves)
		!call apply(moves, state)
		!call p2n(state, nstate)
		!call n2c(nstate, cstate, cmap)
		!!call cprint(cstate)
		!call multiView(cstate)
		!n = n + size(moves, 1)

		!print *, cmap
		deallocate(moves)
		call advancedMoves(moves)
		!print *, cmap
		call apply(moves, state)
		call p2n(state, nstate)
		call n2c(nstate, cstate, cmap)
		!call cprint(cstate)
		call multiView(cstate)

		orientedState(:,:) = state(:,:)
		call orient(orientedState)

	end do

	write(sn, '(i10)') n

	write(*,*)
	write(*,*) "Congratulations!  You just solved Rubik's cryptic" &
		//" cube in only ", trim(adjustl(sn)), " moves!"

end subroutine game

!=======================================================================

subroutine advancedMoves(moves)

	use textmod

	character :: line*1000

	integer, allocatable :: moves(:,:)

	! This subroutine provides more advanced functionality than
	! readMoves and apply.  Lowercase moves, cube rotations, slice
	! moves, undo, etc. are allowed.

	write(*,*)
	write(*,*) 'Enter moves, or type "h" for help:'
	read(*, '(a)') line

	line = adjustl(line)

	if (line(1:1) == 'h') then

		write(*,*)
		write(*,*) 'The faces of the cube are labelled right, up, '     &
		  //'left, down, front, and back.  The case sensitive letters ' &
		  //'R, U, L, D, F, and B will turn their respective face '     &
		  //'clockwise as if you were looking at that face.'
		write(*,*)
		write(*,*) 'The cube can be solved using only those six '      &
		  //'commands.  You may enter a series of moves on one line, ' &
		  //'separated by spaces (max 1000 characters).'
		write(*,*)
		write(*,*) 'To speed things up, you may append any of those ' &
		  //"six commands with an apostrophe (') to turn the face "   &
		  //'counterclockwise, or a number two (2) to turn the face ' &
		  //'twice.'
		write(*,*)
		write(*,*) 'To rotate the whole cube about an axis, enter '   &
		  //'x, y, or z.'
		write(*,*)

		! TODO:  add help on lowercase moves (ruldfb) for twisting two
		! layers at a time, MES moves, ...

		write(*,*) 'Press enter to continue ...'
		read(*, '(a)') line

	end if

	call convertAdvancedMoves(line, moves)

end subroutine advancedMoves

!=======================================================================

subroutine convertAdvancedMoves(hmoves, moves)

	use textmod

	character :: fmap*18, tmap*3, spaces*1000, hmoves*(*)

	integer :: i, j, k, m, n, twoMap(2,6), rotMap(6,3)

	integer, allocatable :: moves(:,:)

	! This subroutine provides more advanced functionality than
	! readMoves and apply.  Lowercase moves, cube rotations, slice
	! moves, undo, etc. are allowed.

	! map face letters to numbers
	fmap = 'RULDFBMESruldfbxyz'

	! map two layer moves to individual slice moves
	! columns 1 indicates the face to be turned,
	! columns 2 indicates the twist amount
	twoMap(:,1) = (/ 7, 3 /)
	twoMap(:,2) = (/ 8, 3 /)
	twoMap(:,3) = (/ 7, 1 /)
	twoMap(:,4) = (/ 8, 1 /)
	twoMap(:,5) = (/ 9, 1 /)
	twoMap(:,6) = (/ 9, 3 /)

	! map cube rotations to individual slices (odd columns) and twist
	! amounts (even columns)
	rotMap(:,1) = (/ 1, 1, 7, 3, 3, 3 /)
	rotMap(:,2) = (/ 2, 1, 8, 3, 4, 3 /)
	rotMap(:,3) = (/ 5, 1, 9, 1, 6, 3 /)

	! map twist amount to numbers
	tmap = "'2 "

	call catx(' ', 1000, spaces)

	! determine number of moves
	n = 0
	do i = 1, len(trim(hmoves))
		if (isAlpha(hmoves(i:i))) then
			j = index(fmap, hmoves(i:i))
			if (j > 0  .and. j <= 9)  n = n + 1  ! face and slice moves
			if (j > 9  .and. j <= 15) n = n + 2  ! two layer moves
			if (j > 15 .and. j <= 18) n = n + 3  ! cube rotations
		end if
	end do

	if (allocated(moves)) deallocate(moves)

	! convert human readable moves to array
	allocate(moves(n,2))
	j = 1
	i = 1
	do while (i <= n)

		! skip to next move
		do while(.not. isAlpha(hmoves(j:j)))
			j = j + 1
		end do

		! determine which face(s) is turned
		k = index(fmap, hmoves(j:j))

		! determine twist amount
		m = index(tmap, hmoves(j+1:j+1))

		if (k == 0) then
			write(*,*)
			write(*,'(a36,i4)')'ERROR: unrecognized input in column ',j
			write(*,*)
			write(*,*) trim(hmoves)
			write(*,*) spaces(1: j - 1)//'^'
			write(*,*)
			return
		end if

		if (m == 0) then
			write(*,*)
			write(*,'(a36,i4)')'ERROR: unrecognized input in column',j+1
			write(*,*)
			write(*,*) trim(hmoves)
			write(*,*) spaces(1: j)//'^'
			write(*,*)
			return
		end if

		j = j + 2

		if (k > 0 .and. k <= 9) then

			! face and slice moves
			moves(i,1) = k
			moves(i,2) = m
			i = i + 1

		else if (k > 9 .and. k <= 15) then

			! two layer moves
			moves(i,1) = k - 9
			moves(i,2) = m

			moves(i+1,1) = twoMap(1,k-9)
			moves(i+1,2) = modulo(m*twoMap(2,k-9), 4)

			i = i + 2

		else if (k > 15 .and. k <= 18) then

			! cube rotations
			moves(i,1) = rotMap(1,k-15)
			moves(i,2) = m

			moves(i+1,1) = rotMap(3,k-15)
			moves(i+1,2) = modulo(m*rotMap(4,k-15), 4)

			moves(i+2,1) = rotMap(5,k-15)
			moves(i+2,2) = modulo(m*rotMap(6,k-15), 4)

			i = i + 3

		end if

	end do

end subroutine convertAdvancedMoves

!=======================================================================

subroutine printCube(w, h, t)

	integer :: w, h, t, indent, i, j, k, m

	! This subroutine prints a 3d cube with width w, height h, and
	! thickness t for each cubie.

	! This subroutine will be extremely difficult to write for any
	! general w, h, and t.

	!w = 6
	!h = 4
	!t = 3

	indent = 8

	! UP BACK EDGE
	! leading spaces
	do i = 1, indent + 3*w
		write(*, '(a1)', advance = 'no') ' '
	end do

	! edge
	do i = 1, 3 * (w + 1)
		write(*, '(a1)', advance = 'no') '_'
	end do

	write(*,*)

	! UP FACE AND UPPER PART OF RIGHT FACE
	do m = 1, 3

		! BACK ROW
		do j = 1, t

			! leading space
			do i = 1, indent + 3*w - j - t * (m - 1)
				write(*, '(a1)', advance = 'no') ' '
			end do

			do k = 1, 3
				write(*, '(a1)', advance = 'no') '/'
				do i = 1, w
					if (j == t) then
						write(*, '(a1)', advance = 'no') '_'
					else
						write(*, '(a1)', advance = 'no') ' '
					end if
				end do
			end do

			! up right edge
			write(*, '(a1)', advance = 'no') '/'

			do i = 1, j - 1
				write(*, '(a1)', advance = 'no') ' '
			end do

			! right face vertical edges
			if (j == t - 1 .and. m == 2) then
				write(*, '(a1)', advance = 'no') '/'
			else
				write(*, '(a1)', advance = 'no') '|'
			end if
			! j == 5 (== t - 1), m == 2

			do i = 1, m - 1
				do k = 1, t - 1
					if (j + k + (i-1)*t == h + 1) then
						! right face horizontal edges
						write(*, '(a1)', advance = 'no') '/'
					else
						write(*, '(a1)', advance = 'no') ' '
					end if
				end do
				! back right edge
				if (j == 2 .and. m == 3 .and. i == 1) then
					write(*, '(a1)', advance = 'no') '/'
				else
					write(*, '(a1)', advance = 'no') '|'
				end if
				!write(*, '(i1)', advance = 'no') m
			end do

			write(*,*)

		end do

	end do

end subroutine printCube

!=======================================================================

subroutine multiView(c)

	use textmod
	character :: c(54), spaces*100

	! This subroutine prints 3D multiviews providing a view of each
	! face of the cube.

	call catx(' ', 100, spaces)

	! right-up-front view and back-bottom-left view
	write(*,*)
	write(*, '(a28)', advance = 'no') spaces
	write(*,*) 'UP'
	write(*,*) '                    '//c(21)//'        '//c(20)
	write(*, '(a17)', advance = 'no') spaces
	write(*,*) '___________________________ '//c(19)
	write(*, '(a16)', advance = 'no') spaces
	write(*,*) '/        /        /        /|'
	write(*, '(a14)', advance = 'no') spaces
	write(*,*) c(10)//'/   '//c(1)//'    /   '//c(2)//'    /   '//c(3) &
		//'    / |'
	write(*, '(a14)', advance = 'no') spaces
	write(*,*) '/        /        /        /  |'
	write(*, '(a13)', advance = 'no') spaces
	write(*,*) '/________/________/________/   |'
	write(*, '(a12)', advance = 'no') spaces
	write(*,*)'/        /        /        /|   |                    y'
	write(*, '(a10)', advance = 'no') spaces
	write(*,*) c(11)//'/   '//c(4)//'    /   '//c(5)//'    /   '//c(6) &
		//'    / | '//c(18)//' |                     |'
	write(*, '(a10)', advance = 'no') spaces
	write(*,*) '/        /        /        /  |   |                  ' &
		//'   |___ x'
	write(*, '(a9)', advance = 'no') spaces
	write(*,*) '/________/________/________/   |  /|                 ' &
		//'   /'
	write(*, '(a8)', advance = 'no') spaces
	write(*,*) '/        /        /        /| '//c(17)//' | / |      ' &
		//'             /'
	write(*, '(a7)', advance = 'no') spaces
	write(*,*) '/   '//c(7)//'    /   '//c(8)//'    /   '//c(9)//     &
		'    / |   |/  |'//c(31)//'                z'
	write(*, '(a6)', advance = 'no') spaces
	write(*,*) '/        /        /        /  |   /   |     RIGHT'
	write(*, '(a4)', advance = 'no') spaces
	write(*,*) c(12)//'/________/________/________/   |  /|   |'
	write(*, '(a5)', advance = 'no') spaces
	write(*,*) '|        |        |        | '//c(16)//' | / | '//    &
		c(30)//' |'
	write(*, '(a5)', advance = 'no') spaces
	write(*,*) '|        |        |        |   |/  |  /|'
	write(*, '(a5)', advance = 'no') spaces
	write(*,*) '|   '//c(13)//'    |   '//c(14)//'    |   '//c(15)//  &
		'    |   /   | / |'
	write(*, '(a5)', advance = 'no') spaces
	write(*,*) '|        |        |        |  /| '//c(29)//' |/  |' &
		//c(43)
	write(*, '(a5)', advance = 'no') spaces
	write(*,*) '|        |        |        | / |   /   |          ' &
		//'          DOWN'
	write(*, '(a5)', advance = 'no') spaces
	write(*,*) '|________|________|________|/  |  /|   |'
	write(*, '(a5)', advance = 'no') spaces
	write(*,*) '|        |        |        |   | / | '//c(42)//' |'   &
		//'             '//c(40)//'       '//c(41)
	write(*,*) '     |        |        |        | '//c(28)//' |/  | ' &
		//' /          ___________________________ '//c(42)
	write(*,*) '    '//c(24)//'|   '//c(25)//'    |   '//c(26)//'    ' &
		//'|   '//c(27)//'    |   /   | /'//c(54)//'         /        ' &
		//'/        /        /|'
	write(*,*) '     |        |        |        |  /| '//c(41)//' |/' &
		//'         '//c(39)//'/   '//c(48)//'    /   '//c(51)//'   ' &
		//' /   '//c(54)//'    / |'
	write(*,*) '     |        |        |        | / |   /          /' &
		//'        /        /        /  |'
	write(*,*) '     |________|________|________|/  |  /          /_' &
		//'_______/________/________/   |'
	write(*,*) '     |        |        |        |   | /'//c(51)//'  ' &
		//'       /        /        /        /|   |'
	write(*,*) '     |        |        |        | '//c(40)//' |/    ' &
		//'     '//c(38)//'/   '//c(47)//'    /   '//c(50)//'    /   ' &
		//c(53)//'    / | '//c(43)//' |'
	write(*,*) '    '//c(36)//'|   '//c(37)//'    |   '//c(38)//'   ' &
		//' |   '//    &
		c(39)//'    |   /          /        /        /        /  |   |'
	write(*,*) '     |        |        |        |  /          /_____' &
		//'___/________/________/   |  /|'
	write(*,*) '     |        |        |        | /          /      ' &
		//'  /        /        /| '//c(44)//' | / |'
	write(*,*) '     |________|________|________|/          /   '//   &
		c(46)//'    /   '//c(49)//'    /   '//c(52)//'    / |   |/  |' &
		//c(30)
	write(*,*) '         '//c(46)//'        '//c(47)//'           '// &
		'  '//c(48)//'          /        ' &
		//'/        /        /  |   /   |   BACK'
	write(*,*) '                                         '//c(37)// &
		'/________/________/________/   |  /|   |'
	write(*,*) '                FRONT                     |        |' &
		//'        |        | '//c(45)//' | / | '//c(31)//' |'
	write(*,*) '                                          |        |' &
		//'        |        |   |/  |  /|'
	write(*,*) '                                          |   '//     &
		c(36)//'    |   '//c(35)//'    |   '//c(34)//'    |   /   | / |'
	write(*,*) '                                          |        |' &
		//'        |        |  /| '//c(32)//' |/  |'//c(18)
	write(*,*) '                                          |        |' &
		//'        |        | / |   /   |'
	write(*,*) '                                          |________|' &
		//'________|________|/  |  /|   |'
	write(*,*) '                                          |        |' &
		//'        |        |   | / | '//c(19)//' |'
	write(*,*) '                                          |        |' &
		//'        |        | '//c(33)//' |/  |  /'
	write(*,*) '                                         '//c(25)// &
		'|   '//c(24)//'    |   '//c(23)//'    |   '//c(22)//'    |  ' &
		//' /   | /'//c(1)
	write(*,*) '                                          |        |' &
		//'        |        |  /| '//c(20)//' |/'
	write(*,*) '                                          |        |' &
		//'        |        | / |   /'
	write(*,*) '                                LEFT      |________|' &
		//'________|________|/  |  /'
	write(*,*) '                                          |        |' &
		//'        |        |   | /'//c(2)
	write(*,*) '                                          |        |' &
		//'        |        | '//c(21)//' |/'
	write(*,*) '                                         '//c(13)// &
		'|   '//c(12)//'    |   '//c(11)//'    |   '//c(10)//'    |   /'
	write(*,*) '                                          |        |' &
		//'        |        |  /'
	write(*,*) '                                          |        |' &
		//'        |        | /'
	write(*,*) '                                          |________|' &
		//'________|________|/'
	write(*,*) '                                              '//c(7) &
		//'        '//c(4)//'              '//c(1)

end subroutine multiView

!=======================================================================

subroutine orient(state)

	character :: hmoves*2

	integer :: state(26,2)

	integer, allocatable :: moves(:,:)

	! This subroutine orients the cube so the white center is on the
	! up face and the blue center is on the front face.

	! orient up face
	if (state(22,1) /= 2) then

		if (state(21,1) == 2) then
			hmoves = "z'"
		else if (state(23,1) == 2) then
			hmoves = "z "
		else if (state(24,1) == 2) then
			hmoves = "z2"
		else if (state(25,1) == 2) then
			hmoves = "x "
		else if (state(26,1) == 2) then
			hmoves = "x'"
		end if

		call convertAdvancedMoves(hmoves, moves)
		call apply(moves, state)

	end if

	! orient front face
	do while (state(25,1) /= 5)
		hmoves = "y"
		call convertAdvancedMoves(hmoves, moves)
		call apply(moves, state)
	end do

end subroutine orient

!=======================================================================

subroutine cycleSides()

	character :: hmoves*20, cmap*6

	integer :: state(26,2), state0(26,2), i

	integer, allocatable :: moves(:,:)

	! solved state
	state0( 1:12, 1) = (/ (i, i = 1, 12) /)
	state0(13:20, 1) = (/ (i, i = 1,  8) /)
	state0(21:26, 1) = (/ (i, i = 1,  6) /)
	state0(:,2) = 1
	state(:,:) = state0(:,:)

	cmap = 'owrybg'

	hmoves = "R L' B F' R' L B' F"
	call convertAdvancedMoves(hmoves, moves)
	call apply(moves, state)

	i = 4

	do while (any(state(:,:) /= state0(:,:)))
		call apply(moves, state)
		i = i + 4
	end do

	print *, i

end subroutine cycleSides

!=======================================================================

end module rubik

