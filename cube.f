
      program cube

      use cubemod

      implicit none

      character :: smoves*500, cstate(54), cmap*6
      integer, allocatable :: moves(:,:), rmoves(:,:)
      integer :: i, state(26,2), j, nstate(54)

      ! state(i,:) = (/ a, b /)
      ! where i is the position, a is the peice, and b is its orientation

      ! initial (solved) cube state:
      ! edge piece positions and orientations:
      state(1 ,:) = (/ 1 , 1 /)
      state(2 ,:) = (/ 2 , 1 /)
      state(3 ,:) = (/ 3 , 1 /)
      state(4 ,:) = (/ 4 , 1 /)
      state(5 ,:) = (/ 5 , 1 /)
      state(6 ,:) = (/ 6 , 1 /)
      state(7 ,:) = (/ 7 , 1 /)
      state(8 ,:) = (/ 8 , 1 /)
      state(9 ,:) = (/ 9 , 1 /)
      state(10,:) = (/ 10, 1 /)
      state(11,:) = (/ 11, 1 /)
      state(12,:) = (/ 12, 1 /)
      ! corner piece positions and orientations:
      state(13,:) = (/ 1 , 1 /)
      state(14,:) = (/ 2 , 1 /)
      state(15,:) = (/ 3 , 1 /)
      state(16,:) = (/ 4 , 1 /)
      state(17,:) = (/ 5 , 1 /)
      state(18,:) = (/ 6 , 1 /)
      state(19,:) = (/ 7 , 1 /)
      state(20,:) = (/ 8 , 1 /)
      ! center piece positions:
      state(21,:) = (/ 1 , 1 /)
      state(22,:) = (/ 2 , 1 /)
      state(23,:) = (/ 3 , 1 /)
      state(24,:) = (/ 4 , 1 /)
      state(25,:) = (/ 5 , 1 /)
      state(26,:) = (/ 6 , 1 /)

      ! square numbers
      !           1  2  3
      !           4  5  6
      !           7  8  9
      ! 10 11 12  13 14 15  16 17 18  19 20 21
      ! 22 23 24  25 26 27  28 29 30  31 32 33
      ! 34 35 36  37 38 39  40 41 42  43 44 45
      !           46 47 48
      !           49 50 51
      !           52 53 54
      ! Face numbers are shown below:
      !         2
      !      3  5  1  6
      !         4
      ! Edge position numbers are shown below:
      !          2  
      !        3   1
      !          4  
      !   3      4      1      2  
      ! 6   7  7   8  8   5  5   6
      !   11     12     9      10 
      !          12  
      !        11  9
      !          10  
      ! Corner position numbers are shown below:
      !        2   1
      !        3   4
      ! 2   3  3   4  4   1  1   2
      ! 6   7  7   8  8   5  5   6
      !        7   8
      !        6   5

      cmap = 'owrybg'
      !! corner(i,:) = (/ f1, f2, f3 /)
      !! where f1 - f3 are the face numbers on corner position number i
      !! f1 is the face with the lowest number.  The rest of the faces
      !! cycle right-hand rule out the cube.
      !corners(1,:) = (/ 1, 6, 2 /)
      !corners(2,:) = (/ 2, 6, 3 /)
      !corners(3,:) = (/ 2, 3, 5 /)
      !corners(4,:) = (/ 1, 2, 5 /)
      !corners(5,:) = (/ 1, 4, 6 /)
      !corners(6,:) = (/ 3, 6, 4 /)
      !corners(7,:) = (/ 3, 4, 5 /)
      !corners(8,:) = (/ 1, 5, 4 /)
      !! edges(i,:) = (/ f1, f2 /)
      !edges(1 ,:) = (/ 1, 2 /)
      !edges(2 ,:) = (/ 2, 6 /)
      !edges(3 ,:) = (/ 2, 3 /)
      !edges(4 ,:) = (/ 2, 5 /)
      !edges(5 ,:) = (/ 1, 6 /)
      !edges(6 ,:) = (/ 3, 6 /)
      !edges(7 ,:) = (/ 3, 5 /)
      !edges(8 ,:) = (/ 1, 5 /)
      !edges(9 ,:) = (/ 1, 4 /)
      !edges(10,:) = (/ 4, 6 /)
      !edges(11,:) = (/ 3, 4 /)
      !edges(12,:) = (/ 4, 5 /)

      !call scramble(moves, 25)
      !do i = 1, 25
      !    print *, moves(i, :)
      !end do

      !call render(moves, smoves)

      !call cprint(state)

      ! checkerboard pattern
      !allocate(moves(6,2))
      !moves(1,:) = (/ 1, 2 /)
      !moves(2,:) = (/ 3, 2 /)
      !moves(3,:) = (/ 2, 2 /)
      !moves(4,:) = (/ 4, 2 /)
      !moves(5,:) = (/ 5, 2 /)
      !moves(6,:) = (/ 6, 2 /)

      !call scramble(moves, 20)
      !call apply(state, moves)
      !call render(moves)

      !call p2n(state, nstate)
      !call n2c(nstate, cstate, cmap)
      !!call cprint(cstate)
      !call multiView(cstate)
      !call c2n(cstate, nstate, cmap)
      !call n2p(nstate, state)
      !call p2n(state, nstate)
      !call n2c(nstate, cstate, cmap)
      !call multiView(cstate)

      !call reverse(moves, rmoves)
      !call render(rmoves)

      !call readState(state)

      !call readState(cstate, cmap)
      !call cprint(cstate)
      !call c2n(cstate, nstate, cmap)
      !call n2p(nstate, state)

      !call readMoves(moves)
      !call render(moves)

      !call multiView(cstate)

      !allocate(moves(1,2))
      !moves(1,:) = (/ 7, 1 /)
      !call apply(state, moves)
      !call p2n(state, nstate)
      !call n2c(nstate, cstate, cmap)
      !write(*,*)
      !call cprint(cstate)

      call game()
      !call cycleSides()

      end program cube

