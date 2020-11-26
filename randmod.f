
	module randmod

	contains

        double precision function random()

			! This function generates a random real number
			! between 0 and 1.

			implicit none

			integer, dimension(8) :: values
			integer :: t

			logical, save :: init = .false.

			if (.not. init) then
				init = .true.

				call date_and_time(values = values)

				!print *, 'values = ', values

				t = values(8) + 1000 * (values(7) + 60 * (values(6) + 60
     &              * (values(5) + 24 * values(3))))

				!print *, 't = ', t

				call srand(t)
				!call srand(0)

			end if

			random = rand()

		end function

	end module

