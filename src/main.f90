
program main

	use rubik

	implicit none

	integer :: io
	type(rubik_settings) :: settings

	io = 0
	call load_args(settings, io)
	call game(settings)

end program main

