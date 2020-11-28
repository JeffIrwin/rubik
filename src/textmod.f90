
module textmod

contains

!=======================================================================

logical*1 function isAlpha(c)

	implicit none
	character :: c
	integer :: ic

	! This subroutine determines if a character c is alphabetic or not.

	isAlpha = .true.
	ic = ichar(c)
	if (ic < 65 .or. ic > 122 .or. (ic > 90 .and. ic < 97)) then
		isAlpha = .false.
	end if

end function isAlpha

!=======================================================================

subroutine catx(word, n, wordx)

	implicit none
	character :: word*(*), wordx*(*)
	integer :: n, i

	if (n == 0) then
		wordx = ''
	end if

	wordx = word

	do i = 2, n
		wordx = word//wordx
	end do

end subroutine catx

!=======================================================================

end module textmod

