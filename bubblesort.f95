program bubblesort
    ! Nicholas Maynard
    ! CSI 501
    ! Lab 4
    ! 02/23/2023
    ! This program implements the bubble sort algorithm for analysis.
 
    implicit none

    character*50 :: InFile
    real, allocatable :: A(:)
    real :: Temp
    integer :: i, j, n

    ! Ask user for File input
    print*, 'Enter input file: '
    read(*,*) InFile
    open(42,file=InFile)
    open(13,file=('sort' // trim(InFile)))

    !Skip a header line
    read(42,*)
    ! Read in size of file
    read(42,*) n 
    ! Allocate size of array based on file
    allocate(A(n))
    ! Import Array
    do i = 1, n
        read(42,*) A(i)
    enddo

    ! Run Bubble Sort algorithm.
    do i = 1, n
        do j = 1, n - 1
            if (A(j) .gt. A(j+1)) then
                Temp = A(j)
                A(j) = A(j+1)
                A(j+1) = Temp
            endif
        enddo
    enddo

    ! Output Results
    do i = 1, n
        write(13,*) A(i)
    enddo 

    ! Deallocate Memory
    deallocate(A)
       
 end program bubblesort
 