program prototype_main
use prototype_module, only: file_t, folder_t, inode_t
implicit none
type(file_t), target :: file1, file2, file3
type(folder_t), target :: folder1
type(folder_t) :: folder2
class(inode_t), allocatable :: clone_folder

    file1%name = "file1"
    file2%name = "file2"
    file3%name = "file3"
    
    folder1%name = "folder1"
    allocate(folder1%children(1))
    folder1%children(1)%inode => file1
    
    folder2%name = "folder2"
    allocate(folder2%children(3))
    folder2%children(1)%inode => folder1
    folder2%children(2)%inode => file2
    folder2%children(3)%inode => file3
    
    print *, "Printing hierarchy for Folder2"
    call folder2%print("   ")
    
    clone_folder = folder2%clone()
    print *, "Printing hierarchy for clone Folder"
    call clone_folder%print("   ")

end program prototype_main

!> Results shall be:

!  Printing hierarchy for Folder2
!     folder2
!        folder1
!              file1
!        file2
!        file3
!  Printing hierarchy for clone Folder
!     folder2_clone
!        folder1_clone
!              file1_clone
!        file2_clone
!        file3_clone