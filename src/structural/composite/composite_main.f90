program composite_main
    use composite_module, only: file_type, folder_type
    implicit none
    type(file_type), target :: file1, file2, file3
    type(folder_type), target :: folder1
    type(folder_type) :: folder2

    file1%name = "File1"
    file2%name = "File2"
    file3%name = "File3"

    folder1%name = "Folder1"
    folder2%name = "Folder2"

    allocate (folder1%components(1))
    folder1%components(1)%node => file1

    allocate (folder2%components(3))
    folder2%components(1)%node => file2
    folder2%components(2)%node => file3
    folder2%components(3)%node => folder1

    call folder2%search("rose")

end program composite_main

!> Results shall be:

!  Searching recursively for keyword rose in folder Folder2
!  Searching for keyword rose in file File2
!  Searching for keyword rose in file File3
!  Searching recursively for keyword rose in folder Folder1
!  Searching for keyword rose in file File1