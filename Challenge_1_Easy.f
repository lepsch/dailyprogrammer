      !> Remove left spaces from string as the intrinsic function
      !! @param[in] str String to remove left spaces
      !! @return Returns the string without left spaces
      character*(*) function adjustl(str)
      implicit none
      character*(*) str
        integer i
        intrinsic LEN
        do i=1,LEN(str)
          if(str(i:i).ne.' ') then
            adjustl = str(i:)
            exit
          endif
        enddo
      end

      !> Formats the output to an unit
      !! @param[in] unit Unit number to write
      !! @param[in] name Asked name
      !! @param[in] age Asked age
      !! @param[in] login Asked login
      subroutine output(unit,name,age,login)
      implicit none
      integer unit,age
      character*(*) name,login
        intrinsic LEN_TRIM
        character*10 ageStr,adjustl
        ! formats integer to string
        write(ageStr, '(I10)') age
        ageStr = adjustl(ageStr)
      
        write(unit, '(2A$)')'Your name is ', name(:LEN_TRIM(name))
        write(unit, '(3A$)')', you are ', ageStr(:LEN_TRIM(ageStr)),
     _    ' years old'
        write(unit, '(2A)')', and your username is ',
     _    login(:LEN_TRIM(login))
      end

      !> Formats output to the screen
      !! @param[in] name Asked name
      !! @param[in] age Asked age
      !! @param[in] login Asked login
      subroutine output_to_screen(name,age,login)
      implicit none
      integer age
      character*(*) name, login
        integer SCREEN
        parameter(SCREEN=6)

        call output(SCREEN, name, age, login)
        
      end
      
      !> Formats output to a file
      !! @param[in] filename Filename to output for
      !! @param[in] name Asked name
      !! @param[in] age Asked age
      !! @param[in] login Asked login
      subroutine output_to_file(filename,name,age,login)
      implicit none
      integer age
      character*(*) filename, name, login
        integer file
        parameter(file=10)

        open(file, FILE=filename)
        call output(file, name, age, login)
        close(file)
        
      end
      
      !> http://www.reddit.com/r/dailyprogrammer/comments/pih8x/easy_challenge_1/
      program reddit_dailyprogrammer_easy_challenge_1
      implicit none
      
        character*20 name
        integer age
        character*20 login

        print '(A$)','What is your name? '
        read *,name
        print '(A$)','How old are you? '
        read *,age
        print '(A$)','What is your Reddit username? '
        read *,login

        call output_to_screen(name, age, login)
        call output_to_file('Challenge_1_Easy.txt', name, age, login)

        print '(A$)','Press ENTER to exit.'
        read *
      end
