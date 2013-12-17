      !> Transform a character from lowercase to uppercase
      !! Clears the 6th bit of a character to make it uppercase
      !! @param[in] c Character to transform
      !! @return Returns the uppercase character
      character function up_case(c)
      implicit none
      character c
        if(c.ge.'a'.and.c.le.'z')then
          up_case = CHAR(IBCLR(ICHAR(c), 5))
        else
          up_case = c
        endif
      end

      !> Asks for force
      !! @param[out] mass Mass
      double precision function input_force()
      implicit none
        print '(A$)','Enter the force: '
        read *,input_force
      end

      !> Asks for mass
      !! @param[out] mass Mass
      double precision function input_mass()
      implicit none
        print '(A$)','Enter the mass: '
        read *,input_mass
      end

      !> Asks for acceleration
      !! @param[out] accel Acceleration
      double precision function input_accel()
      implicit none
        print '(A$)','Enter the acceleration: '
        read *,input_accel
      end

      !> http://www.reddit.com/r/dailyprogrammer/comments/pjbj8/easy_challenge_2/
      program easy_2
      implicit none
      character key, up_case
      integer*1 key_code
      ! Key codes
      integer*1 FORCE, MASS, ACCEL, QUIT
      parameter(FORCE=X'46',MASS=X'4D',ACCEL=X'41',QUIT=X'51')
      double precision f, m, a, input_force, input_mass, input_accel

      print *,'This program will compute one of the terms from F=M.A'
      do

        print '(A)', 'F. Force;'
        print '(A)', 'M. Mass;'
        print '(A)', 'A. Acceleration;'
        print '(A)', 'Q. Quit.'

        print '(A$)','Select an option (FMAQ): '
        read *, key

        key = up_case(key)
        key_code = ICHAR(key)

        select case (key_code)
        case (FORCE)
          m = input_mass()
          a = input_accel()
          f =  m * a
          print *,'The force is: ', f
        case (MASS)
          f = input_force()
          a = input_accel()
          m =  f / a
          print *,'The mass is: ', m
        case (ACCEL)
          m = input_mass()
          f = input_force()
          a =  f / m
          print *,'The acceleration is: ', a
        case (QUIT)
          exit
        case default
          print '(A,A)', key, ' is not a valid option!'
        end select

      enddo

      print '(A$)','Press ENTER to exit.'
      read *
      end
