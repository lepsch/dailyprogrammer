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

      !> Max signed integer
      !! @return Returns max signed integer
      integer function max_int()
      implicit none
      intrinsic ISHFT
        max_int = -1
        max_int = ISHFT(max_int, -1)
      end

      !> Choose a random number between lower and higher bound exclusively
      !! @param[in] lower Exclusive lower bound
      !! @param[in] lower Exclusive higher bound
      !! @return Returns a random number between lower and higher exclusively
      integer function exclusive_rand(lower,higher)
      implicit none
      integer higher,lower,range,number,max_int
      intrinsic MOD,RAND
        range = higher - lower - 1
        number = RAND() * max_int()
        exclusive_rand = MOD(number,range)+1+lower
      end

      !> http://www.reddit.com/r/dailyprogrammer/comments/pii6j/difficult_challenge_1/
      program difficult_challenge_1
      implicit none
      intrinsic TIME,SRAND,ICHAR
      integer guess,higher_guess,lower_guess,count,exclusive_rand
      character key
      integer*1 key_code
      integer*1 YES, HIGHER, LOWER
      parameter(YES=X'59',HIGHER=X'48',LOWER=X'4C')
      character up_case
      integer LOWER_BOUND,HIGHER_BOUND
      parameter(LOWER_BOUND=1,HIGHER_BOUND=100)

      print '(A$)','This is a game where I (the computer) will try and'
      print '(A)',' guess a number you choose.'
      print '("Please choose a number between ",I4,$)',LOWER_BOUND
      print '(" and ",I4)',HIGHER_BOUND
      print '(A)','Once you have chosen a number, press ENTER.'
      read *

      ! initializes the random number generator
      call SRAND(TIME())

      count = 1
      lower_guess = LOWER_BOUND-1
      higher_guess = HIGHER_BOUND+1
      guess = exclusive_rand(lower_guess,higher_guess)

      do
        print '("Is your number ",I3,"?",$)',guess
        print '(A$)',' [Yes (y), Higher (h), Lower (l)]'
        read *,key

        key = up_case(key)
        key_code = ICHAR(key)

        select case (key_code)
        case (YES)
          print '(A,I3,A$)','Yippee! It took me ',count,' tries to'
          print '(A,I3,A)',' guess your number, which was ',guess,'.'
          exit
        case (HIGHER)
          lower_guess = guess
        case (LOWER)
          higher_guess = guess
        case default
          print '(A,A)', key, ' is not a valid option!'
          cycle
        end select

        guess = exclusive_rand(lower_guess,higher_guess)
        count = count + 1
      enddo


      print '(A$)','Press ENTER to exit.'
      read *
      end
