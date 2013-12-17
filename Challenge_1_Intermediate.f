      !> Program global constants
      blockdata globals
        implicit none
        integer RECORD_SIZE
        character*(9) RECORD_COUNT_FMT,RECORD_FMT
        common /RECORD_COUNT_FMT/RECORD_COUNT_FMT
        common /RECORD_FMT/RECORD_FMT
        common /RECORD_SIZE/RECORD_SIZE
        data RECORD_SIZE/30/
        data RECORD_COUNT_FMT/'(I10,20X)'/
        data RECORD_FMT/'(A20,I10)'/
      end

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
      
      !> Checks if returned status code from a read/write is end-of-file
      !! @param[in] status IOSTAT status
      !! @return Returns if the status is end-of-file
      logical function is_eof(status)
      implicit none
      integer status
        is_eof = status.lt.0
      end

      !> Checks if returned status code from a read/write is an error
      !! @param[in] status IOSTAT status
      !! @return Returns if the status is an error
      logical function is_error(status)
      implicit none
      integer status
        is_error = status.gt.0
      end
      
      !> Checks if returned status code from a read/write is ok
      !! @param[in] status IOSTAT status
      !! @return Returns if the status is ok
      logical function is_ok(status)
      implicit none
      integer status
        is_ok = status.eq.0
      end

      !> Reads the number of records from database
      !! @return Returns true if no error
      logical function db_read_record_count()
      implicit none
        integer unit,record_count,status
        common /database/ unit,record_count
        logical db_write_record_count,is_eof,is_error,is_ok
        character*(9) FMT
        common /RECORD_COUNT_FMT/FMT
        
        read(unit,FMT,IOSTAT=status,REC=1) record_count

        if(is_eof(status)) then
          record_count=0
          if(.not.db_write_record_count()) then
            print *,'FATAL ERROR: cannot access database'
            stop
          endif
          status = 0
        endif
        
        db_read_record_count = is_ok(status)
        if (is_error(status)) then
          print *,'FATAL ERROR: cannot read record count from database'
          stop
        endif
      end

      !> Writes the number of records to database
      !! @return Returns true if no error
      logical function db_write_record_count()
      implicit none
        integer unit,record_count,status
        common /database/ unit, record_count
        logical is_ok,is_error
        character*(9) FMT
        common /RECORD_COUNT_FMT/ FMT
        
        write(unit,FMT,IOSTAT=status,REC=1) record_count
        db_write_record_count = is_ok(status)
        if (is_error(status)) then
          print *,'ERROR writing record count to database'
          return
        endif
      end

      !> Reads the record data from database
      !! @param[out] name The name
      !! @param[out] hour The hour
      !! @param[in] position The position to read the record
      !! @return Returns true if no error
      logical function db_read_record(name, hour, position)
      implicit none
        character*(*) name
        integer hour,position,status,unit,record_count
        common /database/ unit, record_count
        logical is_ok,is_error
        character*(9) FMT
        common /RECORD_FMT/FMT

        read(unit,FMT,IOSTAT=status,REC=position + 1) name, hour
        db_read_record = is_ok(status)
        if (is_error(status)) then
          print *,'ERROR reading record ',position,' from database'
          return
        endif
      end

      !> Writes the record data to database
      !! @param[in] name The name
      !! @param[in] hour The hour
      !! @param[in] position The position to write the record
      !! @return Returns true if no error
      logical function db_write_record(name, hour, position)
      implicit none
        character*(*) name
        integer hour,position,unit,record_count,status
        common /database/ unit, record_count
        logical is_ok,is_error
        character*(9) FMT
        common /RECORD_FMT/FMT

        write(unit,FMT,IOSTAT=status,REC=position + 1) name,hour
        db_write_record = is_ok(status)
        if (is_error(status)) then
          print *,'ERROR writing record ',position,' to database'
          return
        endif
      end
      
      !> Lists all records
      subroutine do_list()
      implicit none
        integer unit,record_count
        common /database/ unit, record_count
        character*(100) name_buffer
        integer hour
        logical db_read_record
        integer position

        do position=1,record_count
          if(.not.db_read_record(name_buffer, hour, position))return
          print '(I3,A,A20,A,I10)',
     _      position, '. ', name_buffer, ' -> ', hour
        enddo
        
        print *,'Records read: ', record_count
      end

      !> Asks user the record data
      !! @param[out] name The name
      !! @param[out] hour The hour
      subroutine input_record(name, hour)
      implicit none
        character*(*) name
        integer hour

        print '(A$)', 'Event name: '
        read *,name

        do
          print '(A$)', 'Event hour: '
          read *,hour
          if(hour.ge.0.and.hour.le.24) exit
          print *, 'ERROR: Invalid hour'
        enddo
      end

      !> Asks user a record position
      !! @param[out] position Record position
      !! @return Returns if the position is valid
      logical function input_position(position)
      implicit none
        integer unit
        integer record_count
        common /database/ unit, record_count
        integer position
        
        print '(A$)', 'Enter record #: '
        read *,position

        input_position = position.ge.1.and.position.le.record_count
        if(.not.input_position)then
          print *, 'ERROR: invalid record #'
        endif
      end

      !> Asks user and commits record data
      !! @param[in] position The record's position
      !! @return Returns true if no error
      logical function asks_and_write_user_input(position)
      implicit none
        integer position
        character*(100) name_buffer
        integer hour
        logical db_write_record

        call input_record(name_buffer, hour)

        asks_and_write_user_input = db_write_record(name_buffer, hour,
     _    position)
      end

      !> Adds a new record at the end of database
      subroutine do_add()
      implicit none
        integer unit,record_count
        common /database/ unit, record_count
        logical asks_and_write_user_input, db_write_record_count
        
        if(.not.asks_and_write_user_input(record_count + 1)) then
          print *,'ERROR writing record to database'
          return
        endif
        
        record_count = record_count + 1
        if(.not.db_write_record_count()) return
      end

      !> Edits a record
      subroutine do_edit()
      implicit none
        integer unit
        integer record_count
        common /database/ unit, record_count
        integer position
        logical asks_and_write_user_input, input_position

        if(.not.input_position(position)) return
        
        if(.not.asks_and_write_user_input(position)) then
          print *,'ERROR writing record to database'
          return
        endif
      end

      !> Deletes a record
      subroutine do_delete()
      implicit none
        integer unit,record_count
        common /database/ unit, record_count
        integer position,i
        character*(100) name_buffer
        integer hour
        logical db_read_record,db_write_record,db_write_record_count
        logical input_position

        if(.not.input_position(position)) return

        do i=position, record_count - 1
          if(.not.db_read_record(name_buffer, hour, position + 1))return
          if(.not.db_write_record(name_buffer, hour, position)) return
        enddo
        
        name_buffer=' '
        hour=-1
        if(.not.db_write_record(name_buffer, hour, record_count)) return

        record_count = record_count - 1
        if(.not.db_write_record_count()) return
      end

      !> Opens the database
      subroutine db_open()
      implicit none

        character*(*) DATABASE_FILE
        parameter(DATABASE_FILE='Challenge_1_Intermediate.db')
        integer unit,record_count
        common /database/ unit, record_count
        logical db_read_record_count
        data unit/10/
        integer RECORD_SIZE
        common /RECORD_SIZE/RECORD_SIZE
        open(unit, FILE=DATABASE_FILE, ACCESS='DIRECT',
     _    FORM='FORMATTED', RECL=RECORD_SIZE)
        if(.not.db_read_record_count()) stop
      end

      !> Closes the database
      subroutine db_close()
      implicit none

        integer unit
        integer record_count
        common /database/ unit, record_count
        close(unit)

      end

      !> http://www.reddit.com/r/dailyprogrammer/comments/pihtx/intermediate_challenge_1/
      program intermediate_1
      implicit none
      character key, up_case
      integer*1 key_code
      integer*1 LIST, ADD, EDIT, DEL, QUIT
      parameter(LIST=X'4C',ADD=X'41',EDIT=X'45',DEL=X'44',QUIT=X'51')

      call db_open()
      
      do

        print '(A)', 'L. List events;'
        print '(A)', 'A. Add event;'
        print '(A)', 'E. Edit event;'
        print '(A)', 'D. Delete events;'
        print '(A)', 'Q. Quit.'

        print '(A$)','Select an option (LAEDQ): '
        read *, key

        key = up_case(key)
        key_code = ICHAR(key)

        select case (key_code)
        case (LIST)
          call do_list()
        case (ADD)
          call do_add()
        case (EDIT)
          call do_edit()
        case (DEL)
          call do_delete()
        case (QUIT)
          exit
        case default
          print '(A,A)', key, ' is not a valid option!'
        end select

      enddo
      
      call db_close()

      print '(A$)','Press ENTER to exit.'
      read *
      end
