      !> This is a translation to Fortran of firework.c demo from pdcurses library version 3.4
      !! @ref http://pdcurses.sourceforge.net/
      
      subroutine get_color()
      implicitnone
      integer A_BOLD,A_NORMAL,A_COLOR,n,bold,attrset,r,
     &  PDC_COLOR_SHIFT,COLOR_PAIR,x,IAND,ISHFT
      parameter(A_BOLD=X'00800000',A_NORMAL=0,
     &  PDC_COLOR_SHIFT=24,A_COLOR=-16777216)
      external attrset
      COLOR_PAIR(x)=IAND(ISHFT(x,PDC_COLOR_SHIFT),A_COLOR)

        n = RAND() * 2
        if(MOD(n,2).eq.1)then
          bold = A_BOLD
        else
          bold = A_NORMAL
        endif
        n = RAND() * 8
        r = attrset(%VAL(COLOR_PAIR(MOD(n,8)) + bold))
      end

      subroutine explode(row,col)
      implicitnone
      integer row,col
      integer r,erase,mvaddstr
      external erase,mvaddstr

        r = erase()
        r = mvaddstr(%VAL(row),%VAL(col),%REF('-'//CHAR(0)))
        call myrefresh()

        col = col - 1

        call get_color()
        r = mvaddstr(%VAL(row-1),%VAL(col),%REF(' - '//CHAR(0)))
        r = mvaddstr(%VAL(row)  ,%VAL(col),%REF('-+-'//CHAR(0)))
        r = mvaddstr(%VAL(row+1),%VAL(col),%REF(' - '//CHAR(0)))
        call myrefresh()

        col = col - 1

        call get_color()
        r = mvaddstr(%VAL(row - 2),%VAL(col),%REF(' --- '//CHAR(0)))
        r = mvaddstr(%VAL(row - 1),%VAL(col),%REF('-+++-'//CHAR(0)))
        r = mvaddstr(%VAL(row),    %VAL(col),%REF('-+#+-'//CHAR(0)))
        r = mvaddstr(%VAL(row + 1),%VAL(col),%REF('-+++-'//CHAR(0)))
        r = mvaddstr(%VAL(row + 2),%VAL(col),%REF(' --- '//CHAR(0)))
        call myrefresh()

        call get_color()
        r = mvaddstr(%VAL(row - 2),%VAL(col),%REF(' +++ '//CHAR(0)))
        r = mvaddstr(%VAL(row - 1),%VAL(col),%REF('++#++'//CHAR(0)))
        r = mvaddstr(%VAL(row),    %VAL(col),%REF('+# #+'//CHAR(0)))
        r = mvaddstr(%VAL(row + 1),%VAL(col),%REF('++#++'//CHAR(0)))
        r = mvaddstr(%VAL(row + 2),%VAL(col),%REF(' +++ '//CHAR(0)))
        call myrefresh()

        call get_color()
        r = mvaddstr(%VAL(row - 2),%VAL(col),%REF('  #  '//CHAR(0)))
        r = mvaddstr(%VAL(row - 1),%VAL(col),%REF('## ##'//CHAR(0)))
        r = mvaddstr(%VAL(row),    %VAL(col),%REF('#   #'//CHAR(0)))
        r = mvaddstr(%VAL(row + 1),%VAL(col),%REF('## ##'//CHAR(0)))
        r = mvaddstr(%VAL(row + 2),%VAL(col),%REF('  #  '//CHAR(0)))
        call myrefresh()

        call get_color()
        r = mvaddstr(%VAL(row - 2),%VAL(col),%REF(' # # '//CHAR(0)))
        r = mvaddstr(%VAL(row - 1),%VAL(col),%REF('#   #'//CHAR(0)))
        r = mvaddstr(%VAL(row),    %VAL(col),%REF('     '//CHAR(0)))
        r = mvaddstr(%VAL(row + 1),%VAL(col),%REF('#   #'//CHAR(0)))
        r = mvaddstr(%VAL(row + 2),%VAL(col),%REF(' # # '//CHAR(0)))
        call myrefresh()
      end

      subroutine myrefresh()
      implicitnone
      integer LINES,COLS,DELAYSIZE
      common /LINES/LINES
      common /COLS/COLS
      parameter(DELAYSIZE=200)
      integer r,napms,move,refresh
      external napms,move,refresh

        r = napms(%VAL(DELAYSIZE))
        r = move(%VAL(LINES-1),%VAL(COLS-1))
        r = refresh()

      end

      program firework
      implicitnone
      integer*2 COLOR_BLACK,COLOR_RED,COLOR_BLUE,COLOR_GREEN,COLOR_CYAN,
     &  COLOR_MAGENTA,COLOR_YELLOW,COLOR_WHITE
      parameter(COLOR_BLACK=0,COLOR_RED=4,COLOR_GREEN=2,COLOR_BLUE=1,
     &  COLOR_CYAN=COLOR_BLUE+COLOR_GREEN,
     &  COLOR_MAGENTA=COLOR_RED+COLOR_BLUE,
     &  COLOR_YELLOW=COLOR_RED+COLOR_GREEN,
     &  COLOR_WHITE=7)
      integer*2 color_table(0:7)
      data color_table/COLOR_RED, COLOR_BLUE, COLOR_GREEN, COLOR_CYAN,
     &  COLOR_RED, COLOR_MAGENTA, COLOR_YELLOW, COLOR_WHITE/
      integer LINES,COLS
      common /LINES/LINES
      common /COLS/COLS
      integer TRUE,ERR,A_NORMAL
      parameter(TRUE=1,ERR=-1,A_NORMAL=0)
      integer i,start,end,row,diff,flag,direction,MOD,n
      integer r,nodelay,noecho,start_color,init_pair,getch,getmaxy,
     &  getmaxx,endwin,attrset,mvaddstr,erase,wgetch,curs_set
      logical has_colors
      integer*8 initscr,stdscr,newwin,win
      external initscr,nodelay,noecho,has_colors,start_color,init_pair,
     &  getch,getmaxy,getmaxx,endwin,newwin,attrset,mvaddstr,erase,
     &  wgetch,curs_set
      character trail*(2)

        stdscr = initscr()
        r = nodelay(%VAL(stdscr), %VAL(TRUE))
        r = noecho()
        r = curs_set(%VAL(0))

        win = newwin(%VAL(0),%VAL(0),%VAL(0),%VAL(0))
        LINES = getmaxy(%VAL(win))
        COLS = getmaxx(%VAL(win))

        if(has_colors()) r = start_color()

        do i=0,8
          r = init_pair(%VAL(i),%VAL(color_table(i)),
     &      %VAL(COLOR_BLACK))
        enddo

        call SRAND(TIME())
        flag = 0

        dowhile(wgetch(%VAL(stdscr)).eq.ERR)
          do
            n = RAND() * (COLS-3)
            start = MOD(n, COLS-3)
            n = RAND() * (COLS-3)
            end = MOD(n, COLS-3)
            if(start.lt.2) start = 2
            if(end.lt.2) end = 2
            if(start.gt.end)then
              direction = -1
            else
              direction = 1
            endif
            diff = ABS(start-end)
            if(diff.ge.2.and.diff.lt.(LINES - 2)) exit
          enddo

          r = attrset(%VAL(A_NORMAL))

          do row=0,diff
            if(direction.lt.0)then
              trail = CHAR(92) // CHAR(0)
            else
              trail = '/' // CHAR(0)
            endif
            r = mvaddstr(%VAL(LINES-row),
     &        %VAL(row*direction+start), %REF(trail))

            if(flag.ne.0)then
              call myrefresh()
              r = erase()
              flag = 0
            endif
            flag = flag + 1
          enddo

          if(flag.ne.0)then
            call myrefresh()
            flag = 0
          endif
          flag = flag + 1

          call explode(LINES-row, diff*direction+start)
          r = erase()
          call myrefresh()
        enddo

        r = endwin()

      end
