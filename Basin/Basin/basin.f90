
    !*******************  2023.9  聂启阳 北海道大学  ******************

    module model0                        !@@@@@模块置于引用之前编译

    integer,allocatable  ::  dir(:,:),basin(:,:)
    integer ::hang,lie
    endmodule

    program basinpress
    use model0
    implicit none
    integer i,j,x3,y3
    real(kind=8) x0, y0, GC, x1, y1, x2, y2
    character DATE
    character(len=30) DATE1,DATE2,DATE3,DATE4,DATE5,DATE6

    open (222,FILE='dir.asc')

    write(*,*)'流域出口X坐标'
    read(*,*)x1
    write(*,*)'流域出口y坐标'
    read(*,*)y1
    
    read (222,'(A30)') DATE1
    read (222,'(A30)') DATE2
    read (222,'(A30)') DATE3
    read (222,'(A30)') DATE4
    read (222,'(A30)') DATE5
    read (222,'(A30)') DATE6
    rewind (222)
    !********************************  读数据前空位  **************************************
    read (222,*) DATE,lie	!读取总列数
    read (222,*) DATE,hang	!读取总行数
    read (222,*) DATE,x0    !左下角x
    read (222,*) DATE,y0
    read (222,*) DATE,GC	!网格尺寸
    read (222,*)

    x2=(x1-x0)/GC
    y2=(y1-y0)/GC
    x3=x2+1
    y3=hang-int(y2)

    allocate (dir(lie,hang),basin(lie,hang))

    read (222,*) dir
    !********************************  计算basin  **************************************

    basin=-9999
    call GetBasin(x3,y3)

    !********************************  计算结束  **************************************
    open (555,FILE='basin.asc')

    write (555,'(A30)') DATE1
    write (555,'(A30)') DATE2
    write (555,'(A30)') DATE3
    write (555,'(A30)') DATE4
    write (555,'(A30)') DATE5
    write (555,'(A30)') DATE6
    do I=1,hang
        write (555,'(<lie>I7)') (basin(j,i),j=1,lie)
    enddo

    close(222)
    close(555)
    deallocate (dir,basin)
    write(*,*)'计算结束'
    read(*,*)
    end program
    
    !********************************  子程序  ********************************
    !采用了递归，所以Debug模式下可能导致栈溢出或内存访问错误，可切换至Release状态编译。
    
    recursive subroutine GetBasin(xx,yy)  !添加recursive关键字该子程序才可递归
    use model0
    implicit none
    integer, intent(in) :: xx, yy
    
    basin(xx,yy)=1
    if(yy+1<=hang)then
        if (dir(xx,yy+1)==64)then
            call GetBasin(xx,yy+1)
        endif
    endif
    if(yy+1<=hang.and.xx+1<=lie)then
        if (dir(xx+1,yy+1)==32)then
            call GetBasin(xx+1,yy+1)
        endif
    endif
    if(xx+1<=lie)then
        if (dir(xx+1,yy)==16)then
            call GetBasin(xx+1,yy)
        endif
    endif
    if(xx+1<=lie.and.yy-1>0)then
        if (dir(xx+1,yy-1)==8)then
            call GetBasin(xx+1,yy-1)
        endif
    endif
    if(yy-1>0)then
        if (dir(xx,yy-1)==4)then
            call GetBasin(xx,yy-1)
        endif
    endif
    if(yy-1>0.and.xx-1>0)then
        if (dir(xx-1,yy-1)==2)then
            call GetBasin(xx-1,yy-1)
        endif
    endif
    if(xx-1>0)then
        if (dir(xx-1,yy)==1)then
            call GetBasin(xx-1,yy)
        endif
    endif
    if(yy+1<=hang.and.xx-1>0)then
        if (dir(xx-1,yy+1)==128)then
            call GetBasin(xx-1,yy+1)
        endif
    endif
    return
    end subroutine GetBasin