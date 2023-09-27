
    !*******************  2023.9  ������ ��������ѧ  ******************

    module model0                        !@@@@@ģ����������֮ǰ����

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

    write(*,*)'�������X����'
    read(*,*)x1
    write(*,*)'�������y����'
    read(*,*)y1
    
    read (222,'(A30)') DATE1
    read (222,'(A30)') DATE2
    read (222,'(A30)') DATE3
    read (222,'(A30)') DATE4
    read (222,'(A30)') DATE5
    read (222,'(A30)') DATE6
    rewind (222)
    !********************************  ������ǰ��λ  **************************************
    read (222,*) DATE,lie	!��ȡ������
    read (222,*) DATE,hang	!��ȡ������
    read (222,*) DATE,x0    !���½�x
    read (222,*) DATE,y0
    read (222,*) DATE,GC	!����ߴ�
    read (222,*)

    x2=(x1-x0)/GC
    y2=(y1-y0)/GC
    x3=x2+1
    y3=hang-int(y2)

    allocate (dir(lie,hang),basin(lie,hang))

    read (222,*) dir
    !********************************  ����basin  **************************************

    basin=-9999
    call GetBasin(x3,y3)

    !********************************  �������  **************************************
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
    write(*,*)'�������'
    read(*,*)
    end program
    
    !********************************  �ӳ���  ********************************
    !�����˵ݹ飬����Debugģʽ�¿��ܵ���ջ������ڴ���ʴ��󣬿��л���Release״̬���롣
    
    recursive subroutine GetBasin(xx,yy)  !���recursive�ؼ��ָ��ӳ���ſɵݹ�
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