
    !*******************  Nie Qiyang                             ******************
    !*******************  2018.8  长安大学    * 基础功能          ******************
    !*******************  2023.9  北海道大学  * 更新定点流域范围  ******************

    MODULE JuZhen                        

    INTEGER,ALLOCATABLE  ::  dir(:,:)
    INTEGER(8),ALLOCATABLE  ::  ACC(:,:),basin(:,:)
    integer ::hang,lie
    ENDMODULE

    program shuilichangdu
    use JuZhen
    implicit none
    INTEGER i,j
    integer xx,yy,dirr,yuzhi,CellACC,x3,y3
    real(kind=8) x0, y0, GC, x1, y1, x2, y2
    real gaocha
    CHARACTER DATE
    CHARACTER(len=30) DATE1,DATE2,DATE3,DATE4,DATE5,DATE6
    INTEGER,ALLOCATABLE  ::  HeWang(:,:)

    OPEN (222,FILE='dir.asc')
    

    write(*,*)'???入'
    read(*,*)Yuzhi
    !write(*,*)'流域出口X坐?'
    !read(*,*)x1
    !write(*,*)'流域出口y坐?'
    !read(*,*)y1
    x1=142.15069
    y1=44.21886
    
    READ (222,'(A30)') DATE1
    READ (222,'(A30)') DATE2
    READ (222,'(A30)') DATE3
    READ (222,'(A30)') DATE4
    READ (222,'(A30)') DATE5
    READ (222,'(A30)') DATE6
    REWIND(222)
    !********************************  ?数据前空位  **************************************
    READ (222,*) DATE,lie	!?取?列数
    READ (222,*) DATE,hang	!?取?行数
    READ (222,*) DATE,x0    !左下角x
    READ (222,*) DATE,y0
    READ (222,*) DATE,GC	!网格尺寸
    READ (222,*)
    
    x2=(x1-x0)/GC
    y2=(y1-y0)/GC
    x3=x2+1
    y3=hang-int(y2)
    
    write(*,*)x1,y1,x2,y2,x3,y3
    
    allocate (dir(hang,lie),acc(hang,lie),HeWang(hang,Lie),basin(hang,lie))

    DO I=1,hang
        READ (222,*) (dir(i,j),J=1,lie)
    ENDDO
    !********************************  ?算ACC  **************************************
    write(*,*)'******************  ?始?算  ******************'
    do i=1,hang
        do j=1,lie
            if (dir(i,j)>0)then
                ACC(i,j)=1           !?流域内acc?予初?1方便累加?算
            endif
        enddo
    enddo

    do i=1,hang
        do j=1,lie
            if (dir(i,j)>0)then
                xx=i
                yy=j
                call GetACC(xx,yy)
            else
                ACC(i,j)=-9999
            endif
        end do
    end do
    
    do i=1,hang
        do j=1,lie
            if (dir(i,j)>0)then
                ACC(i,j)=ACC(i,j)-1          !?流域内acc?去1，?原。
            endif
        enddo
    enddo
    
    basin=-9999 
    call GetBasin(y3,x3)

    !********************************  ?算?束  **************************************
    OPEN (333,FILE='HeWang.asc')
    OPEN (444,FILE='acc.asc')
    OPEN (555,FILE='basin.asc')
    !?出流域ACC
    WRITE (444,'(A30)') DATE1
    WRITE (444,'(A30)') DATE2
    WRITE (444,'(A30)') DATE3
    WRITE (444,'(A30)') DATE4
    WRITE (444,'(A30)') DATE5
    WRITE (444,'(A30)') DATE6

    DO I=1,hang
        write (444,'(<lie>I20)') (ACC(i,j),J=1,lie)!?出?度要注意掌控，太?浪?空?，太短大?的acc数?大不能表?完整。
    ENDDO
    
    DO j=1,lie
        DO i=1,hang
            if(Acc(i,j)>=YuZhi)then
                HeWang(i,j)=1
            else
                HeWang(i,j)=-9999
            endif
        ENDDO
    ENDDO
    
    WRITE (333,'(A30)') DATE1
    WRITE (333,'(A30)') DATE2
    WRITE (333,'(A30)') DATE3
    WRITE (333,'(A30)') DATE4
    WRITE (333,'(A30)') DATE5
    WRITE (333,'(A30)') DATE6
    DO I=1,hang
        write (333,'(<lie>I7)') (HeWang(i,j),J=1,lie)
    ENDDO
    
    WRITE (555,'(A30)') DATE1
    WRITE (555,'(A30)') DATE2
    WRITE (555,'(A30)') DATE3
    WRITE (555,'(A30)') DATE4
    WRITE (555,'(A30)') DATE5
    WRITE (555,'(A30)') DATE6
    DO I=1,hang
        write (555,'(<lie>I7)') (basin(i,j),J=1,lie)
    ENDDO
    
    close(222)
    close(333)
    close(444)
    close(555)
    DEALLOCATE (dir,acc,HeWang,basin)
    write(*,*)'?算?束'
    read(*,*)
    end program
    !********************************  子程序  ********************************

    recursive subroutine GetACC(xx,yy)  
    use JuZhen
    implicit none
    integer xx,yy
    if(xx>0.and.xx<=hang.and.yy>0.and.yy<=lie)then
        if (ACC(xx,yy)==1) then
            if(yy+1<=lie)then
                if (dir(xx,yy+1)==16)then
                    call GetACC(xx,yy+1)
                    ACC(xx,yy)=ACC(xx,yy)+ACC(xx,yy+1)
                endif
            endif
            if(yy+1<=lie.and.xx+1<=hang)then
                if (dir(xx+1,yy+1)==32)then
                    call GetACC(xx+1,yy+1)
                    ACC(xx,yy)=ACC(xx,yy)+ACC(xx+1,yy+1)
                endif
            endif
            if(xx+1<=hang)then
                if (dir(xx+1,yy)==64)then
                    call GetACC(xx+1,yy)
                    ACC(xx,yy)=ACC(xx,yy)+ACC(xx+1,yy)
                endif
            endif
            if(xx+1<=hang.and.yy-1>0)then
                if (dir(xx+1,yy-1)==128)then
                    call GetACC(xx+1,yy-1)
                    ACC(xx,yy)=ACC(xx,yy)+ACC(xx+1,yy-1)
                endif
            endif
            if(yy-1>0)then
                if (dir(xx,yy-1)==1)then
                    call GetACC(xx,yy-1)
                    ACC(xx,yy)=ACC(xx,yy)+ACC(xx,yy-1)
                endif
            endif
            if(yy-1>0.and.xx-1>0)then
                if (dir(xx-1,yy-1)==2)then
                    call GetACC(xx-1,yy-1)
                    ACC(xx,yy)=ACC(xx,yy)+ACC(xx-1,yy-1)
                endif
            endif
            if(xx-1>0)then
                if (dir(xx-1,yy)==4)then
                    call GetACC(xx-1,yy)
                    ACC(xx,yy)=ACC(xx,yy)+ACC(xx-1,yy)
                endif
            endif
            if(yy+1<=lie.and.xx-1>0)then
                if (dir(xx-1,yy+1)==8)then
                    call GetACC(xx-1,yy+1)
                    ACC(xx,yy)=ACC(xx,yy)+ACC(xx-1,yy+1)
                endif
            endif
        endif
    endif
    return
    end subroutine GetACC
    
    
    recursive subroutine GetBasin(xx,yy)  
    use JuZhen
    implicit none
    integer xx,yy
    !if(xx>0.and.xx<=hang.and.yy>0.and.yy<=lie)then
        !if (ACC(xx,yy)>0) then
            basin(xx,yy)=1
            if(yy+1<=lie)then
                if (dir(xx,yy+1)==16)then
                    call GetBasin(xx,yy+1)
                endif
            endif
            if(yy+1<=lie.and.xx+1<=hang)then
                if (dir(xx+1,yy+1)==32)then
                    call GetBasin(xx+1,yy+1)
                endif
            endif
            if(xx+1<=hang)then
                if (dir(xx+1,yy)==64)then
                    call GetBasin(xx+1,yy)
                endif
            endif
            if(xx+1<=hang.and.yy-1>0)then
                if (dir(xx+1,yy-1)==128)then
                    call GetBasin(xx+1,yy-1)
                endif
            endif
            if(yy-1>0)then
                if (dir(xx,yy-1)==1)then
                    call GetBasin(xx,yy-1)
                endif
            endif
            if(yy-1>0.and.xx-1>0)then
                if (dir(xx-1,yy-1)==2)then
                    call GetBasin(xx-1,yy-1)
                endif
            endif
            if(xx-1>0)then
                if (dir(xx-1,yy)==4)then
                    call GetBasin(xx-1,yy)
                endif
            endif
            if(yy+1<=lie.and.xx-1>0)then
                if (dir(xx-1,yy+1)==8)then
                    call GetBasin(xx-1,yy+1)
                endif
            endif
        !elseif(ACC(xx,yy)==0) then
        !    basin(xx,yy)=1
        !endif
    !endif
    return
    end subroutine GetBasin
