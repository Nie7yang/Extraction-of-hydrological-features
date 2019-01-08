 
    !*********************  长安大学——聂启阳  *********************
    
    
    MODULE JuZhen                        !@@@@@模块置于引用之前编译
        
        INTEGER,ALLOCATABLE  ::  dir(:,:),ACC(:,:)
        integer ::hang,lie
    ENDMODULE
    
    program shuilichangdu
        use JuZhen
        implicit none
		INTEGER GC,i,j
        integer xx,yy,dirr,yuzhi,CellACC
        real gaocha,GCC
        CHARACTER DATE 
  
		OPEN (222,FILE='dir.txt')
        OPEN (333,FILE='acc.txt')
       
!********************************  读数据前空位  **************************************
			READ (222,*) DATE,lie	!读取总列数
			READ (222,*) DATE,hang	!读取总行数
            READ (222,*)
            READ (222,*)
			READ (222,*) !DATE,GC	!网格尺寸
            READ (222,*)

            allocate (dir(hang,lie),acc(hang,lie))
                   
			DO I=1,hang
                    READ (222,*) (dir(i,j),J=1,lie)
            ENDDO	
!********************************  计算ACC  **************************************	
            write(*,*)'******************  开始计算  ******************'
            do i=1,hang
                do j=1,lie
                    if (dir(i,j)>0)then 
                        ACC(i,j)=1           !给流域内acc赋予初值1方便累加计算
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
                        ACC(i,j)=ACC(i,j)-1          !给流域内acc减去1，还原。
                    endif
                enddo
            enddo
            
            DO I=1,hang					
                    write (333,'(<lie>I6)') (ACC(i,j),J=1,lie)
            ENDDO	

	
            close(222)            
            close(333)

            DEALLOCATE (dir,acc)
            write(*,*)'计算结束'
            read(*,*)
            end program
!********************************  子程序  ********************************

            recursive subroutine GetACC(xx,yy)  !添加recursive关键字该子程序才可递归
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
