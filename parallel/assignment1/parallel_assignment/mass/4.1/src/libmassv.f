!*****************************************************************************
!Mathematical Acceleration Subsystem Component V4.1                           
!                                                                             
!Licensed Materials - Property of IBM                                         
!5724-K76	                                                             
!(C) Copyright IBM Corporation 1985, 2004.  All Rights Reserved.              
!US Government Users Restricted Rights - Use, duplication or                  
!disclosure restricted by GSA ADP Schedule Contract with IBM Corp.            
!*****************************************************************************

      subroutine vrec(y,x,n)
      real*8 x(*),y(*)
      do 10 j=1,n
      y(j)=1.d0/x(j)
   10 continue
      return
      end
      subroutine vsrec(y,x,n)
      real*4 x(*),y(*)
      do 10 j=1,n
      y(j)=1.d0/x(j)
   10 continue
      return
      end
@process float(rsqrt)
      subroutine vrsqrt(y,x,n)
      real*8 x(*),y(*)
      do 10 j=1,n
      y(j)=1.d0/sqrt(x(j))
   10 continue
      return
      end
@process float(rsqrt)
      subroutine vsrsqrt(y,x,n)
      real*4 x(*),y(*)
      do 10 j=1,n
      y(j)=1.d0/sqrt(x(j))
   10 continue
      return
      end
      subroutine vsqrt(y,x,n)
      real*8 x(*),y(*)
      do 10 j=1,n
      y(j)=sqrt(x(j))
   10 continue
      return
      end
      subroutine vssqrt(y,x,n)
      real*4 x(*),y(*)
      do 10 j=1,n
      y(j)=sqrt(x(j))
   10 continue
      return
      end
