        subroutine orbit(t,y,yp)
            real t,y(2),yp(2)
            yp(1)=-52*y(1)-100*y(2)+exp(-t)
            yp(2)=y(1)+sin(t)
            return
        end

        subroutine runge(t, h, y, i)
            external orbit
            real y(2), yp(2), k1(2), h, temp(2), k2(2)
            real k3(2), k4(2), results(20, 2)
            integer i
            call orbit(t, y, yp)

            k1=h*yp
            temp=y+k1/2.0
            call orbit (t+h/2, temp, yp)
            k2=h*yp
            temp=y+k2/2.0
            call orbit (t+h/2, temp, yp)
            k3=h*yp
            temp=y+k3
            call orbit(t+h, temp, yp)
            k4=h*yp
            y=y+(k1+2*k2+2*k3+k4)/6
                if (mod(i, 100)==0) then
                    results(i/100,:)=y
                    print *, results(i/100,:)
                end if
        end subroutine runge

        external orbit
        real t,y(2), tfinal,tprint
        integer i
        i=1
        t=0.0
        y(1)=1.0
        y(2)=0.0

        tfinal=2.0
        tprint=0.001
        h=0.001

   10   call runge(t, h, y, i)
        t=tprint+t
        i=i+1
        if(t.lt.tfinal) go to 10
        stop
        end
