unit InterpolatorClass;

interface

    uses
        system.SysUtils, system.Math;

    type
        TInterpolator = class
            strict private
                x0, y0,
                x1, y1  : double;
                function lineGradient() : double;
            public
                constructor create(); overload;
                constructor create(x0In, x1In, y0In, y1In : double); overload;
                destructor destroy(); override;
                procedure setPoints(x0In, x1In, y0In, y1In : double);
                function interpolate(xIn : double) : double;
        end;

implementation

    //private
        function TInterpolator.lineGradient() : double;
            begin
                result := (y1 - y0) / (x1 - x0);
            end;

    //public
        constructor TInterpolator.create();
            begin
                inherited create();
            end;

        constructor TInterpolator.create(x0In, x1In, y0In, y1In : double);
            begin
                create();

                setPoints(x0In, x1In, y0In, y1In);
            end;

        destructor TInterpolator.Destroy();
            begin
                inherited Destroy();
            end;

        procedure TInterpolator.setPoints(x0In, x1In, y0In, y1In : double);
            begin
                x0 := x0In;
                x1 := x1In;
                y0 := y0In;
                y1 := y1In;
            end;

        function TInterpolator.interpolate(xIn : double): Double;
            begin
                result := lineGradient() * (xIn - x0) + y0;
            end;

end.
