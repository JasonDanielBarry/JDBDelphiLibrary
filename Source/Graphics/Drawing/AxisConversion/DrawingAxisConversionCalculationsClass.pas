unit DrawingAxisConversionCalculationsClass;

interface

    uses
        System.SysUtils, system.Math, system.Types,
        GeneralMathMethods,
        GeometryTypes,
        DrawingAxisConversionBaseClass
        ;

    type
        TDrawingAxisConvertionCalculator = class(TDrawingAxisConverterBase)
            private
                //canvas-to-drawing
                    function L_to_X(const L_In : double) : double; inline;
                    function T_to_Y(const T_In : double) : double; inline;
                //drawing-to-canvas
                    function X_to_L(const X_In : double) : double; inline;
                    function Y_to_T(const Y_In : double) : double; inline;
                //canvas-to-drawing
                    function LT_to_XY(const L_In, T_In : double) : TGeomPoint; overload; inline;
                //drawing-to-canvas
                    function XY_to_LTF(const X_In, Y_In : double) : TPointF; overload; inline;
                    function XY_to_LT(const X_In, Y_In : double) : TPoint; overload; inline;
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //space conversions
                    //canvas to region
                        function dL_To_dX(const dL_In : double) : double; inline;
                        function dT_To_dY(const dT_In : double) : double; inline;
                    //region to canvas
                        function dX_To_dL(const dX_In : double) : double; inline;
                        function dY_To_dT(const dY_In : double) : double; inline;
                //convertion calculations
                    //canvas-to-drawing
                        function LT_to_XY(const pointIn : TPointF) : TGeomPoint; overload;
                        function LT_to_XY(const pointIn : TPoint) : TGeomPoint; overload;
                        function arrLT_to_arrXY(const arrLT_In : TArray<TPointF>) : TArray<TGeomPoint>; overload;
                        function arrLT_to_arrXY(const arrLT_In : TArray<TPoint>) : TArray<TGeomPoint>; overload;
                    //drawing-to-canvas
                        //double versions
                            function XY_to_LTF(const pointIn : TGeomPoint) : TPointF; overload;
                            function arrXY_to_arrLTF(const arrXY_In : TArray<TGeomPoint>) : TArray<TPointF>;
                        //integer versions
                            function XY_to_LT(const pointIn : TGeomPoint) : TPoint; overload;
                            function arrXY_to_arrLT(const arrXY_In : TArray<TGeomPoint>) : TArray<TPoint>;
        end;

implementation

    //private
        //canvasSpace-to-drawing
            function TDrawingAxisConvertionCalculator.L_to_X(const L_In : double) : double;
                var
                    deltaX : double;
                begin
                    //x(l) = (D/w)l + xmin

                    deltaX := dL_To_dX( L_In );

                    result := drawingRegion.minPoint.x + deltaX;

//                    result := (( calculateRegionDomain() / canvasWidth() ) * L_In) + drawingRegion.minPoint.x;
                end;

            function TDrawingAxisConvertionCalculator.T_to_Y(const T_In : double) : double;
                var
                    deltaY : double;
                begin
                    //y(t) = -(R/h)t + ymax

                    deltaY := dT_To_dY( T_In );

                    result := drawingRegion.maxPoint.y + deltaY;
                    
//                    result := -(( calculateRegionRange() / canvasHeight() ) * T_In) + drawingRegion.maxPoint.y;
                end;

        //drawing-to-canvas
            //double verions
                function TDrawingAxisConvertionCalculator.X_to_L(const X_In : double) : double;
                    var
                        deltaX : double;
                    begin
                        //l(x) = (w/D)(x - xmin)

                        deltaX := X_In - drawingRegion.minPoint.x;

                        result := dX_To_dL( deltaX );
                    end;

                function TDrawingAxisConvertionCalculator.Y_to_T(const Y_In : double) : double;
                    var
                        deltaY : double;
                    begin
                        //t(y) = -(h/R)(y - ymax)

                        deltaY := Y_In - drawingRegion.maxPoint.y;

                        result := dY_To_dT( deltaY );
                    end;

        //canvasSpace-to-drawing
            function TDrawingAxisConvertionCalculator.LT_to_XY(const L_In, T_In : double) : TGeomPoint;
                var
                    pointOut : TGeomPoint;
                begin
                    pointOut.x := L_to_X(L_In);
                    pointOut.y := T_to_Y(T_In);

                    result := pointOut;
                end;

        //drawing-to-canvas
            function TDrawingAxisConvertionCalculator.XY_to_LTF(const X_In, Y_In : double) : TPointF;
                var
                    pointOut : TPointF;
                begin
                    pointOut.x := X_to_L(X_In);
                    pointOut.y := Y_to_T(Y_In);

                    result := pointOut;
                end;

            function TDrawingAxisConvertionCalculator.XY_to_LT(const X_In, Y_In : double) : TPoint;
                var
                    pointF : TPointF;
                begin
                    pointF := XY_to_LTF(X_In, Y_In);

                    result := point( round(pointF.X), round(pointF.Y) )
                end;

    //public
        //constructor
            constructor TDrawingAxisConvertionCalculator.create();
                begin
                    inherited create();
                end;

        //destructor
            destructor TDrawingAxisConvertionCalculator.destroy();
                begin
                    inherited destroy();
                end;

        //space conversions
            //canvas to region
                function TDrawingAxisConvertionCalculator.dL_To_dX(const dL_In : double) : double;
                    begin
                        //dx/dl = (D/w)
                        //dx = dl(D/w)

                        result := dL_In * (calculateRegionDomain() / canvasWidth());
                    end;

                function TDrawingAxisConvertionCalculator.dT_To_dY(const dT_In : double) : double;
                    begin
                        //dy/dt = -(R/h)
                        //dy = -dt(R/h)

                        result := -dT_In * (calculateRegionRange() / canvasHeight());
                    end;

            //region to canvas
                function TDrawingAxisConvertionCalculator.dX_To_dL(const dX_In : double) : double;
                    begin
                        //dl/dx = (w/D)

                        result := dX_In * (canvasWidth() / calculateRegionDomain());
                    end;

                function TDrawingAxisConvertionCalculator.dY_To_dT(const dY_In : double) : double;
                    begin
                        //dt/dy = -(h/R)

                        result := -dY_In * (canvasHeight() / calculateRegionRange());
                    end;

        //convertion calculations
            //canvasSpace-to-drawing
                function TDrawingAxisConvertionCalculator.LT_to_XY(const pointIn : TPointF) : TGeomPoint;
                    begin
                        result := LT_to_XY(pointIn.X, pointIn.Y);
                    end;

                function TDrawingAxisConvertionCalculator.LT_to_XY(const pointIn : TPoint) : TGeomPoint;
                    var
                        newPoint : TPointF;
                    begin
                        newPoint := TPointF.create(pointIn);

                        result := LT_to_XY(newPoint);
                    end;

                function TDrawingAxisConvertionCalculator.arrLT_to_arrXY(const arrLT_In : TArray<TPointF>) : TArray<TGeomPoint>;
                    var
                        i, arrLen       : integer;
                        arrPointsOut    : TArray<TGeomPoint>;
                    begin
                        arrLen := length(arrLT_In);

                        SetLength(arrPointsOut, arrLen);

                        for i := 0 to (arrLen - 1) do
                            arrPointsOut[i] := LT_to_XY(arrLT_In[i]);

                        result := arrPointsOut;
                    end;

                function TDrawingAxisConvertionCalculator.arrLT_to_arrXY(const arrLT_In : TArray<TPoint>) : TArray<TGeomPoint>;
                    var
                        i               : integer;
                        arrPointF       : TArray<TPointF>;
                    begin
                        SetLength(arrPointF, length(arrLT_In));

                        for i := 0 to (length(arrPointF) - 1) do
                            arrPointF[i] := TPointF.create(arrLT_In[i]);

                        result := arrLT_to_arrXY(arrPointF);
                    end;

            //drawing-to-canvas
                //double verions
                    function TDrawingAxisConvertionCalculator.XY_to_LTF(const pointIn : TGeomPoint) : TPointF;
                        begin
                            result := XY_to_LTF(pointIn.x, pointIn.y);
                        end;

                    function TDrawingAxisConvertionCalculator.arrXY_to_arrLTF(const arrXY_In : TArray<TGeomPoint>) : TArray<TPointF>;
                        var
                            i, arrLen       : integer;
                            arrPointsOut    : TArray<TPointF>;
                        begin
                            arrLen := length(arrXY_In);

                            SetLength(arrPointsOut, arrLen);

                            for i := 0 to (arrLen - 1) do
                                arrPointsOut[i] := XY_to_LTF(arrXY_In[i]);

                            result := arrPointsOut;
                        end;

                //integer versions
                    function TDrawingAxisConvertionCalculator.XY_to_LT(const pointIn : TGeomPoint) : TPoint;
                        begin
                            result := XY_to_LT( pointIn.x, pointIn.y );
                        end;

                    function TDrawingAxisConvertionCalculator.arrXY_to_arrLT(const arrXY_In : TArray<TGeomPoint>) : TArray<TPoint>;
                        var
                            i, x_Int, y_Int : integer;
                            arrPointF       : TArray<TPointF>;
                            arrPointsOut    : TArray<TPoint>;
                        begin
                            arrPointF := arrXY_to_arrLTF(arrXY_In);

                            SetLength(arrPointsOut, length(arrPointF));

                            for i := 0 to (length(arrPointsOut) - 1) do
                                begin
                                    x_Int := round(arrPointF[i].X);
                                    y_Int := round(arrPointF[i].Y);

                                    arrPointsOut[i] := point(x_Int, y_Int);
                                end;

                            result := arrPointsOut;
                        end;

end.
