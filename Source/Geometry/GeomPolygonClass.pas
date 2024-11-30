unit GeomPolygonClass;

interface

    uses
        System.SysUtils, system.Math,
        DrawingTypes,
        GeometryTypes,
        GeometryMathMethods,
        GeomPolyLineClass;

    type
        TGeomPolygon = class(TGeomPolyLine)
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //accessors
                    function getDrawingType() : EDrawingType; override;
                //drawing points
                    function getDrawingPoints() : TArray<TGeomPoint>; override;
        end;

implementation

    //public
        //constructor
            constructor TGeomPolygon.create();
                begin
                    inherited create();
                end;

        //destructor
            destructor TGeomPolygon.destroy();
                begin
                    inherited destroy();
                end;
        //accessors
            function TGeomPolygon.getDrawingType() : EDrawingType;
                begin
                    result := EDrawingType.dtPolygon;
                end;

        //drawing points
            function TGeomPolygon.getDrawingPoints() : TArray<TGeomPoint>;
                var
                    arrLen              : integer;
                    arrDrawingPointsOut : TArray<TGeomPoint>;
                begin
                    arrDrawingPointsOut := inherited getDrawingPoints();

                    arrLen := length(arrDrawingPointsOut);

                    SetLength(arrDrawingPointsOut, arrLen + 1);

                    arrLen := arrLen + 1;

                    //close polygon
                        arrDrawingPointsOut[arrLen - 1] := arrDrawingPointsOut[0];

                    result := arrDrawingPointsOut;
                end;


end.
