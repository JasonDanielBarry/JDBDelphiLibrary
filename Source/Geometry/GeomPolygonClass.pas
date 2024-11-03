unit GeomPolygonClass;

interface

    uses
        System.SysUtils, system.Math,
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
                    function getGeomType() : EGeomType; override;
                //drawing points
                    function drawingPoints() : TArray<TGeomPoint>; override;
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
            function TGeomPolygon.getGeomType() : EGeomType;
                begin
                    result := EGeomType.gtPolygon;
                end;

        //drawing points
            function TGeomPolygon.drawingPoints() : TArray<TGeomPoint>;
                var
                    arrLen              : integer;
                    arrDrawingPointsOut : TArray<TGeomPoint>;
                begin
                    arrDrawingPointsOut := inherited drawingPoints();

                    arrLen := length(arrDrawingPointsOut);

                    SetLength(arrDrawingPointsOut, arrLen + 1);

                    arrLen := arrLen + 1;

                    //close polygon
                        arrDrawingPointsOut[arrLen - 1] := arrDrawingPointsOut[0];

                    result := arrDrawingPointsOut;
                end;


end.
