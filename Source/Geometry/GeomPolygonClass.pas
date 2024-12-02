unit GeomPolygonClass;

interface

    uses
        System.SysUtils, system.Math,
        GraphicDrawingTypes,
        GeometryTypes,
        GeometryMathMethods,
        GeomLineClass,
        GeomPolyLineClass;

    type
        TGeomPolygon = class(TGeomPolyLine)
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //accessors
                    function getDrawingType() : EGraphicDrawing; override;
                //drawing points
                    function getDrawingPoints() : TArray<TGeomPoint>; override;
                //calculations
                    function calculatePerimeter() : double;
                    function calculatePolygonArea() : double; overload;
                    class function calculatePolygoneArea(const arrPointsIn : TArray<TGeomPoint>) : double; overload; static;
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
            function TGeomPolygon.getDrawingType() : EGraphicDrawing;
                begin
                    result := EGraphicDrawing.gdPolygon;
                end;

        //drawing points
            function TGeomPolygon.getDrawingPoints() : TArray<TGeomPoint>;
                var
                    arrLen              : integer;
                    arrDrawingPointsOut : TArray<TGeomPoint>;
                begin
                    arrDrawingPointsOut := inherited getDrawingPoints();

                    arrLen := length(arrDrawingPointsOut);

                    //close polygon
                        SetLength(arrDrawingPointsOut, arrLen + 1);

                        arrDrawingPointsOut[arrLen] := arrDrawingPointsOut[0];

                    result := arrDrawingPointsOut;
                end;

        //calculations
            function TGeomPolygon.calculatePerimeter() : double;
                var
                    closingLineLength,
                    polylineLength          : double;
                    closingLineStartPoint,
                    closingLineEndPoint    : TGeomPoint;
                begin
                    //calculate the closing line length
                        //start point is the polyline last vertex
                            closingLineStartPoint := arrVertices[vertexCount() - 1];

                        //end point is polyline first vertex
                            closingLineEndPoint := arrVertices[0];

                        //get the length
                            closingLineLength := TGeomLine.calculateLength( closingLineStartPoint, closingLineEndPoint );

                    //get the polyline length
                        polylineLength := calculatePolylineLength();

                    //the polygon perimeter = polyline length + closing line length
                        result := closingLineLength + polylineLength;
                end;

            function TGeomPolygon.calculatePolygonArea() : double;
                begin
                    result := calculatePolygoneArea( arrVertices );
                end;

            class function TGeomPolygon.calculatePolygoneArea(const arrPointsIn : TArray<TGeomPoint>) : double;
                var
                    i, arrLen   : integer;
                    areaSum     : double;
                begin
                    //shoelace formula calculation

                    areaSum := 0;

                    arrLen := Length(arrPointsIn);

                    //shoelace calculation
                        for i := 0 to (arrLen - 2) do
                            areaSum := areaSum + geomTriangleArea(arrPointsIn[i], arrPointsIn[i + 1]);

                        areaSum := areaSum + geomTriangleArea(arrPointsIn[arrLen - 1], arrPointsIn[0]);

                    result := areaSum;
                end;


end.
