unit GeometryMathMethods;

interface

    uses
        System.SysUtils, system.Math,
        GeneralMathMethods,
        LineIntersectionMethods,
        LinearAlgebraTypes,
        MatrixMethods,
        GeometryTypes
        ;

    //calculate distance between 2 lines
        function geomLineLength(point1In, point2In : TGeomPoint) : double;

    //calculate triangle area
        //given three vertices
            function geomTriangleArea(point1In, point2In, point3In : TGeomPoint) : double; overload;

        //given two vertices
            function geomTriangleArea(point1In, point2In : TGeomPoint) : double; overload;

    //calculate the area of a polygon
        //shoelace formula calculation
            function geomPolygonArea(arrGeomPointsIn : TArray<TGeomPoint>) : double;

    //calculate the intersection point of two lines
        function geomLineIntersectionPoint( out     linesIntersectOut               : boolean;
                                            const   line1Point0In, line1Point1In,
                                                    line2Point0In, line2Point1In    : TGeomPoint) : TGeomPoint;

implementation

    //calculate distance between 2 lines
        function geomLineLength(point1In, point2In : TGeomPoint) : double;
            var
                x1, y1, z1,
                x2, y2, z2 : double;
            begin
                x1 := point1In.x;
                y1 := point1In.y;
                z1 := point1In.z;

                x2 := point2In.x;
                y2 := point2In.y;
                z2 := point2In.z;

                result := lineLength(   x1, y1, z1,
                                        x2, y2, z2  );
            end;

    //calculate triangle area
        //helper method
            function triangleArea(const x1, y1,
                                        x2, y2,
                                        x3, y3  : double) : double;
                var
                    coordinateMatrix : TLAMatrix;
                begin
                    coordinateMatrix := [
                                            [x1, y1, 1],
                                            [x2, y2, 1],
                                            [x3, y3, 1]
                                        ];

                    result := 0.5 * matrixDeterminant(coordinateMatrix);
                end;

        //given three vertices
            function geomTriangleArea(point1In, point2In, point3In : TGeomPoint) : double;
                var
                    x1, y1,
                    x2, y2,
                    x3, y3 : double;
                begin
                    //extract values from points
                        x1 := point1In.x;
                        y1 := point1In.y;

                        x2 := point2In.x;
                        y2 := point2In.y;

                        x3 := point3In.x;
                        y3 := point3In.y;

                    result := triangleArea( x1, y1,
                                            x2, y2,
                                            x3, y3  );
                end;

        //given two vertices
            function geomTriangleArea(point1In, point2In : TGeomPoint) : double;
                var
                    point3 : TGeomPoint;
                begin
                    point3 := TGeomPoint.create(0, 0);

                    result := geomTriangleArea(point1In, point2In, point3);
                end;

    //calculate the area of a polygon
        //shoelace formula calculation
            function geomPolygonArea(arrGeomPointsIn : TArray<TGeomPoint>) : double;
                var
                    i, arrLen   : integer;
                    areaSum     : double;
                begin
                    areaSum := 0;

                    arrLen := Length(arrGeomPointsIn);

                    //shoelace calculation
                        for i := 0 to (arrLen - 2) do
                            areaSum := areaSum + geomTriangleArea(arrGeomPointsIn[i], arrGeomPointsIn[i + 1]);

                        areaSum := areaSum + geomTriangleArea(arrGeomPointsIn[arrLen - 1], arrGeomPointsIn[0]);

                    result := areaSum;
                end;

    //calculate the intersection point of two lines
        function geomLineIntersectionPoint( out     linesIntersectOut               : boolean;
                                            const   line1Point0In, line1Point1In,
                                                    line2Point0In, line2Point1In    : TGeomPoint) : TGeomPoint;
            var
                l1x0, l1y0, l1x1, l1y1,
                l2x0, l2y0, l2x1, l2y1  : double;
                intersectionPoint       : TGeomPoint;
                intersectionDataOut     : TGeomLineIntersectionData;
            begin
                //line 1 info
                    l1x0 := line1Point0In.x;
                    l1y0 := line1Point0In.y;
                    l1x1 := line1Point1In.x;
                    l1y1 := line1Point1In.y;

                //line 2 info
                    l2x0 := line2Point0In.x;
                    l2y0 := line2Point0In.y;
                    l2x1 := line2Point1In.x;
                    l2y1 := line2Point1In.y;

                result := TGeomPoint.create(
                                                lineIntersectionPoint(  linesIntersectOut,
                                                                        l1x0, l1y0, l1x1, l1y1,
                                                                        l2x0, l2y0, l2x1, l2y1  )
                                           );
            end;

end.
