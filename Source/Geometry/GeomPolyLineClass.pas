unit GeomPolyLineClass;

interface

    uses
        System.SysUtils, system.Math,
        GeometryTypes,
        GeometryMathMethods,
        GeometryBaseClass,
        GeomLineClass;

    type
        TGeomPolyLine = class(TGeomBase)
            private
                //member variables
                    arrLines : TArray<TGeomLine>;
                //helper methods
                    //get line count
                        function lineCount() : integer;
                    //update the polyline
                        procedure updatePolyLine();
            protected
                //member variables
                    arrVertices : TArray<TGeomPoint>;
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //accessors
                    function getGeomType() : EGeomType; override;
                    function getVertex(indexIn : integer) : TGeomPoint;
                //modifiers
                    //add a new vertex and line
                        function addVertex(xIn, yIn : double) : boolean; overload;
                        function addVertex(xIn, yIn, zIn : double) : boolean; overload;
                        function addVertex(newVertexIn : TGeomPoint) : boolean; overload;
                    //edit a currently selected vertex
                        procedure editVertex(   indexIn         : integer;
                                                xIn, yIn, zIn   : double    ); overload;
                        procedure editVertex(   indexIn     : integer;
                                                newPointIn  : TGeomPoint); overload;
                //calculations
                    function lineLength() : double;
                    function polygonPerimeter() : double;
                    function polygonArea() : double;
                //helper methods
                    function vertexCount() : integer;
                //bounding box
                    function boundingBox() : TGeomBox; override;
                //drawing points
                    function drawingPoints() : TArray<TGeomPoint>; override;
        end;

implementation

    //private
        //helper methods
            //get the line count
                function TGeomPolyLine.lineCount() : integer;
                    begin
                        result := length(arrLines);
                    end;

            //update the polyline
                procedure TGeomPolyLine.updatePolyLine();
                    var
                        i : integer;
                    begin
                        for i := 0 to (lineCount() - 1) do
                            arrLines[i].setPoints(arrVertices[i], arrVertices[i + 1]);
                    end;

    //protected

    //public
        //constructor
            constructor TGeomPolyLine.create();
                begin
                    inherited create();

                    SetLength(arrLines,     0);
                    SetLength(arrVertices,  0);
                end;

        //destructor
            destructor TGeomPolyLine.destroy();
                var
                    i : integer;
                begin
                    //free all the line classes
                        for i := 0 to (lineCount() - 1) do
                            begin
                                freeAndNil(arrLines[i]);
                            end;

                    SetLength(arrLines, 0);

                    inherited destroy();
                end;

        //accessors
            function TGeomPolyLine.getGeomType() : EGeomType;
                begin
                    result := EGeomType.gtPolyline;
                end;

            function TGeomPolyLine.getVertex(indexIn : integer) : TGeomPoint;
                begin
                    result := arrVertices[indexIn];
                end;

        //add a line to the array of lines
            function TGeomPolyLine.addVertex(xIn, yIn : double) : boolean;
                begin
                    result := addVertex(xIn, yIn, 0);
                end;

            function TGeomPolyLine.addVertex(xIn, yIn, zIn : double) : boolean;
                begin
                    result := addVertex(
                                            TGeomPoint.create(xIn, yIn, zIn)
                                       );
                end;

            function TGeomPolyLine.addVertex(newVertexIn : TGeomPoint) : boolean;
                var
                    samePointTest                       : boolean;
                    i                                   : integer;
                    dx, dy, dz, dp                      : double;
                    newLineStartPoint, newLineEndPoint  : TGeomPoint;
                begin
                    result := true;

                    //test to see if the new point already exists
                        for i := 0 to (vertexCount() - 1) do
                            begin
                                dx := abs(newVertexIn.x - arrVertices[i].x);
                                dy := abs(newVertexIn.y - arrVertices[i].y);
                                dz := abs(newVertexIn.z - arrVertices[i].z);

                                dp := sqrt( power(dx, 2) + power(dy, 2) + power(dz, 2) );

                                samePointTest := (dp < 1e-6);

                                if (samePointTest = True) then
                                    begin
                                        updatePolyLine();

                                        result := false;

                                        exit;
                                    end;
                            end;

                    //increment vertex array
                        SetLength(arrVertices, vertexCount() + 1);

                    //add new vertex to array
                        arrVertices[vertexCount() - 1] := newVertexIn;

                    //after adding the first point a line cannot be made - test if there are enough points to make a line
                        if (length(arrVertices) > 1) then
                            begin
                                //create new Line
                                    SetLength(arrLines, vertexCount() - 1);

                                //get the points for the new line
                                    newLineStartPoint   := arrVertices[vertexCount() - 2];
                                    newLineEndPoint     := arrVertices[vertexCount() - 1];

                                //create new line
                                    arrLines[lineCount() - 1] := TGeomLine.create(newLineStartPoint, newLineEndPoint);
                            end;

                    //double check the points are assigned correctly
                        updatePolyLine();
                end;

            //edit a currently selected vertex
                procedure TGeomPolyLine.editVertex( indexIn         : integer;
                                                    xIn, yIn, zIn   : double    );
                    begin
                        arrVertices[indexIn].x := xIn;
                        arrVertices[indexIn].y := yIn;
                        arrVertices[indexIn].z := zIn;

                        updatePolyLine();
                    end;

                procedure TGeomPolyLine.editVertex( indexIn     : integer;
                                                    newPointIn  : TGeomPoint);
                    begin
                        editVertex( indexIn,
                                    newPointIn.x, newPointIn.y, newPointIn.z);
                    end;

        //calculations
            function TGeomPolyLine.lineLength() : double;
                var
                    i           : integer;
                    lengthSum   : double;
                begin
                    lengthSum := 0;

                    //add all the line length together that comprise the polyline
                        for i := 0 to (lineCount() - 1) do
                            lengthSum := lengthSum + arrLines[i].lineLength();

                    result := lengthSum;
                end;

            function TGeomPolyLine.polygonPerimeter() : double;
                var
                    closingLineLength,
                    polyLineLength          : double;
                    startPoint, endPoint    : TGeomPoint;
                    closingLine             : TGeomLine;
                begin
                    //define the closing line
                        //start point is the polyline last vertex
                            startPoint := arrVertices[vertexCount() - 1];
                        //end point is polyline first vertex
                            endPoint := arrVertices[0];

                        closingLine := TGeomLine.create(startPoint, endPoint);

                        //get the length
                            closingLineLength := closingLine.lineLength();

                        //free closing line
                            FreeAndNil(closingLine);

                    //get the self's length
                        polyLineLength := self.lineLength();

                    //the polygon perimeter = polyline (self) length + closing line length
                        result := closingLineLength + polyLineLength;
                end;

            function TGeomPolyLine.polygonArea() : double;
                begin
                    result := geomPolygonArea(arrVertices);
                end;

        //helper methods
            function TGeomPolyLine.vertexCount() : integer;
                begin
                    result := Length(arrVertices);
                end;

        //bounding box
            function TGeomPolyLine.boundingBox() : TGeomBox;
                var
                    i           : integer;
                    boxOut      : TGeomBox;
                    arrGeomBase : TArray<TGeomBase>;
                begin
                    SetLength(arrGeomBase, length(arrLines));

                    for i := 0 to (length(arrLines) - 1) do
                        arrGeomBase[i] := arrLines[i];

                    boxOut := determineBoundingBox( arrGeomBase );

                    result := boxOut;
                end;

        //drawing points
            function TGeomPolyLine.drawingPoints() : TArray<TGeomPoint>;
                begin
                    result := arrVertices;
                end;

end.
