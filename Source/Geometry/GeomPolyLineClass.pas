unit GeomPolyLineClass;

interface

    uses
        System.SysUtils, system.Math,
        GraphicDrawingTypes,
        GeometryTypes, GeomBox,
        GeometryMathMethods,
        GeometryBaseClass,
        GeomLineClass;

    type
        TGeomPolyLine = class(TGeomBase)
            private
                //
            protected
                //
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //accessors
                    function getDrawingType() : EGraphicObjectType; override;
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
                    function calculatePolylineLength() : double; overload;
                    class function calculatePolylineLength(const arrPointsIn : TArray<TGeomPoint>) : double; overload; static;
                //helper methods
                    function vertexCount() : integer;
                //bounding box
                    function boundingBox() : TGeomBox; override;
                //drawing points
                    function getDrawingPoints() : TArray<TGeomPoint>; override;
                //shift geometry
                    procedure shift(const deltaXIn, deltaYIn, deltaZIn : double); override;
        end;

implementation

    //private
        //

    //protected

    //public
        //constructor
            constructor TGeomPolyLine.create();
                begin
                    inherited create();

                    SetLength(arrGeomPoints,  0);
                end;

        //destructor
            destructor TGeomPolyLine.destroy();
                begin
                    inherited destroy();
                end;

        //accessors
            function TGeomPolyLine.getDrawingType() : EGraphicObjectType;
                begin
                    result := EGraphicObjectType.gdPolyline;
                end;

            function TGeomPolyLine.getVertex(indexIn : integer) : TGeomPoint;
                begin
                    result := arrGeomPoints[indexIn];
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
                                dx := abs(newVertexIn.x - arrGeomPoints[i].x);
                                dy := abs(newVertexIn.y - arrGeomPoints[i].y);
                                dz := abs(newVertexIn.z - arrGeomPoints[i].z);

                                dp := sqrt( power(dx, 2) + power(dy, 2) + power(dz, 2) );

                                samePointTest := (dp < 1e-6);

                                if (samePointTest = True) then
                                    begin
                                        result := false;

                                        exit;
                                    end;
                            end;

                    //increment vertex array
                        SetLength(arrGeomPoints, vertexCount() + 1);

                    //add new vertex to array
                        arrGeomPoints[vertexCount() - 1].copyPoint( newVertexIn );
                end;

            //edit a currently selected vertex
                procedure TGeomPolyLine.editVertex( indexIn         : integer;
                                                    xIn, yIn, zIn   : double    );
                    begin
                        arrGeomPoints[indexIn].setPoint( xIn, yIn, zIn );
                    end;

                procedure TGeomPolyLine.editVertex( indexIn     : integer;
                                                    newPointIn  : TGeomPoint);
                    begin
                        arrGeomPoints[indexIn].copyPoint( newPointIn );
                    end;

        //calculations
            function TGeomPolyLine.calculatePolylineLength() : double;
                begin
                    result := calculatePolylineLength( arrGeomPoints );
                end;

            class function TGeomPolyLine.calculatePolylineLength(const arrPointsIn : TArray<TGeomPoint>) : double;
                var
                    i           : integer;
                    lengthSum   : double;
                begin
                    lengthSum := 0;

                    for i := 0 to (length(arrPointsIn) - 2) do
                        lengthSum := lengthSum + TGeomLine.calculateLength( arrPointsIn[i], arrPointsIn[i + 1] );

                    result := lengthSum;
                end;

        //helper methods
            function TGeomPolyLine.vertexCount() : integer;
                begin
                    result := Length(arrGeomPoints);
                end;

        //bounding box
            function TGeomPolyLine.boundingBox() : TGeomBox;
                begin
                    result := TGeomBox.determineBoundingBox( arrGeomPoints );
                end;

        //drawing points
            function TGeomPolyLine.getDrawingPoints() : TArray<TGeomPoint>;
                begin
                    result := arrGeomPoints;
                end;

        //shift geometry
            procedure TGeomPolyLine.shift(const deltaXIn, deltaYIn, deltaZIn : double);
                var
                    i : integer;
                begin
                    for i := 0 to ( vertexCount() - 1 ) do
                        arrGeomPoints[i].shiftPoint( deltaXIn, deltaYIn, deltaZIn );
                end;

end.
