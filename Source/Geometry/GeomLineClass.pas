unit GeomLineClass;

interface

    uses
        system.sysUtils, system.Math, system.Types,
        GraphicDrawingTypes,
        VectorMethods,
        GeometryMathMethods,
        GeometryTypes, GeomBox,
        GeometryBaseClass, GeomSpaceVectorClass;

    type
        TGeomLine = class(TGeomBase)
            strict private
                const
                    //line vector index constants
                        x : integer = 0;
                        y : integer = 1;
                        z : integer = 2;
                var
                    startPoint, endPoint    : TGeomPoint;
                    lineVector              : TGeomSpaceVector;
                //helper methods
                    //calculat line projections on 3 axes
                        procedure calculateAxisProjections();
                    //assign points
                        procedure assignPoints(startPointIn, endPointIn : TGeomPoint);
            strict protected
                //
            public
                //constructor
                    constructor create(); overload;
                    constructor create(startPointIn, endPointIn : TGeomPoint); overload;
                //destructor
                    destructor destroy(); override;
                //accessors
                    function getDrawingType() : EGraphicObjectType; override;
                    function getStartPoint() : TGeomPoint;
                    function getEndPoint() : TGeomPoint;
                //modifiers
                    procedure setStartPoint(const xIn, yIn : double); overload;
                    procedure setStartPoint(const xIn, yIn, zIn : double); overload;
                    procedure setStartPoint(startPointIn : TGeomPoint); overload;
                    procedure setEndPoint(const xIn, yIn : double); overload;
                    procedure setEndPoint(const xIn, yIn, zIn : double); overload;
                    procedure setEndPoint(endPointIn : TGeomPoint); overload;
                    procedure setPoints(startPointIn, endPointIn : TGeomPoint);
                //calculattions
                    //line length
                        function calculateLength() : double; overload;
                        class function calculateLength(const startPointIn, endPointIn : TGeomPoint) : double; overload; static;
                    //unit vector
                        function unitVector() : TGeomSpaceVector;
                    //parametric line equation point
                        function parametricEquationPoint(tIn : double) : TGeomPoint;
                    //line intersection
                        function calculateLineIntersection( const lineIn        : TGeomLine;
                                                            const freeLineIn    : boolean = True) : TGeomLineIntersectionData; overload;
                        class function calculateLineIntersection(   const line1In, line2In  : TGeomLine;
                                                                    const freeLinesIn       : boolean = True    ) : TGeomLineIntersectionData; overload; static;
                //bounding box
                    function boundingBox() : TGeomBox; override;
                //drawing points
                    function getDrawingPoints() : TArray<TGeomPoint>; override;
                //shift geometry
                    procedure shift(const deltaXIn, deltaYIn, deltaZIn : double); override;

        end;

implementation

//----------------------------------------------------------------------------------------------------
    //private
        //helper methods
            //calculat line projections on 3 axes
                //x-axis (x-component)
                    procedure TGeomLine.calculateAxisProjections();
                        begin
                            lineVector[x] := endPoint.x - startPoint.x;
                            lineVector[y] := endPoint.y - startPoint.y;
                            lineVector[z] := endPoint.z - startPoint.z;
                        end;

            //assign points
                procedure TGeomLine.assignPoints(startPointIn, endPointIn : TGeomPoint);
                    begin
                        startPoint.copyPoint( startPointIn );
                        endPoint.copyPoint( endPointIn );

                        calculateAxisProjections();
                    end;

    //protected

    //public
        //constructor
            constructor TGeomLine.create();
                begin
                    inherited create();

                    lineVector := TGeomSpaceVector.create();

                    lineVector.setDimensions(3);
                end;

            constructor TGeomLine.create(startPointIn, endPointIn : TGeomPoint);
                begin
                    create();

                    assignPoints(startPointIn, endPointIn);
                end;

        //destructor
            destructor TGeomLine.destroy();
                begin
                    FreeAndNil(lineVector);

                    inherited destroy();
                end;

        //calculations
            //line length
                function TGeomLine.calculateLength() : double;
                    begin
                        result := calculateLength( startPoint, endPoint );
                    end;

                class function TGeomLine.calculateLength(const startPointIn, endPointIn : TGeomPoint) : double;
                    var
                        dx, dy, dz : double;
                    begin
                        dx := endPointIn.x - startPointIn.x; 
                        dy := endPointIn.y - startPointIn.y; 
                        dz := endPointIn.z - startPointIn.z; 
                    
                        result := vectorNormalise([dx, dy, dx]);                                           
                    end;

            //unit vector
                function TGeomLine.unitVector() : TGeomSpaceVector;
                    begin
                        result := lineVector.calculateUnitVector();
                    end;

            //parametric line equation point
                function TGeomLine.parametricEquationPoint(tIn : double) : TGeomPoint;
                    var
                        lineUnitVector  : TGeomSpaceVector;
                        pointOut        : TGeomPoint;
                    begin
                        lineUnitVector := unitVector();

                        //(x, y, z) = (x0, y0, z0) + t<ux, uy, uz>
                            pointOut.x := startPoint.x + tIn * lineUnitVector[0];
                            pointOut.y := startPoint.y + tIn * lineUnitVector[1];
                            pointOut.z := startPoint.z + tIn * lineUnitVector[2];

                        FreeAndNil(lineUnitVector);

                        result := pointOut;
                    end;

            //line intersection
                function TGeomLine.calculateLineIntersection(   const lineIn        : TGeomLine;
                                                                const freeLineIn    : boolean = True) : TGeomLineIntersectionData;
                    var
                        lineIntersectionDataOut : TGeomLineIntersectionData;
                    begin
                        //get intersection data
                            lineIntersectionDataOut := calculateLineIntersection(self, lineIn, false);

                        //determine intersection point region
                            if (lineIntersectionDataOut.intersectionExists) then
                                begin
                                    if (self.boundingBox().pointIsWithin(lineIntersectionDataOut.point)) then
                                        lineIntersectionDataOut.relativeToBound := EBoundaryRelation.brInside
                                    else
                                        lineIntersectionDataOut.relativeToBound := EBoundaryRelation.brOutside;
                                end;

                        //free line if necessary
                            if (freeLineIn) then
                                FreeAndNil(lineIn);

                        result := lineIntersectionDataOut;
                    end;

                class function TGeomLine.calculateLineIntersection( const line1In, line2In  : TGeomLine;
                                                                    const freeLinesIn       : boolean = True    ) : TGeomLineIntersectionData;
                    var
                        lineIntersectionDataOut     : TGeomLineIntersectionData;
                    procedure
                        _getIntersectionPoint();
                            var
                                line1Point0, line1Point1,
                                line2Point0, line2Point1 : TGeomPoint;
                            begin
                                //get points from lines
                                    //line 1
                                        line1Point0 := line1In.getStartPoint();
                                        line1Point1 := line1In.getEndPoint();

                                    //line 2
                                        line2Point0 := line2In.getStartPoint();
                                        line2Point1 := line2In.getEndPoint();

                                //calculate intersection point
                                    lineIntersectionDataOut.point := geomLineIntersectionPoint( lineIntersectionDataOut.intersectionExists,
                                                                                                line1Point0, line1Point1,
                                                                                                line2Point0, line2Point1                    );
                            end;
                    procedure
                        _determineIntersectionRegion();
                            var
                                isWithinLine1, isWithinLine2    : boolean;
                                line1Bound, line2Bound          : TGeomBox;
                            begin
                                //get line bounding boxes
                                    line1Bound := line1In.boundingBox();
                                    line2Bound := line2In.boundingBox();

                                //test if point is on either line
                                    isWithinLine1 := line1Bound.pointIsWithin(lineIntersectionDataOut.point);
                                    isWithinLine2 := line2Bound.pointIsWithin(lineIntersectionDataOut.point);

                                if (isWithinLine1 OR isWithinLine2) then
                                    lineIntersectionDataOut.relativeToBound := EBoundaryRelation.brInside
                                else
                                    lineIntersectionDataOut.relativeToBound := EBoundaryRelation.brOutside;
                            end;
                    procedure
                        _freeLines();
                            begin
                                //free lines if necessary
                                    if (freeLinesIn) then
                                        begin
                                            FreeAndNil(line1In);
                                            FreeAndNil(line2In);
                                        end;
                            end;
                    begin
                        _getIntersectionPoint();

                        if (lineIntersectionDataOut.intersectionExists) then
                            _determineIntersectionRegion();

                        _freeLines();

                        result := lineIntersectionDataOut;
                    end;

        //accessors
            function TGeomLine.getDrawingType() : EGraphicObjectType;
                begin
                    result := EGraphicObjectType.gdLine;
                end;

            function TGeomLine.getStartPoint() : TGeomPoint;
                begin
                    result := startPoint;
                end;

            function TGeomLine.getEndPoint() : TGeomPoint;
                begin
                    result := endPoint;
                end;

        //modifiers
            procedure TGeomLine.setStartPoint(const xIn, yIn : double);
                begin
                    setStartPoint( xIn, yIn, 0 );
                end;

            procedure TGeomLine.setStartPoint(const xIn, yIn, zIn : double);
                var
                    newStartPoint : TGeomPoint;
                begin
                    newStartPoint := TGeomPoint.create( xIn, yIn, zIn );

                    setStartPoint( newStartPoint );
                end;

            procedure TGeomLine.setStartPoint(startPointIn : TGeomPoint);
                begin
                    assignPoints(startPointIn, endPoint);
                end;

            procedure TGeomLine.setEndPoint(const xIn, yIn : double);
                begin
                    setEndPoint( xIn, yIn, 0 );
                end;

            procedure TGeomLine.setEndPoint(const xIn, yIn, zIn : double);
                var
                    newEndPoint : TGeomPoint;
                begin
                    newEndPoint := TGeomPoint.create( xIn, yIn, zIn );

                    setEndPoint( newEndPoint );
                end;

            procedure TGeomLine.setEndPoint(endPointIn : TGeomPoint);
                begin
                    assignPoints(startPoint, endPointIn);
                end;

            procedure TGeomLine.setPoints(startPointIn, endPointIn : TGeomPoint);
                begin
                    assignPoints(startPointIn, endPointIn);
                end;

        //bounding box
            function TGeomLine.boundingBox() : TGeomBox;
                var
                    boxOut : TGeomBox;
                begin
                    boxOut := TGeomBox.create(startPoint, endPoint);

                    result := boxOut;
                end;

        //drawing points
            function TGeomLine.getDrawingPoints() : TArray<TGeomPoint>;
                var
                    arrPointsOut : TArray<TGeomPoint>;
                begin
                    SetLength(arrPointsOut, 2);

                    arrPointsOut[0] := startPoint;
                    arrPointsOut[1] := endPoint;

                    result := arrPointsOut;
                end;

        //shift geometry
            procedure TGeomLine.shift(const deltaXIn, deltaYIn, deltaZIn : double);
                begin
                    startPoint.shiftPoint( deltaXIn, deltaYIn, deltaZIn );
                    endPoint.shiftPoint( deltaXIn, deltaYIn, deltaZIn );
                end;

end.
