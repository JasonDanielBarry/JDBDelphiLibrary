unit GeomLineClass;

interface

    uses
        system.sysUtils, system.Math, system.Types,
        GeometryMathMethods,
        GeometryTypes, GeometryBaseClass, GeomSpaceVectorClass;

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
                        procedure updatePoints();
            strict protected
                //
            public
                //constructor
                    constructor create(); overload;
                    constructor create(startPointIn, endPointIn : TGeomPoint); overload;
                //destructor
                    destructor destroy(); override;
                //accessors
                    function getGeomType() : EGeomType; override;
                    function getStartPoint() : TGeomPoint;
                    function getEndPoint() : TGeomPoint;
                //modifiers
                    procedure setStartPoint(startPointIn : TGeomPoint);
                    procedure setEndPoint(endPointIn : TGeomPoint);
                    procedure setPoints(startPointIn, endPointIn : TGeomPoint);
                //calculattions
                    //line length
                        function lineLength() : double;
                    //unit vector
                        function unitVector() : TGeomSpaceVector;
                    //parametric line equation point
                        function parametricEquationPoint(tIn : double) : TGeomPoint;
                    //line intersection
                        function intersection(  const lineIn        : TGeomLine;
                                                const freeLineIn    : boolean = True) : TGeomLineIntersectionData;
                //bounding box
                    function boundingBox() : TGeomBox; override;
                //drawing points
                    function drawingPoints() : TArray<TGeomPoint>; override;
        end;
//----------------------------------------------------------------------------------------------------
    //calculate intersection point
        function geomLineIntersection(  const line1In, line2In  : TGeomLine;
                                        const freeLinesIn       : boolean = True) : TGeomLineIntersectionData;

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
                        startPoint  := startPointIn;
                        endPoint    := endPointIn;

                        calculateAxisProjections();
                    end;

                procedure TGeomLine.updatePoints();
                    begin
                        assignPoints(startPoint, endPoint);
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
                function TGeomLine.lineLength() : double;
                    begin
                        result := lineVector.normalise();
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
                function TGeomLine.intersection(const lineIn        : TGeomLine;
                                                const freeLineIn    : boolean = True) : TGeomLineIntersectionData;
                    var
                        lineIntersectionDataOut : TGeomLineIntersectionData;
                    begin
                        //get intersection data
                            lineIntersectionDataOut := geomLineIntersection(self, lineIn, false);

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

        //accessors
            function TGeomLine.getGeomType() : EGeomType;
                begin
                    result := EGeomType.gtLine;
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
            procedure TGeomLine.setStartPoint(startPointIn : TGeomPoint);
                begin
                    startPoint := startPointIn;

                    updatePoints();
                end;

            procedure TGeomLine.setEndPoint(endPointIn : TGeomPoint);
                begin
                    endPoint := endPointIn;

                    updatePoints();
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
            function TGeomLine.drawingPoints() : TArray<TGeomPoint>;
                var
                    arrPointsOut : TArray<TGeomPoint>;
                begin
                    SetLength(arrPointsOut, 2);

                    arrPointsOut[0] := startPoint;
                    arrPointsOut[1] := endPoint;

                    result := arrPointsOut;
                end;
//----------------------------------------------------------------------------------------------------
    //calculate intersection point
        function geomLineIntersection(  const line1In, line2In  : TGeomLine;
                                        const freeLinesIn       : boolean = True) : TGeomLineIntersectionData;
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
                        isWithinLine1, isWithinLine2   : boolean;
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

end.
