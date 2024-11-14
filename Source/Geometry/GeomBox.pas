unit GeomBox;

interface

    uses
        System.SysUtils, system.Math, system.Math.Vectors, system.Types,
        GeneralMathMethods,
        GeometryTypes
        ;

    type
        TGeomBox = record
            var
                minPoint, maxPoint : TGeomPoint;
            //construction
                constructor create(const point1In, point2In : TGeomPoint); overload;
                constructor create(const arrGeomBoxesIn : TArray<TGeomBox>); overload;
            //copy other
                procedure copyOther(const otherBoxIn : TGeomBox);
            //centre point
                function calculateCentreX() : double;
                function calculateCentreY() : double;
                function calculateCentreZ() : double;
                function getCentrePoint() : TGeomPoint;
            //set boundaries
                procedure setXBounds(const xMinIn, xMaxIn : double);
                procedure setYBounds(const yMinIn, yMaxIn : double);
                procedure setZBounds(const zMinIn, zMaxIn : double);
                procedure setBounds(const   xMinIn, xMaxIn,
                                            yMinIn, yMaxIn,
                                            zMinIn, zMaxIn  : double);
            //set points
                procedure setPoints(const point1In, point2In : TGeomPoint);
            //shift box
                procedure shiftX(const deltaXIn : double);
                procedure shiftY(const deltaYIn : double);
                procedure shiftZ(const deltaZIn : double);
                procedure shiftBox(const deltaXIn, deltaYIn : double); overload;
                procedure shiftBox(const deltaXIn, deltaYIn, deltaZIn : double); overload;
            //comparison
                function pointIsWithin(const pointIn : TGeomPoint) : boolean;
            //calculate dimensions
                function calculateXDimension() : double;
                function calculateYDimension() : double;
                function calculateZDimension() : double;
            //min and max properties
                property xMin : double read minPoint.x;
                property yMin : double read minPoint.y;
                property zMin : double read minPoint.z;
                property xMax : double read maxPoint.x;
                property yMax : double read maxPoint.y;
                property zMax : double read maxPoint.z;
            //determine the bounding box from an array of bounding boxes
                class function determineBoundingBox(arrGeomBoxesIn : TArray<TGeomBox>) : TGeomBox; static;
        end;

implementation

    //construction
        constructor TGeomBox.create(const point1In, point2In : TGeomPoint);
            begin
                setPoints( point1In, point2In );
            end;

        constructor TGeomBox.create(const arrGeomBoxesIn : TArray<TGeomBox>);
            begin
                Self.copyOther( determineBoundingBox(arrGeomBoxesIn) );
            end;

    //copy other
        procedure TGeomBox.copyOther(const otherBoxIn : TGeomBox);
            begin
                self.setPoints( otherBoxIn.minPoint, otherBoxIn.maxPoint );
            end;

    //centre point
        function TGeomBox.calculateCentreX() : double;
            begin
                result := mean([minPoint.x, maxPoint.x]);
            end;

        function TGeomBox.calculateCentreY() : double;
            begin
                result := mean([minPoint.y, maxPoint.y]);
            end;

        function TGeomBox.calculateCentreZ() : double;
            begin
                result := mean([minPoint.z, maxPoint.z]);
            end;

        function TGeomBox.getCentrePoint() : TGeomPoint;
            var
                centreX, centreY, centreZ : double;
            begin
                centreX := calculateCentreX();
                centreY := calculateCentreY();
                centreZ := calculateCentreZ();

                result := TGeomPoint.create(centreX, centreY, centreZ);
            end;

        //set boundaries
            procedure TGeomBox.setXBounds(const xMinIn, xMaxIn : double);
                begin
                    minPoint.x := xMinIn;
                    maxPoint.x := xMaxIn;
                end;

            procedure TGeomBox.setYBounds(const yMinIn, yMaxIn : double);
                begin
                    minPoint.y := yMinIn;
                    maxPoint.y := yMaxIn;
                end;

            procedure TGeomBox.setZBounds(const zMinIn, zMaxIn : double);
                begin
                    minPoint.z := zMinIn;
                    maxPoint.z := zMaxIn;
                end;

            procedure TGeomBox.setBounds(const  xMinIn, xMaxIn,
                                                yMinIn, yMaxIn,
                                                zMinIn, zMaxIn  : double);
                begin
                    minPoint.setPoint(xMinIn, yMinIn, zMinIn);
                    maxPoint.setPoint(xMaxIn, yMaxIn, zMaxIn);
                end;

    //set points
        procedure TGeomBox.setPoints(const point1In, point2In : TGeomPoint);
            begin
                //min point
                    minPoint.x := min(point1In.x, point2In.x);
                    minPoint.y := min(point1In.y, point2In.y);
                    minPoint.z := min(point1In.z, point2In.z);
                //max point
                    maxPoint.x := max(point1In.x, point2In.x);
                    maxPoint.y := max(point1In.y, point2In.y);
                    maxPoint.z := max(point1In.z, point2In.z);
            end;

    //shift box
        procedure TGeomBox.shiftX(const deltaXIn : double);
            begin
                minPoint.shiftX( deltaXIn );
                maxPoint.shiftX( deltaXIn );
            end;

        procedure TGeomBox.shiftY(const deltaYIn : double);
            begin
                minPoint.shiftY( deltaYIn );
                maxPoint.shiftY( deltaYIn );
            end;

        procedure TGeomBox.shiftZ(const deltaZIn : double);
            begin
                minPoint.shiftZ( deltaZIn );
                maxPoint.shiftZ( deltaZIn );
            end;

        procedure TGeomBox.shiftBox(const deltaXIn, deltaYIn : double);
            begin
                shiftX( deltaXIn );
                shiftY( deltaYIn );
            end;

        procedure TGeomBox.shiftBox(const deltaXIn, deltaYIn, deltaZIn : double);
            begin
                shiftBox( deltaXIn, deltaYIn );
                shiftZ( deltaZIn );
            end;

    //comparison
        function TGeomBox.pointIsWithin(const pointIn: TGeomPoint): boolean;
            var
                greaterThanMinPoint, lessThanMaxPoint : boolean;
            begin
                greaterThanMinPoint := pointIn.greaterThanOrEqual(minPoint);

                lessThanMaxPoint := pointIn.lessThanOrEqual(maxPoint);

                result := (greaterThanMinPoint AND lessThanMaxPoint);
            end;

    //calculate dimensions
        function TGeomBox.calculateXDimension() : double;
            begin
                result := maxPoint.x - minPoint.x;
            end;

        function TGeomBox.calculateYDimension() : double;
            begin
                result := maxPoint.y - minPoint.y;
            end;

        function TGeomBox.calculateZDimension() : double;
            begin
                result := maxPoint.z - minPoint.z;
            end;

    //bounding box function
        class function TGeomBox.determineBoundingBox(arrGeomBoxesIn : TArray<TGeomBox>) : TGeomBox;
            var
                i                               : integer;
                localMinPoint, localMaxPoint    : TGeomPoint;
                boundingBoxOut                  : TGeomBox;
            begin
                localMinPoint := arrGeomBoxesIn[0].minPoint;
                localMaxPoint := arrGeomBoxesIn[0].maxPoint;

                for i := 1 to (length(arrGeomBoxesIn) - 1) do
                    begin
                        //look for min x, y, z
                            localMinPoint.x := min( localMinPoint.x, arrGeomBoxesIn[i].minPoint.x );
                            localMinPoint.y := min( localMinPoint.y, arrGeomBoxesIn[i].minPoint.y );
                            localMinPoint.z := min( localMinPoint.z, arrGeomBoxesIn[i].minPoint.z );

                        //look for max x, y, z
                            localMaxPoint.x := max( localMaxPoint.x, arrGeomBoxesIn[i].maxPoint.x );
                            localMaxPoint.y := max( localMaxPoint.y, arrGeomBoxesIn[i].maxPoint.y );
                            localMaxPoint.z := max( localMaxPoint.z, arrGeomBoxesIn[i].maxPoint.z );
                    end;

                boundingBoxOut := TGeomBox.create(localMinPoint, localMaxPoint);

                result := boundingBoxOut;
            end;

end.
