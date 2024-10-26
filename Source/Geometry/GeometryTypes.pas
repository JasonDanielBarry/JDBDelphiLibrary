unit GeometryTypes;

interface

     uses
        System.SysUtils, system.Math, system.Math.Vectors, system.Types,
        GeneralMathMethods
        ;

     type
        EAxis = (eaX = 0, eaY = 1, eaZ = 2);
        EBoundaryRelation = (brInside = 0, brOn = 1, brOutside = 2);
        EGeomType = (gtLine = 0, gtPolyline = 1, gtPolygon = 2, gtSpaceVector = 3);
//----------------------------------------------------------------------------------------------------
        TGeomPoint = record
            var
                x, y, z : double;
            //constructors
                constructor create(const xIn, yIn, zIn : double); overload;
                constructor create(const xIn, yIn : double); overload;
                constructor create(const PointFIn : TPointF); overload;
                constructor create(const PointIn : TPoint); overload;
            //set point
                procedure setPoint(const xIn, yIn, zIn : double); overload;
                procedure setPoint(const xIn, yIn : double); overload;
                procedure setPoint(const PointFIn : TPointF); overload;
                procedure setPoint(const PointIn : TPoint); overload;
            //shift point
                procedure shiftX(const deltaXIn : double);
                procedure shiftY(const deltaYIn : double);
                procedure shiftZ(const deltaZIn : double);
                procedure shiftPoint(const deltaXIn, deltaYIn : double); overload;
                procedure shiftPoint(const deltaXIn, deltaYIn, deltaZIn : double); overload;
            //comparison
                function greaterThan(const pointIn : TGeomPoint) : boolean;
                function greaterThanOrEqual(const pointIn : TGeomPoint) : boolean;
                function isEqual(const pointIn : TGeomPoint) : boolean;
                function lessThan(const pointIn : TGeomPoint) : boolean;
                function lessThanOrEqual(const pointIn : TGeomPoint) : boolean;
        end;
//----------------------------------------------------------------------------------------------------
        TGeomBox = record
            var
                minPoint, maxPoint : TGeomPoint;
            //construction
                constructor create(const point1In, point2In : TGeomPoint); overload;
                constructor create(const arrGeomBoxesIn : TArray<TGeomBox>); overload;
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

        end;
//----------------------------------------------------------------------------------------------------
        TGeomLineIntersectionData = record
            intersectionExists  : boolean;
            relativeToBound     : EBoundaryRelation;
            point               : TGeomPoint;
        end;
//----------------------------------------------------------------------------------------------------
     function determineBoundingBox(arrGeomBoxesIn : TArray<TGeomBox>) : TGeomBox;

implementation

//----------------------------------------------------------------------------------------------------
    //TGeomPoint
        //constructors
            constructor TGeomPoint.create(const xIn, yIn, zIn : double);
                begin
                    setPoint( xIn, yIn, zIn );
                end;

            constructor TGeomPoint.create(const xIn, yIn : double);
                begin
                    create(xIn, yIn, 0);
                end;

            constructor TGeomPoint.create(const PointFIn : TPointF);
                begin
                    create(PointFIn.X, PointFIn.Y);
                end;

            constructor TGeomPoint.create(const PointIn : TPoint);
                begin
                    create(PointIn.X, PointIn.Y)
                end;

        //set point
            procedure TGeomPoint.setPoint(const xIn, yIn, zIn : double);
                begin
                    x := xIn;
                    y := yIn;
                    z := zIn;
                end;

            procedure TGeomPoint.setPoint(const xIn, yIn : double);
                begin
                    setPoint( xIn, yIn, 0 );
                end;

            procedure TGeomPoint.setPoint(const PointFIn : TPointF);
                begin
                    setPoint( PointFIn.X, PointFIn.Y );
                end;

            procedure TGeomPoint.setPoint(const PointIn : TPoint);
                begin
                    setPoint( pointIn.X, PointIn.Y );
                end;

        //shift point
            procedure TGeomPoint.shiftX(const deltaXIn : double);
                begin
                    self.x := self.x + deltaXIn;
                end;

            procedure TGeomPoint.shiftY(const deltaYIn : double);
                begin
                    self.y := self.y + deltaYIn;
                end;

            procedure TGeomPoint.shiftZ(const deltaZIn : double);
                begin
                    self.z := self.z + deltaZIn;
                end;

            procedure TGeomPoint.shiftPoint(const deltaXIn, deltaYIn : double);
                begin
                    shiftX( deltaXIn );
                    shiftY( deltaYIn );
                end;

            procedure TGeomPoint.shiftPoint(const deltaXIn, deltaYIn, deltaZIn : double);
                begin
                    shiftPoint( deltaXIn, deltaYIn );
                    shiftZ( deltaZIn );
                end;

        //comparison
            function TGeomPoint.greaterThan(const pointIn : TGeomPoint) : boolean;
                begin
                    result :=       (pointIn.x < self.x)
                                AND (pointIn.y < self.y)
                                AND (pointIn.z < self.z)
                end;

            function TGeomPoint.greaterThanOrEqual(const pointIn: TGeomPoint): Boolean;
                begin
                    result :=       (pointIn.x <= self.x)
                                AND (pointIn.y <= self.y)
                                AND (pointIn.z <= self.z)
                end;

            function TGeomPoint.isEqual(const pointIn : TGeomPoint) : boolean;
                begin
                    result :=       isAlmostEqual(pointIn.x, self.x)
                                AND isAlmostEqual(pointIn.y, self.y)
                                AND isAlmostEqual(pointIn.z, self.z)
                end;

            function TGeomPoint.lessThan(const pointIn: TGeomPoint): boolean;
                begin
                    result :=       (self.x < pointIn.x)
                                AND (self.y < pointIn.y)
                                AND (self.z < pointIn.z)
                end;

            function TGeomPoint.lessThanOrEqual(const pointIn: TGeomPoint): Boolean;
                begin
                    result :=       (self.x <= pointIn.x)
                                AND (self.y <= pointIn.y)
                                AND (self.z <= pointIn.z)
                end;
//----------------------------------------------------------------------------------------------------
    //TGeomBox
        //construction
            constructor TGeomBox.create(const point1In, point2In : TGeomPoint);
                begin
                    setPoints( point1In, point2In );
                end;

            constructor TGeomBox.create(const arrGeomBoxesIn : TArray<TGeomBox>);
                begin
                    Self := determineBoundingBox(arrGeomBoxesIn);
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
//----------------------------------------------------------------------------------------------------
    //bounding box function
        function determineBoundingBox(arrGeomBoxesIn : TArray<TGeomBox>) : TGeomBox;
            var
                i                   : integer;
                minPoint, maxPoint  : TGeomPoint;
                boundingBoxOut      : TGeomBox;
            begin
                minPoint := arrGeomBoxesIn[0].minPoint;
                maxPoint := arrGeomBoxesIn[0].maxPoint;

                for i := 1 to (length(arrGeomBoxesIn) - 1) do
                    begin
                        //look for min x, y, z
                            minPoint.x := min( minPoint.x, arrGeomBoxesIn[i].minPoint.x );
                            minPoint.y := min( minPoint.y, arrGeomBoxesIn[i].minPoint.y );
                            minPoint.z := min( minPoint.z, arrGeomBoxesIn[i].minPoint.z );

                        //look for max x, y, z
                            maxPoint.x := max( maxPoint.x, arrGeomBoxesIn[i].maxPoint.x );
                            maxPoint.y := max( maxPoint.y, arrGeomBoxesIn[i].maxPoint.y );
                            maxPoint.z := max( maxPoint.z, arrGeomBoxesIn[i].maxPoint.z );
                    end;

                boundingBoxOut := TGeomBox.create(minPoint, maxPoint);

                result := boundingBoxOut;
            end;

end.
