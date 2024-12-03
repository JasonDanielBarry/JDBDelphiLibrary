unit GeometryTypes;

interface

     uses
        System.SysUtils, system.Math, system.Types,
        GeneralMathMethods
        ;

     type
        {$SCOPEDENUMS ON}
            EAxis = (eaX = 0, eaY = 1, eaZ = 2);
            EBoundaryRelation = (brInside = 0, brOn = 1, brOutside = 2);
        {$SCOPEDENUMS OFF}

        TGeomPoint = record
            var
                x, y, z : double;
            //constructors
                constructor create(const xIn, yIn, zIn : double); overload;
                constructor create(const xIn, yIn : double); overload;
                constructor create(const PointFIn : TPointF); overload;
                constructor create(const PointIn : TPoint); overload;
            //set point
                procedure copyPoint(const otherGeomPointIn : TGeomPoint);
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
            //scale point distance from other point
                procedure scalePoint(   const scaleFactorIn     : double;
                                        const referencePointIn  : TGeomPoint    );
            //comparison
                function greaterThan(const pointIn : TGeomPoint) : boolean;
                function greaterThanOrEqual(const pointIn : TGeomPoint) : boolean;
                function isEqual(const pointIn : TGeomPoint) : boolean;
                function lessThan(const pointIn : TGeomPoint) : boolean;
                function lessThanOrEqual(const pointIn : TGeomPoint) : boolean;

            //calculate centre point
                class function calculateCentrePoint(const arrPointsIn : TArray<TGeomPoint>) : TGeomPoint; static;


        end;

        TGeomLineIntersectionData = record
            intersectionExists  : boolean;
            relativeToBound     : EBoundaryRelation;
            point               : TGeomPoint;
        end;

implementation

    //constructors
        constructor TGeomPoint.create(const xIn, yIn, zIn : double);
            begin
                setPoint( xIn, yIn, zIn );
            end;

        constructor TGeomPoint.create(const xIn, yIn : double);
            begin
                setPoint( xIn, yIn );
            end;

        constructor TGeomPoint.create(const PointFIn : TPointF);
            begin
                setPoint( PointFIn );
            end;

        constructor TGeomPoint.create(const PointIn : TPoint);
            begin
                setPoint( PointIn )
            end;

    //set point
        procedure TGeomPoint.copyPoint(const otherGeomPointIn : TGeomPoint);
            begin
                setPoint(
                            otherGeomPointIn.x,
                            otherGeomPointIn.y,
                            otherGeomPointIn.z
                        );
            end;

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
                shiftX( deltaXIn );
                shiftY( deltaYIn );
                shiftZ( deltaZIn );
            end;

    //scale point distance from other point
        procedure TGeomPoint.scalePoint(const scaleFactorIn     : double;
                                        const referencePointIn  : TGeomPoint);
            begin
                //scale the x value
                    scaleLinear(referencePointIn.x, self.x,
                                scaleFactorIn,      self.x  );

                //scale the y value
                    scaleLinear(referencePointIn.y, self.y,
                                scaleFactorIn,      self.y  );

                //scale the z value
                    scaleLinear(referencePointIn.z, self.z,
                                scaleFactorIn,      self.z  );
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
            const
                EQUALITY_TOLERANCE : double = 1e-6;
            begin
                result :=       SameValue(pointIn.x, self.x, EQUALITY_TOLERANCE)
                            AND SameValue(pointIn.y, self.y, EQUALITY_TOLERANCE)
                            AND SameValue(pointIn.z, self.z, EQUALITY_TOLERANCE)
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

    //calculate centre point
        class function TGeomPoint.calculateCentrePoint(const arrPointsIn : TArray<TGeomPoint>) : TGeomPoint;
            var
                i, pointCount                                           : integer;
                centroidX,          centroidY,          centroidZ,
                sumPointMomentX,    sumPointMomentY,    sumPointMomentZ : double;
            begin
                pointCount := length( arrPointsIn );

                sumPointMomentX := 0;
                sumPointMomentY := 0;
                sumPointMomentZ := 0;

                for i := 0 to (pointCount - 1) do
                    begin
                        sumPointMomentX := sumPointMomentX + arrPointsIn[i].x;
                        sumPointMomentY := sumPointMomentY + arrPointsIn[i].y;
                        sumPointMomentZ := sumPointMomentZ + arrPointsIn[i].z;
                    end;

                centroidX := sumPointMomentX / pointCount;
                centroidY := sumPointMomentY / pointCount;
                centroidZ := sumPointMomentZ / pointCount;

                result := TGeomPoint.create( centroidX, centroidY, centroidZ );
            end;

end.
