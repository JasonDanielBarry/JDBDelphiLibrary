unit GraphicArcClass;

interface

    uses
        //Delphi
            system.SysUtils, system.Math, system.types, system.UITypes, System.UIConsts,
            Winapi.D2D1, Vcl.Direct2D,
            vcl.Graphics,
        //custom
            GraphicObjectBaseClass,
            DrawingAxisConversionClass,
            GeometryTypes,
            GeomBox,
            GeometryBaseClass
            ;

    type
        TGraphicArc = class(TGraphicObject)
            private
                var
                    startAngle, endAngle    : double;
                    arcRadii                : TRectF;
                    centrePoint,
                    startPoint,
                    endPoint                : TGeomPoint;
                //normalise angle
                    function normaliseAngle(const degAngleIn : double) : double;
                //calculate startPoint
                    procedure calculateStartAndEndPoint();
                //create arc segment
                    function createArcSegment(const axisConverterIn : TDrawingAxisConverter) : TD2D1ArcSegment;
                //create arc geometry
                    function createArcGeometry(const axisConverterIn : TDrawingAxisConverter) : ID2D1PathGeometry;
            public
                //constructor
                    constructor create( const   lineThicknessIn : integer;
                                        const   arcXRadiusIn,
                                                arcYRadiusIn,
                                                startAngleIn,
                                                endAngleIn      : double;
                                        const   lineColourIn    : TColor;
                                        const   lineStyleIn     : TPenStyle;
                                        const   centrePointIn   : TGeomPoint );
                //destructor
                    destructor destroy(); override;
                //draw to canvas
                    procedure drawToCanvas( const axisConverterIn   : TDrawingAxisConverter;
                                            var canvasInOut         : TDirect2DCanvas       ); override;
                //bounding box
                    function determineBoundingBox() : TGeomBox; override;
        end;

implementation

    //private
        //normalise angle
            function TGraphicArc.normaliseAngle(const degAngleIn : double) : double;
                const
                    CIRLCE_ROTATION : double = 360;
                var
                    angleOut        : double;
                    angleStartSign  : TValueSign;
                function _validAngle(const angleIn : double) : boolean;
                    begin
                        result := ( -360 < angleIn ) AND ( angleIn < 360 );
                    end;
                begin
                    angleStartSign := sign( degAngleIn );

                    angleOut := degAngleIn;

                    while NOT( _validAngle( angleOut ) ) do
                        angleOut := angleOut - ( angleStartSign * CIRLCE_ROTATION );

                    result := angleOut;
                end;

        //calculate startPoint
            procedure TGraphicArc.calculateStartAndEndPoint();
                var
                    sinComponent, cosComponent : double;
                begin
                    //find start point
                        SinCos( DegToRad( startAngle ), sinComponent, cosComponent );

                        startPoint.x := centrePoint.x + (cosComponent * arcRadii.Width);
                        startPoint.y := centrePoint.y + (sinComponent * arcRadii.Height);

                    //find end point
                        SinCos( DegToRad( endAngle ), sinComponent, cosComponent );

                        endPoint.x := centrePoint.x + (cosComponent * arcRadii.Width);
                        endPoint.y := centrePoint.y + (sinComponent * arcRadii.Height);
                end;

        //create arc segment
            function TGraphicArc.createArcSegment(const axisConverterIn : TDrawingAxisConverter) : TD2D1ArcSegment;
                var
                    widthLT, heightLT,
                    sweepAngle          : double;
                    endPointLT          : TPointF;
                    arcSegmentOut       : TD2D1ArcSegment;
                begin
                    //centre point
                        endPointLT := axisConverterIn.XY_to_LT( endPoint );

                        arcSegmentOut.point := D2D1PointF( endPointLT.X, endPointLT.Y );

                    //calculate size
                        widthLT     := axisConverterIn.dX_To_dL( arcRadii.Width );
                        heightLT    := -axisConverterIn.dY_To_dT( arcRadii.Height );

                        ArcSegmentOut.size := D2D1SizeF( widthLT, heightLT );

                    //sweep angle
                        sweepAngle := endAngle - startAngle;

                        ArcSegmentOut.rotationAngle := sweepAngle;

                        if ( 180 < abs( sweepAngle ) ) then
                            ArcSegmentOut.arcSize := D2D1_ARC_SIZE.D2D1_ARC_SIZE_LARGE
                        else
                            ArcSegmentOut.arcSize := D2D1_ARC_SIZE.D2D1_ARC_SIZE_SMALL;

                    //arc direction
                        if ( 0 < sweepAngle ) then
                            ArcSegmentOut.sweepDirection := D2D1_SWEEP_DIRECTION.D2D1_SWEEP_DIRECTION_COUNTER_CLOCKWISE
                        else
                            ArcSegmentOut.sweepDirection := D2D1_SWEEP_DIRECTION.D2D1_SWEEP_DIRECTION_CLOCKWISE;

                    result := ArcSegmentOut;
                end;

        //create arc geometry
            function TGraphicArc.createArcGeometry(const axisConverterIn : TDrawingAxisConverter) : ID2D1PathGeometry;
                var

                    geometrySink    : ID2D1GeometrySink;
                    pathGeometryOut : ID2D1PathGeometry;
                    arcSegment      : TD2D1ArcSegment;
                    startPointLT      : TPointF;
                begin
                    //create path geometry
                        D2DFactory( D2D1_FACTORY_TYPE.D2D1_FACTORY_TYPE_MULTI_THREADED ).CreatePathGeometry( pathGeometryOut );

                    //open path geometry
                        pathGeometryOut.Open( geometrySink );

                    //create geometry sink
                        //calculate start point
                            startPointLT := axisConverterIn.XY_to_LT( startPoint );

                        //create arc segment
                            arcSegment := createArcSegment( axisConverterIn );

                        geometrySink.BeginFigure( D2D1PointF( startPointLT.x, startPointLT.y ), D2D1_FIGURE_BEGIN.D2D1_FIGURE_BEGIN_HOLLOW );

                        geometrySink.AddArc( arcSegment );

                        geometrySink.EndFigure( D2D1_FIGURE_END.D2D1_FIGURE_END_OPEN );

                        geometrySink.Close();

                    result := pathGeometryOut;
                end;

    //public
        //constructor
            constructor TGraphicArc.create( const   lineThicknessIn : integer;
                                            const   arcXRadiusIn,
                                                    arcYRadiusIn,
                                                    startAngleIn,
                                                    endAngleIn      : double;
                                            const   lineColourIn    : TColor;
                                            const   lineStyleIn     : TPenStyle;
                                            const   centrePointIn   : TGeomPoint );
                begin
                    inherited create(   false,
                                        lineThicknessIn,
                                        TColors.Null,
                                        lineColourIn,
                                        lineStyleIn     );

                    startAngle      := normaliseAngle( startAngleIn );
                    endAngle        := normaliseAngle( endAngleIn );

                    arcRadii.Width  := arcXRadiusIn;
                    arcRadii.Height := arcYRadiusIn;

                    centrePoint.copyPoint( centrePointIn );

                    calculateStartAndEndPoint();
                end;

        //destructor
            destructor TGraphicArc.destroy();
                begin
                    inherited destroy();
                end;

        //draw to canvas
            procedure TGraphicArc.drawToCanvas( const axisConverterIn   : TDrawingAxisConverter;
                                                var canvasInOut         : TDirect2DCanvas       );
                var
                    pathGeometry : ID2D1PathGeometry;
                begin
                    pathGeometry := createArcGeometry( axisConverterIn );

                    setLineProperties( canvasInOut );

                    canvasInOut.DrawGeometry( pathGeometry );
                end;

        //bounding box
            function TGraphicArc.determineBoundingBox() : TGeomBox;
                var
                    boxOut : TGeomBox;
                begin
                    boxOut.setCentrePoint( centrePoint );

                    boxOut.setDimensions( 2 * arcRadii.Width, 2 * arcRadii.Height );

                    result := boxOut;
                end;


end.
