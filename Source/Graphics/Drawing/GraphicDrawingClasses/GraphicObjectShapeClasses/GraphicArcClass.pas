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
            strict private
                var
                    startAngle, endAngle        : double;
                    arcRadii                    : TRectF;
                    centrePointXY,
                    startPointXY, endPointXY    : TGeomPoint;
                //calculate startPointXY
                    procedure calculateArcStartAndEndPoints();
                //create arc segment
                    function createArcSegment(const axisConverterIn : TDrawingAxisConverter) : TD2D1ArcSegment;
                //create arc geometry
                    function createArcGeometry( const filledIn          : boolean;
                                                const axisConverterIn   : TDrawingAxisConverter ) : ID2D1PathGeometry;
            public
                //constructor
                    constructor create( const   filledIn        : boolean;
                                        const   lineThicknessIn : integer;
                                        const   arcXRadiusIn,
                                                arcYRadiusIn,
                                                startAngleIn,
                                                endAngleIn      : double;
                                        const   fillColourIn,
                                                lineColourIn    : TColor;
                                        const   lineStyleIn     : TPenStyle;
                                        const   centrePointIn   : TGeomPoint );
                //destructor
                    destructor destroy(); override;
                //draw to canvas
                    procedure drawToCanvas( const axisConverterIn   : TDrawingAxisConverter;
                                            var canvasInOut         : TDirect2DCanvas       ); override;
                //bounding box
                    function determineBoundingBox() : TGeomBox; override;
                //test for valid input angles
                    class function validArcAngles(const startAngleIn, endAngleIn : double) : boolean;
        end;

implementation

    //normalise angle
        function normaliseAngle(const degAngleIn : double) : double;
            const
                CIRLCE_ROTATION : double = 360;
            var
                angleOut        : double;
                angleStartSign  : TValueSign;
            function _validAngle(const angleIn : double) : boolean;
                begin
                    _validAngle := ( -360 < angleIn ) AND ( angleIn < 360 );
                end;
            begin
                angleStartSign := sign( degAngleIn );

                angleOut := degAngleIn;

                while NOT( _validAngle( angleOut ) ) do
                    angleOut := angleOut - ( angleStartSign * CIRLCE_ROTATION );

                result := angleOut;
            end;

    //private
        //calculate startPointXY
            procedure TGraphicArc.calculateArcStartAndEndPoints();
                var
                    sinComponent, cosComponent : double;
                begin
                    //find start point
                        SinCos( DegToRad( startAngle ), sinComponent, cosComponent );

                        startPointXY.x := centrePointXY.x + (cosComponent * arcRadii.Width);
                        startPointXY.y := centrePointXY.y + (sinComponent * arcRadii.Height);

                    //find end point
                        SinCos( DegToRad( endAngle ), sinComponent, cosComponent );

                        endPointXY.x := centrePointXY.x + (cosComponent * arcRadii.Width);
                        endPointXY.y := centrePointXY.y + (sinComponent * arcRadii.Height);
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
                        endPointLT := axisConverterIn.XY_to_LT( endPointXY );

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
            function TGraphicArc.createArcGeometry( const filledIn          : boolean;
                                                    const axisConverterIn   : TDrawingAxisConverter ) : ID2D1PathGeometry;
                var
                    figureEnd       : D2D1_FIGURE_END;
                    geometrySink    : ID2D1GeometrySink;
                    pathGeometryOut : ID2D1PathGeometry;
                    arcSegment      : TD2D1ArcSegment;
                    arcStartPointLT : TPointF;
                begin
                    //create path geometry
                        D2DFactory( D2D1_FACTORY_TYPE.D2D1_FACTORY_TYPE_MULTI_THREADED ).CreatePathGeometry( pathGeometryOut );

                    //open path geometry
                        pathGeometryOut.Open( geometrySink );

                    //create geometry sink
                        //calculate start point
                            arcStartPointLT := axisConverterIn.XY_to_LT( startPointXY );

                        //create arc segment
                            arcSegment := createArcSegment( axisConverterIn );

                        if ( filledIn ) then
                            begin
                                var centrePointLT := axisConverterIn.XY_to_LT( centrePointXY );

                                figureEnd := D2D1_FIGURE_END.D2D1_FIGURE_END_CLOSED;

                                geometrySink.BeginFigure( D2D1PointF( centrePointLT.x, centrePointLT.y ), D2D1_FIGURE_BEGIN.D2D1_FIGURE_BEGIN_FILLED );
                                geometrySink.AddLine( D2D1PointF( arcStartPointLT.X, arcStartPointLT.Y ) );
                            end
                        else
                            begin
                                figureEnd := D2D1_FIGURE_END.D2D1_FIGURE_END_OPEN;

                                geometrySink.BeginFigure( D2D1PointF( arcStartPointLT.x, arcStartPointLT.y ), D2D1_FIGURE_BEGIN.D2D1_FIGURE_BEGIN_HOLLOW );
                            end;

                        geometrySink.AddArc( arcSegment );

                        geometrySink.EndFigure( figureEnd );

                        geometrySink.Close();

                    result := pathGeometryOut;
                end;

    //public
        //constructor
            constructor TGraphicArc.create( const   filledIn        : boolean;
                                            const   lineThicknessIn : integer;
                                            const   arcXRadiusIn,
                                                    arcYRadiusIn,
                                                    startAngleIn,
                                                    endAngleIn      : double;
                                            const   fillColourIn,
                                                    lineColourIn    : TColor;
                                            const   lineStyleIn     : TPenStyle;
                                            const   centrePointIn   : TGeomPoint );
                begin
                    inherited create(   filledIn,
                                        lineThicknessIn,
                                        fillColourIn,
                                        lineColourIn,
                                        lineStyleIn     );

                    startAngle      := normaliseAngle( startAngleIn );
                    endAngle        := normaliseAngle( endAngleIn );

                    arcRadii.Width  := arcXRadiusIn;
                    arcRadii.Height := arcYRadiusIn;

                    centrePointXY.copyPoint( centrePointIn );

                    calculateArcStartAndEndPoints();
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
                    filled          : boolean;
                    pathGeometry    : ID2D1PathGeometry;
                begin
                    filled := setFillProperties( canvasInOut );

                    pathGeometry := createArcGeometry( filled, axisConverterIn );

                    //fill arc shape
                        if ( filled ) then
                            canvasInOut.FillGeometry( pathGeometry );

                    //draw arc line
                        setLineProperties( canvasInOut );
                        canvasInOut.DrawGeometry( pathGeometry );
                end;

        //bounding box
            function TGraphicArc.determineBoundingBox() : TGeomBox;
                var
                    boxOut : TGeomBox;
                begin
                    boxOut.setCentrePoint( centrePointXY );

                    boxOut.setDimensions( 2 * arcRadii.Width, 2 * arcRadii.Height );

                    result := boxOut;
                end;

        //test for valid input angles
            class function TGraphicArc.validArcAngles(const startAngleIn, endAngleIn : double) : boolean;
                var
                    startAngleNorm, endAngleNorm : double;
                begin
                    startAngleNorm  := normaliseAngle( startAngleIn );
                    endAngleNorm    := normaliseAngle( endAngleIn );

                    result := NOT(      SameValue( startAngleNorm, endAngleNorm, 1e-3 )
                                    OR  SameValue( startAngleNorm, endAngleNorm + 360, 1e-3 )
                                    OR  SameValue( startAngleNorm + 360, endAngleNorm, 1e-3 )   );
                end;



end.
