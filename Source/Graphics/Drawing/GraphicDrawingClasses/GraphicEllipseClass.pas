unit GraphicEllipseClass;

interface

    uses
        Winapi.D2D1,
        system.SysUtils, system.Types,
        Vcl.Direct2D, vcl.Graphics,
        GeometryTypes,
        GeomBox,
        GraphicDrawingTypes,
        GraphicObjectBaseClass,
        DrawingAxisConversionClass
        ;

    type
        TGraphicEllipse = class(TGraphicObject)
            private
                var
                    ellipseBox : TGeomBox;
                //convert geom box to ellipse
                    function convertGeomBoxToEllipse(   const geomBoxIn         : TGeomBox;
                                                        const axisConverterIn   : TDrawingAxisConverter ) : TD2D1Ellipse;
            public
                //constructor
                    constructor create( const   filledIn        : boolean;
                                        const   lineThicknessIn : integer;
                                                ellipseWidthIn,
                                                ellipseHeightIn : double;
                                        const   fillColourIn,
                                                lineColourIn    : TColor;
                                        const   lineStyleIn     : TPenStyle;
                                        const   centrePointIn   : TGeomPoint    );
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
        //convert geom box to ellipse
            function TGraphicEllipse.convertGeomBoxToEllipse(   const geomBoxIn         : TGeomBox;
                                                                const axisConverterIn   : TDrawingAxisConverter ) : TD2D1Ellipse;
                var
                    centrePointF    : TPointF;
                    ellipseOut      : TD2D1Ellipse;
                begin
                    //calculate the ellipse centre point
                        centrePointF := axisConverterIn.XY_to_LT( ellipseBox.getCentrePoint() );

                        ellipseOut.point.x := centrePointF.X;
                        ellipseOut.point.y := centrePointF.y;

                    //calculate the ellipse dimensions
                        ellipseOut.radiusX := 0.5 * axisConverterIn.dX_To_dL( ellipseBox.calculateXDimension() );
                        ellipseOut.radiusY := 0.5 * axisConverterIn.dY_To_dT( ellipseBox.calculateYDimension() );

                    result := ellipseOut;
                end;

    //public
        //constructor
            constructor TGraphicEllipse.create( const   filledIn        : boolean;
                                                const   lineThicknessIn : integer;
                                                        ellipseWidthIn,
                                                        ellipseHeightIn : double;
                                                const   fillColourIn,
                                                        lineColourIn    : TColor;
                                                const   lineStyleIn     : TPenStyle;
                                                const   centrePointIn   : TGeomPoint    );
                var
                    localMinPoint, localMaxPoint : TGeomPoint;
                begin
                    inherited create(   filledIn,
                                        lineThicknessIn,
                                        fillColourIn,
                                        lineColourIn,
                                        lineStyleIn         );

                    localMinPoint := TGeomPoint.create(0, 0);
                    localMaxPoint := TGeomPoint.create(ellipseWidthIn, ellipseHeightIn);

                    ellipseBox := TGeomBox.create( localMinPoint, localMaxPoint );

                    ellipseBox.setCentrePoint( centrePointIn );
                end;

        //destructor
            destructor TGraphicEllipse.destroy();
                begin
                    inherited destroy();
                end;

        //draw to canvas
            procedure TGraphicEllipse.drawToCanvas( const axisConverterIn   : TDrawingAxisConverter;
                                                    var canvasInOut         : TDirect2DCanvas       );
                var
                    drawingEllipse : TD2D1Ellipse;
                begin
                    //get the drawing ellipse
                        drawingEllipse := convertGeomBoxToEllipse( ellipseBox, axisConverterIn );

                    //draw fill
                        if ( setFillProperties(canvasInOut) ) then
                            canvasInOut.FillEllipse( drawingEllipse );

                    //draw line
                        setLineProperties( canvasInOut );
                        canvasInOut.DrawEllipse( drawingEllipse );
                end;

        //bounding box
            function TGraphicEllipse.determineBoundingBox() : TGeomBox;
                begin
                    result := ellipseBox;
                end;

end.
