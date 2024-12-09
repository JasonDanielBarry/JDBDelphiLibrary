unit GraphicRectangleClass;

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
        TGraphicRectangle = class(TGraphicObject)
            private
                var
                    cornerRadius : double;
                    rectangleBox : TGeomBox;
                //convert a TGeomBox to TD2D1Rectangle
                    function convertGeomBoxToD2DRect(   const cornerRadiusIn    : double;
                                                        const geomBoxIn         : TGeomBox;
                                                        const axisConverterIn   : TDrawingAxisConverter ) : TD2D1RoundedRect;
            public
                //constructor
                    constructor create( const   filledIn                : boolean;
                                        const   lineThicknessIn         : integer;
                                        const   rectangleCornerRadiusIn,
                                                rectangleWidthIn,
                                                rectangleHeightIn       : double;
                                        const   fillColourIn,
                                                lineColourIn            : TColor;
                                        const   lineStyleIn             : TPenStyle;
                                        const   bottomLeftPointIn       : TGeomPoint    );
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
        //convert a TGeomBox to TD2D1Rectangle
            function TGraphicRectangle.convertGeomBoxToD2DRect( const cornerRadiusIn    : double;
                                                                const geomBoxIn         : TGeomBox;
                                                                const axisConverterIn   : TDrawingAxisConverter ) : TD2D1RoundedRect;
                var
                    bottomLeft, topRight    : TPointF;
                    rectOut                 : TD2D1RoundedRect;
                begin
                    //set radius
                        rectOut.radiusX := axisConverterIn.dX_To_dL( cornerRadiusIn );
                        rectOut.radiusY := axisConverterIn.dY_To_dT( cornerRadiusIn );

                    //get bottom-left and top-right points
                        bottomLeft  := axisConverterIn.XY_to_LT( geomBoxIn.minPoint );
                        topRight    := axisConverterIn.XY_to_LT( geomBoxIn.maxPoint );

                    //set rectangle bounds
                        rectOut.rect.left   := bottomLeft.X;
                        rectOut.rect.bottom := bottomLeft.Y;
                        rectOut.rect.right  := topRight.X;
                        rectOut.rect.top    := topRight.Y;

                    result := rectOut;
                end;

    //public
        //constructor
            constructor TGraphicRectangle.create(   const   filledIn                : boolean;
                                                    const   lineThicknessIn         : integer;
                                                    const   rectangleCornerRadiusIn,
                                                            rectangleWidthIn,
                                                            rectangleHeightIn       : double;
                                                    const   fillColourIn,
                                                            lineColourIn            : TColor;
                                                    const   lineStyleIn             : TPenStyle;
                                                    const   bottomLeftPointIn       : TGeomPoint    );
                var
                    localMinPoint, localMaxPoint : TGeomPoint;
                begin
                    inherited create(   filledIn,
                                        lineThicknessIn,
                                        fillColourIn,
                                        lineColourIn,
                                        lineStyleIn     );

                    objectDrawingType := EGraphicObjectType.gdRectangle;

                    cornerRadius := rectangleCornerRadiusIn;

                    rectangleBox := TGeomBox.newBox( rectangleWidthIn, rectangleHeightIn );

                    rectangleBox.shiftBox( bottomLeftPointIn.x, bottomLeftPointIn.y );
                end;

        //destructor
            destructor TGraphicRectangle.destroy();
                begin
                    inherited destroy();
                end;

        //draw to canvas
            procedure TGraphicRectangle.drawToCanvas(   const axisConverterIn   : TDrawingAxisConverter;
                                                        var canvasInOut         : TDirect2DCanvas       );
                var
                    drawingRect : TD2D1RoundedRect;
                begin
                    drawingRect := convertGeomBoxToD2DRect( cornerRadius, rectangleBox, axisConverterIn );

                    //draw rectangle fill
                        if ( setFillProperties( canvasInOut ) ) then
                            canvasInOut.FillRoundedRectangle( drawingRect );

                    //draw rectangle line
                        setLineProperties( canvasInOut );
                        canvasInOut.DrawRoundedRectangle( drawingRect );
                end;

        //bounding box
            function TGraphicRectangle.determineBoundingBox() : TGeomBox;
                begin
                    result := rectangleBox;
                end;

end.
