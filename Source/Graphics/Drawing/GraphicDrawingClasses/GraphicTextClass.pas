unit GraphicTextClass;

interface

    uses
        Winapi.D2D1,
        system.SysUtils, system.Types,

        Vcl.Direct2D,
        vcl.Graphics,
        GeometryTypes,
        GeomBox,
        GraphicDrawingTypes,
        DrawingAxisConversionClass,
        GraphicObjectBaseClass
        ;

    type
        TGraphicText = class(TGraphicObject)
            private
                var
                    textSize            : integer;
                    textString          : string;
                    textColour          : TColor;
                    textFontStyles      : TFontStyles;
                    textHandlePointXY   : TGeomPoint;
                procedure setFontProperties(var canvasInOut : TDirect2DCanvas);
            public
                //constructor
                    constructor create( const   textSizeIn          : integer;
                                        const   textStringIn        : string;
                                        const   textColourIn        : TColor;
                                        const   textFontStylesIn    : TFontStyles;
                                        const   textHandlePointIn   : TGeomPoint    );
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
        procedure TGraphicText.setFontProperties(var canvasInOut : TDirect2DCanvas);
            begin
                //set font properties
                    canvasInOut.Font.size   := textSize;
                    canvasInOut.Font.Color  := textColour;
                    canvasInOut.Font.Name   := 'Segoe UI';
                    canvasInOut.Font.Style  := textFontStyles;
                    canvasInOut.Brush.Style := TBrushStyle.bsClear;
            end;

    //public
        //constructor
            constructor TGraphicText.create(const   textSizeIn          : integer;
                                            const   textStringIn        : string;
                                            const   textColourIn        : TColor;
                                            const   textFontStylesIn    : TFontStyles;
                                            const   textHandlePointIn   : TGeomPoint );
                begin
                    inherited create();

                    textSize            := textSizeIn;
                    textString          := textStringIn;
                    textColour          := textColourIn;
                    textFontStyles      := textFontStylesIn;
                    textHandlePointXY   := textHandlePointIn;
                end;

        //destructor
            destructor TGraphicText.destroy();
                begin
                    inherited destroy();
                end;

        //draw to canvas
            procedure TGraphicText.drawToCanvas(const axisConverterIn   : TDrawingAxisConverter;
                                                var canvasInOut         : TDirect2DCanvas       );
                var
                    textDrawingPointLT : TPointF;
                begin
                    setFontProperties( canvasInOut );

                    //get text position on canvas
                        textDrawingPointLT := axisConverterIn.XY_to_LT( textHandlePointXY );

                    //try..finally used as the first time draw is called the axis converter might not have all information needed to convert points correctly yet
                    //which causes textDrawingPointLT X and Y to be INF
                        try
                            canvasInOut.TextOut( round(textDrawingPointLT.X), round(textDrawingPointLT.Y), textString );
                        finally
                            canvasInOut.Brush.Style := TBrushStyle.bsSolid;
                        end;
                end;

        //bounding box
            function TGraphicText.determineBoundingBox() : TGeomBox;
                begin
                    result := TGeomBox.create( textHandlePointXY, textHandlePointXY );
                end;

end.
