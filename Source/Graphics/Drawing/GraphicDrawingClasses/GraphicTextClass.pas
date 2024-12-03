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
                    textHeight          : integer;
                    textString          : string;
                    textColour          : TColor;
                    textFontStyles      : TFontStyles;
                    textHandlePointXY   : TGeomPoint;
            public
                //constructor
                    constructor create( const   textHeightIn        : integer;
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

    //public
        //constructor
            constructor TGraphicText.create(const   textHeightIn        : integer;
                                            const   textStringIn        : string;
                                            const   textColourIn        : TColor;
                                            const   textFontStylesIn    : TFontStyles;
                                            const   textHandlePointIn   : TGeomPoint );
                begin
                    inherited create();

                    textHeight          := textHeightIn;
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
                    //set font properties
                        canvasInOut.Font.Size   := textHeight;
                        canvasInOut.Font.Color  := textColour;
                        canvasInOut.Font.Style  := textFontStyles;
                        canvasInOut.Brush.Style := TBrushStyle.bsClear;

                    //get text position on canvas
                        textDrawingPointLT := axisConverterIn.XY_to_LT( textHandlePointXY );

                    try //try..except used as the first time draw is called the axis converter might not have all information needed to convert points correctly yet
                        canvasInOut.TextOut( round(textDrawingPointLT.x), round(textDrawingPointLT.y), textString );
                    except

                    end;

                    canvasInOut.Brush.Style := TBrushStyle.bsSolid;
                end;

        //bounding box
            function TGraphicText.determineBoundingBox() : TGeomBox;
                begin
                    result := TGeomBox.create( textHandlePointXY, textHandlePointXY );
                end;


end.
