unit GraphicTextClass;

interface

    uses
        Winapi.D2D1,
        system.SysUtils, system.Types, system.Classes,
        Vcl.Direct2D, vcl.Graphics, vcl.Themes, vcl.StdCtrls,
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
                    addUnderlay         : boolean;
                    textSize            : integer;
                    textRotationAngle   : double;
                    textString          : string;
                    textAlignment       : TAlignment;
                    textLayout          : TTextLayout;
                    textColour          : TColor;
                    textFontStyles      : TFontStyles;
                    textHandlePointXY   : TGeomPoint;
                //set font properties
                    procedure setFontProperties(var canvasInOut : TDirect2DCanvas);
                //calculate the translation of the text for the alignment settings
                    procedure calculateTextJustificationAndLayoutTranslation(   out xTranslationOut, yTranslationOut    : double;
                                                                                out textExtentOut                       : Tsize;
                                                                                var canvasInOut                         : TDirect2DCanvas   );
                //dray the text underlay
                    procedure drawTextUnderlay( const xIn, yIn      : integer;
                                                const textExtentIn  : TSize;
                                                var canvasInOut     : TDirect2DCanvas );
            public
                //constructor
                    constructor create( const   addUnderlayIn       : boolean;
                                        const   textSizeIn          : integer;
                                        const   textRotationAngleIn : double;
                                        const   textStringIn        : string;
                                        const   textAlignmentIn     : TAlignment;
                                        const   textLayoutIn        : TTextLayout;
                                        const   textColourIn        : TColor;
                                        const   textFontStylesIn    : TFontStyles;
                                        const   textHandlePointIn   : TGeomPoint    );
                //destructor
                    destructor destroy(); override;
                //modifiers
                    procedure setTextString(const newTextIn : string);
                    procedure setHandlePoint(   const xIn, yIn : double);
                    procedure setAlignmentAndLayout(const alignmentIn   : TAlignment;
                                                    const layoutIn      : TTextLayout);
                //draw to canvas
                    procedure drawToCanvas( const axisConverterIn   : TDrawingAxisConverter;
                                            var canvasInOut         : TDirect2DCanvas       ); override;
                //bounding box
                    function determineBoundingBox() : TGeomBox; override;
        end;

implementation

    //private
        //set font properties
            procedure TGraphicText.setFontProperties(var canvasInOut : TDirect2DCanvas);
                begin
                    //set font properties
                        canvasInOut.Font.size   := textSize;
                        canvasInOut.Font.Color  := TStyleManager.ActiveStyle.GetSystemColor( textColour );
                        canvasInOut.Font.Name   := 'Segoe UI';
                        canvasInOut.Font.Style  := textFontStyles;
                        canvasInOut.Brush.Style := TBrushStyle.bsClear;
                end;

        //calculate the translation of the text for the alignment settings
            procedure TGraphicText.calculateTextJustificationAndLayoutTranslation(  out xTranslationOut, yTranslationOut    : double;
                                                                                    out textExtentOut                       : Tsize;
                                                                                    var canvasInOut                         : TDirect2DCanvas   );
                begin
                    textExtentOut := canvasInOut.TextExtent( textString );

                    //x - translation
                        case ( textAlignment ) of
                            TAlignment.taLeftJustify:
                                xTranslationOut := 0;

                            TAlignment.taCenter:
                                xTranslationOut := textExtentOut.Width / 2;

                            TAlignment.taRightJustify:
                                xTranslationOut := textExtentOut.Width;
                        end;

                    //y - translation
                        case ( textLayout ) of
                            TTextLayout.tlTop:
                                yTranslationOut := 0;

                            TTextLayout.tlCenter:
                                yTranslationOut := textExtentOut.Height / 2;

                            TTextLayout.tlBottom:
                                yTranslationOut := textExtentOut.Height;
                        end;
                end;

        //dray the text underlay
            procedure TGraphicText.drawTextUnderlay(const xIn, yIn      : integer;
                                                    const textExtentIn  : TSize;
                                                    var canvasInOut     : TDirect2DCanvas );
                var
                    fillColour : TColor;
                begin
                    if NOT( addUnderlay ) then
                        exit();

                    fillColour := TStyleManager.ActiveStyle.GetStyleColor( TStyleColor.scGenericBackground );

                    canvasInOut.Brush.Color := fillColour;
                    canvasInOut.Brush.Style := TBrushStyle.bsSolid;

                    canvasInOut.FillRect( Rect( xIn, yIn, xIn + textExtentIn.Width, yIn + textExtentIn.Height ) );
                end;

    //public
        //constructor
            constructor TGraphicText.create(const   addUnderlayIn       : boolean;
                                            const   textSizeIn          : integer;
                                            const   textRotationAngleIn : double;
                                            const   textStringIn        : string;
                                            const   textAlignmentIn     : TAlignment;
                                            const   textLayoutIn        : TTextLayout;
                                            const   textColourIn        : TColor;
                                            const   textFontStylesIn    : TFontStyles;
                                            const   textHandlePointIn   : TGeomPoint    );
                begin
                    inherited create();

                    addUnderlay         := addUnderlayIn;
                    textSize            := textSizeIn;
                    textRotationAngle   := textRotationAngleIn;
                    textString          := textStringIn;
                    textAlignment       := textAlignmentIn;
                    textLayout          := textLayoutIn;
                    textColour          := textColourIn;
                    textFontStyles      := textFontStylesIn;
                    textHandlePointXY   := textHandlePointIn;
                end;

        //destructor
            destructor TGraphicText.destroy();
                begin
                    inherited destroy();
                end;

        //modifiers
            procedure TGraphicText.setTextString(const newTextIn : string);
                begin
                    textString := newTextIn;
                end;

            procedure TGraphicText.setHandlePoint(const xIn, yIn : double);
                begin
                    textHandlePointXY.setPoint( xIn, yIn );
                end;

            procedure TGraphicText.setAlignmentAndLayout(   const alignmentIn   : TAlignment;
                                                            const layoutIn      : TTextLayout   );
                begin
                    textAlignment   := alignmentIn;
                    textLayout      := layoutIn;
                end;

        //draw to canvas
            procedure TGraphicText.drawToCanvas(const axisConverterIn   : TDrawingAxisConverter;
                                                var canvasInOut         : TDirect2DCanvas       );
                var
                    x, y                        : integer;
                    xTranslation, yTranslation  : double;
                    textExtent                  : Tsize;
                    textDrawingPointLT          : TPointF;
                    transformMatrix             : TD2DMatrix3x2F;
                begin
                    //set the canvas dont properties
                        setFontProperties( canvasInOut );

                    //get text position on canvas
                        textDrawingPointLT := axisConverterIn.XY_to_LT( textHandlePointXY );

                    //get transformation matrix - positive angles result in clockwise rotation
                        transformMatrix := TD2DMatrix3x2F.Rotation( -textRotationAngle, textDrawingPointLT.x, textDrawingPointLT.y );

                    //calculate rotation matrix
                        canvasInOut.RenderTarget.SetTransform( transformMatrix );

                    //calculate text translation
                        calculateTextJustificationAndLayoutTranslation( xTranslation, yTranslation, textExtent, canvasInOut );

                        x := round(textDrawingPointLT.X - xTranslation);
                        y := round(textDrawingPointLT.Y - yTranslation);

                    //draw underlay (if required)
                        drawTextUnderlay( x, y, textExtent, canvasInOut );

                    //draw text to canvas
                        canvasInOut.TextOut( round(textDrawingPointLT.X - xTranslation), round(textDrawingPointLT.Y - yTranslation), textString );

                    //reset canvas roation
                        canvasInOut.RenderTarget.SetTransform( TD2DMatrix3x2F.Identity );
                end;

        //bounding box
            function TGraphicText.determineBoundingBox() : TGeomBox;
                begin
                    result := TGeomBox.create( textHandlePointXY, textHandlePointXY );
                end;

end.
