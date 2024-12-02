unit GraphicObjectBaseClass;

interface

    uses
        Winapi.D2D1, Vcl.Direct2D,
        vcl.Graphics,
        GraphicDrawingTypes,
        DrawingAxisConversionClass
        ;

    type
        TGraphicObject = class
            private

            protected
                var
                    lineThickness       : integer;
                    objectDrawingType   : EGraphicDrawing;
                    fillColour,
                    lineColour          : TColor;
                    lineStyle           : TPenStyle;
                //set canvas properties for drawing
                    //fill
                        procedure setFillProperties(var canvasInOut : TDirect2DCanvas);
                    //line
                        procedure setLineProperties(var canvasInOut : TDirect2DCanvas);

            public
                //constructor
                    constructor create( const   lineThicknessIn : integer;
                                        const   drawingTypeIn   : EGraphicDrawing;
                                        const   fillColourIn,
                                                lineColourIn    : TColor;
                                        const   lineStyleIn     : TPenStyle     );
                //destructor
                    destructor destroy(); override;
                //draw to canvas
                    procedure drawToCanvas( const axisConverterIn   : TDrawingAxisConverter;
                                            var canvasInOut         : TDirect2DCanvas       ); virtual; abstract;
        end;

implementation

    //private
        //

    //protected
        //set canvas properties for drawing
            //fill
                procedure TGraphicObject.setFillProperties(var canvasInOut : TDirect2DCanvas);
                    begin
                        canvasInOut.Brush.Color := fillColour;
                        canvasInOut.Brush.Style := TBrushStyle.bsSolid;
                    end;

            //line
                procedure TGraphicObject.setLineProperties(var canvasInOut : TDirect2DCanvas);
                    begin
                        canvasInOut.Pen.Color := lineColour;
                        canvasInOut.Pen.Style := lineStyle;
                        canvasInOut.Pen.Width := lineThickness;
                    end;

    //public
        //constructor
            constructor TGraphicObject.create(  const   lineThicknessIn : integer;
                                                const   drawingTypeIn   : EGraphicDrawing;
                                                const   fillColourIn,
                                                        lineColourIn    : TColor;
                                                const   lineStyleIn     : TPenStyle     );
                begin
                    inherited create();

                    lineThickness       := lineThicknessIn;
                    objectDrawingType   := drawingTypeIn;
                    fillColour          := fillColourIn;
                    lineColour          := lineColourIn;
                    lineStyle           := lineStyleIn;
                end;

        //destructor
            destructor TGraphicObject.destroy();
                begin
                    inherited destroy();
                end;

end.
