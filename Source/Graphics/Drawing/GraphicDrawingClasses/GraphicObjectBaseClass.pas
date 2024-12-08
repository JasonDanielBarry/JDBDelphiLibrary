unit GraphicObjectBaseClass;

interface

    uses
        Winapi.D2D1,
        system.UITypes,
        Vcl.Direct2D, vcl.Graphics,
        GeomBox,
        GraphicDrawingTypes,
        DrawingAxisConversionClass
        ;

    type
        TGraphicObject = class
            private
                var
                    filled              : boolean;
                    lineThickness       : integer;
                    fillColour,
                    lineColour          : TColor;
                    lineStyle           : TPenStyle;
            protected
                var
                    objectDrawingType   : EGraphicObjectType;
                //set canvas properties for drawing
                    //fill
                        function setFillProperties(var canvasInOut : TDirect2DCanvas) : boolean;
                    //line
                        procedure setLineProperties(var canvasInOut : TDirect2DCanvas);
            public
                //constructor
                    constructor create(); overload;
                    constructor create( const   filledIn        : boolean;
                                        const   lineThicknessIn : integer;
                                        const   fillColourIn,
                                                lineColourIn    : TColor;
                                        const   lineStyleIn     : TPenStyle ); overload;
                //destructor
                    destructor destroy(); override;
                //draw to canvas
                    procedure drawToCanvas( const axisConverterIn   : TDrawingAxisConverter;
                                            var canvasInOut         : TDirect2DCanvas       ); virtual; abstract;
                //bounding box
                    function determineBoundingBox() : TGeomBox; virtual; abstract;
        end;

implementation

    uses
        vcl.forms;

    //private
        //

    //protected
        //set canvas properties for drawing
            //fill
                function TGraphicObject.setFillProperties(var canvasInOut : TDirect2DCanvas) : boolean;
                    begin
                        //hollow object
                            if ( NOT(filled) ) then
                                begin
                                    Application.MessageBox('', '');

                                    canvasInOut.Brush.Style := TBrushStyle.bsClear;
                                    result := False;
                                    exit();
                                end;

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
            constructor TGraphicObject.create();
                begin
                    inherited create();
                end;

            constructor TGraphicObject.create(  const   filledIn        : boolean;
                                                const   lineThicknessIn : integer;
                                                const   fillColourIn,
                                                        lineColourIn    : TColor;
                                                const   lineStyleIn     : TPenStyle );
                begin
                    inherited create();

                    filled          := filledIn;
                    lineThickness   := lineThicknessIn;
                    fillColour      := fillColourIn;
                    lineColour      := lineColourIn;
                    lineStyle       := lineStyleIn;
                end;

        //destructor
            destructor TGraphicObject.destroy();
                begin
                    inherited destroy();
                end;

end.
