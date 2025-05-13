unit GraphicObjectBaseClass;

interface

    uses
        Winapi.D2D1,
        system.UITypes,
        Vcl.Direct2D, vcl.Graphics, vcl.Themes,
        GeomBox,
        GraphicDrawingTypes,
        DrawingAxisConversionClass
        ;

    type
        TGraphicObject = class
            private
                var
                    filled          : boolean;
                    lineThickness   : integer;
                    fillColour,
                    lineColour      : TColor;
                    lineStyle       : TPenStyle;
            protected
                var
                    objectScaleType : EScaleType;
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
                                        const   lineStyleIn     : TPenStyle;
                                        const   scaleTypeIn     : EScaleType = EScaleType.scDrawing);overload;
                //destructor
                    destructor destroy(); override;
                //modifiers
                    procedure setObjectScaleType(const scaleTypeIn : EScaleType);
                //draw to canvas
                    procedure drawToCanvas( const axisConverterIn   : TDrawingAxisConverter;
                                            var canvasInOut         : TDirect2DCanvas       ); overload; virtual; abstract;
                    class procedure drawAllToCanvas(const arrGraphicObjectsIn   : TArray<TGraphicObject>;
                                                    const axisConverterIn       : TDrawingAxisConverter;
                                                    var canvasInOut             : TDirect2DCanvas       ); inline; static;
                //bounding box
                    function determineBoundingBox() : TGeomBox; overload; virtual; abstract;
                    class function determineBoundingBox(const arrGraphicObjectsIn : TArray<TGraphicObject>) : TGeomBox; overload; static;
        end;

implementation

    //private
        //

    //protected
        //set canvas properties for drawing
            //fill
                function TGraphicObject.setFillProperties(var canvasInOut : TDirect2DCanvas) : boolean;
                    begin
                        result := filled;

                        //hollow object
                            if ( NOT(filled) ) then
                                begin
                                    canvasInOut.Brush.Style := TBrushStyle.bsClear;
                                    exit( False );
                                end;

                        canvasInOut.Brush.Color := TStyleManager.ActiveStyle.GetSystemColor( fillColour );
                        canvasInOut.Brush.Style := TBrushStyle.bsSolid;
                    end;

            //line
                procedure TGraphicObject.setLineProperties(var canvasInOut : TDirect2DCanvas);
                    begin
                        canvasInOut.Pen.Color := TStyleManager.ActiveStyle.GetSystemColor( lineColour );
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
                                                const   lineStyleIn     : TPenStyle;
                                                const   scaleTypeIn     : EScaleType = EScaleType.scDrawing);
                begin
                    inherited create();

                    filled          := filledIn;
                    lineThickness   := lineThicknessIn;
                    fillColour      := fillColourIn;
                    lineColour      := lineColourIn;
                    lineStyle       := lineStyleIn;
                    objectScaleType := scaleTypeIn;
                end;

        //destructor
            destructor TGraphicObject.destroy();
                begin
                    inherited destroy();
                end;

        //modifiers
            procedure TGraphicObject.setObjectScaleType(const scaleTypeIn : EScaleType);
                begin
                    objectScaleType := scaleTypeIn;
                end;

        //draw to canvas
            class procedure TGraphicObject.drawAllToCanvas( const arrGraphicObjectsIn   : TArray<TGraphicObject>;
                                                            const axisConverterIn       : TDrawingAxisConverter;
                                                            var canvasInOut             : TDirect2DCanvas           );
                var
                    i, arrLen : integer;
                begin
                    arrLen := length( arrGraphicObjectsIn );

                    for i := 0 to ( arrLen - 1 ) do
                        arrGraphicObjectsIn[i].drawToCanvas( axisConverterIn, canvasInOut );
                end;

        //bounding box
            class function TGraphicObject.determineBoundingBox(const arrGraphicObjectsIn : TArray<TGraphicObject>) : TGeomBox;
                var
                    i, graphicObjectsCount  : integer;
                    arrBoundingBoxes        : TArray<TGeomBox>;
                begin
                    graphicObjectsCount := length( arrGraphicObjectsIn );

                    SetLength( arrBoundingBoxes, graphicObjectsCount );

                    for i := 0 to (graphicObjectsCount - 1) do
                        arrBoundingBoxes[i] := arrGraphicObjectsIn[i].determineBoundingBox();

                    result := TGeomBox.determineBoundingBox( arrBoundingBoxes );
                end;

end.
