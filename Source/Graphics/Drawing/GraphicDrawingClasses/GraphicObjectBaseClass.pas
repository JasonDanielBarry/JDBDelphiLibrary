unit GraphicObjectBaseClass;

interface

    uses
        vcl.Graphics,
        GraphicDrawingTypes
        ;

    type
        TGraphicObject = class
            private
                var
                    lineThickness       : integer;
                    objectDrawingType   : EGraphicDrawing;
                    fillColour,
                    lineColour          : TColor;
                    lineStyle           : TPenStyle;
                //modifiers
                    //line thickness
                        procedure setLineThickness(const lineThicknessIn : integer); inline;
                    //drawing type
                        procedure setGraphicDrawingType(const drawingTypeIn : EGraphicDrawing); inline;
                    //colours
                        procedure setFillColour(const fillColourIn : TColor); inline;
                        procedure setLineColour(const lineColourIn : TColor); inline;
                    //line style
                        procedure setLineStyle(const lineStyleIn : TPenStyle); inline;
            protected
                procedure setValues(const   lineThicknessIn : integer;
                                    const   drawingTypeIn   : EGraphicDrawing;
                                    const   fillColourIn,
                                            lineColourIn    : TColor;
                                    const   lineStyleIn     : TPenStyle     );
            public
                //constructor
                    constructor create( const   lineThicknessIn : integer;
                                        const   drawingTypeIn   : EGraphicDrawing;
                                        const   fillColourIn,
                                                lineColourIn    : TColor;
                                        const   lineStyleIn     : TPenStyle     );
                //destructor
                    destructor destroy(); override;
                //accessors
                    //drawing type
                        function getGraphicDrawingType() : EGraphicDrawing; inline;
                    //line thickness
                        function getLineThickness() : integer; inline;
                    //colours
                        function getFillColour() : TColor; inline;
                        function getLineColour() : TColor; inline;
                    //line style
                        function getLineStyle() : TPenStyle; inline;
        end;

implementation

    //private
        //modifiers
            //line thickness
                procedure TGraphicObject.setLineThickness(const lineThicknessIn : integer);
                    begin
                        lineThickness := lineThicknessIn;
                    end;

            //drawing type
                procedure TGraphicObject.setGraphicDrawingType(const drawingTypeIn : EGraphicDrawing);
                    begin
                        objectDrawingType := drawingTypeIn;
                    end;

            //colours
                procedure TGraphicObject.setFillColour(const fillColourIn : TColor);
                    begin
                        fillColour := fillColourIn;
                    end;

                procedure TGraphicObject.setLineColour(const lineColourIn : TColor);
                    begin
                        lineColour := lineColourIn;
                    end;

            //line style
                procedure TGraphicObject.setLineStyle(const lineStyleIn : TPenStyle);
                    begin
                        lineStyle := lineStyleIn;
                    end;

    //protected
        procedure TGraphicObject.setValues( const   lineThicknessIn : integer;
                                            const   drawingTypeIn   : EGraphicDrawing;
                                            const   fillColourIn,
                                                    lineColourIn    : TColor;
                                            const   lineStyleIn     : TPenStyle     );
            begin
                setLineThickness(       lineThicknessIn     );
                setGraphicDrawingType(  drawingTypeIn       );
                setFillColour(          fillColourIn        );
                setLineColour(          lineColourIn        );
                setLineStyle(           lineStyleIn         );
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

                    setValues(  lineThicknessIn,
                                drawingTypeIn,
                                fillColourIn,
                                lineColourIn,
                                lineStyleIn     );
                end;

        //destructor
            destructor TGraphicObject.destroy();
                begin
                    inherited destroy();
                end;

        //accessors
            //drawing type
                function TGraphicObject.getGraphicDrawingType() : EGraphicDrawing;
                    begin
                        result := objectDrawingType;
                    end;

            //line thickness
                function TGraphicObject.getLineThickness() : integer;
                    begin
                        result := lineThickness;
                    end;

            //colours
                function TGraphicObject.getFillColour() : TColor;
                    begin
                        result := fillColour;
                    end;

                function TGraphicObject.getLineColour() : TColor;
                    begin
                        result := lineColour;
                    end;

            //line style
                function TGraphicObject.getLineStyle() : TPenStyle;
                    begin
                        result := lineStyle;
                    end;

end.
