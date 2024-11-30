unit DrawingObjectBaseClass;

interface

    uses
        vcl.Graphics,
        DrawingTypes
        ;

    type
        TDrawingObject = class
            private
                var
                    lineThickness       : integer;
                    objectDrawingType   : EDrawingType;
                    fillColour,
                    lineColour          : TColor;
                    lineStyle           : TPenStyle;
                //modifiers
                    //line thickness
                        procedure setLineThickness(const lineThicknessIn : integer); inline;
                    //drawing type
                        procedure setDrawingType(const drawingTypeIn : EDrawingType); inline;
                    //colours
                        procedure setFillColour(const fillColourIn : TColor); inline;
                        procedure setLineColour(const lineColourIn : TColor); inline;
                    //line style
                        procedure setLineStyle(const lineStyleIn : TPenStyle); inline;
            protected
                procedure setValues(const   lineThicknessIn : integer;
                                    const   drawingTypeIn   : EDrawingType;
                                    const   fillColourIn,
                                            lineColourIn    : TColor;
                                    const   lineStyleIn     : TPenStyle     );
            public
                //constructor
                    constructor create(); virtual;
                //destructor
                    destructor destroy(); override;
                //accessors
                    //drawing type
                        function getDrawingType() : EDrawingType; inline;
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
                procedure TDrawingObject.setLineThickness(const lineThicknessIn : integer);
                    begin
                        lineThickness := lineThicknessIn;
                    end;

            //drawing type
                procedure TDrawingObject.setDrawingType(const drawingTypeIn : EDrawingType);
                    begin
                        objectDrawingType := drawingTypeIn;
                    end;

            //colours
                procedure TDrawingObject.setFillColour(const fillColourIn : TColor);
                    begin
                        fillColour := fillColourIn;
                    end;

                procedure TDrawingObject.setLineColour(const lineColourIn : TColor);
                    begin
                        lineColour := lineColourIn;
                    end;

            //line style
                procedure TDrawingObject.setLineStyle(const lineStyleIn : TPenStyle);
                    begin
                        lineStyle := lineStyleIn;
                    end;

    //protected
        procedure TDrawingObject.setValues( const   lineThicknessIn : integer;
                                            const   drawingTypeIn   : EDrawingType;
                                            const   fillColourIn,
                                                    lineColourIn    : TColor;
                                            const   lineStyleIn     : TPenStyle     );
            begin
                setLineThickness(   lineThicknessIn     );
                setDrawingType(     drawingTypeIn       );
                setFillColour(      fillColourIn        );
                setLineColour(      lineColourIn        );
                setLineStyle(       lineStyleIn         );
            end;

    //public
        //constructor
            constructor TDrawingObject.create();
                begin
                    inherited create();
                end;

        //destructor
            destructor TDrawingObject.destroy();
                begin
                    inherited destroy();
                end;

        //accessors
            //drawing type
                function TDrawingObject.getDrawingType() : EDrawingType;
                    begin
                        result := objectDrawingType;
                    end;

            //line thickness
                function TDrawingObject.getLineThickness() : integer;
                    begin
                        result := lineThickness;
                    end;

            //colours
                function TDrawingObject.getFillColour() : TColor;
                    begin
                        result := fillColour;
                    end;

                function TDrawingObject.getLineColour() : TColor;
                    begin
                        result := lineColour;
                    end;

            //line style
                function TDrawingObject.getLineStyle() : TPenStyle;
                    begin
                        result := lineStyle;
                    end;

end.
