unit DrawingObjectBaseClass;

interface

    uses
        DrawingTypes
        ;

    type
        TDrawingObject = class
            private
                objectDrawingType : EDrawingType;
            protected
                procedure setDrawingType(const drawingTypeIn : EDrawingType);
            public
                constructor create(); virtual;
                destructor destroy(); override;
                function getDrawingType() : EDrawingType; inline;
        end;

implementation

    //protected
        procedure TDrawingObject.setDrawingType(const drawingTypeIn : EDrawingType);
            begin
                objectDrawingType := drawingTypeIn;
            end;

    //public
        constructor TDrawingObject.create();
            begin
                inherited create();
            end;

        destructor TDrawingObject.destroy();
            begin
                inherited destroy();
            end;

        function TDrawingObject.getDrawingType() : EDrawingType;
            begin
                result := objectDrawingType;
            end;

end.
