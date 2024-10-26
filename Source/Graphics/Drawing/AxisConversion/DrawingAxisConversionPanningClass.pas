unit DrawingAxisConversionPanningClass;

interface

    uses
        System.SysUtils, system.Math, system.Types,
        GeneralMathMethods,
        GeometryTypes,
        DrawingAxisConversionZoomingClass
        ;

    type
        TDrawingAxisPanningConverter = class(TDrawingAxisZoomingConverter)
            private

            protected

            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //shift drawing region
                    procedure shiftDrawingDomain(const deltaXIn : double);
                    procedure shiftDrawingRange(const deltaYIn : double);
                    procedure shiftDrawingRegion(const deltaXIn, deltaYIn : double);
        end;

implementation

    //public
        //constructor
            constructor TDrawingAxisPanningConverter.create();
                begin
                    inherited create();
                end;

        //destructor
            destructor TDrawingAxisPanningConverter.destroy();
                begin
                    inherited destroy();
                end;

        //shift drawing region
            procedure TDrawingAxisPanningConverter.shiftDrawingDomain(const deltaXIn : double);
                begin
                    drawingRegion.shiftX( deltaXIn );
                end;

            procedure TDrawingAxisPanningConverter.shiftDrawingRange(const deltaYIn : double);
                begin
                    drawingRegion.shiftY( deltaYIn );
                end;

            procedure TDrawingAxisPanningConverter.shiftDrawingRegion(const deltaXIn, deltaYIn : double);
                begin
                    drawingRegion.shiftBox(deltaXIn, deltaYIn);
                end;

end.
