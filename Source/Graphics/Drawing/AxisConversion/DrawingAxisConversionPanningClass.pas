unit DrawingAxisConversionPanningClass;

interface

    uses
        System.SysUtils, system.Math, system.Types,
        GeneralMathMethods,
        GeometryTypes, GeomBox,
        DrawingAxisConversionZoomingClass
        ;

    type
        TDrawingAxisPanningConverter = class(TDrawingAxisZoomingConverter)
            public
                //constructor
                    constructor create(); override;
                //destructor
                    destructor destroy(); override;
                //shift drawing region
                    procedure shiftDrawingDomain(const deltaXIn : double);
                    procedure shiftDrawingRange(const deltaYIn : double);
                    procedure shiftDrawingRegion(const deltaXIn, deltaYIn : double);
                //drawing space boundaries
                    procedure setDrawingRegion( const bufferIn : double;
                                                const regionIn : TGeomBox ); override;
                //recentre drawing region
                    procedure recentreDrawingRegion();
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

        //drawing space boundaries
            procedure TDrawingAxisPanningConverter.setDrawingRegion(const bufferIn : double;
                                                                    const regionIn : TGeomBox);
                begin
                    inherited setDrawingRegion( bufferIn, regionIn );
                end;

        //recentre drawing region
            procedure TDrawingAxisPanningConverter.recentreDrawingRegion();
                begin
                    drawingRegion.setCentrePoint( geometryBoundaryCentre );
                end;

end.
