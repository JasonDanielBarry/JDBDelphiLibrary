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
            private
                type
                    TRegionShift = record
                        xShift, yShift : double;
                    end;
                var
                    currentRegionShift : TRegionShift;
                //calculate the shift of the drawing region relative to the geometry boundary
                    procedure updateRegionShift();
            protected
                //zooming
                    procedure zoom(const zoomAboutXIn, zoomAboutYIn, newZoomPercentageIn : double); override;
            public
                //constructor
                    constructor create(); override;
                //destructor
                    destructor destroy(); override;
                //accessors
                    function getCurrentRegionCentreShift() : TGeomPoint;
                //shift drawing region
                    procedure shiftDrawingDomain(const deltaXIn : double);
                    procedure shiftDrawingRange(const deltaYIn : double);
                    procedure shiftDrawingRegion(const deltaXIn, deltaYIn : double);
                //set the drawing region shift to a particular value
                    procedure setDrawingRegionShift(const shiftXIn, shiftYIn : double);
                //drawing space boundaries
                    procedure setDrawingRegion( const bufferIn : double;
                                                const regionIn : TGeomBox ); override;
                //recentre drawing region
                    procedure recentreDrawingRegion();
        end;

implementation

    //private
        procedure TDrawingAxisPanningConverter.updateRegionShift();
            var
                regionCentreX, boundaryCentreX,
                regionCentreY, boundaryCentreY  : double;
            begin
                //calculate centres
                    //region
                        regionCentreX   := drawingRegion.calculateCentreX();
                        regionCentreY   := drawingRegion.calculateCentreY();

                    //boundary
                        boundaryCentreX := geometryBoundary.calculateCentreX();
                        boundaryCentreY := geometryBoundary.calculateCentreY();

                //calculate the region's centre shift relative to the geometry boundary's
                    currentRegionShift.xShift := regionCentreX - boundaryCentreX;
                    currentRegionShift.yShift := regionCentreY - boundaryCentreY;
            end;

    //protected
        //zooming
            procedure TDrawingAxisPanningConverter.zoom(const zoomAboutXIn, zoomAboutYIn, newZoomPercentageIn : double);
                begin
                    inherited zoom(zoomAboutXIn, zoomAboutYIn, newZoomPercentageIn);

                    updateRegionShift();
                end;

    //public
        //constructor
            constructor TDrawingAxisPanningConverter.create();
                begin
                    inherited create();

                    updateRegionShift();
                end;

        //destructor
            destructor TDrawingAxisPanningConverter.destroy();
                begin
                    inherited destroy();
                end;

        //accessors
            function TDrawingAxisPanningConverter.getCurrentRegionCentreShift() : TGeomPoint;
                begin
                    result := TGeomPoint.create(
                                                    currentRegionShift.xShift,
                                                    currentRegionShift.yShift
                                               );
                end;

        //shift drawing region
            procedure TDrawingAxisPanningConverter.shiftDrawingDomain(const deltaXIn : double);
                begin
                    drawingRegion.shiftX( deltaXIn );

                    updateRegionShift();
                end;

            procedure TDrawingAxisPanningConverter.shiftDrawingRange(const deltaYIn : double);
                begin
                    drawingRegion.shiftY( deltaYIn );

                    updateRegionShift();
                end;

            procedure TDrawingAxisPanningConverter.shiftDrawingRegion(const deltaXIn, deltaYIn : double);
                begin
                    drawingRegion.shiftBox(deltaXIn, deltaYIn);

                    updateRegionShift();
                end;

        //set the drawing region shift to a particular value
            procedure TDrawingAxisPanningConverter.setDrawingRegionShift(const shiftXIn, shiftYIn : double);
                var
                    requiredShift : TRegionShift;
                begin
                    //calculate the required shift
                        requiredShift.xShift := shiftXIn - currentRegionShift.xShift;
                        requiredShift.yShift := shiftYIn - currentRegionShift.yShift;

                    //shift by the required amount
                        shiftDrawingRegion( requiredShift.xShift, requiredShift.yShift );

                    updateRegionShift();
                end;

        //drawing space boundaries
            procedure TDrawingAxisPanningConverter.setDrawingRegion(const bufferIn : double;
                                                                    const regionIn : TGeomBox);
                begin
                    inherited setDrawingRegion( bufferIn, regionIn );

                    updateRegionShift();
                end;

        //recentre drawing region
            procedure TDrawingAxisPanningConverter.recentreDrawingRegion();
                begin
                    setDrawingRegionShift(0, 0);
                end;

end.
