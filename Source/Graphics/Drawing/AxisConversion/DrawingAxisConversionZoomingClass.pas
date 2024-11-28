unit DrawingAxisConversionZoomingClass;

interface

    uses
        System.SysUtils, system.Math, system.Types,
        GeneralMathMethods,
        GeometryTypes, GeomBox,
        DrawingAxisConversionCalculationsClass
        ;

    type
        TDrawingAxisZoomingConverter = class(TDrawingAxisConvertionCalculator)
            private
                //helper methods
                    function calculateZoomScaleFactor(const newZoomPercentage : double) : double;
                //zooming by percent
                    procedure zoom( const newZoomPercentageIn   : double;
                                    const zoomAboutPointIn      : TGeomPoint ); overload;
                    procedure zoom(const newZoomPercentageIn : double); overload;
            protected
                var
                    geometryBoundaryCentre  : TGeomPoint;
                    geometryBoundary        : TGeomBox; //the geometry boundary stores the geometry group's boundary
            public
                //constructor
                    constructor create(); override;
                //destructor
                    destructor destroy(); override;
                //modifiers
                    procedure setGeometryBoundary(const boundaryBoxIn : TGeomBox);
                    procedure resetDrawingRegionToGeometryBoundary();
                //zooming methods
                    function calculateCurrentZoomPercentage() : double;
                    procedure zoomIn(   const zoomPercentageIn : double;
                                        const zoomAboutPointIn : TGeomPoint ); overload;
                    procedure zoomIn(const zoomPercentageIn : double); overload;
                    procedure zoomOut(  const zoomPercentageIn : double;
                                        const zoomAboutPointIn : TGeomPoint ); overload;
                    procedure zoomOut(const zoomPercentageIn : double); overload;
                    procedure setZoom(const newZoomPercentageIn : double);
        end;

implementation

    //private
        //helper methods
            function TDrawingAxisZoomingConverter.calculateZoomScaleFactor(const newZoomPercentage : double) : double;
                var
                    currentZoomPercentage : double;
                begin
                    //the scale factor is used to size the domain and range
                    // < 1 the region shrinks which zooms the drawing in
                    // > 1 the region grows which zooms the drawing out

                    currentZoomPercentage := calculateCurrentZoomPercentage();

                    result := currentZoomPercentage / newZoomPercentage;
                end;

        //zooming by percent
            procedure TDrawingAxisZoomingConverter.zoom(const newZoomPercentageIn   : double;
                                                        const zoomAboutPointIn      : TGeomPoint);
                var
                    zoomScaleFactor : double;
                begin
                    //check new zoom percentage
                        if ( newZoomPercentageIn < 1e-3 ) then
                            exit();

                    //zoom to the desired factor about the specified point
                        //get the zoom factor
                            zoomScaleFactor := calculateZoomScaleFactor( newZoomPercentageIn );

                    //scale the drawing region
                        drawingRegion.scaleBox( zoomScaleFactor, zoomAboutPointIn );
                end;

            procedure TDrawingAxisZoomingConverter.zoom(const newZoomPercentageIn : double);
                var
                    zoomScaleFactor : double;
                begin
                    //check new zoom percentage
                        if ( newZoomPercentageIn < 1e-3 ) then
                            exit();

                    //zoom to the desired factor about the specified point
                        //get the zoom factor
                            zoomScaleFactor := calculateZoomScaleFactor( newZoomPercentageIn );

                    //scale the drawing region
                        drawingRegion.scaleBox( zoomScaleFactor );
                end;

    //public
        //constructor
            constructor TDrawingAxisZoomingConverter.create();
                begin
                    inherited create();

                    geometryBoundary.minPoint.setPoint( 0, 0, 0 );
                    geometryBoundary.maxPoint.setPoint( 0, 0, 0 );
                end;

        //destructor
            destructor TDrawingAxisZoomingConverter.destroy();
                begin
                    inherited destroy();
                end;

        //modifiers
            procedure TDrawingAxisZoomingConverter.setGeometryBoundary(const boundaryBoxIn : TGeomBox);
                begin
                    geometryBoundary.copyBox( boundaryBoxIn );

                    geometryBoundaryCentre.copyPoint( geometryBoundary.getCentrePoint() );
                end;

            procedure TDrawingAxisZoomingConverter.resetDrawingRegionToGeometryBoundary();
                begin
                    setDrawingRegion( 5, geometryBoundary );
                end;

        //zooming methods
            function TDrawingAxisZoomingConverter.calculateCurrentZoomPercentage() : double;
                var
                    domainZoomPercentage, rangeZoomPercentage : double;
                begin
                    //zoom is the ratio of the geometry boundary to the drawing region
                    //105 is for the 5% buffer on the drawing region when reset using the geometry boundary
                        domainZoomPercentage    := 105 * geometryBoundary.calculateXDimension() / drawingRegion.calculateXDimension();
                        rangeZoomPercentage     := 105 * geometryBoundary.calculateYDimension() / drawingRegion.calculateYDimension();

                    result := max(domainZoomPercentage, rangeZoomPercentage);
                end;


            procedure TDrawingAxisZoomingConverter.zoomIn(  const zoomPercentageIn : double;
                                                            const zoomAboutPointIn : TGeomPoint );
                var
                    currentZoomPercentage, newZoomPercentage : double;
                begin
                    if (zoomPercentageIn <= 0) then
                        exit();

                    currentZoomPercentage := calculateCurrentZoomPercentage();

                    //to zoom in the current zoom percentage is multiplied by a scale factor
                        newZoomPercentage := currentZoomPercentage * (1 + zoomPercentageIn / 100);

                    zoom( newZoomPercentage, zoomAboutPointIn );
                end;

            procedure TDrawingAxisZoomingConverter.zoomIn(const zoomPercentageIn : double);
                begin
                    if (zoomPercentageIn <= 0) then
                        exit();

                    zoomIn(
                                zoomPercentageIn,
                                drawingRegion.getCentrePoint()
                          );
                end;

            procedure TDrawingAxisZoomingConverter.zoomOut( const zoomPercentageIn : double;
                                                            const zoomAboutPointIn : TGeomPoint );
                var
                    currentZoomPercentage, newZoomPercentage : double;
                begin
                    if (zoomPercentageIn <= 0) then
                        exit();

                    currentZoomPercentage := calculateCurrentZoomPercentage();

                    //to zoom out the current zoom percentage is divided by a scale factor
                        newZoomPercentage := currentZoomPercentage / (1 + zoomPercentageIn / 100);

                    zoom( newZoomPercentage, zoomAboutPointIn );
                end;

            procedure TDrawingAxisZoomingConverter.zoomOut(const zoomPercentageIn : double);
                begin
                    if (zoomPercentageIn <= 0) then
                        exit();

                    zoomOut(
                                zoomPercentageIn,
                                drawingRegion.getCentrePoint()
                           );
                end;

            procedure TDrawingAxisZoomingConverter.setZoom(const newZoomPercentageIn : double);
                begin
                    if (newZoomPercentageIn < 1e-3) then
                        exit();

                    zoom( newZoomPercentageIn );
                end;

end.
