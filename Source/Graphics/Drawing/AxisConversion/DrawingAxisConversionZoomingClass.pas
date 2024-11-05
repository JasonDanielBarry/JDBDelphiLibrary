unit DrawingAxisConversionZoomingClass;

interface

    uses
        System.SysUtils, system.Math, system.Types,
        GeneralMathMethods,
        GeometryTypes,
        DrawingAxisConversionBaseClass, DrawingAxisConversionCalculationsClass
        ;

    type
        TDrawingAxisZoomingConverter = class(TDrawingAxisConvertionCalculator)
            private
                //helper methods
                    function calculateZoomScaleFactor(const newZoomPercentage : double) : double;
                    function calculateCurrentZoomPercentage() : double;
                //rescaling methods
                    function rescaleRegionDimension(const   currentRegionDimensionIn,
                                                            currentRegionDimensionMinIn,    currentRegionDimensionMaxIn,
                                                            scaleFactorIn,                  scaleAboutValueIn           : double) : TArray<double>;
                    procedure rescaleDomain(const scaleAboutXIn, scaleFactorIn : double);
                    procedure rescaleRange(const scaleAboutYIn, scaleFactorIn : double);
                    procedure rescaleRegion(const scaleAboutXIn, scaleAboutYIn, scaleFactorIn : double);
                //zooming by percent
                    procedure zoom( const newZoomPercentageIn   : double;
                                    const zoomAboutPointIn      : TGeomPoint ); overload;
                    procedure zoom(const newZoomPercentageIn : double); overload;
            protected
                var
                    geometryBoundary : TGeomBox; //the drawing boundary stores a geometry group's boundary
                //helper methods
                    //boundary dimensions
                        function calculateBoundaryDomain() : double; inline;
                        function calculateBoundaryRange() : double; inline;
                    //boundary centre
                        function calculateBoundaryDomainCentre() : double; inline;
                        function calculateBoundaryRangeCentre() : double; inline;
                //zooming by percent
                    procedure zoom(const zoomAboutXIn, zoomAboutYIn, newZoomPercentageIn : double); overload; virtual;
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //accessors
                    function getCurrentZoomPercentage() : double;
                //modifiers
                    procedure setGeometryBoundary(const domainMinIn, domainMaxIn, rangeMinIn, rangeMaxIn : double); overload;
                    procedure setGeometryBoundary(const boundaryBoxIn : TGeomBox); overload;
                    procedure resetDrawingRegionToGeometryBoundary();
                //zooming methods
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

            function TDrawingAxisZoomingConverter.calculateCurrentZoomPercentage() : double;
                var
                    domainZoomPercentage, rangeZoomPercentage : double;
                begin
                    //zoom is the ratio of the geometry boundary to the drawing region
                    //105 is for the 5% buffer on the drawing region when reset using the geometry boundary
                        domainZoomPercentage    := 105 * calculateBoundaryDomain() / calculateRegionDomain();
                        rangeZoomPercentage     := 105 * calculateBoundaryRange() / calculateRegionRange();

                    result := max(domainZoomPercentage, rangeZoomPercentage);
                end;

        //rescaling methods
            function TDrawingAxisZoomingConverter.rescaleRegionDimension(const  currentRegionDimensionIn,
                                                                                currentRegionDimensionMinIn,    currentRegionDimensionMaxIn,
                                                                                scaleFactorIn,                  scaleAboutValueIn           : double) : TArray<double>;
                var
                    newRegionDimension,
                    newRegionDimensionMin, newRegionDimensionMax,
                    RegionDimensionDifference,
                    minToAbout, minToAboutRatio, regionDimensionMinShift,
                    aboutToMax, aboutToMaxRatio, RegionDimensionMaxShift : double;
                begin
                    //calculate the new dimesion
                        newRegionDimension := currentRegionDimensionIn * scaleFactorIn;

                    //calculate the difference between the new and current domains (sign is important)
                        RegionDimensionDifference := newRegionDimension - currentRegionDimensionIn;

                    //calculate lengths to the min and max of the scaleAbout value
                        minToAbout := scaleAboutValueIn - currentRegionDimensionMinIn;
                        aboutToMax := currentRegionDimensionMaxIn - scaleAboutValueIn;

                    //calculate the ratio between the about length and the current dimension
                        minToAboutRatio := minToAbout / currentRegionDimensionIn;
                        aboutToMaxRatio := aboutToMax / currentRegionDimensionIn;

                    //calculate the max and min shift
                        regionDimensionMinShift := (RegionDimensionDifference * minToAboutRatio);
                        RegionDimensionMaxShift := (RegionDimensionDifference * aboutToMaxRatio);

                    //calculate the new dimension min and max
                        newRegionDimensionMin := currentRegionDimensionMinIn - regionDimensionMinShift;
                        newRegionDimensionMax := currentRegionDimensionMaxIn + RegionDimensionMaxShift;

                    result := [newRegionDimensionMin, newRegionDimensionMax];
                end;

            procedure TDrawingAxisZoomingConverter.rescaleDomain(const scaleAboutXIn, scaleFactorIn : double);
                var
                    currentRegionDomain,
                    currentRegionDomainMin, currentRegionDomainMax,
                    newRegionDomainMin,     newRegionDomainMax      : double;
                    regionDomainMinAndMax                           : TArray<double>;
                begin
                    //get current info
                        currentRegionDomain       := calculateRegionDomain();
                        currentRegionDomainMin    := regionDomainMin();
                        currentRegionDomainMax    := regionDomainMax();

                    //calculate new domain min and max
                        regionDomainMinAndMax := rescaleRegionDimension(
                                                                            currentRegionDomain,
                                                                            currentRegionDomainMin,
                                                                            currentRegionDomainMax,
                                                                            scaleFactorIn,
                                                                            scaleAboutXIn
                                                                       );

                        newRegionDomainMin := regionDomainMinAndMax[0];
                        newRegionDomainMax := regionDomainMinAndMax[1];

                    setDomain( newRegionDomainMin, newRegionDomainMax );
                end;

            procedure TDrawingAxisZoomingConverter.rescaleRange(const scaleAboutYIn, scaleFactorIn : double);
                var
                    currentRegionRange,
                    currentRegionRangeMin,  currentRegionRangeMax,
                    newRegionRangeMin,      newRegionRangeMax       : double;
                    rangeRegionMinAndMax                            : TArray<double>;
                begin
                    //get current info
                        currentRegionRange       := calculateRegionRange();
                        currentRegionRangeMin    := regionRangeMin();
                        currentRegionRangeMax    := regionRangeMax();

                    //calculate new range min and max
                        rangeRegionMinAndMax := rescaleRegionDimension(
                                                                            currentRegionRange,
                                                                            currentRegionRangeMin,
                                                                            currentRegionRangeMax,
                                                                            scaleFactorIn,
                                                                            scaleAboutYIn
                                                                      );

                        newRegionRangeMin := rangeRegionMinAndMax[0];
                        newRegionRangeMax := rangeRegionMinAndMax[1];

                    setRange( newRegionRangeMin, newRegionRangeMax );
                end;

            procedure TDrawingAxisZoomingConverter.rescaleRegion(const scaleAboutXIn, scaleAboutYIn, scaleFactorIn : double);
                begin
                    rescaleDomain( scaleAboutXIn, scaleFactorIn );
                    rescaleRange( scaleAboutYIn, scaleFactorIn );
                end;

        //zooming by percent
            procedure TDrawingAxisZoomingConverter.zoom(const zoomAboutXIn, zoomAboutYIn, newZoomPercentageIn : double);
                var
                    zoomScaleFactor : double;
                begin
                    //check new zoom percentage
                        if ( newZoomPercentageIn < 1e-3) then
                            exit();

                    //zoom to the desired factor about the specified point
                        //get the zoom factor
                            zoomScaleFactor := calculateZoomScaleFactor( newZoomPercentageIn );

                        rescaleRegion(zoomAboutXIn, zoomAboutYIn, zoomScaleFactor);
                end;

            procedure TDrawingAxisZoomingConverter.zoom(const newZoomPercentageIn   : double;
                                                        const zoomAboutPointIn      : TGeomPoint);
                begin
                    zoom(zoomAboutPointIn.X, zoomAboutPointIn.y, newZoomPercentageIn);
                end;

            procedure TDrawingAxisZoomingConverter.zoom(const newZoomPercentageIn : double);
                var
                    regionDomainCentre, regionRangeCentre : double;
                begin
                    regionDomainCentre  := calculateRegionDomainCentre();
                    regionRangeCentre   := calculateRegionRangeCentre();

                    zoom( regionDomainCentre, regionRangeCentre, newZoomPercentageIn );
                end;

    //protected
        //helper methods
            //boundary dimensions
                function TDrawingAxisZoomingConverter.calculateBoundaryDomain() : double;
                    begin
                        result := geometryBoundary.maxPoint.x - geometryBoundary.minPoint.x;
                    end;

                function TDrawingAxisZoomingConverter.calculateBoundaryRange() : double;
                    begin
                        result := geometryBoundary.maxPoint.y - geometryBoundary.minPoint.y;
                    end;

            //boundary centre
                function TDrawingAxisZoomingConverter.calculateBoundaryDomainCentre() : double;
                    begin
                        result := geometryBoundary.getCentreX();
                    end;

                function TDrawingAxisZoomingConverter.calculateBoundaryRangeCentre() : double;
                    begin
                        result := geometryBoundary.getCentreY();
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

        //accessors
            function TDrawingAxisZoomingConverter.getCurrentZoomPercentage() : double;
                var
                    currentZoomPercentageOut : double;
                begin
                    currentZoomPercentageOut := calculateCurrentZoomPercentage();

                    result := currentZoomPercentageOut;
                end;

        //modifiers
            procedure TDrawingAxisZoomingConverter.setGeometryBoundary(const domainMinIn, domainMaxIn, rangeMinIn, rangeMaxIn : double);
                begin
                    geometryBoundary.minPoint.x := domainMinIn;
                    geometryBoundary.minPoint.y := rangeMinIn;
                    geometryBoundary.minPoint.z := 0;

                    geometryBoundary.maxPoint.x := domainMaxIn;
                    geometryBoundary.maxPoint.y := rangeMaxIn;
                    geometryBoundary.maxPoint.z := 0;
                end;

            procedure TDrawingAxisZoomingConverter.setGeometryBoundary(const boundaryBoxIn : TGeomBox);
                begin
                    setGeometryBoundary(
                                            boundaryBoxIn.minPoint.x,
                                            boundaryBoxIn.maxPoint.x,
                                            boundaryBoxIn.minPoint.y,
                                            boundaryBoxIn.maxPoint.y
                                      );
                end;

            procedure TDrawingAxisZoomingConverter.resetDrawingRegionToGeometryBoundary();
                begin
                    setDrawingRegion(5, geometryBoundary);
                end;

        //zooming methods
            procedure TDrawingAxisZoomingConverter.zoomIn(  const zoomPercentageIn : double;
                                                            const zoomAboutPointIn : TGeomPoint );
                var
                    currentZoomPercentage, newZoomPercentage : double;
                begin
                    if (zoomPercentageIn <= 0) then
                        exit();

                    currentZoomPercentage := calculateCurrentZoomPercentage();

                    newZoomPercentage := currentZoomPercentage * (1 + zoomPercentageIn / 100);

                    zoom( newZoomPercentage, zoomAboutPointIn );
                end;

            procedure TDrawingAxisZoomingConverter.zoomIn(const zoomPercentageIn : double);
                begin
                    if (zoomPercentageIn <= 0) then
                        exit();

                    zoomIn(
                                zoomPercentageIn,
                                TGeomPoint.create( calculateRegionDomainCentre(), calculateRegionRangeCentre() )
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

                    newZoomPercentage := currentZoomPercentage / (1 + zoomPercentageIn / 100);

                    zoom( newZoomPercentage, zoomAboutPointIn );
                end;

            procedure TDrawingAxisZoomingConverter.zoomOut(const zoomPercentageIn : double);
                begin
                    if (zoomPercentageIn <= 0) then
                        exit();

                    zoomOut(
                                zoomPercentageIn,
                                TGeomPoint.create( calculateRegionDomainCentre(), calculateRegionRangeCentre() )
                           );
                end;

            procedure TDrawingAxisZoomingConverter.setZoom(const newZoomPercentageIn : double);
                begin
                    if (newZoomPercentageIn < 1e-3) then
                        exit();

                    zoom( newZoomPercentageIn );
                end;

end.
