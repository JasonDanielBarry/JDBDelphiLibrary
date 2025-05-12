unit GraphicGridClass;

interface

    uses
        //Delphi
            system.SysUtils, System.Math, system.types, system.UITypes, System.UIConsts, System.Classes,
            Winapi.D2D1, Vcl.Direct2D,
            vcl.Graphics, Vcl.StdCtrls, Vcl.Themes,
        //custom
            RoundingMethods,
            GraphicObjectBaseClass,
            DrawingAxisConversionClass,
            GeometryTypes, GeomBox,
            GeomLineClass,
            GraphicLineClass,
            GraphicTextClass
            ;

    type
        TGridVisibilitySettings  = record
            axisLabelsVisible,
            axesVisible,
            majorGridLinesVisible,
            minorGridLinesVisible : boolean;
            procedure copyOther(const otherGridVisibilitySettingsIn : TGridVisibilitySettings);
            procedure setValues(const showAxisLabelsIn, showAxesIn, showMajorGridLinesIn, showMinorGridLinesIn : boolean);
            function allElementsDisabled() : boolean; inline;
        end;

        TGraphicGrid = class(TGraphicObject)
            private
                var
                    xStart, yStart, xEnd, yEnd,
                    gridLineHorizontalIncrement,
                    gridLineVerticalIncrement       : double;
                    axisValueText                   : TGraphicText;
                    axisLine,
                    majorGridLine, minorGridLine    : TGraphicLine;
                    gridVisibilitySettings          : TGridVisibilitySettings;
                //instantiate grid line classes
                    procedure createGridLines();
                //calculate major grid line increments
                    function calculateMajorGridLineIncrement(   const divisionsIn       : integer;
                                                                const regionDimensionIn : double    ) : double;
                    procedure calculateMajorGridLineIncrements( const axisConverterIn       : TDrawingAxisConverter;
                                                                out horizontalIncrementOut,
                                                                    verticalIncrementOut    : double                );
                //calculate the drawing start and end values
                    procedure calculateDrawingStartAndEndValues(const axisConverterIn       : TDrawingAxisConverter;
                                                                out xStartOut, yStartOut,
                                                                    xEndOut, yEndOut        : double                );
                //draw minor grid lines
                    //horizontal
                        procedure drawMinorHorizontalGridLinesForSingleIncrement(   const   yStartIn, xMinIn, xMaxIn,
                                                                                            minorIncrementIn            : double;
                                                                                    const   axisConverterIn             : TDrawingAxisConverter;
                                                                                    var canvasInOut                     : TDirect2DCanvas       );

                        procedure drawMinorHorizontalGridLines( const   yStartIn, yEndIn,
                                                                        xMinIn, xMaxIn,
                                                                        majorIncrementIn    : double;
                                                                const   axisConverterIn     : TDrawingAxisConverter;
                                                                var canvasInOut             : TDirect2DCanvas       );
                    //vertical
                        procedure drawMinorVerticalGridLinesForSingleIncrement( const   xStartIn, yMinIn, yMaxIn,
                                                                                        minorIncrementIn            : double;
                                                                                const   axisConverterIn             : TDrawingAxisConverter;
                                                                                var canvasInOut                     : TDirect2DCanvas       );
                        procedure drawMinorVerticalGridLines(   const   xStartIn, xEndIn,
                                                                        yMinIn, yMaxIn,
                                                                        majorIncrementIn    : double;
                                                                const   axisConverterIn     : TDrawingAxisConverter;
                                                                var canvasInOut             : TDirect2DCanvas       );


                    procedure drawMinorGridLines(   const   xStartIn, yStartIn,
                                                            xEndIn, yEndIn,
                                                            horizontalIncrementIn,
                                                            verticalIncrementIn     : double;
                                                    const   axisConverterIn         : TDrawingAxisConverter;
                                                    var canvasInOut                 : TDirect2DCanvas       );
                //draw major grid lines
                    //horizontal
                        procedure drawMajorHorizontalGridLines( const   yStartIn, yEndIn,
                                                                        xMinIn, xMaxIn,
                                                                        majorIncrementIn    : double;
                                                                const   axisConverterIn     : TDrawingAxisConverter;
                                                                var canvasInOut             : TDirect2DCanvas       );
                    //vertical
                        procedure drawMajorVerticalGridLines(   const   xStartIn, xEndIn,
                                                                        yMinIn, yMaxIn,
                                                                        majorIncrementIn    : double;
                                                                const   axisConverterIn     : TDrawingAxisConverter;
                                                                var canvasInOut             : TDirect2DCanvas       );
                    procedure drawMajorGridLines(   const   xStartIn, yStartIn,
                                                            xEndIn, yEndIn,
                                                            horizontalIncrementIn,
                                                            verticalIncrementIn     : double;
                                                    const   axisConverterIn         : TDrawingAxisConverter;
                                                    var canvasInOut                 : TDirect2DCanvas       );
                //axis lines
                    procedure drawAxisLines(const   xStartIn, yStartIn,
                                                    xEndIn, yEndIn      : double;
                                            const   axisConverterIn     : TDrawingAxisConverter;
                                                    var canvasInOut     : TDirect2DCanvas       );
                //draw axis labels
                    function determineLabelPosition(const axisMinIn, axisMaxIn : double) : double;
                    function determineLabelValueString(const valueIn, incrementIn : double) : string;
                    //x-axis
                        procedure determineXAxisLabelPosition(  const yMinIn, yMaxIn    : double;
                                                                out yOut                : double );
                        procedure drawXAxisLabel(   const   incrementIn,
                                                            xValueIn, yValueIn  : double;
                                                    const   axisConverterIn     : TDrawingAxisConverter;
                                                    var canvasInOut             : TDirect2DCanvas       );
                        procedure drawXAxisLabels(  const   incrementIn,
                                                            xStartIn, xEndIn,
                                                            yMinIn, yMaxIn      : double;
                                                    const   axisConverterIn     : TDrawingAxisConverter;
                                                    var canvasInOut             : TDirect2DCanvas       );
                    //y-axis
                        procedure determineYAxisLabelPosition(  const xMinIn, xMaxIn    : double;
                                                                out xOut                : double );
                        procedure drawYAxisLabel(   const   incrementIn,
                                                            xValueIn, yValueIn  : double;
                                                    const   axisConverterIn     : TDrawingAxisConverter;
                                                    var canvasInOut             : TDirect2DCanvas           );
                        procedure drawYAxisLabels(  const   incrementIn,
                                                            yStartIn, yEndIn,
                                                            xMinIn, xMaxIn      : double;
                                                    const   axisConverterIn     : TDrawingAxisConverter;
                                                    var canvasInOut             : TDirect2DCanvas       );
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //modifiers
                    procedure setGridElementsVisiblity(const gridVisibilitySettingsIn : TGridVisibilitySettings);
                //draw to canvas
                    procedure drawToCanvas( const axisConverterIn   : TDrawingAxisConverter;
                                            var canvasInOut         : TDirect2DCanvas       ); override;
                //draw axis labels
                    procedure drawAxisLabels(   const   axisConverterIn         : TDrawingAxisConverter;
                                                var canvasInOut                 : TDirect2DCanvas       );
        end;

implementation

    //grid visibility settings
        procedure TGridVisibilitySettings.copyOther(const otherGridVisibilitySettingsIn : TGridVisibilitySettings);
            begin
                self.setValues(
                                    otherGridVisibilitySettingsIn.axisLabelsVisible,
                                    otherGridVisibilitySettingsIn.axesVisible,
                                    otherGridVisibilitySettingsIn.majorGridLinesVisible,
                                    otherGridVisibilitySettingsIn.minorGridLinesVisible
                              );
            end;

        procedure TGridVisibilitySettings.setValues(const showAxisLabelsIn, showAxesIn, showMajorGridLinesIn, showMinorGridLinesIn : boolean);
            begin
                self.axisLabelsVisible      := showAxisLabelsIn;
                self.axesVisible            := showAxesIn;
                self.majorGridLinesVisible  := showMajorGridLinesIn;
                self.minorGridLinesVisible  := showMinorGridLinesIn;
            end;

        function TGridVisibilitySettings.allElementsDisabled() : boolean;
            var
                atLeastOneElementIsVisible : boolean;
            begin
                atLeastOneElementIsVisible := axisLabelsVisible OR axesVisible OR majorGridLinesVisible OR minorGridLinesVisible;

                result := NOT( atLeastOneElementIsVisible );
            end;

    //private
        //instantiate grid line classes
            procedure TGraphicGrid.createGridLines();
                var
                    tempLine : TGeomLine;
                begin
                    tempLine := TGeomLine.create();
                    tempLine.setStartPoint( 0, 0 );
                    tempLine.setEndPoint( 1, 1 );

                    axisLine        := TGraphicLine.create( 2, clWindowText, TPenStyle.psSolid, tempLine );
                    majorGridLine   := TGraphicLine.create( 1, clGrayText, TPenStyle.psSolid, tempLine );
                    minorGridLine   := TGraphicLine.create( 1, clInactiveCaption, TPenStyle.psSolid, tempLine );

                    FreeAndNil( tempLine );
                end;

        //calculate major grid line increments
            function TGraphicGrid.calculateMajorGridLineIncrement(  const divisionsIn       : integer;
                                                                    const regionDimensionIn : double    ) : double;
                var
                    OOMPower            : integer;
                    orderOfMagnitude,
                    roundingBase,
                    majorIncrementOut   : double;
                begin
                    //get the order of magnitude of the region dimension
                        orderOfMagnitude := Log10( regionDimensionIn );

                    //calculate the rounding base using 1 order of magnitude lower
                        if ( orderOfMagnitude < 0 ) then
                            OOMPower := Trunc( orderOfMagnitude ) - 2
                        else
                            OOMPower := Trunc( orderOfMagnitude ) - 1;

                        roundingBase := power( 10, OOMPower );

                    //calculate major grid line increment for the largest dimension
                        majorIncrementOut := roundToBaseMultiple( regionDimensionIn / divisionsIn, roundingBase );

                    result := majorIncrementOut;
                end;

            procedure TGraphicGrid.calculateMajorGridLineIncrements(const axisConverterIn       : TDrawingAxisConverter;
                                                                    out horizontalIncrementOut,
                                                                        verticalIncrementOut    : double                );
                const
                    MAJOR_DIVISIONS : integer = 10;
                var
                    divisions                       : integer;
                    regionDomain, regionRange,
                    orderOfMagnitude, roundingBase  : double;
                    drawingRegion                   : TGeomBox;
                    canvasSize                      : TSize;
                begin
                    //get the drawing region and canvas size
                        canvasSize      := axisConverterIn.getCanvasDimensions();
                        drawingRegion   := axisConverterIn.getDrawingRegion();

                    //get the region dimensions
                        regionDomain    := drawingRegion.calculateXDimension();
                        regionRange     := drawingRegion.calculateYDimension();

                    //calculate increments based on canvas dimensions
                        if ( canvasSize.Height < canvasSize.Width ) then
                            begin
                                divisions := round( MAJOR_DIVISIONS * (canvasSize.Height / canvasSize.Width) );

                                horizontalIncrementOut  := calculateMajorGridLineIncrement( MAJOR_DIVISIONS, regionDomain );
                                verticalIncrementOut    := calculateMajorGridLineIncrement( divisions, regionRange );
                            end
                        else
                            begin
                                divisions := round( MAJOR_DIVISIONS * (canvasSize.Width / canvasSize.Height) );

                                horizontalIncrementOut  := calculateMajorGridLineIncrement( divisions, regionDomain );;
                                verticalIncrementOut    := calculateMajorGridLineIncrement( MAJOR_DIVISIONS, regionRange );
                            end;
                end;

        //calculate the drawing starting values
            function calculateStartValue(const regionMinIn, gridLineIncrementIn : double) : double;
                begin
                    result := roundToBaseMultiple( regionMinIn, gridLineIncrementIn, TRoundingMode.rmUp ) - gridLineIncrementIn;
                end;

            procedure TGraphicGrid.calculateDrawingStartAndEndValues(   const axisConverterIn       : TDrawingAxisConverter;
                                                                        out xStartOut, yStartOut,
                                                                            xEndOut, yEndOut        : double                );
                var
                    drawingRegion : TGeomBox;
                begin
                    //get the drawing region
                        drawingRegion := axisConverterIn.getDrawingRegion();

                    //get start values
                        xStartOut := calculateStartValue( drawingRegion.xMin, gridLineHorizontalIncrement );
                        yStartOut := calculateStartValue( drawingRegion.yMin, gridLineVerticalIncrement );

                    //get end values
                        xEndOut := drawingRegion.xMax;
                        yEndOut := drawingRegion.yMax;
                end;

        //draw minor grid lines
            //horizontal
                procedure TGraphicGrid.drawMinorHorizontalGridLinesForSingleIncrement(  const   yStartIn, xMinIn, xMaxIn,
                                                                                                minorIncrementIn            : double;
                                                                                        const   axisConverterIn             : TDrawingAxisConverter;
                                                                                        var canvasInOut                     : TDirect2DCanvas       );
                    var
                        start_i,
                        i       : integer;
                        y       : double;
                    begin
                        if ( gridVisibilitySettings.majorGridLinesVisible ) then
                            start_i := 1
                        else
                            start_i := 0;

                        for i := start_i to 4 do
                            begin
                                y := yStartIn + i * minorIncrementIn;

                                minorGridLine.setStartPoint( xMinIn, y );
                                minorGridLine.setEndPoint( xMaxIn, y );

                                minorGridLine.drawToCanvas( axisConverterIn, canvasInOut );
                            end;
                    end;

                procedure TGraphicGrid.drawMinorHorizontalGridLines(const   yStartIn, yEndIn,
                                                                            xMinIn, xMaxIn,
                                                                            majorIncrementIn    : double;
                                                                    const   axisConverterIn     : TDrawingAxisConverter;
                                                                    var canvasInOut             : TDirect2DCanvas       );
                    var
                        y, minorIncrement : double;
                    begin
                        y := yStartIn;
                        minorIncrement := majorIncrementIn / 5;

                        while ( y < yEndIn ) do
                            begin
                                drawMinorHorizontalGridLinesForSingleIncrement( y, xMinIn, xMaxIn, minorIncrement, axisConverterIn, canvasInOut );

                                y := y + majorIncrementIn;
                            end;
                    end;

            //vertical
                procedure TGraphicGrid.drawMinorVerticalGridLinesForSingleIncrement(const   xStartIn, yMinIn, yMaxIn,
                                                                                            minorIncrementIn            : double;
                                                                                    const   axisConverterIn             : TDrawingAxisConverter;
                                                                                    var canvasInOut                     : TDirect2DCanvas       );
                    var
                        start_i,
                        i       : integer;
                        x       : double;
                    begin
                        if ( gridVisibilitySettings.majorGridLinesVisible ) then
                            start_i := 1
                        else
                            start_i := 0;

                        for i := start_i to 4 do
                            begin
                                x := xStartIn + i * minorIncrementIn;

                                minorGridLine.setStartPoint( x, yMinIn );
                                minorGridLine.setEndPoint( x, yMaxIn );

                                minorGridLine.drawToCanvas( axisConverterIn, canvasInOut );
                            end;
                    end;

                procedure TGraphicGrid.drawMinorVerticalGridLines(  const   xStartIn, xEndIn,
                                                                            yMinIn, yMaxIn,
                                                                            majorIncrementIn    : double;
                                                                    const   axisConverterIn     : TDrawingAxisConverter;
                                                                    var canvasInOut             : TDirect2DCanvas       );
                    var
                        x, minorIncrement : double;
                    begin
                        x := xStartIn;
                        minorIncrement := majorIncrementIn / 5;

                        while ( x < xEndIn ) do
                            begin
                                drawMinorVerticalGridLinesForSingleIncrement( x, yMinIn, yMaxIn, minorIncrement, axisConverterIn, canvasInOut );

                                x := x + majorIncrementIn;
                            end;
                    end;

            procedure TGraphicGrid.drawMinorGridLines(  const   xStartIn, yStartIn,
                                                                xEndIn, yEndIn,
                                                                horizontalIncrementIn,
                                                                verticalIncrementIn     : double;
                                                        const   axisConverterIn         : TDrawingAxisConverter;
                                                        var canvasInOut                 : TDirect2DCanvas       );
                begin
                    if NOT( gridVisibilitySettings.minorGridLinesVisible ) then
                        exit();

                    drawMinorHorizontalGridLines( yStartIn, yEndIn, xStartIn, xEndIn, verticalIncrementIn, axisConverterIn, canvasInOut );

                    drawMinorVerticalGridLines( xStartIn, xEndIn, yStartIn, yEndIn, horizontalIncrementIn, axisConverterIn, canvasInOut );
                end;

        //draw major grid lines
            //horizontal
                procedure TGraphicGrid.drawMajorHorizontalGridLines(const   yStartIn, yEndIn,
                                                                            xMinIn, xMaxIn,
                                                                            majorIncrementIn    : double;
                                                                    const   axisConverterIn     : TDrawingAxisConverter;
                                                                    var canvasInOut             : TDirect2DCanvas       );
                    var
                        y : double;
                    begin
                        //exit if the line increment is too small
                            if ( IsZero( majorIncrementIn, 1e-9 ) ) then
                                exit();

                        //initialise the starting y value
                            y := yStartIn;

                        //draw the grid lines
                            while ( y < yEndIn ) do
                                begin
                                    if ( IsZero( y, 1e-9 ) AND (gridVisibilitySettings.axesVisible) ) then
                                        begin
                                            y := y + majorIncrementIn;
                                            Continue;
                                        end;

                                    majorGridLine.setStartPoint( xMinIn, y );
                                    majorGridLine.setEndPoint( xMaxIn, y );

                                    majorGridLine.drawToCanvas( axisConverterIn, canvasInOut );

                                    y := y + majorIncrementIn;
                                end;
                    end;

            //vertical
                procedure TGraphicGrid.drawMajorVerticalGridLines(  const   xStartIn, xEndIn,
                                                                            yMinIn, yMaxIn,
                                                                            majorIncrementIn    : double;
                                                                    const   axisConverterIn     : TDrawingAxisConverter;
                                                                    var canvasInOut             : TDirect2DCanvas      );
                    var
                        x : double;
                    begin
                        //exit if the line increment is too small
                            if ( IsZero( majorIncrementIn, 1e-9 ) ) then
                                exit();

                        //initialise the starting x value
                            x := xStartIn;

                        //draw the grid lines
                            while ( x < xEndIn ) do
                                begin
                                    if ( IsZero( x, 1e-9 ) AND (gridVisibilitySettings.axesVisible) ) then
                                        begin
                                            x := x + majorIncrementIn;
                                            Continue;
                                        end;

                                    majorGridLine.setStartPoint( x, yMinIn );
                                    majorGridLine.setEndPoint( x, yMaxIn );

                                    majorGridLine.drawToCanvas( axisConverterIn, canvasInOut );

                                    x := x + majorIncrementIn;
                                end;
                    end;

            procedure TGraphicGrid.drawMajorGridLines(  const   xStartIn, yStartIn,
                                                                xEndIn, yEndIn,
                                                                horizontalIncrementIn,
                                                                verticalIncrementIn     : double;
                                                        const   axisConverterIn         : TDrawingAxisConverter;
                                                        var canvasInOut                 : TDirect2DCanvas       );
                begin
                    if NOT( gridVisibilitySettings.majorGridLinesVisible ) then
                        exit();

                    drawMajorHorizontalGridLines( yStartIn, yEndIn, xStartIn, xEndIn, verticalIncrementIn, axisConverterIn, canvasInOut );

                    drawMajorVerticalGridLines( xStartIn, xEndIn, yStartIn, yEndIn, horizontalIncrementIn, axisConverterIn, canvasInOut );
                end;

        //axis lines
            procedure TGraphicGrid.drawAxisLines(   const   xStartIn, yStartIn,
                                                            xEndIn, yEndIn      : double;
                                                    const   axisConverterIn     : TDrawingAxisConverter;
                                                    var canvasInOut             : TDirect2DCanvas           );
                begin
                    if NOT( gridVisibilitySettings.axesVisible ) then
                        exit();

                    //x - axis
                        axisLine.setStartPoint( xStartIn, 0 );
                        axisLine.setEndPoint( xEndIn, 0 );
                        axisLine.drawToCanvas( axisConverterIn, canvasInOut );

                    //y - axis
                        axisLine.setStartPoint( 0, yStartIn );
                        axisLine.setEndPoint( 0, yEndIn );
                        axisLine.drawToCanvas( axisConverterIn, canvasInOut );
                end;

        //draw axis labels
            function TGraphicGrid.determineLabelPosition(const axisMinIn, axisMaxIn : double) : double;
                var
                    sameSign            : boolean;
                    minSign, maxSign    : TValueSign;
                begin
                    result := 0;

                    //get the min and max sign
                        minSign := sign( axisMinIn );
                        maxSign := sign( axisMaxIn );

                    //text for the same sign
                        sameSign := (minSign = maxSign);

                    //if the signs are different then return 0
                        if NOT( sameSign ) then
                            exit( 0 );

                    if (minSign = -1 ) then
                        exit( axisMaxIn );

                    if (minSign = 1) then
                        exit( axisMinIn );
                end;

            function TGraphicGrid.determineLabelValueString(const valueIn, incrementIn : double) : string;
                var
                    precision, digits,
                    orderOfMagnitude    : integer;
                begin
                    orderOfMagnitude := Floor( Log10( abs( incrementIn ) ) );

                    digits      := max( -orderOfMagnitude, 0 );
                    precision   := 5 + digits;

                    result := FloatToStrF( valueIn, ffFixed, precision, digits );
                end;

            //x-axis
                procedure TGraphicGrid.determineXAxisLabelPosition( const yMinIn, yMaxIn    : double;
                                                                    out yOut                : double );
                    begin
                        yOut := determineLabelPosition( yMinIn, yMaxIn );

                        if ( IsZero(abs(yOut), 1e-6 ) ) then
                            begin
                                axisValueText.setAlignmentAndLayout( TAlignment.taCenter, TTextLayout.tlCenter );
                                exit();
                            end;

                        if ( yOut < 0 ) then
                            begin
                                axisValueText.setAlignmentAndLayout( TAlignment.taCenter, TTextLayout.tlTop );
                                exit();
                            end;

                        axisValueText.setAlignmentAndLayout( TAlignment.taCenter, TTextLayout.tlBottom );
                    end;

                procedure TGraphicGrid.drawXAxisLabel(  const   incrementIn,
                                                                xValueIn, yValueIn  : double;
                                                        const   axisConverterIn     : TDrawingAxisConverter;
                                                        var canvasInOut             : TDirect2DCanvas       );
                    var
                        xValueString : string;
                    begin
                        xValueString := determineLabelValueString( xValueIn, incrementIn );

                        axisValueText.setHandlePoint( xValueIn, yValueIn );
                        axisValueText.setTextString( xValueString );

                        axisValueText.drawToCanvas( axisConverterIn, canvasInOut );
                    end;

                procedure TGraphicGrid.drawXAxisLabels( const   incrementIn,
                                                                xStartIn, xEndIn,
                                                                yMinIn, yMaxIn      : double;
                                                        const   axisConverterIn     : TDrawingAxisConverter;
                                                        var canvasInOut             : TDirect2DCanvas       );
                    var
                        x, y : double;
                    begin
                        x := xStartIn;

                        determineXAxisLabelPosition( yMinIn, yMaxIn, y );

                        while ( x < XEndIn ) do
                            begin
                                drawXAxisLabel( incrementIn, x, y, axisConverterIn, canvasInOut );

                                x := x + incrementIn;
                            end;
                    end;

            //y-axis
                procedure TGraphicGrid.determineYAxisLabelPosition( const xMinIn, xMaxIn    : double;
                                                                    out xOut                : double );
                    begin
                        xOut := determineLabelPosition( xMinIn, xMaxIn );

                        if ( IsZero(abs(xOut), 1e-6 ) ) then
                            begin
                                axisValueText.setAlignmentAndLayout( TAlignment.taCenter, TTextLayout.tlCenter );
                                exit();
                            end;

                        if ( xOut < 0 ) then
                            begin
                                axisValueText.setAlignmentAndLayout( TAlignment.taRightJustify, TTextLayout.tlCenter );
                                exit();
                            end;

                        axisValueText.setAlignmentAndLayout( TAlignment.taLeftJustify, TTextLayout.tlCenter );
                    end;

                procedure TGraphicGrid.drawYAxisLabel(  const   incrementIn,
                                                                xValueIn, yValueIn  : double;
                                                        const   axisConverterIn     : TDrawingAxisConverter;
                                                        var canvasInOut             : TDirect2DCanvas           );
                    var
                        yValueString : string;
                    begin
                        yValueString := determineLabelValueString( yValueIn, incrementIn );

                        axisValueText.setHandlePoint( xValueIn, yValueIn );
                        axisValueText.setTextString( yValueString );

                        axisValueText.drawToCanvas( axisConverterIn, canvasInOut );
                    end;

                procedure TGraphicGrid.drawYAxisLabels( const   incrementIn,
                                                                yStartIn, yEndIn,
                                                                xMinIn, xMaxIn      : double;
                                                        const   axisConverterIn     : TDrawingAxisConverter;
                                                        var canvasInOut             : TDirect2DCanvas       );
                    var
                        x, y : double;
                    begin
                        y := yStartIn;

                        determineYAxisLabelPosition( xMinIn, xMaxIn, x );

                        while ( y < yEndIn ) do
                            begin
                                drawYAxisLabel( incrementIn, x, y, axisConverterIn, canvasInOut );

                                y := y + incrementIn;
                            end;
                    end;

    //public
        //constructor
            constructor TGraphicGrid.create();
                begin
                    inherited create();

                    createGridLines();
                    axisValueText := TGraphicText.create( True, 9, 0, '', TAlignment.taLeftJustify, TTextLayout.tlTop, TColors.SysWindowText, [], TGeomPoint.create(0, 0) );
                end;

        //destructor
            destructor TGraphicGrid.destroy();
                begin
                    FreeAndNil( axisLine );
                    FreeAndNil( majorGridLine );
                    FreeAndNil( minorGridLine );
                    FreeAndNil( axisValueText );

                    inherited destroy();
                end;

        //modifiers
            procedure TGraphicGrid.setGridElementsVisiblity(const gridVisibilitySettingsIn : TGridVisibilitySettings);
                begin
                    gridVisibilitySettings.copyOther( gridVisibilitySettingsIn );
                end;

        //draw to canvas
            procedure TGraphicGrid.drawToCanvas(const axisConverterIn   : TDrawingAxisConverter;
                                                var canvasInOut         : TDirect2DCanvas       );
                begin
                    if ( gridVisibilitySettings.allElementsDisabled() ) then
                        exit();

                    //calculate line increments
                        calculateMajorGridLineIncrements( axisConverterIn, gridLineHorizontalIncrement, gridLineVerticalIncrement );

                    //calculate starting x and y values
                        calculateDrawingStartAndEndValues( axisConverterIn, xStart, yStart, xEnd, yEnd );

                    //disable anti-aliasing for drawing lines
                        canvasInOut.RenderTarget.SetAntialiasMode( TD2D1AntiAliasMode.D2D1_ANTIALIAS_MODE_ALIASED );

                        //minor grid lines
                            drawMinorGridLines( xStart, yStart, xEnd, yEnd, gridLineHorizontalIncrement, gridLineVerticalIncrement, axisConverterIn, canvasInOut );

                        //major grid lines
                            drawMajorGridLines( xStart, yStart, xEnd, yEnd, gridLineHorizontalIncrement, gridLineVerticalIncrement, axisConverterIn, canvasInOut );

                        //axis lines
                            drawAxisLines( xStart, yStart, xEnd, yEnd, axisConverterIn, canvasInOut );

                    //reactivate anti-aliasing
                        canvasInOut.RenderTarget.SetAntialiasMode( TD2D1AntiAliasMode.D2D1_ANTIALIAS_MODE_PER_PRIMITIVE );
                end;

        //draw axis labels
            procedure TGraphicGrid.drawAxisLabels(  const   axisConverterIn         : TDrawingAxisConverter;
                                                    var canvasInOut                 : TDirect2DCanvas       );
                var
                    xMin, xMax, yMin, yMax  : double;
                    drawingRegion           : TGeomBox;
                begin
                    if NOT( gridVisibilitySettings.axisLabelsVisible ) then
                        exit();

                    drawingRegion := axisConverterIn.getDrawingRegion();

                    xMin := drawingRegion.xMin;
                    xMax := drawingRegion.xMax;
                    yMin := drawingRegion.yMin;
                    yMax := drawingRegion.yMax;

                    drawXAxisLabels( gridLineHorizontalIncrement, xStart, xEnd, yMin, yMax, axisConverterIn, canvasInOut );

                    drawYAxisLabels( gridLineVerticalIncrement, yStart, yEnd, xMin, xMax, axisConverterIn, canvasInOut );
                end;


end.
