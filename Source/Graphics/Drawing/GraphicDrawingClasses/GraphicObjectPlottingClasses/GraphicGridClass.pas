unit GraphicGridClass;

interface

    uses
        //Delphi
            system.SysUtils, System.Math, system.types, system.UITypes, System.UIConsts,
            Winapi.D2D1, Vcl.Direct2D,
            vcl.Graphics,
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
        TGraphicGrid = class(TGraphicObject)
            private
                var
                    graphicText     : TGraphicText;
                    axisLine,
                    majorGridLine,
                    minorGridLine   : TGraphicLine;
                //instantiate grid line classes
                    procedure createGridLines();
                //calculate major grid line increments
                    function calculateMajorGridLineIncrement(   const divisionsIn       : integer;
                                                                const regionDimensionIn : double    ) : double;
                    procedure calculateMajorGridLineIncrements( const axisConverterIn       : TDrawingAxisConverter;
                                                                out horizontalIncrementOut,
                                                                    verticalIncrementOut    : double                );
                //draw grid lines
                    //horizontal
                        procedure drawMinorHorizontalGridLines( const   yStartIn, xMinIn, xMaxIn,
                                                                        minorIncrementIn            : double;
                                                                const   axisConverterIn             : TDrawingAxisConverter;
                                                                var canvasInOut                     : TDirect2DCanvas       );
                        procedure drawHorizontalGridLines(  const lineIncrementIn   : double;
                                                            const axisConverterIn   : TDrawingAxisConverter;
                                                            var canvasInOut         : TDirect2DCanvas       );
                    //vertical
                        procedure drawMinorVerticalGridLines(   const   xStartIn, yMinIn, yMaxIn,
                                                                        minorIncrementIn            : double;
                                                                const   axisConverterIn             : TDrawingAxisConverter;
                                                                var canvasInOut                     : TDirect2DCanvas       );
                        procedure drawVerticalGridLines(const lineIncrementIn   : double;
                                                        const axisConverterIn   : TDrawingAxisConverter;
                                                        var canvasInOut         : TDirect2DCanvas       );
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //draw to canvas
                    procedure drawToCanvas( const axisConverterIn   : TDrawingAxisConverter;
                                            var canvasInOut         : TDirect2DCanvas       ); override;
        end;

implementation

    //private
        //instantiate grid line classes
            procedure TGraphicGrid.createGridLines();
                var
                    tempLine : TGeomLine;
                begin
                    tempLine := TGeomLine.create();
                    tempLine.setStartPoint( 0, 0 );
                    tempLine.setEndPoint( 1, 1 );

                    axisLine        := TGraphicLine.create( 1, clWindowText, TPenStyle.psSolid, tempLine );
                    majorGridLine   := TGraphicLine.create( 1, clGrayText, TPenStyle.psSolid, tempLine );
                    minorGridLine   := TGraphicLine.create( 1, clInactiveCaption, TPenStyle.psSolid, tempLine );

                    FreeAndNil( tempLine );
                end;

        //calculate major grid line increments
            function TGraphicGrid.calculateMajorGridLineIncrement(  const divisionsIn       : integer;
                                                                    const regionDimensionIn : double    ) : double;
                var
                    orderOfMagnitude,
                    roundingBase,
                    majorIncrementOut  : double;
                begin
                    //get the order of magnitude of the region dimension
                        orderOfMagnitude := trunc( Log10( regionDimensionIn ) );

                    //calculate the rounding base using 1order of magnitude lower
                        roundingBase := power( 10, orderOfMagnitude - 1 );

                    //calculate major grid line increment for the largest dimension
                        majorIncrementOut := roundToBaseMultiple( regionDimensionIn / divisionsIn, roundingBase );

                    result := majorIncrementOut;
                end;

            procedure TGraphicGrid.calculateMajorGridLineIncrements(const axisConverterIn       : TDrawingAxisConverter;
                                                                    out horizontalIncrementOut,
                                                                        verticalIncrementOut    : double                );
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
                                divisions := round( 10 * (canvasSize.Height / canvasSize.Width) );

                                horizontalIncrementOut  := calculateMajorGridLineIncrement( 10, regionDomain );
                                verticalIncrementOut    := calculateMajorGridLineIncrement( divisions, regionRange );
                            end
                        else
                            begin
                                divisions := round( 10 * (canvasSize.Width / canvasSize.Height) );

                                horizontalIncrementOut  := calculateMajorGridLineIncrement( divisions, regionDomain );;
                                verticalIncrementOut    := calculateMajorGridLineIncrement( 10, regionRange );
                            end;
                end;

        //draw grid lines
            //horizontal
                procedure TGraphicGrid.drawMinorHorizontalGridLines(const   yStartIn, xMinIn, xMaxIn,
                                                                            minorIncrementIn            : double;
                                                                    const   axisConverterIn             : TDrawingAxisConverter;
                                                                    var canvasInOut                     : TDirect2DCanvas       );
                    var
                        i : integer;
                        y : double;
                    begin
                        for i := 1 to 4 do
                            begin
                                y := yStartIn + i * minorIncrementIn;

                                minorGridLine.setStartPoint( xMinIn, y );
                                minorGridLine.setEndPoint( xMaxIn, y );

                                minorGridLine.drawToCanvas( axisConverterIn, canvasInOut );
                            end;
                    end;

                procedure TGraphicGrid.drawHorizontalGridLines( const lineIncrementIn   : double;
                                                                const axisConverterIn   : TDrawingAxisConverter;
                                                                var canvasInOut         : TDirect2DCanvas       );
                    var
                        y, xMin, xMax   : double;
                        drawingRegion   : TGeomBox;
                    begin
                        //exit if the line increment is too small
                            if ( IsZero( lineIncrementIn, 1e-6 ) ) then
                                exit();

                        //get the drawing region
                            drawingRegion := axisConverterIn.getDrawingRegion();

                        //initialise the starting x value
                            y := roundToBaseMultiple( drawingRegion.yMin, lineIncrementIn, TRoundingMode.rmUp );

                        //cache the y-extents
                            xMin := drawingRegion.xMin;
                            xMax := drawingRegion.xMax;

                        //draw the grid lines
                            while ( y < drawingRegion.yMax ) do
                                begin
                                    majorGridLine.setStartPoint( xMin, y );
                                    majorGridLine.setEndPoint( xMax, y );

                                    majorGridLine.drawToCanvas( axisConverterIn, canvasInOut );

                                    drawMinorHorizontalGridLines( y, xMin, xMax, lineIncrementIn / 5, axisConverterIn, canvasInOut );

                                    y := y + lineIncrementIn;
                                end;
                    end;

            //vertical
                procedure TGraphicGrid.drawMinorVerticalGridLines(  const   xStartIn, yMinIn, yMaxIn,
                                                                            minorIncrementIn            : double;
                                                                    const   axisConverterIn             : TDrawingAxisConverter;
                                                                    var canvasInOut                     : TDirect2DCanvas       );
                    var
                        i : integer;
                        x : double;
                    begin
                        for i := 1 to 4 do
                            begin
                                x := xStartIn + i * minorIncrementIn;

                                minorGridLine.setStartPoint( x, yMinIn );
                                minorGridLine.setEndPoint( x, yMaxIn );

                                minorGridLine.drawToCanvas( axisConverterIn, canvasInOut );
                            end;
                    end;

                procedure TGraphicGrid.drawVerticalGridLines(   const lineIncrementIn   : double;
                                                                const axisConverterIn   : TDrawingAxisConverter;
                                                                var canvasInOut         : TDirect2DCanvas       );
                    var
                        x, yMin, yMax   : double;
                        drawingRegion   : TGeomBox;
                    begin
                        //exit if the line increment is too small
                            if ( IsZero( lineIncrementIn, 1e-6 ) ) then
                                exit();

                        //get the drawing region
                            drawingRegion := axisConverterIn.getDrawingRegion();

                        //initialise the starting x value
                            x := roundToBaseMultiple( drawingRegion.xMin, lineIncrementIn, TRoundingMode.rmUp );

                        //cache the y-extents
                            yMin := drawingRegion.yMin;
                            yMax := drawingRegion.yMax;

                        //draw the grid lines
                            while ( x < drawingRegion.xMax ) do
                                begin
                                    majorGridLine.setStartPoint( x, yMin );
                                    majorGridLine.setEndPoint( x, yMax );

                                    majorGridLine.drawToCanvas( axisConverterIn, canvasInOut );

                                    drawMinorVerticalGridLines( x, yMin, yMax, lineIncrementIn / 5, axisConverterIn, canvasInOut );

                                    x := x + lineIncrementIn;
                                end;
                    end;

    //public
        //constructor
            constructor TGraphicGrid.create();
                begin
                    inherited create();

                    createGridLines();
                    graphicText := TGraphicText.create( 9, 0, '', TColors.Black, [], TGeomPoint.create(0, 0) );
                end;

        //destructor
            destructor TGraphicGrid.destroy();
                begin
                    FreeAndNil( majorGridLine );
                    FreeAndNil( minorGridLine );
                    FreeAndNil( graphicText );

                    inherited destroy();
                end;

        //draw to canvas
            procedure TGraphicGrid.drawToCanvas(const axisConverterIn   : TDrawingAxisConverter;
                                                var canvasInOut         : TDirect2DCanvas       );
                var
                    horizontalGridLineIncrement, verticalGridLineIncrement : double;
                begin
                    //calculate line increments
                        calculateMajorGridLineIncrements( axisConverterIn, horizontalGridLineIncrement, verticalGridLineIncrement );

                    //draw horizontal lines
                        drawHorizontalGridLines( verticalGridLineIncrement, axisConverterIn, canvasInOut );

                    //draw vertical lines
                        drawVerticalGridLines( horizontalGridLineIncrement, axisConverterIn, canvasInOut );
                end;

end.
