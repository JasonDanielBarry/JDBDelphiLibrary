unit GraphicGridClass;

interface

    uses
        //Delphi
            system.SysUtils, System.Math, system.types, system.UITypes, System.UIConsts, System.Classes,
            Winapi.D2D1, Vcl.Direct2D,
            vcl.Graphics, Vcl.StdCtrls,
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
                    axisValueText   : TGraphicText;
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
                //draw axis labels
                    function determineLabelPosition(const axisMinIn, axisMaxIn : double) : double;
                    function determineLabelValueString(const valueIn, incrementIn : double) : string;
                    //x-axis
                        procedure determineXAxisLabelPosition(  const yMinIn, yMaxIn    : double;
                                                                out yOut                : double );
                        procedure drawXAxisLabels(  const   incrementIn,
                                                            xValueIn, yValueIn  : double;
                                                    const   axisConverterIn     : TDrawingAxisConverter;
                                                    var canvasInOut             : TDirect2DCanvas       );
                    //y-axis
                        procedure determineYAxisLabelPosition(  const xMinIn, xMaxIn    : double;
                                                                out xOut                : double );
                        procedure drawYAxisLabels(  const   incrementIn,
                                                            xValueIn, yValueIn  : double;
                                                    const   axisConverterIn     : TDrawingAxisConverter;
                                                    var canvasInOut             : TDirect2DCanvas           );
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
                            if ( IsZero( lineIncrementIn, 1e-9 ) ) then
                                exit();

                        //get the drawing region
                            drawingRegion := axisConverterIn.getDrawingRegion();

                        //initialise the starting x value
                            y := roundToBaseMultiple( drawingRegion.yMin, lineIncrementIn, TRoundingMode.rmUp ) - lineIncrementIn;

                        //cache the y-extents
                            xMin := drawingRegion.xMin;
                            xMax := drawingRegion.xMax;

                        //draw the grid lines
                            while ( y < drawingRegion.yMax ) do
                                begin
                                    var xText : double;

                                    majorGridLine.setStartPoint( xMin, y );
                                    majorGridLine.setEndPoint( xMax, y );

                                    majorGridLine.drawToCanvas( axisConverterIn, canvasInOut );

                                    drawMinorHorizontalGridLines( y, xMin, xMax, lineIncrementIn / 5, axisConverterIn, canvasInOut );

                                    determineYAxisLabelPosition( xMin, xMax, xText );

                                    drawYAxisLabels( lineIncrementIn, xText, y, axisConverterIn, canvasInOut );

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
                            if ( IsZero( lineIncrementIn, 1e-9 ) ) then
                                exit();

                        //get the drawing region
                            drawingRegion := axisConverterIn.getDrawingRegion();

                        //initialise the starting x value
                            x := roundToBaseMultiple( drawingRegion.xMin, lineIncrementIn, TRoundingMode.rmUp ) - lineIncrementIn;

                        //cache the y-extents
                            yMin := drawingRegion.yMin;
                            yMax := drawingRegion.yMax;

                        //draw the grid lines
                            while ( x < drawingRegion.xMax ) do
                                begin
                                    var yText : double;

                                    majorGridLine.setStartPoint( x, yMin );
                                    majorGridLine.setEndPoint( x, yMax );

                                    majorGridLine.drawToCanvas( axisConverterIn, canvasInOut );

                                    drawMinorVerticalGridLines( x, yMin, yMax, lineIncrementIn / 5, axisConverterIn, canvasInOut );

                                    determineXAxisLabelPosition( yMin, yMax, yText );

                                    drawXAxisLabels( lineIncrementIn, x, yText, axisConverterIn, canvasInOut );

                                    x := x + lineIncrementIn;
                                end;
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

                procedure TGraphicGrid.drawXAxisLabels( const   incrementIn,
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

                procedure TGraphicGrid.drawYAxisLabels( const   incrementIn,
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

    //public
        //constructor
            constructor TGraphicGrid.create();
                begin
                    inherited create();

                    createGridLines();
                    axisValueText := TGraphicText.create( True, 9, 0, '', TAlignment.taLeftJustify, TTextLayout.tlTop, TColors.Black, [], TGeomPoint.create(0, 0) );
                end;

        //destructor
            destructor TGraphicGrid.destroy();
                begin
                    FreeAndNil( majorGridLine );
                    FreeAndNil( minorGridLine );
                    FreeAndNil( axisValueText );

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
