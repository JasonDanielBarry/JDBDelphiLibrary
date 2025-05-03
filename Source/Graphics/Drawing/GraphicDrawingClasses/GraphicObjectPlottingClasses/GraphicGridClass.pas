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
                    majorGridLine,
                    minorGridLine   : TGraphicLine;
                procedure createGridLines();
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
        procedure TGraphicGrid.createGridLines();
            var
                tempLine : TGeomLine;
            begin
                tempLine := TGeomLine.create();
                tempLine.setStartPoint( 0, 0 );
                tempLine.setEndPoint( 1, 1 );

                majorGridLine := TGraphicLine.create( 3, TColors.Black, TPenStyle.psSolid, tempLine );
                minorGridLine := TGraphicLine.create( 1, TColors.Black, TPenStyle.psSolid, tempLine );

                FreeAndNil( tempLine );
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
                    i                               : integer;
                    xMajor, xMinor, y,
                    majorIncrement, minorIncrement,
                    orderOfMagnitude,
                    roundingBase,
                    drawingDomain, drawingRange     : double;
                    drawingRegion                   : TGeomBox;
                begin
                    //This is a big fat mess and needs to be neatened by separating into methods, but the concept works


                    drawingRegion := axisConverterIn.getDrawingRegion();
                    drawingDomain := drawingRegion.calculateXDimension();


                    orderOfMagnitude := trunc( Log10( drawingDomain ) );
                    roundingBase := power( 10, orderOfMagnitude - 1 );

                    majorIncrement := roundToBaseMultiple( drawingDomain / 5, roundingBase );
                    minorIncrement := majorIncrement / 5;

                    xMajor := roundToBaseMultiple( drawingRegion.xMin, majorIncrement, TRoundingMode.rmUp );

                    while ( (xMajor < drawingRegion.xMax) AND (majorIncrement <> 0) ) do
                        begin
                            majorGridLine.setStartPoint( xMajor, drawingRegion.yMin );
                            majorGridLine.setEndPoint( xMajor, drawingRegion.yMax );

                            majorGridLine.drawToCanvas( axisConverterIn, canvasInOut );

                            xMinor := xMajor + minorIncrement;

                            for i := 1 to 4 do
                                begin
                                    minorGridLine.setStartPoint( xMinor, drawingRegion.yMin );
                                    minorGridLine.setEndPoint( xMinor, drawingRegion.yMax );

                                    minorGridLine.drawToCanvas( axisConverterIn, canvasInOut );

                                    xMinor := xMinor + minorIncrement;
                                end;

                            xMajor := xMajor + majorIncrement;
                        end;
                end;

end.
