unit DrawingAxisConversionBaseClass;

interface

    uses
        System.SysUtils, system.Math, system.Types,
        GeometryTypes,
        GeomBox;

    type
        TDrawingAxisConverterBase = class
            protected
                var
                    canvasDimensions    : TSize;
                    drawingRegion       : TGeomBox;
                //modifiers
                    //drawing space boundaries
                        procedure setDrawingRegion(const domainMinIn, domainMaxIn, rangeMinIn, rangeMaxIn : double); overload;
            public
                //constructor
                    constructor create(); virtual;
                //destructor
                    destructor destroy(); override;
                //accessors
                    function getDrawingRegion() : TGeomBox;
                //modifiers
                    //canvas boundaries
                        procedure setCanvasDimensions(const canvasHeightIn, canvasWidthIn : integer);
                    //drawing space boundaries
                        procedure setDrawingRegion( const bufferIn : double;
                                                    const regionIn : TGeomBox ); overload;
                //helper methods
                    //domain
                        function calculateRegionDomain() : double;
                    //range
                        function calculateRegionRange() : double;

        end;

implementation

    //protected
        //modifiers
            //drawingRegion space boundaries
                procedure TDrawingAxisConverterBase.setDrawingRegion(const domainMinIn, domainMaxIn, rangeMinIn, rangeMaxIn : double);
                    begin
                        drawingRegion.setBounds(domainMinIn,    domainMaxIn,
                                                rangeMinIn,     rangeMaxIn,
                                                0,              0           );
                    end;
        
    //public
        //constructor
            constructor TDrawingAxisConverterBase.create();
                begin
                    inherited create();

                    setDrawingRegion( 0, 0, 0, 0 );
                end;

        //destructor
            destructor TDrawingAxisConverterBase.destroy();
                begin
                    inherited destroy();
                end;

        //accessors
            function TDrawingAxisConverterBase.getDrawingRegion() : TGeomBox;
                begin
                    result := drawingRegion;
                end;

        //modifiers
            //canvasSpace boundaries
                procedure TDrawingAxisConverterBase.setCanvasDimensions(const canvasHeightIn, canvasWidthIn : integer);
                    begin
                        canvasDimensions.Width := canvasWidthIn;
                        canvasDimensions.Height := canvasHeightIn;
                    end;

            //drawingRegion space boundaries
                procedure TDrawingAxisConverterBase.setDrawingRegion(   const bufferIn : double;
                                                                        const regionIn : TGeomBox );
                    var
                        buffer,
                        newDomain, newRange : double;
                    begin
                        //set valid buffer
                            buffer := min(5, bufferIn);
                            buffer := max(buffer, 0);

                        //test buffer is valid
                            if (bufferIn < 0) then
                                exit();

                        newDomain   := (1 + (buffer / 100)) * regionIn.calculateXDimension();
                        newRange    := (1 + (buffer / 100)) * regionIn.calculateYDimension();

                        drawingRegion.copyBox( regionIn );
                        drawingRegion.setDimensions( newDomain, newRange );
                    end;

        //helper methods
            //domain
                function TDrawingAxisConverterBase.calculateRegionDomain() : double;
                    begin
                        result := drawingRegion.calculateXDimension();
                    end;

            //range
                function TDrawingAxisConverterBase.calculateRegionRange() : double;
                    begin
                        result := drawingRegion.calculateYDimension();
                    end;

end.
