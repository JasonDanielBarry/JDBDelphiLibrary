unit DrawingGeometryClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, System.UIConsts,
        //custom
            ColourMethods,
            DrawingAxisConversionClass,
            GeometryTypes,
            GeometryBaseClass
            ;

    type
        TDrawingGeometry = class
            strict private
                var
                    lineThickness   : integer;
                    fillAlphaColour,
                    lineAlphaColour : TAlphaColor;
                    fillColour,
                    lineColour      : TColor;
                    geometry        : TGeomBase;
                    drawingPoints   : TArray<TGeomPoint>;
                //free geometry object
                    procedure freeGeometry();
                //set values
                    procedure setValues(const   lineThicknessIn : integer;
                                        const   geometryIn      : TGeomBase);
            public
                //constructor
                    constructor create( const   lineThicknessIn : integer;
                                        const   fillColourIn,
                                                lineColourIn    : TAlphaColor;
                                        const   geometryIn      : TGeomBase     ); overload;
//                    constructor create( const   lineThicknessIn : integer;
//                                        const   fillColourIn,
//                                                lineColourIn    : TColor;
//                                        const   geometryIn      : TGeomBase ); overload;
                //destructor
                    destructor destroy(); override;
                //line thickness
                    function getLineThickness() : integer; inline;
                //colours
                    function getFillAlphaColour() : TAlphaColor; inline;
                    function getLineAlphaColour() : TAlphaColor; inline;
                    function getFillColour() : TColor; inline;
                    function getLineColour() : TColor; inline;
                //get the geometry object
                    function getGeometry() : TGeomBase; inline;
                //get drawing points
                    function getDrawingPoints() : TArray<TGeomPoint>; inline;
        end;

implementation

    //private
        procedure TDrawingGeometry.freeGeometry();
            begin
                try
                    FreeAndNil( geometry );
                except

                end;
            end;

        //set values
            procedure TDrawingGeometry.setValues(   const   lineThicknessIn : integer;
                                                    const   geometryIn      : TGeomBase );
                begin
                    freeGeometry();

                    lineThickness   := lineThicknessIn;
                    geometry        := geometryIn;

                    drawingPoints   := geometryIn.getDrawingPoints();
                end;

    //public
        //constructor
            constructor TDrawingGeometry.create(const   lineThicknessIn : integer;
                                                const   fillColourIn,
                                                        lineColourIn    : TAlphaColor;
                                                const   geometryIn      : TGeomBase );
                begin
                    inherited create();

                    setValues( lineThicknessIn, geometryIn );

                    fillAlphaColour := fillColourIn;
                    lineAlphaColour := lineColourIn;

                    drawingPoints   := geometryIn.getDrawingPoints();
                end;

//            constructor TDrawingGeometry.create(const   lineThicknessIn : integer;
//                                                const   fillColourIn,
//                                                        lineColourIn    : TColor;
//                                                const   geometryIn      : TGeomBase );
//                begin
//                    inherited create();
//
//                    setValues( lineThickness, geometryIn );
//
//                    fillColour := fillColourIn;
//                    lineColour := lineColourIn;
//                end;

        //destructor
            destructor TDrawingGeometry.destroy();
                begin
                    freeGeometry();

                    inherited destroy();
                end;

        //line thickness
            function TDrawingGeometry.getLineThickness() : integer;
                begin
                    result := lineThickness;
                end;

        //colours
            function TDrawingGeometry.getFillAlphaColour() : TAlphaColor;
                begin
                    result := fillAlphaColour;
                end;

            function TDrawingGeometry.getLineAlphaColour() : TAlphaColor;
                begin
                    result := lineAlphaColour;
                end;

            function TDrawingGeometry.getFillColour() : TColor;
                begin
//                    result := fillColour;

                    result := AlphaColorToColor( fillAlphaColour );
                end;

            function TDrawingGeometry.getLineColour() : TColor;
                begin
//                    result := lineColour;

                    result := AlphaColorToColor( lineAlphaColour );
                end;

        //get the geometry object
            function TDrawingGeometry.getGeometry() : TGeomBase;
                begin
                    result := geometry;
                end;

        //get drawing points
            function TDrawingGeometry.getDrawingPoints() : TArray<TGeomPoint>;
                begin
                    result := drawingPoints;
                end;

end.
