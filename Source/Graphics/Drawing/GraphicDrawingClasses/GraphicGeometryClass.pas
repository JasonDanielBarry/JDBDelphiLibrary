unit GraphicGeometryClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, System.UIConsts,
            vcl.Graphics,
        //custom
            ColourMethods,
            GraphicDrawingTypes,
            GraphicObjectBaseClass,
            DrawingAxisConversionClass,
            GeometryTypes,
            GeometryBaseClass
            ;

    type
        TGraphicGeometry = class(TGraphicObject)
            strict private
                var
                    geometry        : TGeomBase;
                    drawingPoints   : TArray<TGeomPoint>;
                //free geometry object
                    procedure freeGeometry();
                //set the geometry object
                    procedure setGeometry(const geometryIn : TGeomBase);
            public
                //constructor
                    constructor create( const   lineThicknessIn : integer;
                                        const   fillColourIn,
                                                lineColourIn    : TColor;
                                        const   lineStyleIn     : TPenStyle;
                                        const   geometryIn      : TGeomBase ); overload;
                    constructor create( const   lineThicknessIn : integer;
                                        const   lineColourIn    : TColor;
                                        const   lineStyleIn     : TPenStyle;
                                        const   geometryIn      : TGeomBase ); overload;
                //destructor
                    destructor destroy(); override;
                //get the geometry object
                    function getGeometry() : TGeomBase; inline;
                //get drawing points
                    function getDrawingPoints() : TArray<TGeomPoint>; inline;
        end;

implementation

    //private
        //free geometry object
            procedure TGraphicGeometry.freeGeometry();
                begin
                    try
                        FreeAndNil( geometry );
                    except

                    end;
                end;

        //set the geometry object
            procedure TGraphicGeometry.setGeometry(const geometryIn : TGeomBase);
                begin
                    //free current geometry
                        freeGeometry();

                    //assign new
                        geometry := geometryIn;

                    //get the drawing points
                        drawingPoints := geometryIn.getDrawingPoints();
                end;

    //public
        //constructor
            constructor TGraphicGeometry.create(const   lineThicknessIn : integer;
                                                const   fillColourIn,
                                                        lineColourIn    : TColor;
                                                const   lineStyleIn     : TPenStyle;
                                                const   geometryIn      : TGeomBase );
                begin
                    inherited create(   lineThicknessIn,
                                        geometryIn.getDrawingType(),
                                        fillColourIn,
                                        lineColourIn,
                                        lineStyleIn                 );

                    freeGeometry();

                    setGeometry( geometryIn );
                end;

            constructor TGraphicGeometry.create(const   lineThicknessIn : integer;
                                                const   lineColourIn    : TColor;
                                                const   lineStyleIn     : TPenStyle;
                                                const   geometryIn      : TGeomBase );
                begin
                    create( lineThicknessIn,
                            TColors.Null,
                            lineColourIn,
                            lineStyleIn,
                            geometryIn          );
                end;

        //destructor
            destructor TGraphicGeometry.destroy();
                begin
                    freeGeometry();

                    inherited destroy();
                end;

        

        //get the geometry object
            function TGraphicGeometry.getGeometry() : TGeomBase;
                begin
                    result := geometry;
                end;

        //get drawing points
            function TGraphicGeometry.getDrawingPoints() : TArray<TGeomPoint>;
                begin
                    result := drawingPoints;
                end;

end.
