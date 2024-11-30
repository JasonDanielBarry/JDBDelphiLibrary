unit DrawingGeometryClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, System.UIConsts,
            vcl.Graphics,
        //custom
            ColourMethods,
            DrawingTypes,
            DrawingObjectBaseClass,
            DrawingAxisConversionClass,
            GeometryTypes,
            GeometryBaseClass
            ;

    type
        TDrawingGeometry = class(TDrawingObject)
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
            procedure TDrawingGeometry.freeGeometry();
                begin
                    try
                        FreeAndNil( geometry );
                    except

                    end;
                end;

        //set the geometry object
            procedure TDrawingGeometry.setGeometry(const geometryIn : TGeomBase);
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
            constructor TDrawingGeometry.create(const   lineThicknessIn : integer;
                                                const   fillColourIn,
                                                        lineColourIn    : TColor;
                                                const   lineStyleIn     : TPenStyle;
                                                const   geometryIn      : TGeomBase );
                begin
                    inherited create();

                    freeGeometry();

                    setValues(  lineThicknessIn,
                                geometryIn.getDrawingType(),
                                fillColourIn,
                                lineColourIn,
                                lineStyleIn                 );

                    setGeometry( geometryIn );
                end;

            constructor TDrawingGeometry.create(const   lineThicknessIn : integer;
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
            destructor TDrawingGeometry.destroy();
                begin
                    freeGeometry();

                    inherited destroy();
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
