unit DrawingGeometryClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes, System.UIConsts,
        //custom
            DrawingAxisConversionClass,
            GeometryTypes,
            GeometryBaseClass
            ;

    type
        TDrawingGeometry = class
            strict private
                var
                    fLineThickness  : integer;
                    fFillColour,
                    fLineColour     : TAlphaColor;
                    fGeometry       : TGeomBase;
                    fDrawingPoints  : TArray<TGeomPoint>;
                //free geometry object
                    procedure freeGeometry();
            public
                //constructor
                    constructor create( const   lineThicknessIn : integer;
                                        const   fillColourIn,
                                                lineColourIn    : TAlphaColor;
                                        const   geometryIn      : TGeomBase     );
                //destructor
                    destructor destroy(); override;
                //line thickness
                    property LineThickness : integer read fLineThickness;
                //colours
                    property FillColour : TAlphaColor read fFillColour;
                    property LineColour : TAlphaColor read fLineColour;
                //get the geometry object
                    property Geometry : TGeomBase read fGeometry;
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

    //public
        //constructor
            constructor TDrawingGeometry.create(const   lineThicknessIn : integer;
                                                const   fillColourIn,
                                                        lineColourIn    : TAlphaColor;
                                                const   geometryIn      : TGeomBase );
                begin
                    inherited create();

                    freeGeometry();

                    fLineThickness  := lineThicknessIn;
                    fFillColour     := fillColourIn;
                    fLineColour     := lineColourIn;
                    fGeometry       := geometryIn;

                    fDrawingPoints  := geometryIn.getDrawingPoints();
                end;

        //destructor
            destructor TDrawingGeometry.destroy();
                begin
                    freeGeometry();

                    inherited destroy();
                end;

//        //line thickness
//            function TDrawingGeometry.getLineThickness() : integer;
//                begin
//                    result := lineThickness;
//                end;
//
//        //colours
//            function TDrawingGeometry.getFillColour() : TAlphaColor;
//                begin
//                    result := fillColour;
//                end;
//
//            function TDrawingGeometry.getLineColour() : TAlphaColor;
//                begin
//                    result := lineColour;
//                end;
//
//        //get the geometry object
//            function TDrawingGeometry.getGeometry() : TGeomBase;
//                begin
//                    result := geometry;
//                end;

end.
