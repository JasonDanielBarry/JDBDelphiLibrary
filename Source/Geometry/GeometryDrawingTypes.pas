unit GeometryDrawingTypes;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UITypes,
        //custom
            GeometryTypes,
            GeometryBaseClass,
            GeomLineClass, GeomPolyLineClass
            ;

    type
        TDrawingGeometry = record
            lineThickness   : integer;
            fillColour,
            lineColour      : TAlphaColor;
            geometry        : TGeomBase;
            constructor create( const   lineThicknessIn : integer;
                                const   fillColourIn,
                                        lineColourIn    : TAlphaColor;
                                const   geometryIn      : TGeomBase     );
            procedure setValues(const   lineThicknessIn : integer;
                                const   fillColourIn,
                                        lineColourIn    : TAlphaColor;
                                const   geometryIn      : TGeomBase     );
            procedure freeGeometry();
        end;

implementation

    constructor TDrawingGeometry.create(const   lineThicknessIn : integer;
                                        const   fillColourIn,
                                                lineColourIn    : TAlphaColor;
                                        const   geometryIn      : TGeomBase     );
        begin
            setValues(  lineThicknessIn,
                        fillColourIn,
                        lineColourIn,
                        geometryIn      );
        end;

    procedure TDrawingGeometry.setValues(   const   lineThicknessIn : integer;
                                            const   fillColourIn,
                                                    lineColourIn    : TAlphaColor;
                                            const   geometryIn      : TGeomBase     );
        begin
            lineThickness   := lineThicknessIn;
            geometry        := geometryIn;
            fillColour      := fillColourIn;
            lineColour      := lineColourIn;
        end;

    procedure TDrawingGeometry.freeGeometry();
        begin
            try
                FreeAndNil( geometry );
            except

            end;
        end;

end.
