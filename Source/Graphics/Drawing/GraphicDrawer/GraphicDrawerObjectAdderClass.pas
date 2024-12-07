unit GraphicDrawerObjectAdderClass;

interface

    uses
        //Delphi
            system.SysUtils, system.types, system.UIConsts, system.UITypes, system.Generics.Collections,
            vcl.Graphics,
            vcl.Direct2D, Winapi.D2D1,
        //custom
            ColourMethods,
            DrawingAxisConversionClass,
            GraphicObjectBaseClass,
            GraphicGeometryClass, GraphicLineClass, GraphicPolylineClass, GraphicPolygonClass, GraphicTextClass,
            GraphicDrawerBaseClass,
            GeometryTypes,
            GeomLineClass, GeomPolyLineClass, GeomPolygonClass
            ;

    type
        TGraphicDrawerObjectAdder = class(TGraphicDrawerBase)
            public
                //constructor
                    constructor create(); override;
                //destructor
                    destructor destroy(); override;
                //add different drawing graphic objects
                    //geometry
                        procedure addLine(  const lineIn            : TGeomLine;
                                            const lineThicknessIn   : integer = 2;
                                            const colourIn          : TColor = TColors.Black;
                                            const styleIn           : TPenStyle = TPenStyle.psSolid );
                        procedure addPolyline(  const polylineIn        : TGeomPolyLine;
                                                const lineThicknessIn   : integer = 2;
                                                const colourIn          : TColor = TColors.Black;
                                                const styleIn           : TPenStyle = TPenStyle.psSolid );
                        procedure addPolygon(   const polygonIn         : TGeomPolygon;
                                                const lineThicknessIn   : integer = 2;
                                                const fillColourIn      : TColor = TColors.Null;
                                                const lineColourIn      : TColor = TColors.Black;
                                                const lineStyleIn       : TPenStyle = TPenStyle.psSolid     );
                    //text
                        procedure addText(  const   textStringIn        : string;
                                            const   textXIn, textYIn    : double;
                                            const   textSizeIn          : integer = 9;
                                            const   textColourIn        : TColor = TColors.Black;
                                            const   textFontStylesIn    : TFontStyles = []          );
                //modifiers
                    procedure setCurrentDrawingLayer(const layerKeyIn : string); virtual; abstract;
        end;

implementation

    //public
        //constructor
            constructor TGraphicDrawerObjectAdder.create();
                begin
                    inherited create();
                end;

        //destructor
            destructor TGraphicDrawerObjectAdder.destroy();
                begin
                    inherited destroy();
                end;

        //add different drawing graphic objects
            //geometry
                procedure TGraphicDrawerObjectAdder.addLine(const lineIn            : TGeomLine;
                                                            const lineThicknessIn   : integer = 2;
                                                            const colourIn          : TColor = TColors.Black;
                                                            const styleIn           : TPenStyle = TPenStyle.psSolid );
                    var
                        newGraphicGeometry : TGraphicGeometry;
                    begin
                        newGraphicGeometry := TGraphicLine.create(  lineThicknessIn,
                                                                    colourIn,
                                                                    styleIn,
                                                                    lineIn          );

                        addGraphicObject( newGraphicGeometry );
                    end;

                procedure TGraphicDrawerObjectAdder.addPolyline(const polylineIn        : TGeomPolyLine;
                                                                const lineThicknessIn   : integer = 2;
                                                                const colourIn          : TColor = TColors.Black;
                                                                const styleIn           : TPenStyle = TPenStyle.psSolid );
                    var
                        newGraphicGeometry : TGraphicGeometry;
                    begin
                        newGraphicGeometry := TGraphicPolyline.create(  lineThicknessIn,
                                                                        colourIn,
                                                                        styleIn,
                                                                        polylineIn      );

                        addGraphicObject( newGraphicGeometry );
                    end;

                procedure TGraphicDrawerObjectAdder.addPolygon( const polygonIn         : TGeomPolygon;
                                                                const lineThicknessIn   : integer = 2;
                                                                const fillColourIn      : TColor = TColors.Null;
                                                                const lineColourIn      : TColor = TColors.Black;
                                                                const lineStyleIn       : TPenStyle = TPenStyle.psSolid );
                    var
                        newGraphicGeometry : TGraphicGeometry;
                    begin
                        newGraphicGeometry := TGraphicPolygon.create(   lineThicknessIn,
                                                                        fillColourIn,
                                                                        lineColourIn,
                                                                        lineStyleIn,
                                                                        polygonIn       );

                        addGraphicObject( newGraphicGeometry );
                    end;

            //text
                procedure TGraphicDrawerObjectAdder.addText(const   textStringIn        : string;
                                                            const   textXIn, textYIn    : double;
                                                            const   textSizeIn          : integer = 9;
                                                            const   textColourIn        : TColor = TColors.Black;
                                                            const   textFontStylesIn    : TFontStyles = []      );
                    var
                        textTopLeftPoint    : TGeomPoint;
                        newGraphicText      : TGraphicText;
                    begin
                        if ( trim(textStringIn) = '' ) then
                            exit();

                        textTopLeftPoint := TGeomPoint.create( textXIn, textYIn );

                        newGraphicText := TGraphicText.create(  textSizeIn,
                                                                trim( textStringIn ),
                                                                textColourIn,
                                                                textFontStylesIn,
                                                                textTopLeftPoint    );

                        addGraphicObject( newGraphicText );
                    end;

end.
