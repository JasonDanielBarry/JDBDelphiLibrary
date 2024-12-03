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
            GraphicGeometryClass, GraphicLineClass, GraphicPolylineClass, GraphicPolygonClass,
            GraphicDrawerBaseClass,
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
            procedure TGraphicDrawerObjectAdder.addLine(const lineIn            : TGeomLine;
                                                        const lineThicknessIn   : integer = 2;
                                                        const colourIn          : TColor = TColors.Black;
                                                        const styleIn           : TPenStyle = TPenStyle.psSolid );
                var
                    newDrawingGeometry : TGraphicGeometry;
                begin
                    newDrawingGeometry := TGraphicLine.create(  lineThicknessIn,
                                                                colourIn,
                                                                styleIn,
                                                                lineIn          );

                    addGraphicObject( newDrawingGeometry );
                end;

            procedure TGraphicDrawerObjectAdder.addPolyline(const polylineIn        : TGeomPolyLine;
                                                            const lineThicknessIn   : integer = 2;
                                                            const colourIn          : TColor = TColors.Black;
                                                            const styleIn           : TPenStyle = TPenStyle.psSolid );
                var
                    newDrawingGeometry : TGraphicGeometry;
                begin
                    newDrawingGeometry := TGraphicPolyline.create(  lineThicknessIn,
                                                                    colourIn,
                                                                    styleIn,
                                                                    polylineIn      );

                    addGraphicObject( newDrawingGeometry );
                end;

            procedure TGraphicDrawerObjectAdder.addPolygon( const polygonIn         : TGeomPolygon;
                                                            const lineThicknessIn   : integer = 2;
                                                            const fillColourIn      : TColor = TColors.Null;
                                                            const lineColourIn      : TColor = TColors.Black;
                                                            const lineStyleIn       : TPenStyle = TPenStyle.psSolid );
                var
                    newDrawingGeometry : TGraphicGeometry;
                begin
                    newDrawingGeometry := TGraphicPolygon.create(   lineThicknessIn,
                                                                    fillColourIn,
                                                                    lineColourIn,
                                                                    lineStyleIn,
                                                                    polygonIn       );

                    addGraphicObject( newDrawingGeometry );
                end;


end.
