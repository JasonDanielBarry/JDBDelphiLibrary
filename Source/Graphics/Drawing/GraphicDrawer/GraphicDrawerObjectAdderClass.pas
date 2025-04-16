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
            GraphicArcClass, GraphicEllipseClass, GraphicGeometryClass,
            GraphicLineClass, GraphicPolylineClass, GraphicPolygonClass,
            GraphicRectangleClass, GraphicTextClass, GraphicArrowClass,
            GraphicArrowGroupClass,
            GraphicDrawingTypes,
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
                    //arc
                        procedure addArc(   const   arcCentreXIn,
                                                    arcCentreYIn,
                                                    arcXRadiusIn,
                                                    arcYRadiusIn,
                                                    startAngleIn,
                                                    endAngleIn      : double;
                                            const   lineThicknessIn : integer = 2;
                                            const   lineColourIn    : TColor = TColors.Black;
                                            const   lineStyleIn     : TPenStyle = TPenStyle.psSolid );
                    //ellipse
                        procedure addEllipse(   const   diameterXIn,  diameterYIn,
                                                        centreXIn,    centreYIn     : double;
                                                const   filledIn                    : boolean = True;
                                                const   lineThicknessIn             : integer = 2;
                                                const   fillColourIn                : TColor = TColors.Null;
                                                const   lineColourIn                : TColor = TColors.Black;
                                                const   lineStyleIn                 : TPenStyle = TPenStyle.psSolid );
                    //geometry
                        //line
                            procedure addLine(  const lineIn            : TGeomLine;
                                                const lineThicknessIn   : integer = 2;
                                                const colourIn          : TColor = TColors.Black;
                                                const styleIn           : TPenStyle = TPenStyle.psSolid );
                        //polyline
                            procedure addPolyline(  const polylineIn        : TGeomPolyLine;
                                                    const lineThicknessIn   : integer = 2;
                                                    const colourIn          : TColor = TColors.Black;
                                                    const styleIn           : TPenStyle = TPenStyle.psSolid );
                        //polygon
                            procedure addPolygon(   const polygonIn         : TGeomPolygon;
                                                    const filledIn          : boolean = True;
                                                    const lineThicknessIn   : integer = 2;
                                                    const fillColourIn      : TColor = TColors.Null;
                                                    const lineColourIn      : TColor = TColors.Black;
                                                    const lineStyleIn       : TPenStyle = TPenStyle.psSolid     );
                    //rectanlge
                        procedure addRectangle( const   widthIn, heightIn,
                                                        leftIn, bottomIn    : double;
                                                const   filledIn            : boolean = True;
                                                const   lineThicknessIn     : integer = 2;
                                                const   cornerRadiusIn      : double = 0;
                                                const   fillColourIn        : TColor = TColors.Null;
                                                const   lineColourIn        : TColor = TColors.Black;
                                                const   lineStyleIn         : TPenStyle = TPenStyle.psSolid );
                    //text
                        procedure addText(  const   textXIn, textYIn    : double;
                                            const   textStringIn        : string;
                                            const   textSizeIn          : integer = 9;
                                            const   textRotationAngleIn : double = 0;
                                            const   textColourIn        : TColor = TColors.Black;
                                            const   textFontStylesIn    : TFontStyles = []          );
                    //groups
                        //arrow
                            procedure addArrow( const   arrowLengthIn,
                                                        directionAngleIn    : double;
                                                const   arrowOriginPointIn  : TGeomPoint;
                                                const   arrowOriginIn       : EArrowOrigin = EArrowOrigin.aoTail;
                                                const   filledIn            : boolean = True;
                                                const   lineThicknessIn     : integer = 3;
                                                const   fillColourIn        : TColor = TColors.Null;
                                                const   lineColourIn        : TColor = TColors.Black;
                                                const   lineStyleIn         : TPenStyle = TPenStyle.psSolid         );
                        //arrow group
                            //single line
                                procedure addArrowGroup(const   arrowLengthIn           : double;
                                                        const   arrowGroupLineIn        : TGeomLine;
                                                        const   arrowOriginIn           : EArrowOrigin = EArrowOrigin.aoTail;
                                                        const   arrowGroupDirectionIn   : EArrowGroupDirection = EArrowGroupDirection.agdNormal;
                                                        const   userDirectionAngleIn    : double = 0;
                                                        const   filledIn                : boolean = True;
                                                        const   lineThicknessIn         : integer = 3;
                                                        const   fillColourIn            : TColor = TColors.Null;
                                                        const   lineColourIn            : TColor = TColors.Black;
                                                        const   lineStyleIn             : TPenStyle = TPenStyle.psSolid                         ); overload;
                            //polyline
                                procedure addArrowGroup(const   arrowLengthIn           : double;
                                                        const   arrowGroupPolylineIn    : TGeomPolyLine;
                                                        const   arrowOriginIn           : EArrowOrigin = EArrowOrigin.aoTail;
                                                        const   arrowGroupDirectionIn   : EArrowGroupDirection = EArrowGroupDirection.agdNormal;
                                                        const   userDirectionAngleIn    : double = 0;
                                                        const   filledIn                : boolean = True;
                                                        const   lineThicknessIn         : integer = 3;
                                                        const   fillColourIn            : TColor = TColors.Null;
                                                        const   lineColourIn            : TColor = TColors.Black;
                                                        const   lineStyleIn             : TPenStyle = TPenStyle.psSolid                         ); overload;




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
            //arc
                procedure TGraphicDrawerObjectAdder.addArc( const   arcCentreXIn,
                                                                    arcCentreYIn,
                                                                    arcXRadiusIn,
                                                                    arcYRadiusIn,
                                                                    startAngleIn,
                                                                    endAngleIn      : double;
                                                            const   lineThicknessIn : integer = 2;
                                                            const   lineColourIn    : TColor = TColors.Black;
                                                            const   lineStyleIn     : TPenStyle = TPenStyle.psSolid );
                    var
                        centrePoint     : TGeomPoint;
                        newGraphicArc   : TGraphicArc;
                    begin
                        centrePoint := TGeomPoint.create( arcCentreXIn, arcCentreYIn );

                        newGraphicArc := TGraphicArc.create(    lineThicknessIn,
                                                                arcXRadiusIn, arcYRadiusIn,
                                                                startAngleIn, endAngleIn,
                                                                lineColourIn,
                                                                lineStyleIn,
                                                                centrePoint                 );

                        addGraphicObject( newGraphicArc );
                    end;

            //ellipse
                procedure TGraphicDrawerObjectAdder.addEllipse( const   diameterXIn,  diameterYIn,
                                                                        centreXIn,    centreYIn     : double;
                                                                const   filledIn                    : boolean = True;
                                                                const   lineThicknessIn             : integer = 2;
                                                                const   fillColourIn                : TColor = TColors.Null;
                                                                const   lineColourIn                : TColor = TColors.Black;
                                                                const   lineStyleIn                 : TPenStyle = TPenStyle.psSolid );
                    var
                        centrePoint         : TGeomPoint;
                        newGraphicEllipse   : TGraphicEllipse;
                    begin
                        centrePoint := TGeomPoint.create( centreXIn, centreYIn );

                        newGraphicEllipse := TGraphicEllipse.create(    filledIn,
                                                                        lineThicknessIn,
                                                                        diameterXIn, diameterYIn,
                                                                        fillColourIn,
                                                                        lineColourIn,
                                                                        lineStyleIn,
                                                                        centrePoint                 );

                        addGraphicObject( newGraphicEllipse );
                    end;

            //geometry
                //line
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

                //polyline
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

                //polygon
                    procedure TGraphicDrawerObjectAdder.addPolygon( const polygonIn         : TGeomPolygon;
                                                                    const filledIn          : boolean = True;
                                                                    const lineThicknessIn   : integer = 2;
                                                                    const fillColourIn      : TColor = TColors.Null;
                                                                    const lineColourIn      : TColor = TColors.Black;
                                                                    const lineStyleIn       : TPenStyle = TPenStyle.psSolid );
                        var
                            newGraphicGeometry : TGraphicGeometry;
                        begin
                            newGraphicGeometry := TGraphicPolygon.create(   filledIn,
                                                                            lineThicknessIn,
                                                                            fillColourIn,
                                                                            lineColourIn,
                                                                            lineStyleIn,
                                                                            polygonIn       );

                            addGraphicObject( newGraphicGeometry );
                        end;

            //rectanlge
                procedure TGraphicDrawerObjectAdder.addRectangle(   const   widthIn, heightIn,
                                                                            leftIn, bottomIn    : double;
                                                                    const   filledIn            : boolean = True;
                                                                    const   lineThicknessIn     : integer = 2;
                                                                    const   cornerRadiusIn      : double = 0;
                                                                    const   fillColourIn        : TColor = TColors.Null;
                                                                    const   lineColourIn        : TColor = TColors.Black;
                                                                    const   lineStyleIn         : TPenStyle = TPenStyle.psSolid );
                    var
                        newGraphicRectangle : TGraphicRectangle;
                        bottomLeftPoint     : TGeomPoint;
                    begin
                        bottomLeftPoint := TGeomPoint.create( leftIn, bottomIn );

                        newGraphicRectangle := TGraphicRectangle.create(    filledIn,
                                                                            lineThicknessIn,
                                                                            cornerRadiusIn,
                                                                            widthIn, heightIn,
                                                                            fillColourIn,
                                                                            lineColourIn,
                                                                            lineStyleIn,
                                                                            bottomLeftPoint     );

                        addGraphicObject( newGraphicRectangle );
                    end;

            //text
                procedure TGraphicDrawerObjectAdder.addText(const   textXIn, textYIn    : double;
                                                            const   textStringIn        : string;
                                                            const   textSizeIn          : integer = 9;
                                                            const   textRotationAngleIn : double = 0;
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
                                                                textRotationAngleIn,
                                                                trim( textStringIn ),
                                                                textColourIn,
                                                                textFontStylesIn,
                                                                textTopLeftPoint    );

                        addGraphicObject( newGraphicText );
                    end;

            //groups
                //arrow
                    procedure TGraphicDrawerObjectAdder.addArrow(   const   arrowLengthIn,
                                                                            directionAngleIn    : double;
                                                                    const   arrowOriginPointIn  : TGeomPoint;
                                                                    const   arrowOriginIn       : EArrowOrigin = EArrowOrigin.aoTail;
                                                                    const   filledIn            : boolean = True;
                                                                    const   lineThicknessIn     : integer = 3;
                                                                    const   fillColourIn        : TColor = TColors.Null;
                                                                    const   lineColourIn        : TColor = TColors.Black;
                                                                    const   lineStyleIn         : TPenStyle = TPenStyle.psSolid         );
                        var
                            newGraphicArrow : TGraphicArrow;
                        begin
                            newGraphicArrow := TGraphicArrow.create(    filledIn,
                                                                        lineThicknessIn,
                                                                        arrowLengthIn,
                                                                        directionAngleIn,
                                                                        fillColourIn,
                                                                        lineColourIn,
                                                                        lineStyleIn,
                                                                        arrowOriginIn,
                                                                        arrowOriginPointIn  );

                            addGraphicObject( newGraphicArrow );
                        end;

                //arrow group
                    procedure TGraphicDrawerObjectAdder.addArrowGroup(  const   arrowLengthIn           : double;
                                                                        const   arrowGroupLineIn        : TGeomLine;
                                                                        const   arrowOriginIn           : EArrowOrigin = EArrowOrigin.aoTail;
                                                                        const   arrowGroupDirectionIn   : EArrowGroupDirection = EArrowGroupDirection.agdNormal;
                                                                        const   userDirectionAngleIn    : double = 0;
                                                                        const   filledIn                : boolean = True;
                                                                        const   lineThicknessIn         : integer = 3;
                                                                        const   fillColourIn            : TColor = TColors.Null;
                                                                        const   lineColourIn            : TColor = TColors.Black;
                                                                        const   lineStyleIn             : TPenStyle = TPenStyle.psSolid                             );
                        var
                            newGraphicArrowGroup : TGraphicArrowGroup;
                        begin
                            newGraphicArrowGroup := TGraphicArrowGroup.create(  filledIn,
                                                                                lineThicknessIn,
                                                                                arrowLengthIn,
                                                                                userDirectionAngleIn,
                                                                                fillColourIn,
                                                                                lineColourIn,
                                                                                lineStyleIn,
                                                                                arrowOriginIn,
                                                                                arrowGroupDirectionIn,
                                                                                arrowGroupLineIn        );

                            addGraphicObject( newGraphicArrowGroup );
                        end;

                    procedure TGraphicDrawerObjectAdder.addArrowGroup(  const   arrowLengthIn           : double;
                                                                        const   arrowGroupPolylineIn    : TGeomPolyLine;
                                                                        const   arrowOriginIn           : EArrowOrigin = EArrowOrigin.aoTail;
                                                                        const   arrowGroupDirectionIn   : EArrowGroupDirection = EArrowGroupDirection.agdNormal;
                                                                        const   userDirectionAngleIn    : double = 0;
                                                                        const   filledIn                : boolean = True;
                                                                        const   lineThicknessIn         : integer = 3;
                                                                        const   fillColourIn            : TColor = TColors.Null;
                                                                        const   lineColourIn            : TColor = TColors.Black;
                                                                        const   lineStyleIn             : TPenStyle = TPenStyle.psSolid                         );
                        var
                            newGraphicArrowGroup : TGraphicArrowGroup;
                        begin
                            newGraphicArrowGroup := TGraphicArrowGroup.create(  filledIn,
                                                                                lineThicknessIn,
                                                                                arrowLengthIn,
                                                                                userDirectionAngleIn,
                                                                                fillColourIn,
                                                                                lineColourIn,
                                                                                lineStyleIn,
                                                                                arrowOriginIn,
                                                                                arrowGroupDirectionIn,
                                                                                arrowGroupPolylineIn    );

                            addGraphicObject( newGraphicArrowGroup );
                        end;

end.
