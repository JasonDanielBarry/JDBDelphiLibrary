unit GraphicArrowClass;

interface

    uses
        Winapi.D2D1,
        system.SysUtils, system.UITypes,
        Vcl.Direct2D, vcl.Graphics,
        GeomBox, GeometryTypes,
        GeomLineClass, GeomPolygonClass,
        GraphicDrawingTypes,
        DrawingAxisConversionClass,
        GraphicLineClass, GraphicPolygonClass,
        GraphicObjectGroupClass
        ;

    type
        TGraphicArrow = class(TGraphicObjectGroup)
            private
                var
                    arrowHead : TGeomPolygon;
                    arrowTail : TGeomLine;
                //create arrow geometry
                    function createArrowHead() : TGeomPolygon;
                    function createArrowTail() : TGeomLine;
                    procedure createArrowGeometry(  const   arrowLengthIn,
                                                            directionAngleIn    : double;
                                                    const   arrowOriginIn       : EArrowOrigin;
                                                    const   arrowOriginPointIn  : TGeomPoint    );
            public
                //constructor
                    constructor create( const   filledIn            : boolean;
                                        const   lineThicknessIn     : integer;
                                        const   arrowLengthIn,
                                                directionAngleIn    : double;
                                        const   fillColourIn,
                                                lineColourIn        : TColor;
                                        const   lineStyleIn         : TPenStyle;
                                        const   arrowOriginIn       : EArrowOrigin;
                                        const   arrowOriginPointIn  : TGeomPoint    );
                //destructor
                    destructor destroy(); override;
                //get geometry
                    function getArrowHeadPoint() : TGeomPoint;
                    function getArrowTailPoint() : TGeomPoint;
        end;

implementation

    const
        ARROW_HEAD_HEIGHT   : double = 0.066;
        ARROW_HEAD_WIDTH    : double = 0.022;

    //private
        //create arrow geometry
            function TGraphicArrow.createArrowHead() : TGeomPolygon;
                var
                    arrowHeadPolygonOut : TGeomPolygon;
                begin
                    arrowHeadPolygonOut := TGeomPolygon.create();

                    arrowHeadPolygonOut.addVertex( 1, 0 );
                    arrowHeadPolygonOut.addVertex( 1 - ARROW_HEAD_HEIGHT, ARROW_HEAD_WIDTH );
                    arrowHeadPolygonOut.addVertex( 1 - ARROW_HEAD_HEIGHT, -ARROW_HEAD_WIDTH );

                    result := arrowHeadPolygonOut;
                end;

            function TGraphicArrow.createArrowTail() : TGeomLine;
                var
                    arrowTailLineOut : TGeomLine;
                begin
                    arrowTailLineOut := TGeomLine.create();

                    arrowTailLineOut.setStartPoint( 0, 0 );
                    arrowTailLineOut.setEndPoint( 1 - ARROW_HEAD_HEIGHT, 0 );

                    result := arrowTailLineOut;
                end;

            procedure TGraphicArrow.createArrowGeometry(const   arrowLengthIn,
                                                                directionAngleIn    : double;
                                                        const   arrowOriginIn       : EArrowOrigin;
                                                        const   arrowOriginPointIn  : TGeomPoint    );
                var
                    referencePoint : TGeomPoint;
                begin
                    //objects created here
                        arrowHead := createArrowHead();
                        arrowTail := createArrowTail();

                    //the origin sits by default at the tail
                        if ( arrowOriginIn = EArrowOrigin.aoHead ) then
                            begin
                                arrowHead.shift( -1, 0 );
                                arrowTail.shift( -1, 0 );
                            end;

                        referencePoint := TGeomPoint.create( 0, 0 );

                    //scale the arrow - the arrow is 1 unit in length so the required length IS the scale factor
                        arrowHead.scale( arrowLengthIn, referencePoint );
                        arrowTail.scale( arrowLengthIn, referencePoint );

                    //rotate the arrow
                        arrowHead.rotate( directionAngleIn, referencePoint );
                        arrowTail.rotate( directionAngleIn, referencePoint );

                    //shift the arrow
                        arrowHead.shift( arrowOriginPointIn.x, arrowOriginPointIn.y );
                        arrowTail.shift( arrowOriginPointIn.x, arrowOriginPointIn.y );
                end;

    //public
        //constructor
            constructor TGraphicArrow.create(   const   filledIn            : boolean;
                                                const   lineThicknessIn     : integer;
                                                const   arrowLengthIn,
                                                        directionAngleIn    : double;
                                                const   fillColourIn,
                                                        lineColourIn        : TColor;
                                                const   lineStyleIn         : TPenStyle;
                                                const   arrowOriginIn       : EArrowOrigin;
                                                const   arrowOriginPointIn  : TGeomPoint    );
                var
                    headGraphic : TGraphicPolygon;
                    tailGraphic : TGraphicLine;
                begin
                    inherited create();

                    //create the arrow geometry
                        createArrowGeometry( arrowLengthIn, directionAngleIn, arrowOriginIn, arrowOriginPointIn );

                    //create graphic objects
                        //arrow head
                            headGraphic := TGraphicPolygon.create( filledIn, lineThicknessIn, fillColourIn, lineColourIn, lineStyleIn, arrowHead );
                            addGraphicObjectToGroup( headGraphic );

                        //arrow tail
                            tailGraphic := TGraphicLine.create( lineThicknessIn, lineColourIn, lineStyleIn, arrowTail );
                            addGraphicObjectToGroup( tailGraphic );
                end;

        //destructor
            destructor TGraphicArrow.destroy();
                begin
                    FreeAndNil( arrowHead );
                    FreeAndNil( arrowTail );

                    inherited destroy();
                end;

        //get geometry
            function TGraphicArrow.getArrowHeadPoint() : TGeomPoint;
                begin
                    result := arrowHead.getDrawingPoints()[0];
                end;

            function TGraphicArrow.getArrowTailPoint() : TGeomPoint;
                begin
                    result := arrowTail.getStartPoint();
                end;


end.
