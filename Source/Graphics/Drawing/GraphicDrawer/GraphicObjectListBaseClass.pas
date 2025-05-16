unit GraphicObjectListBaseClass;

interface

    uses
        System.Generics.Collections,
        GraphicObjectBaseClass
        ;

    type
        TGraphicObjectListBase = class(TList< TPair< string, TGraphicObject > >)
            private
                var
                    currentDrawingLayer : string;
            protected
                //add a graphic object to the list
                    procedure addGraphicObject(const graphicObjectIn : TGraphicObject);
            public
                //constructor
                    constructor create();
                //set the layer to which to send the graphic objects
                    procedure setcurrentDrawingLayer(const layerIn : string);
        end;

implementation

    //protected
        //add a graphic object to the list
            procedure TGraphicObjectListBase.addGraphicObject(const graphicObjectIn : TGraphicObject);
                var
                    layerGraphicObjectPair : TPair<string, TGraphicObject>;
                begin
                    layerGraphicObjectPair.Key      := currentDrawingLayer;
                    layerGraphicObjectPair.Value    := graphicObjectIn;

                    self.Add( layerGraphicObjectPair );
                end;

    //public
        //constructor
            constructor TGraphicObjectListBase.create();
                begin
                    inherited create();

                    currentDrawingLayer := 'Default drawing layer';

                    clear();
                end;

        //set the layer to which to send the graphic objects
            procedure TGraphicObjectListBase.setcurrentDrawingLayer(const layerIn : string);
                begin
                    currentDrawingLayer := layerIn;
                end;

end.
