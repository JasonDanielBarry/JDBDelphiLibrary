unit GraphicDrawingTypes;

interface

    type
        {$SCOPEDENUMS ON}
            EGraphicObjectType      = (gdEllipse = 0, gdLine = 1, gdPolygon = 2, gdPolyline = 3, gdSpaceVector = 4, gdRectangle = 5, gdText = 6, gdGroup = 7);
            EArrowOrigin            = (aoHead = 0, aoTail = 1);
            EArrowGroupDirection    = (agdUp = 0, agdDown = 1, agdLeft = 2, agdRight = 3, agdNormal = 4, agdUserDefined = 5);
        {$SCOPEDENUMS OFF}

implementation

end.
