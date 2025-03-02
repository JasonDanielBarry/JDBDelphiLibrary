unit LimitStateMaterialClass;

interface

    uses
        system.SysUtils, system.Math,
        Xml.XMLIntf,
        XMLDocumentMethods
        ;

    type
        TLimitStateMaterial = class
            var
                averageValue,
                variationCoefficient,
                downgradeFactor,
                partialFactor       : double;
            constructor create();
            destructor destroy(); override;
            function cautiousEstimate() : double; virtual;
            function designValue() : double; virtual;
            procedure setValues(const averageValueIn, variationCoefficientIn, downgradeFactorIn, partialFactorIn : double); virtual;
            procedure copyOther(const otherMaterialIn: TLimitStateMaterial); virtual;
            function tryReadFromXMLNode(const XMLNodeIn : IXMLNode) : boolean;
            procedure writeToXMLNode(var XMLNodeInOut : IXMLNode);
        end;

implementation

    constructor TLimitStateMaterial.create();
        begin
            inherited create();
        end;

    destructor TLimitStateMaterial.destroy();
        begin
            inherited destroy();
        end;

    function TLimitStateMaterial.cautiousEstimate() : double;
        var
            av, vc, df : double;
        begin
            av := averageValue;
            vc := variationCoefficient;
            df := downgradeFactor;

            result := av * (1 - vc * df);
        end;

    function TLimitStateMaterial.designValue() : double;
        begin
            result := cautiousEstimate() / partialFactor;
        end;

    procedure TLimitStateMaterial.setValues(const averageValueIn, variationCoefficientIn, downgradeFactorIn, partialFactorIn : double);
        begin
            averageValue          := max(0, averageValueIn);
            variationCoefficient  := max(0, variationCoefficientIn);
            downgradeFactor       := max(0, downgradeFactorIn);
            partialFactor         := max(1, partialFactorIn);
        end;

    procedure TLimitStateMaterial.copyOther(const otherMaterialIn: TLimitStateMaterial);
        begin
            setValues(  otherMaterialIn.averageValue,
                        otherMaterialIn.variationCoefficient,
                        otherMaterialIn.downgradeFactor,
                        otherMaterialIn.partialFactor        );
        end;

    const
        AVERAGE_VALUE           : string = 'AverageValue';
        VARIATION_COEFFICIENT   : string = 'VariationCoefficient';
        DOWNGRADE_FACTOR        : string = 'DowngradeFactor';
        PARTIAL_FACTOR          : string = 'PartialFactor';
        DT_LIMIT_STATE_MATERIAL : string = 'TLimitStateMaterial';


    function TLimitStateMaterial.tryReadFromXMLNode(const XMLNodeIn : IXMLNode) : boolean;
        var
            successfulRead : boolean;
        begin
            if NOT( Assigned( XMLNodeIn ) ) then
                exit( False );

            if NOT( XMLNodeIsDataType( XMLNodeIn, DT_LIMIT_STATE_MATERIAL ) ) then
                exit( False );

            successfulRead := tryReadDoubleFromXMLNode( XMLNodeIn, AVERAGE_VALUE, self.averageValue );
            successfulRead := successfulRead AND tryReadDoubleFromXMLNode( XMLNodeIn, VARIATION_COEFFICIENT, self.variationCoefficient );
            successfulRead := successfulRead AND tryReadDoubleFromXMLNode( XMLNodeIn, DOWNGRADE_FACTOR,      self.downgradeFactor      );
            successfulRead := successfulRead AND tryReadDoubleFromXMLNode( XMLNodeIn, PARTIAL_FACTOR,        self.partialFactor, 1     );

            result := successfulRead;
        end;

    procedure TLimitStateMaterial.writeToXMLNode(var XMLNodeInOut : IXMLNode);
        begin
            setXMLNodeDataType( XMLNodeInOut, DT_LIMIT_STATE_MATERIAL );

            writeDoubleToXMLNode( XMLNodeInOut, AVERAGE_VALUE,          self.averageValue         );
            writeDoubleToXMLNode( XMLNodeInOut, VARIATION_COEFFICIENT,  self.variationCoefficient );
            writeDoubleToXMLNode( XMLNodeInOut, DOWNGRADE_FACTOR,       self.downgradeFactor      );
            writeDoubleToXMLNode( XMLNodeInOut, PARTIAL_FACTOR,         self.partialFactor        );
        end;

end.
