unit LimitStateMaterialClass;

interface

    uses
        system.SysUtils, system.Math;

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

end.
