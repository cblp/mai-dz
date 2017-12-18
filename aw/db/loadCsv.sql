.mode csv
.separator "\t"

CREATE TABLE [BillOfMaterials](
    [BillOfMaterialsID] [int] IDENTITY (1, 1) NOT NULL,
    [ProductAssemblyID] [int] NULL,
    [ComponentID] [int] NOT NULL,
    [StartDate] [datetime] NOT NULL CONSTRAINT [DF_BillOfMaterials_StartDate] DEFAULT (GETDATE()),
    [EndDate] [datetime] NULL,
    [UnitMeasureCode] [nchar](3) NOT NULL,
    [BOMLevel] [smallint] NOT NULL,
    [PerAssemblyQty] [decimal](8, 2) NOT NULL CONSTRAINT [DF_BillOfMaterials_PerAssemblyQty] DEFAULT (1.00),
    [ModifiedDate] [datetime] NOT NULL CONSTRAINT [DF_BillOfMaterials_ModifiedDate] DEFAULT (GETDATE()),
    CONSTRAINT [CK_BillOfMaterials_EndDate] CHECK (([EndDate] > [StartDate]) OR ([EndDate] = NULL)),
    CONSTRAINT [CK_BillOfMaterials_ProductAssemblyID] CHECK ([ProductAssemblyID] <> [ComponentID]),
    CONSTRAINT [CK_BillOfMaterials_BOMLevel] CHECK ((([ProductAssemblyID] = NULL)
        AND ([BOMLevel] = 0) AND ([PerAssemblyQty] = 1.00))
        OR (([ProductAssemblyID] IS NOT NULL) AND ([BOMLevel] >= 1))),
    CONSTRAINT [CK_BillOfMaterials_PerAssemblyQty] CHECK ([PerAssemblyQty] >= 1.00)
);

.import build/BillOfMaterials.csv BillOfMaterials

CREATE TABLE [Product](
    [ProductID] [int] IDENTITY (1, 1) NOT NULL,
    [Name] [Name] NOT NULL,
    [ProductNumber] [nvarchar](25) NOT NULL,
    [MakeFlag] [Flag] NOT NULL CONSTRAINT [DF_Product_MakeFlag] DEFAULT (1),
    [FinishedGoodsFlag] [Flag] NOT NULL CONSTRAINT [DF_Product_FinishedGoodsFlag] DEFAULT (1),
    [Color] [nvarchar](15) NULL,
    [SafetyStockLevel] [smallint] NOT NULL,
    [ReorderPoint] [smallint] NOT NULL,
    [StandardCost] [money] NOT NULL,
    [ListPrice] [money] NOT NULL,
    [Size] [nvarchar](5) NULL,
    [SizeUnitMeasureCode] [nchar](3) NULL,
    [WeightUnitMeasureCode] [nchar](3) NULL,
    [Weight] [decimal](8, 2) NULL,
    [DaysToManufacture] [int] NOT NULL,
    [ProductLine] [nchar](2) NULL,
    [Class] [nchar](2) NULL,
    [Style] [nchar](2) NULL,
    [ProductSubcategoryID] [int] NULL,
    [ProductModelID] [int] NULL,
    [SellStartDate] [datetime] NOT NULL,
    [SellEndDate] [datetime] NULL,
    [DiscontinuedDate] [datetime] NULL,
    [rowguid] uniqueidentifier ROWGUIDCOL NOT NULL CONSTRAINT [DF_Product_rowguid] DEFAULT (NEWID()),
    [ModifiedDate] [datetime] NOT NULL CONSTRAINT [DF_Product_ModifiedDate] DEFAULT (GETDATE()),
    CONSTRAINT [CK_Product_SafetyStockLevel] CHECK ([SafetyStockLevel] > 0),
    CONSTRAINT [CK_Product_ReorderPoint] CHECK ([ReorderPoint] > 0),
    CONSTRAINT [CK_Product_StandardCost] CHECK ([StandardCost] >= 0.00),
    CONSTRAINT [CK_Product_ListPrice] CHECK ([ListPrice] >= 0.00),
    CONSTRAINT [CK_Product_Weight] CHECK ([Weight] > 0.00),
    CONSTRAINT [CK_Product_DaysToManufacture] CHECK ([DaysToManufacture] >= 0),
    CONSTRAINT [CK_Product_ProductLine] CHECK (UPPER([ProductLine]) IN ('S', 'T', 'M', 'R') OR [ProductLine] = NULL),
    CONSTRAINT [CK_Product_Class] CHECK (UPPER([Class]) IN ('L', 'M', 'H') OR [Class] = NULL),
    CONSTRAINT [CK_Product_Style] CHECK (UPPER([Style]) IN ('W', 'M', 'U') OR [Style] = NULL),
    CONSTRAINT [CK_Product_SellEndDate] CHECK (([SellEndDate] >= [SellStartDate]) OR ([SellEndDate] = NULL))
);

.import build/Product.csv Product

UPDATE [Product] set [Class]                 = NULL WHERE [Class]                 = "";
UPDATE [Product] set [Color]                 = NULL WHERE [Color]                 = "";
UPDATE [Product] set [DiscontinuedDate]      = NULL WHERE [DiscontinuedDate]      = "";
UPDATE [Product] set [ProductLine]           = NULL WHERE [ProductLine]           = "";
UPDATE [Product] set [ProductModelID]        = NULL WHERE [ProductModelID]        = "";
UPDATE [Product] set [ProductSubcategoryID]  = NULL WHERE [ProductSubcategoryID]  = "";
UPDATE [Product] set [SellEndDate]           = NULL WHERE [SellEndDate]           = "";
UPDATE [Product] set [Size]                  = NULL WHERE [Size]                  = "";
UPDATE [Product] set [SizeUnitMeasureCode]   = NULL WHERE [SizeUnitMeasureCode]   = "";
UPDATE [Product] set [Style]                 = NULL WHERE [Style]                 = "";
UPDATE [Product] set [Weight]                = NULL WHERE [Weight]                = "";
UPDATE [Product] set [WeightUnitMeasureCode] = NULL WHERE [WeightUnitMeasureCode] = "";

CREATE UNIQUE INDEX [AK_Product_ProductNumber] ON [Product]([ProductNumber]);
CREATE UNIQUE INDEX [AK_Product_Name] ON [Product]([Name]);
CREATE UNIQUE INDEX [AK_Product_rowguid] ON [Product]([rowguid]);

CREATE TABLE [WorkOrder](
    [WorkOrderID] [int] IDENTITY (1, 1) NOT NULL,
    [ProductID] [int] NOT NULL,
    [OrderQty] [int] NOT NULL,
    [StockedQty] [int] NOT NULL, -- AS ISNULL([OrderQty] - [ScrappedQty], 0),
    [ScrappedQty] [smallint] NOT NULL,
    [StartDate] [datetime] NOT NULL,
    [EndDate] [datetime] NULL,
    [DueDate] [datetime] NOT NULL,
    [ScrapReasonID] [smallint] NULL,
    [ModifiedDate] [datetime] NOT NULL CONSTRAINT [DF_WorkOrder_ModifiedDate] DEFAULT (GETDATE()),
    CONSTRAINT [CK_WorkOrder_OrderQty] CHECK ([OrderQty] > 0),
    CONSTRAINT [CK_WorkOrder_ScrappedQty] CHECK ([ScrappedQty] >= 0),
    CONSTRAINT [CK_WorkOrder_EndDate] CHECK (([EndDate] >= [StartDate]) OR ([EndDate] IS NULL))
);

.import build/WorkOrder.csv WorkOrder

UPDATE [WorkOrder] set [ScrapReasonID] = NULL WHERE [ScrapReasonID] = "";

CREATE INDEX [IX_WorkOrder_ScrapReasonID] ON [WorkOrder]([ScrapReasonID]);
CREATE INDEX [IX_WorkOrder_ProductID] ON [WorkOrder]([ProductID]);
