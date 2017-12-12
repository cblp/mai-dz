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
    CONSTRAINT [CK_Product_ProductLine] CHECK (UPPER([ProductLine]) IN ('S', 'T', 'M', 'R') OR [ProductLine] == NULL),
    CONSTRAINT [CK_Product_Class] CHECK (UPPER([Class]) IN ('L', 'M', 'H') OR [Class] == NULL),
    CONSTRAINT [CK_Product_Style] CHECK (UPPER([Style]) IN ('W', 'M', 'U') OR [Style] == NULL),
    CONSTRAINT [CK_Product_SellEndDate] CHECK (([SellEndDate] >= [SellStartDate]) OR ([SellEndDate] == NULL))
);

.mode csv
.separator "\t"
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
