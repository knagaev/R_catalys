USE [patstat2016b]
GO
/****** Object:  Table [dbo].[tls202_appln_title]    Script Date: 07/07/2015 08:16:03 ******/
/*
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
--DROP TABLE [dbo].[tls202_appln_title_da]
GO
CREATE TABLE [dbo].[tls202_appln_title_da](
	[appln_id] [int] NOT NULL DEFAULT ('0'),
	[appln_title_lg] [char](2) NOT NULL DEFAULT (''),
	[appln_title] [varchar](max) COLLATE Danish_Norwegian_CI_AS NOT NULL , --SQL_Danish_Pref_CP1_CI_AS
) ON [PRIMARY]
*/

truncate table [dbo].tls202_appln_title_da
GO

BULK INSERT [dbo].tls202_appln_title_da
FROM 'C:\work\others\Patstat\test_da.txt'
WITH
(
--FIELDTERMINATOR ='"',  
--ROWTERMINATOR = '\n',  
--FIRSTROW = 3,
DATAFILETYPE = 'char',
FORMATFILE = 'C:\work\others\Patstat\tls202_part01.fmt'
--,TABLOCK
)
GO

SELECT TOP 1000 [appln_id]
      ,[appln_title_lg]
      ,[appln_title]
  FROM [patstat2016b].[dbo].[tls202_appln_title_da]