--tail +2 tls202_part01.txt | iconv -f UTF-8 -t UTF-16LE > tls202_part01_utf16.txt

USE [patstat2016b]
GO

truncate table [dbo].tls202_appln_title
GO

GO
BULK INSERT [dbo].tls202_appln_title
FROM 'C:\work\others\Patstat\tls202_part01_utf16.txt'
WITH
(
--FIELDTERMINATOR ='"',  
--ROWTERMINATOR = '\n',  
--FIRSTROW = 3,
DATAFILETYPE = 'widenative',
FORMATFILE = 'C:\work\others\Patstat\tls202_part01.fmt',
TABLOCK
)
GO

SELECT TOP 1000 [appln_id]
      ,[appln_title_lg]
      ,[appln_title]
  FROM [patstat2016b].[dbo].[tls202_appln_title]