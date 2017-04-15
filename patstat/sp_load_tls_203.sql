SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		Константин Нагаев
-- Create date: 15.04.2014
-- Description:	Загрузка tls_203 Patstat
-- =============================================
CREATE PROCEDURE SP_LOAD_TLS_203 
	@lang varchar 
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

	DECLARE @SQLString NVARCHAR(MAX)
	SET @SQLString = 'CREATE TABLE '+@TableName + '( '+@Column1Name+' '+@Column1DataType +' '+@Column1Nullable +') ON PRIMARY '

	EXEC (@SQLString)    

END
GO
