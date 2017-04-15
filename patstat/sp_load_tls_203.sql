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
	@lang varchar(2),
	@datafile varchar(255) = 'G:\Patstat\Patstat Biblio\data_PATSTAT_Biblio_2016_Autumn\tls203\processed\', 
	@formatfile varchar(255) = 'C:\PLR\R_catalys\patstat\tls203.fmt' 
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

	--declare @datafile varchar(255)
	--set @datafile = 'G:\Patstat\Patstat Biblio\data_PATSTAT_Biblio_2016_Autumn\tls203\processed\'
	--declare @formatfile varchar(255)
	--set @formatfile = 'C:\PLR\R_catalys\patstat\tls203.fmt'


	DECLARE @SQLString VARCHAR(MAX)
	SET @SQLString = 'BULK INSERT [dbo].tls203_appln_abstr_en
		FROM ''' + @datafile + '\tls203_' + @lang + '.txt''
		WITH
		(
		BATCHSIZE = 20000, 
		DATAFILETYPE = ''char'',
		FORMATFILE = ''' + @formatfile + '''
		)
		';

	--select @SQLString
	exec (@SQLString) 

END
GO
