use patstat2016b

declare @langs table(id int identity(1,1), lang varchar(10))

insert into @langs (lang)
	select 'ar' union
	select 'bg' union
	select 'cs' union
	select 'da' union
	select 'de' union
	select 'el' union
	select 'en' union
	select 'es' union
	select 'et' union
	select 'fr' union
	select 'hr' union
	select 'it' union
	select 'ja' union
	select 'ko' union
	select 'lt' union
	select 'lv' union
	select 'nl' union
	select 'no' union
	select 'pl' union
	select 'pt' union
	select 'ro' union
	select 'ru' union
	select 'sh' union
	select 'sk' union
	select 'sl' union
	select 'sr' union
	select 'sv' union
	select 'tr' union
	select 'uk' union
	select 'zh'
	 
declare @i int
declare @cnt int
declare @lang varchar(2)

select @i = min(id) - 1, @cnt = max(id) from @langs

while @i < @cnt
begin
     select @i = @i + 1
	 	 
	 select @lang = lang from @langs where id = @i

	 --select @lang
     exec SP_CREATE_TABLE_TLS_203 @lang

end