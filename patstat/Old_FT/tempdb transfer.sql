use tempdb

go

exec sp_helpfile
go

alter database tempdb
modify file (name=tempdev, filename='d:\mssql\data\tempdb.mdf')
go
alter database tempdb
modify file (name=templog, filename='d:\mssql\data\templog.ldf')
go
