create table evaluated_line (
	id serial primary key,
	i int not null,
	"month" text not null,
	line text not null
);
create unique index name on evaluated_line (i, "month");
