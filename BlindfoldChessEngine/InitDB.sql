create table evaluated_line (
	id serial primary key,
	i int not null,
	"month" text not null,
	line text not null,
	is_parsed bool not null default false
);
create unique index evaluated_line_i_month on evaluated_line (i, "month");

CREATE TABLE position (
	evaluated_line_id int not null,
	num_of_halfmoves int not null,
	fen text not null
);