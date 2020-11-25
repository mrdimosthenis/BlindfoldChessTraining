create table evaluated_line (
	id serial primary key,
	i int not null,
	"month" text not null,
	line text not null
);
create unique index evaluated_line_i_month on evaluated_line (i, "month");

CREATE TABLE position (
	id serial primary key,
	evaluated_line_id int not null references evaluated_line(id),
	num_of_halfmoves int not null,
	fen text not null
);
create unique index position_evaluated_line_id_num_of_halfmoves on position (evaluated_line_id, num_of_halfmoves);
