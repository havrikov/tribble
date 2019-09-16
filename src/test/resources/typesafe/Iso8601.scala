import saarland.cispa.se.tribble.dsl._

/*
A grammar for the ISO 8601 Date and Time standard
modeled after Markus Kuhn's explanation of the format
https://www.cl.cam.ac.uk/~mgk25/iso-time.html
*/

Grammar(
  'start := 'date | 'time ~ 'timezone.? | 'date ~ (" " | "T") ~ 'time ~ 'timezone.?,

  // Dates
  'date := 'month_date | 'week_date | 'day_date,
  'month_date := 'separated_month_date | 'unseparated_month_date,
  'week_date := 'separated_week_date | 'unseparated_week_date,
  'day_date := 'separated_day_date | 'unseparated_day_date,

  'separated_month_date := 'year ~ ("-" ~ 'month ~ ("-" ~ 'day_of_month).?).?,
  'unseparated_month_date := 'year ~ ('month ~ 'day_of_month.?).?,

  'separated_week_date := 'year ~ ("-" ~ 'week ~ ("-" ~ 'day_of_week).?).?,
  'unseparated_week_date := 'year ~ ('week ~ 'day_of_week.?).?,

  'separated_day_date := 'year ~ ("-" ~ 'day_of_year).?,
  'unseparated_day_date := 'year ~ 'day_of_year.?,

  'year := "[0-9]{4}".regex,
  'month := "0[1-9]|1[012]".regex,
  'day_of_month := "[0-2][0-9]|3[01]".regex,
  'week := "W" ~ "[0-4][0-9]|5[0123]".regex,
  'day_of_week := "[1-7]".regex,
  'day_of_year := "00[1-9]|0[1-9][0-9]|[12][0-9]{2}|3[0-5][0-9]|36[0-5]".regex,

  // Times
  'time := 'hhmmss ~ 'millis.?,
  'hhmmss := 'separated_hhmmss | 'unseparated_hhmmss,
  'separated_hhmmss := 'hh ~ (":" ~ 'mm ~ (":" ~ 'ss).?).?,
  'unseparated_hhmmss := 'hh ~ ('mm ~ 'ss.?).?,

  'hh := "[01][0-9]|2[0-4]".regex,
  'mm := "[0-5][0-9]|60".regex,
  'ss := "[0-5][0-9]|60".regex,
  'millis := "" ~ "[0-9]{1,4}".regex,

  // Time Zones
  'timezone := 'zulu | 'precise,
  'zulu := "Z",
  'precise := ("+" | "-") ~ ('hh ~ (":".? ~ 'mm).?)
)
