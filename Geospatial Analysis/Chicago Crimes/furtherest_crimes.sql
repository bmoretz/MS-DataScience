CREATE OR REPLACE FUNCTION get_furthest_crimes(crime_type VARCHAR(70))
RETURNS TABLE(
 dist integer,
 blk text,
 n_crimes BIGINT,
 pd_x_coord double precision,
 pd_y_coord double precision,
 cr_x_coord double precision,
 cr_y_coord double precision
)
AS $$
   BEGIN
        RETURN QUERY
			WITH gun_crimes AS
			(
				SELECT
			          district,
				      block,
				      where_is as loc,
			          ST_X(ST_AsText(Where_IS)) AS x_coord,
				      ST_Y(ST_AsText(Where_IS)) AS y_coord
				FROM
				    crimes
				WHERE
				     DESCRIPTION::text LIKE ('%' || crime_type || '%')
			 ), stations AS (
				SELECT
				     CAST( district AS INTEGER ),
				     where_is as loc,
				     ST_X(ST_AsText(Where_IS)) AS x_coord,
				     ST_Y(ST_AsText(Where_IS)) AS y_coord
				FROM
				   police_stations
				WHERE
				    district != 'Headquarters'
			), crime_distances as (
				SELECT
				      C.district,
				      C.block,
				      ST_Distance( C.loc, P.loc ) as crime_dist,
				      C.x_coord as crime_x_coord, 
				      C.y_coord as crime_y_coord,
				      P.x_coord as station_x_coord,
				      P.y_coord as station_y_coord
				FROM
				    gun_crimes as C
				    INNER JOIN stations as P
				          on C.district = P.district
			), distance_ranking AS (
				SELECT
				      district,
				      block,
				      COUNT( * ) OVER ( PARTITION BY district ) as num_crimes,
				      RANK() OVER( PARTITION BY district ORDER BY crime_dist DESC ) AS dist_rank,
				      station_x_coord,
				      station_y_coord,
				      crime_x_coord,
				      crime_y_coord
				FROM
				    crime_distances
			)
			SELECT
			      district,
			      block,
			      num_crimes,
			      station_x_coord,
			      station_y_coord,
			      crime_x_coord,
			      crime_y_coord
			FROM
			    distance_ranking
			WHERE
			     dist_rank = 1
			;
END; $$ 

LANGUAGE 'plpgsql';