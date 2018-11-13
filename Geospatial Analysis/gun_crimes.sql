CREATE OR REPLACE FUNCTION get_gun_crimes(crime_type VARCHAR(70))
RETURNS TABLE(
 dist integer,
 blk text,
 num_crimes bigint,
 district_crimes bigint,
 x_coord double precision,
 y_coord double precision
)
AS $$
   BEGIN
        RETURN QUERY
			WITH crime_types AS (
			    SELECT DISTINCT
			          district,
			          block,
			          COUNT( id ) OVER ( PARTITION BY district, block ) AS block_gun_crimes,
                      COUNT( id ) OVER ( PARTITION BY district ) AS district_gun_crimes                           			          
			    FROM
			        crimes
			    WHERE
			         DESCRIPTION::text LIKE ('%' || crime_type || '%')
			    GROUP BY
			          district,
			          block,
			          location_description,
			          id
			), crime_rank AS (
			SELECT
			      *,
			      RANK() OVER( PARTITION BY district ORDER BY block_gun_crimes DESC ) AS Ranking
			FROM
			    crime_types
			),
			stations AS (
			 SELECT
			       CAST( district AS INTEGER ),
			       ST_X(ST_AsText(Where_IS)),
			       ST_Y(ST_AsText(Where_IS)) 
			 FROM
			     police_stations
			 WHERE
			      district != 'Headquarters'
			)
            SELECT
			      C.district,
			      C.block,
			      C.block_gun_crimes,
			      C.district_gun_crimes,
			      P.ST_X,
			      P.ST_Y
			FROM
			    crime_rank as C
			    INNER JOIN stations as P
			          on C.district = P.district
			WHERE
			     Ranking = 1
			ORDER BY
			      district;
END; $$ 

LANGUAGE 'plpgsql';