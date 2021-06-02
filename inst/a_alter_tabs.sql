ALTER TABLE
  `data`
ADD COLUMN IF NOT EXISTS
  `context` varchar(127) NOT NULL DEFAULT 'caregiver'
AFTER `delivery_id`;

ALTER TABLE
  `agg_data`
ADD COLUMN IF NOT EXISTS
  `context` varchar(127) NOT NULL DEFAULT 'caregiver'
AFTER `unit_name`;

ALTER TABLE
  `agg_data`
ADD COLUMN IF NOT EXISTS
  `delivery_time` timestamp NULL DEFAULT NULL
AFTER `dg`;

CREATE INDEX IF NOT EXISTS index_data_context ON data (context);

CREATE INDEX IF NOT EXISTS index_agg_data_context ON agg_data (context);
