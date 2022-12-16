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

ALTER TABLE
  `agg_data`
ADD COLUMN IF NOT EXISTS
  `delivery_latest_affirm` DATE DEFAULT NULL
AFTER
  `delivery_time`,
ADD COLUMN IF NOT EXISTS
  `delivery_latest_update` DATE DEFAULT NULL
AFTER
  `delivery_time`;

ALTER TABLE
  ind
DROP CONSTRAINT IF EXISTS inconsistent_level_values;

ALTER TABLE
  ind
ADD CONSTRAINT inconsistent_level_values
    CHECK (case
      when (level_direction = 0 and level_green > level_yellow) then false
      when (level_direction = 1 and level_green < level_yellow) then false
      else true end
    );

ALTER TABLE
  `ind`
ADD COLUMN IF NOT EXISTS
  `sformat` VARCHAR(31) DEFAULT ',.0%'
AFTER `type`;

ALTER TABLE
  registry
ADD COLUMN
  description VARCHAR(2047)
AFTER full_name,
ADD COLUMN
  url VARCHAR(1023)
AFTER full_name;

ALTER TABLE
  delivery
ADD COLUMN IF NOT EXISTS
  `terms_version` VARCHAR(127) DEFAULT NULL
AFTER
  `md5_checksum`;

ALTER TABLE
  delivery
ADD COLUMN IF NOT EXISTS
  `latest_affirm` DATE DEFAULT NULL
AFTER
  `time`,
ADD COLUMN IF NOT EXISTS
  `latest_update` DATE DEFAULT NULL
AFTER
  `time`;

CREATE INDEX IF NOT EXISTS index_data_context ON data (context);

CREATE INDEX IF NOT EXISTS index_agg_data_context ON agg_data (context);
