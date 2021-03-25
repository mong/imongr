ALTER TABLE
  `data`
ADD COLUMN IF NOT EXISTS
  `context` varchar(127) NOT NULL DEFAULT 'caregiver'
AFTER `delivery_id`;

CREATE INDEX IF NOT EXISTS index_data_context ON data (context);
