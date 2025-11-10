CREATE TABLE IF NOT EXISTS `nation` (
  `orgnr` int(10) unsigned NOT NULL,
  `full_name` varchar(255) NOT NULL,
  `short_name` varchar(128) NOT NULL,
  PRIMARY KEY (`orgnr`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `rhf` (
  `orgnr` int(10) unsigned NOT NULL,
  `full_name` varchar(255) NOT NULL,
  `short_name` varchar(128) NOT NULL,
  `nation_orgnr` int(10) unsigned NOT NULL,
  `sorting` int(10) unsigned NOT NULL,
  `shortest_name` varchar(20) NOT NULL,
  `url` varchar(128) DEFAULT NULL,
  PRIMARY KEY (`orgnr`),
  KEY `fk_nation_orgnr` (`nation_orgnr`),
  CONSTRAINT `fk_nation_orgnr`
    FOREIGN KEY (`nation_orgnr`) REFERENCES `nation` (`orgnr`)
    ON UPDATE CASCADE
    ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `hf` (
  `orgnr` int(10) unsigned NOT NULL,
  `full_name` varchar(255) NOT NULL,
  `short_name` varchar(128) NOT NULL,
  `rhf_orgnr` int(10) unsigned NOT NULL,
  `sorting` int(10) unsigned NOT NULL,
  `shortest_name` varchar(20) NOT NULL,
  `url` varchar(128) DEFAULT NULL,
  PRIMARY KEY (`orgnr`),
  KEY `fk_rhf_orgnr` (`rhf_orgnr`),
  CONSTRAINT `fk_rhf_orgnr`
    FOREIGN KEY (`rhf_orgnr`) REFERENCES `rhf` (`orgnr`)
    ON UPDATE CASCADE
    ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `hospital` (
  `orgnr` int(10) unsigned NOT NULL,
  `full_name` varchar(255) NOT NULL,
  `short_name` varchar(128) NOT NULL,
  `hf_orgnr` int(10) unsigned NOT NULL,
  PRIMARY KEY (`orgnr`),
  KEY `fk_hf_orgnr` (`hf_orgnr`),
  CONSTRAINT `fk_hf_orgnr`
    FOREIGN KEY (`hf_orgnr`) REFERENCES `hf` (`orgnr`)
    ON UPDATE CASCADE
    ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `registry` (
  `id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(255) NOT NULL,
  `full_name` varchar(255) DEFAULT NULL,
  `url` varchar(1023) DEFAULT NULL,
  `description` varchar(2047) DEFAULT NULL,
  `short_name` varchar(128) DEFAULT NULL,
  `RHF` varchar(128) DEFAULT NULL,
  `first_year` smallint(5) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_name` (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `medfield` (
  `id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(255) NOT NULL,
  `full_name` varchar(255) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_name` (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `registry_medfield` (
  `id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
  `registry_id` smallint(5) unsigned NOT NULL,
  `medfield_id` smallint(5) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  KEY `fk_registry_medfield_registry` (`registry_id`),
  KEY `fk_registry_medfield_medfield` (`medfield_id`),
  CONSTRAINT `fk_registry_medfield_medfield`
    FOREIGN KEY (`medfield_id`) REFERENCES `medfield` (`id`)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  CONSTRAINT `fk_registry_medfield_registry`
    FOREIGN KEY (`registry_id`) REFERENCES `registry` (`id`)
    ON UPDATE CASCADE
    ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `ind` (
  `id` varchar(63) NOT NULL,
  `dg_id` varchar(63) DEFAULT NULL,
  `include` tinyint(1) DEFAULT 1,
  `title` varchar(255) DEFAULT NULL,
  `name` varchar(255) DEFAULT NULL,
  `type` varchar(127) DEFAULT NULL,
  `sformat` varchar(31) DEFAULT ',.0%',
  `measure_unit` varchar(127) DEFAULT NULL,
  `min_denominator` tinyint(3) unsigned DEFAULT NULL,
  `min_value` double(7,3) DEFAULT NULL,
  `max_value` double(7,3) DEFAULT NULL,
  `level_green` double(7,3) DEFAULT NULL,
  `level_yellow` double(7,3) DEFAULT NULL,
  `level_direction` tinyint(4) DEFAULT NULL,
  `short_description` varchar(1023) DEFAULT NULL,
  `long_description` varchar(2047) DEFAULT NULL,
  `registry_id` smallint(5) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  KEY `fk_indicator_registry` (`registry_id`),
  KEY `dg_id` (`dg_id`),
  CONSTRAINT `fk_indicator_registry`
    FOREIGN KEY (`registry_id`) REFERENCES `registry` (`id`)
    ON UPDATE CASCADE,
  CONSTRAINT `ind_ibfk_1`
    FOREIGN KEY (`dg_id`) REFERENCES `ind` (`id`)
    ON DELETE SET NULL
    ON UPDATE CASCADE,
  CONSTRAINT `inconsistent_level_values`
    CHECK (`level_direction` = 1 and `level_green` >= `level_yellow` or
           `level_direction` = 0 and `level_green` <= `level_yellow`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `user` (
  `id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
  `user_name` varchar(255) NOT NULL,
  `name` varchar(255) NOT NULL,
  `phone` varchar(15) DEFAULT NULL,
  `email` varchar(255) NOT NULL,
  `valid` tinyint(1) NOT NULL DEFAULT 1,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `user_registry` (
  `id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
  `user_id` smallint(5) unsigned NOT NULL,
  `registry_id` smallint(5) unsigned NOT NULL,
  `role` varchar(4) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `fk_user_registry_registry` (`registry_id`),
  KEY `fk_user_registry_user` (`user_id`),
  CONSTRAINT `fk_user_registry_registry`
    FOREIGN KEY (`registry_id`) REFERENCES `registry` (`id`)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  CONSTRAINT `fk_user_registry_user`
    FOREIGN KEY (`user_id`) REFERENCES `user` (`id`)
    ON UPDATE CASCADE
    ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `publish` (
  `id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
  `time` timestamp NOT NULL DEFAULT current_timestamp(),
  `md5_checksum` char(32) NOT NULL,
  `terms_version` varchar(127) DEFAULT NULL,
  `user_id` smallint(5) unsigned NOT NULL,
  `registry_id` smallint(5) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  KEY `fk_publish_user` (`user_id`),
  KEY `fk_publish_registry` (`registry_id`),
  CONSTRAINT `fk_publish_registry`
    FOREIGN KEY (`registry_id`) REFERENCES `registry` (`id`)
    ON UPDATE CASCADE,
  CONSTRAINT `fk_publish_user`
    FOREIGN KEY (`user_id`) REFERENCES `user` (`id`)
    ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `delivery` (
  `id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
  `latest` tinyint(1) DEFAULT NULL,
  `time` timestamp NOT NULL DEFAULT current_timestamp(),
  `latest_update` date DEFAULT NULL,
  `latest_affirm` date DEFAULT NULL,
  `md5_checksum` char(32) NOT NULL,
  `terms_version` varchar(127) DEFAULT NULL,
  `user_id` smallint(5) unsigned NOT NULL,
  `publish_id` smallint(5) unsigned DEFAULT NULL,
  `published` tinyint(1) NOT NULL DEFAULT 0,
  PRIMARY KEY (`id`),
  KEY `fk_delivery_user` (`user_id`),
  KEY `fk_delivery_publish` (`publish_id`),
  CONSTRAINT `fk_delivery_publish`
    FOREIGN KEY (`publish_id`) REFERENCES `publish` (`id`)
    ON UPDATE CASCADE,
  CONSTRAINT `fk_delivery_user`
    FOREIGN KEY (`user_id`) REFERENCES `user` (`id`)
    ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `data` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `delivery_id` smallint(5) unsigned NOT NULL,
  `context` varchar(127) NOT NULL DEFAULT 'caregiver',
  `unit_level` varchar(15) NOT NULL,
  `orgnr` int(10) unsigned NOT NULL,
  `year` smallint(5) unsigned NOT NULL,
  `var` double(9,3) NOT NULL,
  `denominator` int(10) unsigned NOT NULL DEFAULT 1,
  `ind_id` varchar(63) NOT NULL,
  PRIMARY KEY (`id`),
  KEY `fk_data_delivery` (`delivery_id`),
  KEY `fk_data_ind` (`ind_id`),
  CONSTRAINT `fk_data_delivery`
    FOREIGN KEY (`delivery_id`) REFERENCES `delivery` (`id`)
    ON DELETE CASCADE,
  CONSTRAINT `fk_data_ind`
    FOREIGN KEY (`ind_id`) REFERENCES `ind` (`id`)
    ON UPDATE CASCADE
    ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `agg_data` (
  `id` mediumint(8) unsigned NOT NULL AUTO_INCREMENT,
  `ind_id` varchar(63) NOT NULL,
  `unit_level` varchar(15) NOT NULL,
  `unit_name` varchar(255) NOT NULL,
  `context` varchar(127) NOT NULL DEFAULT 'caregiver',
  `orgnr` int(10) unsigned NOT NULL,
  `year` smallint(5) unsigned NOT NULL,
  `denominator` int(10) unsigned NOT NULL,
  `var` double(9,5) unsigned NOT NULL,
  `dg` double(6,5) unsigned DEFAULT NULL,
  `delivery_time` timestamp NULL DEFAULT NULL,
  `delivery_latest_update` date DEFAULT NULL,
  `delivery_latest_affirm` date DEFAULT NULL,
  `time` timestamp NOT NULL DEFAULT current_timestamp(),
  PRIMARY KEY (`id`),
  KEY `year` (`year`),
  KEY `ind_id` (`ind_id`),
  KEY `unit_level` (`unit_level`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `notice` (
  `id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
  `text` mediumtext NOT NULL,
  `status` tinytext NOT NULL,
  `ref` tinytext,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `notice_event` (
  `id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
  `text` mediumtext NOT NULL,
  `type` tinytext NOT NULL,
  `ref` tinytext,
  `notice_id` smallint(5) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  CONSTRAINT `fk_notice_event_notice`
    FOREIGN KEY (`notice_id`) REFERENCES `notice` (`id`)
    ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `evaluation` (
  `registry_id` smallint(5) unsigned NOT NULL,
  `user_id` smallint(5) unsigned NOT NULL,
  `time` timestamp NOT NULL DEFAULT current_timestamp(),
  `reported_dg` tinyint(5) NOT NULL,
  `year` smallint(5) NOT NULL,
  `verdict` char(2) NOT NULL,
  `requirement_1` tinyint(1) NOT NULL,
  `requirement_2` tinyint(1) NOT NULL,
  `requirement_3` tinyint(1) NOT NULL,
  `requirement_4` tinyint(1) NOT NULL,
  `requirement_5` tinyint(1) NOT NULL,
  `requirement_6` tinyint(1) NOT NULL,
  `requirement_7` tinyint(1) NOT NULL,
  `requirement_8` tinyint(1) NOT NULL,
  `requirement_9` tinyint(1) NOT NULL,
  `requirement_10` tinyint(1) NOT NULL,
  `requirement_11` tinyint(1) NOT NULL,
  `requirement_12` tinyint(1) NOT NULL,
  `requirement_13` tinyint(1) NOT NULL,
  `requirement_14` tinyint(1) NOT NULL,
  `requirement_15` tinyint(1) NOT NULL,
  `requirement_16` tinyint(1) NOT NULL,
  `requirement_17` tinyint(1) NOT NULL,
  `requirement_18` tinyint(1) NOT NULL,
  `evaluation_text` mediumtext NOT NULL,
  `level_A_comment` mediumtext NOT NULL,
  `level_B_comment` mediumtext NOT NULL,
  UNIQUE KEY `registry_id_year` (`registry_id`,`year`),
  KEY `fk_evaluation_user` (`user_id`),
  CONSTRAINT `fk_evaluation_registry`
    FOREIGN KEY (`registry_id`) REFERENCES `registry` (`id`)
    ON UPDATE CASCADE,
  CONSTRAINT `fk_evaluation_user`
    FOREIGN KEY (`user_id`) REFERENCES `user` (`id`)
    ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `requirements` (
  `id` tinyint(5) NOT NULL,
  `stage_or_level` char(1) NOT NULL,
  `criteria` mediumtext NOT NULL,
  `guide` mediumtext NOT NULL,
  `section` tinytext NOT NULL,
  `introduction_year` smallint(5) NOT NULL,
  `last_year` smallint(5) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `unit_ind` (
  `id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
  `hospital_short_name` varchar(255),
  `hf_short_name` varchar(255),
  `rhf_short_name` varchar(255),
  `ind_id` varchar(63) NOT NULL,
  PRIMARY KEY (`id`),
  CONSTRAINT `fk_unit_ind_ind_id`
    FOREIGN KEY (`ind_id`) REFERENCES `ind` (`id`)
    ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `project` (
  `id` varchar(63) NOT NULL,
  `registry_id` smallint(5) unsigned NOT NULL,
  `start_year` smallint(5) NOT NULL,
  `end_year` smallint(5),
  `title` varchar(255) DEFAULT NULL,
  `short_description` varchar(1023) DEFAULT NULL,
  `long_description` varchar(2047) DEFAULT NULL,
  PRIMARY KEY (`id`),
  CONSTRAINT `fk_projects_registry`
    FOREIGN KEY (`registry_id`) REFERENCES `registry` (`id`)
    ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `project_hospital` (
  `id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
  `project_id` varchar(63) NOT NULL,
  `hospital_short_name` varchar(255) NOT NULL,
  PRIMARY KEY (`id`),
  CONSTRAINT `fk_project_hospital_project`
    FOREIGN KEY (`project_id`) REFERENCES `project` (`id`)
    ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `project_ind` (
  `id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
  `project_id` varchar(63) NOT NULL,
  `ind_id` varchar(63) NOT NULL,
  PRIMARY KEY (`id`),
  CONSTRAINT `fk_project_ind_project`
    FOREIGN KEY (`project_id`) REFERENCES `project` (`id`)
    ON UPDATE CASCADE,
  CONSTRAINT `fk_project_ind`
    FOREIGN KEY (`ind_id`) REFERENCES `ind` (`id`)
    ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;

CREATE TABLE IF NOT EXISTS `publication` (
  `id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
  `doi` varchar(255) NOT NULL,
  `registry_id` smallint(5) unsigned NOT NULL,
  `time` timestamp NOT NULL DEFAULT current_timestamp(),
  `user_id` smallint unsigned NOT NULL,
  `reference` varchar(1023) NOT NULL,
  PRIMARY KEY (`id`),
  CONSTRAINT `fk_publication_registry`
    FOREIGN KEY (`registry_id`) REFERENCES `registry` (`id`)
    ON UPDATE CASCADE,
  CONSTRAINT `fk_publication_user`
    FOREIGN KEY (`user_id`) REFERENCES `user` (`id`)
    ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;
