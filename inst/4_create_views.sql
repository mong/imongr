CREATE VIEW vw_registry_medfield AS
SELECT
  r.name AS registry_name,
  m.name AS medfield_name,
  r.full_name AS registry_full_name,
  m.full_name AS medfield_full_name
FROM
  registry_medfield rm
LEFT JOIN medfield m ON rm.medfield_id=m.id
LEFT JOIN registry r ON rm.registry_id=r.id
ORDER BY
  m.name,
  r.name;

CREATE VIEW vw_flat_organization AS
SELECT
  `hos`.`short_name` AS `hospital`,
  `hos`.`orgnr` AS `orgnr_hospital`,
  `h`.`short_name` AS `hf`,
  `h`.`orgnr` AS `hf_orgnr`,
  `r`.`short_name` AS `rhf`,
  `r`.`orgnr` AS `rhf_orgnr`,
  `n`.`short_name` AS `nation`,
  `n`.`orgnr` AS `nation_orgnr`
FROM
  (
    (
      (
        `hospital` `hos`
        LEFT JOIN `hf` `h` ON(`hos`.`hf_orgnr` = `h`.`orgnr`)
      )
      LEFT JOIN `rhf` `r` ON(`h`.`rhf_orgnr` = `r`.`orgnr`)
    )
    LEFT JOIN `nation` `n` ON(`r`.`nation_orgnr` = `n`.`orgnr`)
  )
ORDER BY
  `r`.`short_name`,
  `h`.`short_name`,
  `hos`.`short_name`;

CREATE VIEW vw_users_registries AS
SELECT
  `u`.`user_name` AS `user_name`,
  `u`.`id` AS `user_id`,
  `r`.`name` AS `name`,
  `r`.`id` AS `registry_id`
FROM
  (
    (
      `user_registry` `ur`
      LEFT JOIN `user` `u` ON(`ur`.`user_id` = `u`.`id`)
    )
    LEFT JOIN `registry` `r` ON(`ur`.`registry_id` = `r`.`id`)
  )
order by
  `u`.`id`,
  `r`.`name`;
