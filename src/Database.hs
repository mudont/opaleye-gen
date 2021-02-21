{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Database where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text
import Opaleye
import Prelude hiding (sum)

data InformationSchemaColumn' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 = InformationSchemaColumn
  { table_catalog :: c1,
    table_schema :: c2,
    table_name :: c3,
    column_name :: c4,
    ordinal_position :: c5,
    column_default :: c6,
    is_nullable :: c7,
    data_type :: c8,
    character_maximum_length :: c9,
    character_octet_length :: c10,
    numeric_precision :: c11,
    numeric_precision_radix :: c12,
    numeric_scale :: c13,
    datetime_precision :: c14,
    interval_type :: c15,
    interval_precision :: c16,
    character_set_catalog :: c17,
    character_set_schema :: c18,
    character_set_name :: c19,
    collation_catalog :: c20,
    collation_schema :: c21,
    collation_name :: c22,
    domain_catalog :: c23,
    domain_schema :: c24,
    domain_name :: c25,
    udt_catalog :: c26,
    udt_schema :: c27,
    udt_name :: c28,
    scope_catalog :: c29,
    scope_schema :: c30,
    scope_name :: c31,
    maximum_cardinality :: c32,
    dtd_identifier :: c33,
    is_self_referencing :: c34,
    is_identity :: c35,
    identity_generation :: c36,
    identity_start :: c37,
    identity_increment :: c38,
    identity_maximum :: c39,
    identity_minimum :: c40,
    identity_cycle :: c41,
    is_generated :: c42,
    generation_expression :: c43,
    is_updatable :: c44
  }

type InformationSchemaColumn =
  InformationSchemaColumn'
    (Maybe Text)
    (Maybe Text)
    Text
    Text
    Int
    (Maybe Text)
    Text
    (Maybe Text)
    (Maybe Int)
    (Maybe Int)
    (Maybe Int)
    (Maybe Int)
    (Maybe Int)
    (Maybe Int)
    (Maybe Text)
    (Maybe Int)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    Text
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    (Maybe Int)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)
    (Maybe Text)

type InformationSchemaColumnTable =
  InformationSchemaColumn'
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column PGText)
    (Column PGText)
    (Column PGInt4)
    (Column (Nullable PGText))
    (Column PGText)
    (Column (Nullable PGText))
    (Column (Nullable PGInt4))
    (Column (Nullable PGInt4))
    (Column (Nullable PGInt4))
    (Column (Nullable PGInt4))
    (Column (Nullable PGInt4))
    (Column (Nullable PGInt4))
    (Column (Nullable PGText))
    (Column (Nullable PGInt4))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column PGText)
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGInt4))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))
    (Column (Nullable PGText))

$(makeAdaptorAndInstance "pInformationSchemaColumn" ''InformationSchemaColumn')

informationSchemaColumnTable :: Table InformationSchemaColumnTable InformationSchemaColumnTable
informationSchemaColumnTable =
  TableWithSchema
    "information_schema"
    "columns"
    ( pInformationSchemaColumn
        InformationSchemaColumn
          { table_catalog = requiredTableField "table_catalog",
            table_schema = requiredTableField "table_schema",
            table_name = requiredTableField "table_name",
            column_name = requiredTableField "column_name",
            ordinal_position = requiredTableField "ordinal_position",
            column_default = requiredTableField "column_default",
            is_nullable = requiredTableField "is_nullable",
            data_type = requiredTableField "data_type",
            character_maximum_length = requiredTableField "character_maximum_length",
            character_octet_length = requiredTableField "character_octet_length",
            numeric_precision = requiredTableField "numeric_precision",
            numeric_precision_radix = requiredTableField "numeric_precision_radix",
            numeric_scale = requiredTableField "numeric_scale",
            datetime_precision = requiredTableField "datetime_precision",
            interval_type = requiredTableField "interval_type",
            interval_precision = requiredTableField "interval_precision",
            character_set_catalog = requiredTableField "character_set_catalog",
            character_set_schema = requiredTableField "character_set_schema",
            character_set_name = requiredTableField "character_set_name",
            collation_catalog = requiredTableField "collation_catalog",
            collation_schema = requiredTableField "collation_schema",
            collation_name = requiredTableField "collation_name",
            domain_catalog = requiredTableField "domain_catalog",
            domain_schema = requiredTableField "domain_schema",
            domain_name = requiredTableField "domain_name",
            udt_catalog = requiredTableField "udt_catalog",
            udt_schema = requiredTableField "udt_schema",
            udt_name = requiredTableField "udt_name",
            scope_catalog = requiredTableField "scope_catalog",
            scope_schema = requiredTableField "scope_schema",
            scope_name = requiredTableField "scope_name",
            maximum_cardinality = requiredTableField "maximum_cardinality",
            dtd_identifier = requiredTableField "dtd_identifier",
            is_self_referencing = requiredTableField "is_self_referencing",
            is_identity = requiredTableField "is_identity",
            identity_generation = requiredTableField "identity_generation",
            identity_start = requiredTableField "identity_start",
            identity_increment = requiredTableField "identity_increment",
            identity_maximum = requiredTableField "identity_maximum",
            identity_minimum = requiredTableField "identity_minimum",
            identity_cycle = requiredTableField "identity_cycle",
            is_generated = requiredTableField "is_generated",
            generation_expression = requiredTableField "generation_expression",
            is_updatable = requiredTableField "is_updatable"
          }
    )
