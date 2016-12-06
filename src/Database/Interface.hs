module Database.Interface
    (
    ) where
import Opaleye (Column, Nullable, Table(Table), PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool)
import Database.PostgreSQL.Simple(connect,execute,execute_,query,query_,SqlError(..))
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Time
