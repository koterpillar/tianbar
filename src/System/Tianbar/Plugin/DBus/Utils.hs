module System.Tianbar.Plugin.DBus.Utils where

import Data.Int
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T

import DBus


unwrapVariants :: (IsValue v, Functor f, IsVariant (f v)) => Proxy v -> f Variant -> Variant
unwrapVariants typ = toVariant . fmap ((`asProxyTypeOf` typ) . fromJust . fromVariant)

unwrapVariantsType :: (Functor f, IsVariant (f Bool), IsVariant (f Double), IsVariant (f Int64), IsVariant (f T.Text), IsVariant (f ObjectPath), IsVariant (f Variant)) =>
                      Type -> f Variant -> Variant
unwrapVariantsType TypeBoolean = unwrapVariants (Proxy :: Proxy Bool)
unwrapVariantsType TypeDouble = unwrapVariants (Proxy :: Proxy Double)
unwrapVariantsType TypeInt64 = unwrapVariants (Proxy :: Proxy Int64)
unwrapVariantsType TypeString = unwrapVariants (Proxy :: Proxy T.Text)
unwrapVariantsType TypeObjectPath = unwrapVariants (Proxy :: Proxy ObjectPath)
unwrapVariantsType TypeVariant = unwrapVariants (Proxy :: Proxy Variant)
unwrapVariantsType typ = error $ show typ ++ " is not a simple variant type"
