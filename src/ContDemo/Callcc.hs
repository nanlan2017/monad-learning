module ContDemo.Callcc where
import           Control.Monad.Trans.Cont


-- ◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩◩

type Data = String
type Text = String
{-
toPDF :: Data -> Cont r Text
toPDF d = callCC $ \exit -> do
    when (broken d) (exit "ERROR")
    makePDF d
-}
