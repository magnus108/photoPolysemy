module Lib.Client.Widgets.Entry
    ( entry
    , userText
    ) where

import Utils.Comonad
import Utils.ListZipper
import Reactive.Threepenny
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Lib.Grade

{-----------------------------------------------------------------------------
    Input widgets
------------------------------------------------------------------------------}
-- | A single-line text entry.
data TextEntry = TextEntry
    { _elementTE :: Element
    , _userTE    :: Tidings Grades
    }

instance Widget TextEntry where getElement = _elementTE

-- | User changes to the text value.
userText :: TextEntry -> Tidings Grades
userText = _userTE

-- | Create a single-line text entry.
entry
    :: Window
    -> Behavior Grades  -- ^ Display value when the element does not have focus.
    -> UI TextEntry
entry win bValue = do -- single text entry

    grades <- currentValue bValue
    let grade = unGrade (extract (unGrades grades))

    input <- UI.input #. "input"
                    # set (attr "id") "focusGrade"
                    # set UI.type_ "text"
                    # set value grade


    bEditing <- stepper False $ and <$>
        unions [True <$ UI.focus input, False <$ UI.blur input]

    liftIOLater $ onChange bValue $ \s -> runUI win $ do
        editing <- liftIO $ currentValue bEditing
        unless editing $ void $ element input # set value (unGrade (extract (unGrades s)))

    let beh = fmap (\b i -> Grades (mapFocus (\y -> Grade i) (unGrades b))) bValue

    let _elementTE = input
        _userTE    = tidings bValue $
            beh <@> UI.valueChange input

    return TextEntry {..}
