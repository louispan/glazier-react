module Todo.Command where

-- | If state changed, then run the notifyListeners IO action
data Command = StateChangedCommand
