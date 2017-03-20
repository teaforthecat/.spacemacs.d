For some reason that I could not determine, tramp and spacemacs does not work
with two-factor auth. It may have something to do with the password cache or
minibuffer focus. Without this customization, I would see "tramp: Sending
Passcode..." in the Messages/minibuffer and it would stall there. I think a nil
value was sent as the passcode, and Dell Defender responded with:

    Enter Synchronous Response: 
    <- cursor here

The cursor is on a blank line so adding "Enter Synchronous Response" to
`tramp-password-prompt-regexp` doesn't have the desired effect. The way that
`tramp-process-actions` works is that the regexp has to match the whole line. This
also means that if the line has a space at the end, the regexp has to
accommodate that. That is why `tramp-passcode-prompt` has a space at the end.
