#!/bin/bash
LINT_FILES=$(fd .hs)
FORMAT_FILES=$(fd .hs | grep -v 'managed-functions/src/Managed/Probe/ToProbe.hs')
stack exec hindent -- $FORMAT_FILES
stack exec hlint -- $LINT_FILES