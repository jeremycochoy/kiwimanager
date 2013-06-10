module Types where

data RegisterFailure = UnknownRegisterError
  | UsernameMissing
  | PasswordMissing
  | UsernameUsed
  | EmailMissing
  | UsernameIllformed
  | EmailIllformed
  | UsernameTooShort
  | UsernameTooLong
  | PasswordTooShort
  | PasswordNotConfirmed

data LoginFailure = UnknownLoginError
