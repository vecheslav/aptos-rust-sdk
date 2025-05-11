use hex::FromHex;
use serde::de::Error;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::str::FromStr;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum AccountAddressParseError {
    #[error("Account address is too long: {0}")]
    TooLong(String),

    #[error("Account address is too short: {0}")]
    TooShort(String),

    #[error("Account address contains a non-hex character: {0}")]
    NonHexCharacter(String),

    #[error("Account address must start with 0x")]
    LeadingZeroXRequired,

    #[error("Account address contains invalid hex characters: {0}")]
    InvalidHexChars(String),
}

/// Represents an Aptos AccountAddress
///
/// An [`AccountAddress`] is underneath just a fixed 32 byte length.
#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Clone, Copy)]
pub struct AccountAddress([u8; AccountAddress::LENGTH]);

impl AccountAddress {
    /// The number of bytes in an address.
    pub const LENGTH: usize = 32;

    /// Hex address: 0x0
    pub const ZERO: Self = Self([0x00; Self::LENGTH]);
    /// Hex address: 0x1
    pub const ONE: Self = Self::get_byte_address(1);
    /// Hex address: 0x2
    pub const TWO: Self = Self::get_byte_address(2);
    /// Hex address: 0x3
    pub const THREE: Self = Self::get_byte_address(3);
    /// Hex address: 0x4
    pub const FOUR: Self = Self::get_byte_address(4);
    /// Max address: 0xff....
    pub const MAX_ADDRESS: Self = Self([0xFF; Self::LENGTH]);

    /// Creates an address form the raw bytes
    pub const fn new(address: [u8; Self::LENGTH]) -> Self {
        Self(address)
    }

    /// Helper function to create static single byte addresses
    const fn get_byte_address(byte: u8) -> Self {
        let mut addr = [0u8; AccountAddress::LENGTH];
        addr[AccountAddress::LENGTH - 1] = byte;
        Self(addr)
    }

    pub const fn to_bytes(&self) -> &[u8; Self::LENGTH] {
        &self.0
    }

    pub fn to_vec(&self) -> Vec<u8> {
        self.0.to_vec()
    }

    pub fn from_hex<T: AsRef<[u8]>>(hex: T) -> Result<Self, AccountAddressParseError> {
        <[u8; Self::LENGTH]>::from_hex(hex)
            .map_err(|e| AccountAddressParseError::InvalidHexChars(format!("{:#}", e)))
            .map(Self)
    }
}

impl Debug for AccountAddress {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self) // Default to use the Display implementation
    }
}

impl Display for AccountAddress {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:#x}", self)
    }
}

impl FromStr for AccountAddress {
    type Err = AccountAddressParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        const NUM_CHARS: usize = AccountAddress::LENGTH * 2;
        let mut has_0x = false;
        let mut working = s.trim();

        // Checks if it has a 0x at the beginning, which is okay
        if working.starts_with("0x") {
            has_0x = true;
            working = &working[2..];
        }

        if working.len() > NUM_CHARS {
            return Err(AccountAddressParseError::TooLong(s.to_string()));
        } else if !has_0x && working.len() < NUM_CHARS {
            return Err(AccountAddressParseError::TooShort(s.to_string()));
        }

        if !working.chars().all(|c| char::is_ascii_hexdigit(&c)) {
            return Err(AccountAddressParseError::NonHexCharacter(s.to_string()));
        }

        let account_address = if has_0x {
            let literal = s.trim();
            if !literal.starts_with("0x") {
                return Err(AccountAddressParseError::LeadingZeroXRequired);
            }

            let hex_len = literal.len() - 2;

            // If the string is too short, pad it
            if hex_len < Self::LENGTH * 2 {
                let mut hex_str = String::with_capacity(Self::LENGTH * 2);
                for _ in 0..Self::LENGTH * 2 - hex_len {
                    hex_str.push('0');
                }
                hex_str.push_str(&literal[2..]);
                AccountAddress::from_hex(hex_str)
            } else {
                AccountAddress::from_hex(&literal[2..])
            }
        } else {
            AccountAddress::from_str(s.trim())
        }?;

        Ok(account_address.into())
    }
}

impl fmt::LowerHex for AccountAddress {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "0x")?;
        }

        for byte in &self.0 {
            write!(f, "{:02x}", byte)?;
        }

        Ok(())
    }
}

impl fmt::UpperHex for AccountAddress {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "0x")?;
        }

        for byte in &self.0 {
            write!(f, "{:02X}", byte)?;
        }

        Ok(())
    }
}

impl<'de> Deserialize<'de> for AccountAddress {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        if deserializer.is_human_readable() {
            let s = <String>::deserialize(deserializer)?;
            AccountAddress::from_str(&s).map_err(D::Error::custom)
        } else {
            // In order to preserve the Serde data model and help analysis tools,
            // make sure to wrap our value in a container with the same name
            // as the original type.
            #[derive(::serde::Deserialize)]
            #[serde(rename = "AccountAddress")]
            struct Value([u8; AccountAddress::LENGTH]);

            let value = Value::deserialize(deserializer)?;
            Ok(AccountAddress::new(value.0))
        }
    }
}

impl Serialize for AccountAddress {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if serializer.is_human_readable() {
            // TODO: We differ from the aptos core representation, by appending 0x (does it matter as it should be parsed)
            self.to_string().serialize(serializer)
        } else {
            // See comment in deserialize.
            serializer.serialize_newtype_struct("AccountAddress", &self.0)
        }
    }
}
