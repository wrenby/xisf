use std::{
    cell::RefCell,
    collections::HashSet,
    io::BufReader,
    fs::File,
    net::{Ipv4Addr, Ipv6Addr},
    path::PathBuf,
};

use error_stack::{Result, report};
use url::Host;

use crate::error::ReadDataBlockError;

#[derive(Debug)]
pub(super) enum Source {
    Monolithic(RefCell<BufReader<File>>),
    Distributed(PathBuf),
}

fn local() -> HashSet<Host> {
    HashSet::from([
        Host::Domain("localhost".into()),
        Host::Ipv4(Ipv4Addr::LOCALHOST),
        Host::Ipv6(Ipv6Addr::LOCALHOST),
    ])
}

/// Opaque storage for everything required to read a data block
///
/// - For monolithic files, it stores the original file handle,
/// - For distributed files, it stores the working directory and a list of trusted hosts
#[derive(Debug)]
pub struct Context {
    pub(super) source: Source,
    trusted_hosts: HashSet<Host>,
}
impl Context {
    pub(crate) fn distributed(path: impl Into<PathBuf>) -> Self {
        Self {
            source: Source::Distributed(path.into()),
            trusted_hosts: local(),
        }
    }

    pub(crate) fn monolithic(file: BufReader<File>) -> Self {
        Self {
            source: Source::Monolithic(RefCell::new(file)),
            trusted_hosts: local(),
        }
    }

    pub(super) fn ensure_trusted(&self, host: Host<&str>) -> Result<(), ReadDataBlockError> {
        let owned = host.to_owned();
        if self.trusted_hosts.contains(&owned) {
            Ok(())
        } else {
            Err(report!(ReadDataBlockError::UntrustedHost(owned)))
        }
    }

    /// Adds a host to the list of allowed hosts for remote data blocks
    ///
    /// localhost is trusted by default (by name and by IPv4/6 addresses)
    pub fn trust_host(&mut self, host: Host) {
        // ignore return value -- I don't care if this is already trusted
        self.trusted_hosts.insert(host);
    }
}