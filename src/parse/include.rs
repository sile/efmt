use crate::items::forms::IncludeDirective;
use crate::items::Module;
use crate::parse::token_stream::{MacroDefine, MacroDefineKey, MacroDefines};
use crate::parse::TokenStream;
use erl_tokenize::Tokenizer;
use std::collections::{BTreeMap, HashSet};
use std::path::{Path, PathBuf};
use std::time::SystemTime;

const CACHE_FORMAT_VERISON: &str = "v0";

#[derive(Debug, Default, Clone)]
pub struct IncludeOptions {
    disable_include: bool,
    include_dirs: Vec<PathBuf>,
    include_cache_dir: Option<PathBuf>, // `None` means the include cache is disabled.
}

impl IncludeOptions {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn disable_include(mut self) -> Self {
        self.disable_include = true;
        self
    }

    pub fn include_dirs(mut self, dirs: Vec<PathBuf>) -> Self {
        self.include_dirs = dirs;
        self
    }

    pub fn include_cache_dir(mut self, dir: PathBuf) -> Self {
        self.include_cache_dir = Some(dir);
        self
    }
}

#[derive(Debug)]
pub struct IncludeHandler {
    options: IncludeOptions,
    included: HashSet<PathBuf>,
}

impl IncludeHandler {
    pub fn new(options: IncludeOptions) -> Self {
        Self {
            options,
            included: HashSet::new(),
        }
    }

    fn cache_path(&self, include: &IncludeDirective) -> Option<PathBuf> {
        let cache_root_dir = if let Some(dir) = &self.options.include_cache_dir {
            dir
        } else {
            return None;
        };
        let key = if let Some(path) = include.var_substituted_path().to_str() {
            sha256::digest(path)
        } else {
            return None;
        };

        let cache_path = cache_root_dir.join(CACHE_FORMAT_VERISON).join(key);
        Some(cache_path)
    }

    fn try_load_macro_defines_from_cache(
        &mut self,
        include: &IncludeDirective,
    ) -> Option<MacroDefines> {
        let cache_path = self.cache_path(include)?;
        if !cache_path.exists() {
            log::debug!(
                "Include cache {:?} (for {:?}) does not exist.",
                cache_path,
                include.path()
            );
            return None;
        }

        let cache_data = match std::fs::read_to_string(&cache_path) {
            Ok(data) => data,
            Err(e) => {
                log::warn!(
                    "Failed to read cache file {:?} for {:?}: {}",
                    cache_path,
                    include.path(),
                    e
                );
                return None;
            }
        };

        let cache_entry: CacheEntry = match serde_json::from_str(&cache_data) {
            Ok(entry) => entry,
            Err(e) => {
                log::warn!(
                    "Failed to load cache file {:?} for {:?}: {}",
                    cache_path,
                    include.path(),
                    e
                );
                return None;
            }
        };

        if !cache_entry.resolved_path.exists() {
            log::warn!(
                "Found a cache entry {:?} for a non-existing file {:?}. This entry will be deleted.",
                cache_path,
                cache_entry.resolved_path
             );
            let _ = std::fs::remove_file(&cache_path);
            return None;
        }

        match std::fs::metadata(&cache_entry.resolved_path) {
            Err(e) => {
                log::warn!(
                    "Failed to get the metadata of the include file {:?}: {}",
                    cache_entry.resolved_path,
                    e
                );
                return None;
            }
            Ok(metadata) => match metadata.modified() {
                Err(e) => {
                    log::warn!(
                        "Failed to get the modified time of the include file {:?}: {}",
                        cache_entry.resolved_path,
                        e
                    );
                    return None;
                }
                Ok(mtime) => {
                    if cache_entry.mtime < mtime {
                        log::warn!("The include file {:?} has been modified since the time it was cached. The cache entry will be deleted.",
                                   cache_entry.resolved_path);
                        let _ = std::fs::remove_file(&cache_path);
                        return None;
                    }
                }
            },
        }

        let macro_defines = cache_entry
            .macro_defines
            .into_iter()
            .map(|(name, define)| (MacroDefineKey::new(name, define.arity()), define))
            .collect();
        Some(macro_defines)
    }

    fn include_dirs<P: AsRef<Path>>(&self, target_file_path: Option<P>) -> Vec<PathBuf> {
        let mut dirs = Vec::new();
        if let Some(d) = target_file_path.as_ref().and_then(|p| p.as_ref().parent()) {
            dirs.push(d.to_path_buf());
        }
        if !self.options.include_dirs.is_empty() {
            dirs.extend(self.options.include_dirs.clone());
        } else if let Some(d) = target_file_path
            .as_ref()
            .and_then(|p| p.as_ref().parent())
            .and_then(|p| p.parent())
        {
            dirs.push(d.to_path_buf());
            dirs.push(d.join("include/"));
            dirs.push(d.join("src/"));
            dirs.push(d.join("test/"));
        }
        dirs
    }

    fn try_load_macro_defines<P: AsRef<Path>>(
        &mut self,
        target_file_path: Option<P>,
        include: &IncludeDirective,
        known_macro_defines: &MacroDefines,
    ) -> Option<MacroDefines> {
        let resolved_path =
            if let Some(path) = include.resolved_path(&self.include_dirs(target_file_path)) {
                path
            } else {
                log::warn!(
                    "Failed to resolve the include file path {:?}",
                    include.path()
                );
                return None;
            };
        log::debug!(
            "The include file {:?} was resolved to the path {:?}",
            include.path(),
            resolved_path
        );

        let text = match std::fs::read_to_string(&resolved_path) {
            Ok(text) => text,
            Err(e) => {
                log::warn!("Failed to read the include file {:?}: {}", resolved_path, e);
                return None;
            }
        };

        let mut tokenizer = Tokenizer::new(text);
        tokenizer.set_filepath(&resolved_path);
        let mut ts = TokenStream::new(tokenizer, self.options.clone());
        ts.set_known_macro_defines(known_macro_defines.clone());
        match ts.parse::<Module>() {
            Err(e) => {
                log::warn!(
                    "Failed to parse the include file {:?}: {}",
                    resolved_path,
                    e
                );
                None
            }
            Ok(_) => Some(ts.new_macro_defines()),
        }
    }

    fn try_save_macro_defines_into_cache<P: AsRef<Path>>(
        &mut self,
        target_file_path: Option<P>,
        include: &IncludeDirective,
        macro_defines: &MacroDefines,
    ) {
        let cache_path = if let Some(path) = self.cache_path(include) {
            path
        } else {
            return;
        };
        if let Some(parent) = cache_path.parent() {
            if !parent.exists() {
                if let Err(e) = std::fs::create_dir_all(parent) {
                    log::warn!(
                        "Failed to the create directory {:?} for include cache: {}",
                        parent,
                        e
                    );
                    return;
                }
            }
        }

        let resolved_path =
            if let Some(path) = include.resolved_path(&self.include_dirs(target_file_path)) {
                path
            } else {
                log::warn!(
                    "Failed to resolve the include file path {:?}",
                    include.path()
                );
                return;
            };

        let mtime = match std::fs::metadata(&resolved_path).and_then(|m| m.modified()) {
            Err(e) => {
                log::warn!(
                    "Failed to get modified time of the include file {:?}: {}",
                    resolved_path,
                    e
                );
                return;
            }
            Ok(mtime) => mtime,
        };

        let entry = CacheEntry {
            resolved_path: resolved_path.clone(),
            mtime,
            macro_defines: macro_defines
                .iter()
                .map(|(k, v)| (k.name().to_owned(), v.clone()))
                .collect(),
        };

        let mut temp = match tempfile::NamedTempFile::new() {
            Err(e) => {
                log::warn!(
                    "Failed to create a temporary cache file for {:?}: {}",
                    resolved_path,
                    e
                );
                return;
            }
            Ok(temp) => temp,
        };
        if let Err(e) = serde_json::to_writer(std::io::BufWriter::new(&mut temp), &entry) {
            log::warn!(
                "Failed to save the cache entry for {:?}: {}",
                resolved_path,
                e
            );
            return;
        }

        if let Err(e) = temp.persist(&cache_path) {
            log::warn!(
                "Failed to persist the cache entry for {:?}: {}",
                resolved_path,
                e
            );
        } else {
            log::debug!(
                "Saved a include cache for {:?} into {:?}",
                include.path(),
                cache_path
            );
        }
    }

    pub(crate) fn include_macro_defines<P: AsRef<Path>>(
        &mut self,
        target_file_path: Option<P>,
        include: &IncludeDirective,
        known_macro_defines: &MacroDefines,
    ) -> MacroDefines {
        if self.options.disable_include {
            log::debug!("Skipped processing an include directive for {:?} as `--directive-include` flag is set.",
                        include.path());
            return MacroDefines::new();
        }

        // To eliminate the overhead of running `erl` command to resolve the path (for `-include_lib(...)`),
        // we use the unresolved version of the path here.
        let unresolved_path = include.var_substituted_path();
        if self.included.contains(&unresolved_path) {
            log::debug!(
                "The include of the path {:?} was skipped as it has already been included before.",
                include.path()
            );
            return BTreeMap::new();
        }
        self.included.insert(unresolved_path);

        if let Some(macro_defines) = self.try_load_macro_defines_from_cache(include) {
            log::debug!(
                "Found {} macro definitions in {:?} (cached).",
                macro_defines.len(),
                include.path()
            );

            return macro_defines;
        }

        if let Some(macro_defines) =
            self.try_load_macro_defines(target_file_path.as_ref(), include, known_macro_defines)
        {
            log::debug!(
                "Found {} macro definitions in {:?}.",
                macro_defines.len(),
                include.path()
            );

            self.try_save_macro_defines_into_cache(target_file_path, include, &macro_defines);
            macro_defines
        } else {
            BTreeMap::new()
        }
    }
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
struct CacheEntry {
    resolved_path: PathBuf,
    mtime: SystemTime,

    // Note that the restriction of `serde_json` we cannot use `MacroDefineKey`s as keys of an object.
    // So we store `MacroDefine`s as a vec and convert them into a `MacroDefines` during the loading phase.
    macro_defines: Vec<(String, MacroDefine)>,
}
