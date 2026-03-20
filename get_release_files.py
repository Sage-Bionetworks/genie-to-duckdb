#!/usr/bin/env python3
"""
Download core data files from every GENIE release on Synapse (syn7492881).

Directory layout:
    data/<release_version>/<filename>

Skipped:
    - case_lists/ subfolders
    - data_gene_panel_*.txt
    - meta_*.txt
    - *.pdf, *.html
    - *.csv (QC/audit files like duplicated_variants, non_somatic, etc.)
"""

import re
import sys
import argparse
from pathlib import Path

import synapseclient
from synapseclient import Synapse

RELEASES_SYN_ID = "syn7492881"

SKIP_PATTERNS = [
    re.compile(r"^data_gene_panel_[A-Z]"),  # individual panel definitions, not the sample-panel matrix
    re.compile(r"^meta_"),
    re.compile(r"\.pdf$"),
    re.compile(r"\.html$"),
    re.compile(r"\.csv$"),
]

def is_relevant(name: str) -> bool:
    return not any(p.search(name) for p in SKIP_PATTERNS)


def get_children(syn: Synapse, parent_id: str):
    return list(syn.getChildren(parent_id))


def human_size(n_bytes: int) -> str:
    for unit in ("B", "KB", "MB", "GB", "TB"):
        if n_bytes < 1024:
            return f"{n_bytes:.1f} {unit}"
        n_bytes /= 1024
    return f"{n_bytes:.1f} PB"


def download_release(syn: Synapse, version_name: str, version_syn_id: str, output_dir: Path, dry_run: bool):
    release_dir = output_dir / version_name
    children = get_children(syn, version_syn_id)

    files_downloaded = 0
    bytes_to_download = 0
    for child in children:
        name = child["name"]
        child_type = child["type"]

        # skip subfolders (case_lists, etc.)
        if child_type == "org.sagebionetworks.repo.model.Folder":
            print(f"  [skip folder] {name}/")
            continue

        if not is_relevant(name):
            print(f"  [skip file]   {name}")
            continue

        dest = release_dir / name
        if dest.exists():
            size = dest.stat().st_size
            print(f"  [exists]      {name} ({human_size(size)})")
            files_downloaded += 1
            continue

        if dry_run:
            entity = syn.get(child["id"], downloadFile=False)
            fh = getattr(entity, "_file_handle", None) or getattr(entity, "fileHandle", None) or {}
            size = fh.get("contentSize", 0) if isinstance(fh, dict) else 0
            bytes_to_download += size
            size_str = human_size(size) if size else "unknown size"
            print(f"  [download]    {name} ({size_str})")
        else:
            print(f"  [download]    {name}")
            release_dir.mkdir(parents=True, exist_ok=True)
            syn.get(child["id"], downloadLocation=str(release_dir), ifcollision="overwrite.local")
        files_downloaded += 1

    return files_downloaded, bytes_to_download


def main():
    parser = argparse.ArgumentParser(description="Download GENIE release files from Synapse.")
    parser.add_argument("--output-dir", default="data", help="Root directory for downloads (default: ./data)")
    parser.add_argument("--dry-run", action="store_true", help="Print what would be downloaded without downloading")
    parser.add_argument("--releases", nargs="*", help="Limit to specific release versions, e.g. 14.0-public 15.1-consortium")
    args = parser.parse_args()

    output_dir = Path(args.output_dir)

    syn = synapseclient.login(silent=True)

    print(f"Listing release groups under {RELEASES_SYN_ID}...")
    release_groups = get_children(syn, RELEASES_SYN_ID)  # e.g. "Release 00", "Release 01", ...

    total_files = 0
    total_bytes = 0
    for group in release_groups:
        if group["type"] != "org.sagebionetworks.repo.model.Folder":
            continue

        versions = get_children(syn, group["id"])  # e.g. "0.1.0", "1.0.1", "14.0-public", ...
        for version in versions:
            if version["type"] != "org.sagebionetworks.repo.model.Folder":
                continue

            version_name = version["name"]
            if args.releases and version_name not in args.releases:
                continue

            print(f"\n=== {version_name} ({version['id']}) ===")
            n, b = download_release(syn, version_name, version["id"], output_dir, args.dry_run)
            total_files += n
            total_bytes += b

    if args.dry_run:
        print(f"\nDry run complete: {total_files} files, {human_size(total_bytes)} to download.")
    else:
        print(f"\nDone. Downloaded {total_files} files.")


if __name__ == "__main__":
    main()
