#!/usr/bin/env python3
"""
DOI Verification Tool for .bib Files
=====================================
Verifies DOIs in a BibLaTeX/BibTeX file against the CrossRef API.
Designed for large bibliographies with polite rate limiting and resume support.

Features:
- Validates DOI format and resolution via CrossRef API
- Cross-checks returned metadata (title, year) against bib entry
- Saves progress after each DOI (resume-safe)
- Polite rate limiting (~2 sec between requests by default)
- Detailed report output (text + CSV)

Usage:
    python verify_dois.py references.bib
    python verify_dois.py references.bib --delay 3 --email your@email.com
    python verify_dois.py references.bib --resume  # resume from last run
    python verify_dois.py references.bib --report   # just regenerate report from saved progress

Author: Built for Jeff's research workflow
"""

import re
import json
import csv
import time
import sys
import os
import argparse
from pathlib import Path
from datetime import datetime, timedelta
from difflib import SequenceMatcher
from urllib.parse import quote

try:
    import requests
except ImportError:
    print("Installing requests...")
    import subprocess
    subprocess.check_call([sys.executable, "-m", "pip", "install", "requests", "-q"])
    import requests


# ── Bib Parsing ──────────────────────────────────────────────────────────────

def parse_bib_entries(bib_path: str) -> list[dict]:
    """Parse a .bib file and extract entries with their fields."""
    with open(bib_path, 'r', encoding='utf-8') as f:
        content = f.read()

    entries = []
    i = 0
    while i < len(content):
        match = re.search(r'@(\w+)\s*\{', content[i:])
        if not match:
            break

        entry_type = match.group(1).upper()
        key_start = i + match.end()

        comma_pos = content.find(',', key_start)
        if comma_pos == -1:
            i = key_start
            continue
        cite_key = content[key_start:comma_pos].strip()

        brace_depth = 1
        j = comma_pos + 1
        while j < len(content) and brace_depth > 0:
            if content[j] == '{':
                brace_depth += 1
            elif content[j] == '}':
                brace_depth -= 1
            j += 1

        entry_body = content[comma_pos + 1:j - 1]
        i = j

        fields = {}
        field_pattern = re.compile(
            r'(\w+)\s*=\s*(?:\{((?:[^{}]|\{[^{}]*\})*)\}|(\d+))',
            re.DOTALL
        )
        for fm in field_pattern.finditer(entry_body):
            field_name = fm.group(1).lower()
            field_value = fm.group(2) if fm.group(2) is not None else fm.group(3)
            field_value = re.sub(r'\s+', ' ', field_value).strip()
            field_value = field_value.replace('{', '').replace('}', '')
            fields[field_name] = field_value

        entries.append({
            'type': entry_type,
            'key': cite_key,
            'fields': fields
        })

    return entries


# ── DOI Verification ─────────────────────────────────────────────────────────

DOI_PATTERN = re.compile(r'^10\.\d{4,}/.+$')

def validate_doi_format(doi: str) -> bool:
    return bool(DOI_PATTERN.match(doi.strip()))


def clean_doi(doi: str) -> str:
    doi = doi.strip()
    for prefix in ['https://doi.org/', 'http://doi.org/', 'https://dx.doi.org/', 'http://dx.doi.org/']:
        if doi.lower().startswith(prefix):
            doi = doi[len(prefix):]
    doi = doi.rstrip('.,;')
    return doi


def verify_doi_crossref(doi: str, email: str = None, session: requests.Session = None) -> dict:
    s = session or requests.Session()
    clean = clean_doi(doi)
    
    result = {
        'original_doi': doi,
        'cleaned_doi': clean,
        'format_valid': validate_doi_format(clean),
        'resolves': False,
        'status_code': None,
        'crossref_title': None,
        'crossref_year': None,
        'crossref_type': None,
        'error': None
    }

    if not result['format_valid']:
        result['error'] = 'Invalid DOI format'
        return result

    url = f'https://api.crossref.org/works/{quote(clean, safe="")}'
    headers = {'Accept': 'application/json'}
    if email:
        headers['User-Agent'] = f'DOI-Verifier/1.0 (mailto:{email})'

    try:
        resp = s.get(url, headers=headers, timeout=30)
        result['status_code'] = resp.status_code

        if resp.status_code == 200:
            result['resolves'] = True
            try:
                data = resp.json()
                work = data.get('message', {})
                
                titles = work.get('title', [])
                if titles:
                    result['crossref_title'] = titles[0]

                date_parts = work.get('published-print', work.get('published-online', work.get('issued', {})))
                if date_parts and 'date-parts' in date_parts:
                    parts = date_parts['date-parts']
                    if parts and parts[0] and parts[0][0]:
                        result['crossref_year'] = str(parts[0][0])

                result['crossref_type'] = work.get('type', '')
            except (json.JSONDecodeError, KeyError, IndexError):
                pass

        elif resp.status_code == 404:
            result['error'] = 'DOI not found (404)'
        elif resp.status_code == 429:
            result['error'] = 'Rate limited (429) — increase delay'
        else:
            result['error'] = f'HTTP {resp.status_code}'

    except requests.exceptions.Timeout:
        result['error'] = 'Request timeout'
    except requests.exceptions.ConnectionError:
        result['error'] = 'Connection error'
    except Exception as e:
        result['error'] = str(e)

    return result


def similarity(a: str, b: str) -> float:
    if not a or not b:
        return 0.0
    a_clean = re.sub(r'[^a-z0-9\s]', '', a.lower())
    b_clean = re.sub(r'[^a-z0-9\s]', '', b.lower())
    return SequenceMatcher(None, a_clean, b_clean).ratio()


# ── Progress Management ──────────────────────────────────────────────────────

def load_progress(progress_path: str) -> dict:
    if os.path.exists(progress_path):
        with open(progress_path, 'r') as f:
            return json.load(f)
    return {}


def save_progress(progress_path: str, progress: dict):
    with open(progress_path, 'w') as f:
        json.dump(progress, f, indent=2)


# ── Report Generation ────────────────────────────────────────────────────────

def generate_report(entries: list[dict], progress: dict, output_dir: str):
    timestamp = datetime.now().strftime('%Y-%m-%d %H:%M')
    
    valid = []
    invalid_format = []
    not_found = []
    errors = []
    title_mismatch = []
    year_mismatch = []
    no_doi = []
    
    for entry in entries:
        key = entry['key']
        doi = entry['fields'].get('doi', '')
        
        if not doi:
            no_doi.append(entry)
            continue
        
        if key not in progress:
            continue
        
        result = progress[key]
        
        if not result.get('format_valid', True):
            invalid_format.append((entry, result))
        elif not result.get('resolves', False):
            if result.get('status_code') == 404:
                not_found.append((entry, result))
            else:
                errors.append((entry, result))
        else:
            valid.append((entry, result))
            
            bib_title = entry['fields'].get('title', '')
            cr_title = result.get('crossref_title', '')
            if bib_title and cr_title and similarity(bib_title, cr_title) < 0.5:
                title_mismatch.append((entry, result))
            
            bib_date = entry['fields'].get('date', entry['fields'].get('year', ''))
            cr_year = result.get('crossref_year', '')
            if bib_date and cr_year and cr_year not in bib_date:
                year_mismatch.append((entry, result))

    # ── Text Report ──
    report_path = os.path.join(output_dir, 'doi_verification_report.txt')
    with open(report_path, 'w') as f:
        f.write("DOI VERIFICATION REPORT\n")
        f.write(f"Generated: {timestamp}\n")
        f.write("=" * 70 + "\n\n")
        
        total_with_doi = len(entries) - len(no_doi)
        checked = len(progress)
        f.write(f"Total entries:        {len(entries)}\n")
        f.write(f"Entries with DOI:     {total_with_doi}\n")
        f.write(f"Entries without DOI:  {len(no_doi)}\n")
        f.write(f"DOIs checked:         {checked}\n")
        f.write(f"DOIs valid:           {len(valid)}\n")
        f.write(f"DOIs not found (404): {len(not_found)}\n")
        f.write(f"Invalid DOI format:   {len(invalid_format)}\n")
        f.write(f"Other errors:         {len(errors)}\n")
        f.write(f"Title mismatches:     {len(title_mismatch)}\n")
        f.write(f"Year mismatches:      {len(year_mismatch)}\n")
        f.write("\n")
        
        if checked < total_with_doi:
            remaining = total_with_doi - checked
            f.write(f"⚠ {remaining} DOIs not yet checked (run with --resume to continue)\n\n")
        
        if not_found:
            f.write("\n" + "─" * 70 + "\n")
            f.write("DOIs NOT FOUND (404) — likely typos or incorrect DOIs\n")
            f.write("─" * 70 + "\n")
            for entry, result in not_found:
                f.write(f"\n  [{entry['key']}]\n")
                f.write(f"  Title: {entry['fields'].get('title', 'N/A')}\n")
                f.write(f"  DOI:   {result.get('cleaned_doi', 'N/A')}\n")
        
        if invalid_format:
            f.write("\n" + "─" * 70 + "\n")
            f.write("INVALID DOI FORMAT\n")
            f.write("─" * 70 + "\n")
            for entry, result in invalid_format:
                f.write(f"\n  [{entry['key']}]\n")
                f.write(f"  Title: {entry['fields'].get('title', 'N/A')}\n")
                f.write(f"  DOI:   {result.get('original_doi', 'N/A')}\n")
        
        if title_mismatch:
            f.write("\n" + "─" * 70 + "\n")
            f.write("TITLE MISMATCHES — DOI may point to wrong article\n")
            f.write("─" * 70 + "\n")
            for entry, result in title_mismatch:
                f.write(f"\n  [{entry['key']}]\n")
                f.write(f"  Bib title:      {entry['fields'].get('title', 'N/A')}\n")
                f.write(f"  CrossRef title:  {result.get('crossref_title', 'N/A')}\n")
                f.write(f"  DOI:             {result.get('cleaned_doi', 'N/A')}\n")
        
        if year_mismatch:
            f.write("\n" + "─" * 70 + "\n")
            f.write("YEAR MISMATCHES — may indicate wrong edition/version\n")
            f.write("─" * 70 + "\n")
            for entry, result in year_mismatch:
                f.write(f"\n  [{entry['key']}]\n")
                f.write(f"  Title:          {entry['fields'].get('title', 'N/A')}\n")
                f.write(f"  Bib date:       {entry['fields'].get('date', entry['fields'].get('year', 'N/A'))}\n")
                f.write(f"  CrossRef year:   {result.get('crossref_year', 'N/A')}\n")
                f.write(f"  DOI:             {result.get('cleaned_doi', 'N/A')}\n")
        
        if errors:
            f.write("\n" + "─" * 70 + "\n")
            f.write("OTHER ERRORS — connection issues, timeouts, etc.\n")
            f.write("─" * 70 + "\n")
            for entry, result in errors:
                f.write(f"\n  [{entry['key']}]\n")
                f.write(f"  DOI:   {result.get('cleaned_doi', 'N/A')}\n")
                f.write(f"  Error: {result.get('error', 'Unknown')}\n")
        
        f.write("\n" + "=" * 70 + "\n")
        f.write("End of report\n")
    
    # ── CSV Report ──
    csv_path = os.path.join(output_dir, 'doi_verification_results.csv')
    with open(csv_path, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerow([
            'cite_key', 'entry_type', 'bib_title', 'bib_date', 'doi',
            'format_valid', 'resolves', 'status_code',
            'crossref_title', 'crossref_year', 'title_similarity',
            'year_match', 'error'
        ])
        
        for entry in entries:
            key = entry['key']
            doi = entry['fields'].get('doi', '')
            if not doi or key not in progress:
                continue
            
            result = progress[key]
            bib_title = entry['fields'].get('title', '')
            bib_date = entry['fields'].get('date', entry['fields'].get('year', ''))
            cr_title = result.get('crossref_title', '')
            cr_year = result.get('crossref_year', '')
            
            title_sim = round(similarity(bib_title, cr_title), 2) if bib_title and cr_title else ''
            year_ok = 'yes' if cr_year and cr_year in bib_date else ('no' if cr_year and bib_date else '')
            
            writer.writerow([
                key, entry['type'], bib_title, bib_date,
                result.get('cleaned_doi', doi),
                result.get('format_valid', ''),
                result.get('resolves', ''),
                result.get('status_code', ''),
                cr_title, cr_year, title_sim, year_ok,
                result.get('error', '')
            ])
    
    return report_path, csv_path


# ── Main ─────────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(
        description='Verify DOIs in a .bib file against CrossRef API',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python verify_dois.py references.bib
  python verify_dois.py references.bib --delay 3 --email you@university.edu
  python verify_dois.py references.bib --resume
  python verify_dois.py references.bib --report
        """
    )
    parser.add_argument('bibfile', help='Path to .bib file')
    parser.add_argument('--delay', type=float, default=2.0,
                        help='Seconds between API requests (default: 2.0)')
    parser.add_argument('--email', type=str, default=None,
                        help='Email for CrossRef polite pool (faster rate limits)')
    parser.add_argument('--resume', action='store_true',
                        help='Resume from last saved progress')
    parser.add_argument('--report', action='store_true',
                        help='Only regenerate report from existing progress (no API calls)')
    parser.add_argument('--output-dir', type=str, default=None,
                        help='Output directory for reports (default: same as bib file)')
    
    args = parser.parse_args()
    
    if not os.path.exists(args.bibfile):
        print(f"Error: File not found: {args.bibfile}")
        sys.exit(1)
    
    output_dir = args.output_dir or os.path.dirname(os.path.abspath(args.bibfile))
    os.makedirs(output_dir, exist_ok=True)
    
    progress_path = os.path.join(output_dir, 'doi_verification_progress.json')
    
    print(f"Parsing {args.bibfile}...")
    entries = parse_bib_entries(args.bibfile)
    entries_with_doi = [e for e in entries if e['fields'].get('doi')]
    
    print(f"  Total entries:     {len(entries)}")
    print(f"  Entries with DOI:  {len(entries_with_doi)}")
    print(f"  Entries w/o DOI:   {len(entries) - len(entries_with_doi)}")
    
    progress = load_progress(progress_path) if (args.resume or args.report) else {}
    
    if args.report:
        print(f"\nRegenerating report from {len(progress)} saved results...")
        report_path, csv_path = generate_report(entries, progress, output_dir)
        print(f"\nReport: {report_path}")
        print(f"CSV:    {csv_path}")
        return
    
    to_check = [e for e in entries_with_doi if e['key'] not in progress]
    already_done = len(entries_with_doi) - len(to_check)
    
    if already_done > 0:
        print(f"\n  Already verified:  {already_done}")
    print(f"  Remaining:         {len(to_check)}")
    
    if not to_check:
        print("\nAll DOIs already verified! Generating report...")
        report_path, csv_path = generate_report(entries, progress, output_dir)
        print(f"\nReport: {report_path}")
        print(f"CSV:    {csv_path}")
        return
    
    est_minutes = (len(to_check) * args.delay) / 60
    est_hours = est_minutes / 60
    if est_hours > 1:
        print(f"\n  Estimated time: ~{est_hours:.1f} hours at {args.delay}s delay")
    else:
        print(f"\n  Estimated time: ~{est_minutes:.0f} minutes at {args.delay}s delay")
    
    if args.email:
        print(f"  Using polite pool with: {args.email}")
    else:
        print("  Tip: Use --email your@uni.edu for CrossRef's polite pool (faster)")
    
    print(f"\n  Progress saved to: {progress_path}")
    print(f"  You can stop anytime (Ctrl+C) and resume with --resume\n")
    
    session = requests.Session()
    
    try:
        for i, entry in enumerate(to_check):
            key = entry['key']
            doi = entry['fields']['doi']
            
            total_done = already_done + i + 1
            pct = (total_done / len(entries_with_doi)) * 100
            print(f"  [{total_done}/{len(entries_with_doi)}] ({pct:.1f}%) {key}: {clean_doi(doi)[:60]}",
                  end='', flush=True)
            
            result = verify_doi_crossref(doi, email=args.email, session=session)
            
            if result['resolves']:
                print(" ✓")
            elif result.get('status_code') == 404:
                print(" ✗ NOT FOUND")
            elif not result['format_valid']:
                print(" ⚠ BAD FORMAT")
            else:
                print(f" ⚠ {result.get('error', 'ERROR')}")
            
            progress[key] = result
            save_progress(progress_path, progress)
            
            if i < len(to_check) - 1:
                time.sleep(args.delay)
    
    except KeyboardInterrupt:
        print(f"\n\n  Interrupted! Progress saved ({len(progress)} DOIs verified).")
        print(f"  Resume with: python verify_dois.py {args.bibfile} --resume\n")
    
    print("\nGenerating report...")
    report_path, csv_path = generate_report(entries, progress, output_dir)
    print(f"\nReport: {report_path}")
    print(f"CSV:    {csv_path}")
    print("Done!")


if __name__ == '__main__':
    main()
