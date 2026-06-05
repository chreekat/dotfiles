"""Create an invoice from a template in Google Drive.

Copies the client's invoice template, fills in dates and the invoice label,
and downloads a PDF. The description and hours are read from
'invoicer next' (run in the contract directory).

Configuration is read from ~/.config/invoicer/config.json.
OAuth client secrets are read from ~/.config/invoicer/client_secrets.json.
"""

import json
import os
import re
import subprocess
import sys
import webbrowser

import keyring
from google.auth.transport.requests import AuthorizedSession, Request
from google.oauth2.credentials import Credentials
from google_auth_oauthlib.flow import InstalledAppFlow
from googleapiclient.discovery import build

SCOPES = [
    "https://www.googleapis.com/auth/spreadsheets",
    "https://www.googleapis.com/auth/drive",
]

CONFIG_DIR = os.path.expanduser("~/.config/invoicer")
CONFIG_PATH = os.path.join(CONFIG_DIR, "config.json")
CLIENT_SECRETS = os.path.join(CONFIG_DIR, "client_secrets.json")
KEYRING_SERVICE = "invoice-create"
KEYRING_KEY = "google-oauth-refresh-token"


def load_config():
    if not os.path.exists(CONFIG_PATH):
        print(f"Config not found: {CONFIG_PATH}", file=sys.stderr)
        print("See README for setup instructions.", file=sys.stderr)
        sys.exit(1)
    return json.load(open(CONFIG_PATH))


def get_credentials():
    """Load credentials from keyring, refreshing or re-authing as needed."""
    if not os.path.exists(CLIENT_SECRETS):
        print(f"OAuth client secrets not found: {CLIENT_SECRETS}", file=sys.stderr)
        print("Create an OAuth 2.0 Desktop client at", file=sys.stderr)
        print("https://console.cloud.google.com/apis/credentials", file=sys.stderr)
        sys.exit(1)

    refresh_token = keyring.get_password(KEYRING_SERVICE, KEYRING_KEY)

    if refresh_token:
        client_data = json.load(open(CLIENT_SECRETS))["installed"]
        creds = Credentials(
            token=None,
            refresh_token=refresh_token,
            token_uri=client_data["token_uri"],
            client_id=client_data["client_id"],
            client_secret=client_data["client_secret"],
            scopes=SCOPES,
        )
        try:
            creds.refresh(Request())
            return creds
        except Exception:
            pass

    flow = InstalledAppFlow.from_client_secrets_file(CLIENT_SECRETS, SCOPES)
    creds = flow.run_local_server(port=0)
    keyring.set_password(KEYRING_SERVICE, KEYRING_KEY, creds.refresh_token)
    return creds


def get_invoicer_output(contract):
    """Run 'invoicer next' in the contract directory."""
    contract_dir = os.path.expanduser(f"~/{contract}")
    result = subprocess.run(
        ["invoicer", "next"],
        capture_output=True,
        text=True,
        cwd=contract_dir,
    )
    if result.returncode != 0:
        print(f"invoicer failed:\n{result.stderr}", file=sys.stderr)
        sys.exit(1)
    return result.stdout


def parse_invoicer_output(output):
    """Extract the 'Ending last month' description and total hours."""
    match = re.search(
        r"(Ending last month: .+?)(?:\n\n|\Z)",
        output,
        re.DOTALL,
    )
    if not match:
        print("Could not find 'Ending last month' in invoicer output:", file=sys.stderr)
        print(output, file=sys.stderr)
        sys.exit(1)

    description = match.group(1).rstrip()

    lines = description.strip().split("\n")
    last_line = lines[-1].strip()
    hours_match = re.search(r"([\d.]+)\s*$", last_line)
    if not hours_match:
        print(f"Could not parse hours from: {last_line}", file=sys.stderr)
        sys.exit(1)

    hours = float(hours_match.group(1))
    return description, hours


def create_invoice(contract, month):
    """Create an invoice for the given contract and month (YYYY-MM)."""
    config = load_config()
    contracts = config["contracts"]

    if contract not in contracts:
        print(f"Unknown contract: {contract}", file=sys.stderr)
        print(f"Known contracts: {', '.join(contracts)}", file=sys.stderr)
        sys.exit(1)

    match = re.fullmatch(r"(\d{4})-(\d{2})", month)
    if not match:
        print(f"Invalid month format: {month} (expected YYYY-MM)", file=sys.stderr)
        sys.exit(1)

    year = int(match.group(1))
    month_num = int(match.group(2))

    contract_config = contracts[contract]
    display_name = contract_config["display_name"]
    template_id = contract_config["template_id"]
    invoices_folder_id = config["invoices_folder_id"]
    pdf_base_dir = os.path.expanduser(config["pdf_dir"])
    title = f"{display_name} Invoice {month}"

    # Get invoicer output
    print(f"Running invoicer next month in ~/{contract}...")
    output = get_invoicer_output(contract)
    description, hours = parse_invoicer_output(output)

    print(f"Description:\n{description}")
    print(f"Hours: {hours}")
    print()

    # Compute dates: invoice date = 3rd of next month, due date = 20th of next month
    if month_num == 12:
        inv_year, inv_month = year + 1, 1
    else:
        inv_year, inv_month = year, month_num + 1
    invoice_date = f"{inv_year}-{inv_month:02d}-03"
    due_date = f"{inv_year}-{inv_month:02d}-20"

    # Authenticate
    creds = get_credentials()
    sheets = build("sheets", "v4", credentials=creds)
    drive = build("drive", "v3", credentials=creds)

    # Check for existing invoice
    existing = drive.files().list(
        q=(
            f"name = '{title}'"
            f" and '{invoices_folder_id}' in parents"
            " and mimeType = 'application/vnd.google-apps.spreadsheet'"
            " and trashed = false"
        ),
        fields="files(id, name)",
    ).execute().get("files", [])

    if existing:
        print(f"Invoice '{title}' already exists in Drive.")
        answer = input("Overwrite? [y/N] ").strip().lower()
        if answer != "y":
            print("Aborted.")
            sys.exit(0)
        for f in existing:
            print(f"Deleting existing invoice {f['id']}...")
            drive.files().delete(fileId=f["id"]).execute()

    # Copy template
    print(f"Copying template to '{title}'...")
    copied = drive.files().copy(
        fileId=template_id,
        body={
            "name": title,
            "parents": [invoices_folder_id],
        },
    ).execute()
    new_id = copied["id"]

    # Write cells
    print("Filling cells...")
    sheets.spreadsheets().values().batchUpdate(
        spreadsheetId=new_id,
        body={
            "valueInputOption": "USER_ENTERED",
            "data": [
                {"range": "Sheet1!E8", "values": [[invoice_date]]},
                {"range": "Sheet1!E9", "values": [[due_date]]},
                {"range": "Sheet1!A15", "values": [[f"Invoice {month}"]]},
                {"range": "Sheet1!B18", "values": [[description]]},
                {"range": "Sheet1!C18", "values": [[hours]]},
            ],
        },
    ).execute()

    # Download as PDF
    pdf_dir = os.path.join(pdf_base_dir, str(year))
    os.makedirs(pdf_dir, exist_ok=True)
    pdf_path = os.path.join(pdf_dir, f"{title}.pdf")

    # Use the spreadsheets export URL (not drive.files().export) so we can pass
    # gridlines=false. The Drive export endpoint has no such knob and renders
    # the sheet's gridlines into the PDF.
    print(f"Downloading PDF to {pdf_path}...")
    export_url = f"https://docs.google.com/spreadsheets/d/{new_id}/export"
    resp = AuthorizedSession(creds).get(
        export_url,
        params={
            "format": "pdf",
            "gridlines": "false",
            "printtitle": "false",
            "sheetnames": "false",
            "fzr": "false",
        },
    )
    resp.raise_for_status()
    with open(pdf_path, "wb") as f:
        f.write(resp.content)

    url = f"https://docs.google.com/spreadsheets/d/{new_id}/edit"
    print()
    print(f"Invoice created: {title}")
    print(f"URL: {url}")
    print(f"PDF: {pdf_path}")

    # Open QR code URL if configured
    qr_url_template = contract_config.get("qr_url")
    if qr_url_template:
        total_raw = sheets.spreadsheets().values().get(
            spreadsheetId=new_id, range="Sheet1!E19",
        ).execute().get("values", [[""]])[0][0]
        amount = re.search(r"[\d,.]+", total_raw).group().replace(",", "")
        qr_url = qr_url_template.format(amount=amount)
        print(f"QR: {qr_url}")
        print("Opening QR code in browser...")
        webbrowser.open(qr_url)


def test_auth():
    """Test authentication by reading a spreadsheet title."""
    config = load_config()
    first_contract = next(iter(config["contracts"].values()))
    template_id = first_contract["template_id"]

    creds = get_credentials()
    sheets = build("sheets", "v4", credentials=creds)

    result = sheets.spreadsheets().get(
        spreadsheetId=template_id,
        fields="properties.title,sheets.properties.title",
    ).execute()

    print("Spreadsheet:", result["properties"]["title"])
    for s in result["sheets"]:
        print("  Sheet tab:", s["properties"]["title"])
    print("Auth OK!")


def main():
    if len(sys.argv) == 2 and sys.argv[1] == "--test-auth":
        test_auth()
    elif len(sys.argv) == 3:
        create_invoice(sys.argv[1], sys.argv[2])
    else:
        print("Usage:", file=sys.stderr)
        print("  invoice-create <contract> <YYYY-MM>", file=sys.stderr)
        print("  invoice-create --test-auth", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
