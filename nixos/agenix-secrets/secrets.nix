let
  me = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICzNy6bOnkcu54nPeN523uvfRq3WbGCgEbTQifWLF+D0";
  honk = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINe+P1f04b+D26xdIguaJaSqnHgIwwfbEn3vFTPu74+W";
  puny = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGjIGzKJTTm/65YzV3q9dHKKGZWjj8l6+C1WWVCZpanu";

in {
  "synapse-secrets-config.yaml.age".publicKeys = [ me honk ];
  "synapse-signing-key.age".publicKeys = [ me honk ];
  "invoiceplane-db-passfile.age".publicKeys = [ me puny ];
  "snowdrift-gitlab-token.age".publicKeys = [ me honk ];
}
