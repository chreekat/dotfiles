let
  me = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICzNy6bOnkcu54nPeN523uvfRq3WbGCgEbTQifWLF+D0";
  honk = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINe+P1f04b+D26xdIguaJaSqnHgIwwfbEn3vFTPu74+W";

in {
  "synapse-secrets-config.yaml.age".publicKeys = [ me honk ];
  "synapse-signing-key.age".publicKeys = [ me honk ];
}
