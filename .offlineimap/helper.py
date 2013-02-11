from keyring import get_password

def password():
    return get_password("gmail", "bryan.richter@gmail.com")
