import sys

import dark.web
import dark.admin

if __name__ == "__main__":
  if len(sys.argv) > 1 and sys.argv[1] == "--migrate":
    dark.admin.migrate_all_graphs()
    sys.exit(0)
  else:
    d = dark.web.Server()
    d.serve()
