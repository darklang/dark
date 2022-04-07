# Static assets tests

These files are used for testing static assets. They were uploaded to the
dark-static-assets google cloud bucket for the canvas test-static-assets-deploy
(which hashes to "u0n38z-mv-t0tkvnwiqyvankw6w").

They are directly accessible via HTTP at links such as
https://test-static-assets-deploy.darksa.com/u0n38z-mv-t0tkvnwiqyvankw6w/abcdef1234/index.html

## Aside
The attempt to upload them via the dev static assets server using dark-cli was not
successful, so in the end I just figured out where they were being uploaded and
uploaded them manually. The dev static assets server doesn't work because the
authentication is against the production server (and in the future we're going to
lean in on that, so it won't be fixed)