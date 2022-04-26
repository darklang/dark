# kill running services
sudo pkill -f "BwdServer" || true
sudo pkill -f "ApiServer" || true

# clear things
rm -rf ./fsharp-backend/Build
rm -rf ./backend/static/blazor
./scripts/build/clear-dotnet-build

# compile
./scripts/build/compile-project fsharp-backend --optimize

du -sh ./backend/static/blazor
du -sh ./fsharp-backend/Build

# run
./scripts/run-fsharp-server --published