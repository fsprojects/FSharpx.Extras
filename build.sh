export MONO_PATH=./packages/FAKE.1.64.5/tools/:$MONO_PATH
export MONO_IOMAP=all
mono packages/FAKE.1.64.5/tools/FAKE.exe build.fsx