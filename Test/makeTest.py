"""Find each {test}.hs file in current directory and run {ghc -iG:\\FishShit\\FISH\\app {file}} to create a test executable
"""
import os

def main():
    os.chdir("G:/FishShit/FISH/Test")
    os.system("ghci MainTest.hs -e main -iG:/FishShit/FISH/app")
    input("\n\nPress any key to continue:\n")

if __name__ == "__main__":
    main()