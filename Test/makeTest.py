"""execute main in MainTest.hs, must be run in the Test directory
"""
import os

def main():
    os.chdir("G:/FishShit/FISH/Test")
    os.system("ghci MainTest.hs -e main -iG:/FishShit/FISH/app")
    input("\n\nPress any key to continue:\n")

if __name__ == "__main__":
    main()