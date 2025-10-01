import unittest
from modules import color

class TestFunctionsInMain(unittest.TestCase):

    def test_styleFrBg(self):
        self.assertEqual(styleFrBg(0, 0, 0), '\x1b[0,0,0m')
        self.assertEqual(styleFrBg(1, 5, 9), '\x1b[1,5,9m')

if __name__ == '__main__':
    unittest.main()
