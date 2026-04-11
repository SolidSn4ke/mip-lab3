module Shape (Shape (..)) where

import Point

data Shape = Triangle Point Point Point | Circle Point Double