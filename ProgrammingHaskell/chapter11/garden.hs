module Garden where

type Gardener = String

data NormalGarden = Gardenia Gardener
                  | Daisy Gardener
                  | Rose Gardener
                  | Lilac Gardener
