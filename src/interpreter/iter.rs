pub struct ConcatenatedIterator<I, J>
where
    I: Iterator,
    J: Iterator<Item = I::Item>,
{
    a: I,
    b: J,
    using_a: bool,
}

impl<I, J> Iterator for ConcatenatedIterator<I, J>
where
    I: Iterator,
    J: Iterator<Item = I::Item>,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.using_a {
            if let Some(item) = self.a.next() {
                Some(item)
            } else {
                self.using_a = false;
                self.b.next()
            }
        } else {
            self.b.next()
        }
    }
}

pub trait CanConcatenate<I, J>
where
    I: Iterator,
    J: Iterator<Item = I::Item>,
{
    fn concat(self, other: J) -> ConcatenatedIterator<I, J>;
    fn concat_single(self, other: I::Item) -> ConcatenatedIterator<I, SingleItemIterator<I::Item>>;
}

impl<I, J> CanConcatenate<I, J> for I
where
    I: Iterator,
    J: Iterator<Item = I::Item>,
{
    fn concat(self, other: J) -> ConcatenatedIterator<I, J> {
        ConcatenatedIterator {
            a: self,
            b: other,
            using_a: true,
        }
    }

    fn concat_single(self, other: I::Item) -> ConcatenatedIterator<I, SingleItemIterator<I::Item>> {
        return ConcatenatedIterator {
            a: self,
            b: SingleItemIterator::new(other),
            using_a: true,
        };
    }
}

pub struct SingleItemIterator<T> {
    item: Option<T>,
}

impl<T> SingleItemIterator<T> {
    pub fn new(item: T) -> Self {
        SingleItemIterator { item: Some(item) }
    }
}

impl<T> Iterator for SingleItemIterator<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.item.take()
    }
}
