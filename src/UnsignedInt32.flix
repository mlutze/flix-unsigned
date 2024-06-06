enum UnsignedInt32(Int32) with Eq, Coerce, Hash

instance Order[UnsignedInt32] {
    pub def compare(u1: UnsignedInt32, u2: UnsignedInt32): Comparison = UnsignedInt32.compare(u1, u2)
}

instance ToString[UnsignedInt32] {
    pub def toString(u: UnsignedInt32): String = UnsignedInt32.toString(u)
}

instance FromString[UnsignedInt32] {
    pub def fromString(s: String): Option[UnsignedInt32] = UnsignedInt32.fromString(s)
}

instance Add[UnsignedInt32] {
    pub def add(u1: UnsignedInt32, u2: UnsignedInt32): UnsignedInt32 = {
        UnsignedInt32.map2(Add.add, u1, u2)
    }
}

instance Sub[UnsignedInt32] {
    pub def sub(u1: UnsignedInt32, u2: UnsignedInt32): UnsignedInt32 = {
        UnsignedInt32.map2(Sub.sub, u1, u2)
    }
}

instance Mul[UnsignedInt32] {
    pub def mul(u1: UnsignedInt32, u2: UnsignedInt32): UnsignedInt32 = {
        UnsignedInt32.map2(Mul.mul, u1, u2)
    }
}

instance Div[UnsignedInt32] {
    pub def div(u1: UnsignedInt32, u2: UnsignedInt32): UnsignedInt32 = {
        import static java.lang.Integer.divideUnsigned(Int32, Int32): Int32 \ {};
        UnsignedInt32.map2(divideUnsigned, u1, u2)
    }
}


mod UnsignedInt32 {
    pub def map(f: Int32 -> Int32 \ ef, u: UnsignedInt32): UnsignedInt32 \ ef = {
        UnsignedInt32(f(coerce(u)))
    }

    pub def map2(f: (Int32, Int32) -> Int32 \ ef, u1: UnsignedInt32, u2: UnsignedInt32): UnsignedInt32 \ ef = {
        UnsignedInt32(f(coerce(u1), coerce(u2)))
    }

    pub def fromString(s: String): Option[UnsignedInt32] = {
        import static java.lang.Integer.parseUnsignedInt(String): Int32 \ {};
        Result.tryCatch(() -> parseUnsignedInt(s))
        |> Result.toOption
        |> Option.map(UnsignedInt32)
    }

    pub def toString(u: UnsignedInt32): String = {
        import static java.lang.Integer.toUnsignedString(Int32): String \ {};
        toUnsignedString(coerce(u))
    }

    pub def compare(u1: UnsignedInt32, u2: UnsignedInt32): Comparison = {
        import static java.lang.Integer.compareUnsigned(Int32, Int32): Int32 \ {};
        compareUnsigned(coerce(u1), coerce(u2))
        |> Comparison.fromInt32
    }
}