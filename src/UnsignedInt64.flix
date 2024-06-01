enum UnsignedInt64(Int64) with Eq

instance Order[UnsignedInt64] {
    pub def compare(u1: UnsignedInt64, u2: UnsignedInt64): Comparison = UnsignedInt64.compare(u1, u2)
}

instance ToString[UnsignedInt64] {
    pub def toString(u: UnsignedInt64): String = UnsignedInt64.toString(u)
}

instance FromString[UnsignedInt64] {
    pub def fromString(s: String): Option[UnsignedInt64] = UnsignedInt64.fromString(s)
}

instance Add[UnsignedInt64] {
    pub def add(u1: UnsignedInt64, u2: UnsignedInt64): UnsignedInt64 = {
        UnsignedInt64.map2(Add.add, u1, u2)
    }
}

instance Sub[UnsignedInt64] {
    pub def sub(u1: UnsignedInt64, u2: UnsignedInt64): UnsignedInt64 = {
        UnsignedInt64.map2(Sub.sub, u1, u2)
    }
}

instance Mul[UnsignedInt64] {
    pub def mul(u1: UnsignedInt64, u2: UnsignedInt64): UnsignedInt64 = {
        UnsignedInt64.map2(Mul.mul, u1, u2)
    }
}

instance Div[UnsignedInt64] {
    pub def div(u1: UnsignedInt64, u2: UnsignedInt64): UnsignedInt64 = {
        import static java.lang.Long.divideUnsigned(Int64, Int64): Int64 \ {};
        UnsignedInt64.map2(divideUnsigned, u1, u2)
    }
}


mod UnsignedInt64 {
    pub def coerce(u: UnsignedInt64): Int64 = {
        let UnsignedInt64.UnsignedInt64(i) = u;
        i
    }

    pub def map(f: Int64 -> Int64 \ ef, u: UnsignedInt64): UnsignedInt64 \ ef = {
        UnsignedInt64(f(coerce(u)))
    }

    pub def map2(f: (Int64, Int64) -> Int64 \ ef, u1: UnsignedInt64, u2: UnsignedInt64): UnsignedInt64 \ ef = {
        UnsignedInt64(f(coerce(u1), coerce(u2)))
    }

    pub def fromString(s: String): Option[UnsignedInt64] = {
        import static java.lang.Long.parseUnsignedLong(String): Int64 \ {};
        Result.tryCatch(() -> parseUnsignedLong(s))
        |> Result.toOption
        |> Option.map(UnsignedInt64)
    }

    pub def toString(u: UnsignedInt64): String = {
        import static java.lang.Long.toUnsignedString(Int64): String \ {};
        toUnsignedString(coerce(u))
    }

    pub def compare(u1: UnsignedInt64, u2: UnsignedInt64): Comparison = {
        import static java.lang.Long.compareUnsigned(Int64, Int64): Int32 \ {};
        compareUnsigned(coerce(u1), coerce(u2))
        |> Comparison.fromInt32
    }
}