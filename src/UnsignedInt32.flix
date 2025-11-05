import java.lang.Integer

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
        UnsignedInt32.map2((i1, i2) -> Integer.divideUnsigned(i1, i2), u1, u2)
    }
}


mod UnsignedInt32 {
    import java.lang.Integer

    pub def map(f: Int32 -> Int32 \ ef, u: UnsignedInt32): UnsignedInt32 \ ef = {
        UnsignedInt32(f(coerce(u)))
    }

    pub def map2(f: (Int32, Int32) -> Int32 \ ef, u1: UnsignedInt32, u2: UnsignedInt32): UnsignedInt32 \ ef = {
        UnsignedInt32(f(coerce(u1), coerce(u2)))
    }

    pub def fromString(s: String): Option[UnsignedInt32] = {
        Result.tryCatch(() -> Integer.parseUnsignedInt(s))
        |> Result.toOption
        |> Option.map(UnsignedInt32)
    }

    pub def toString(u: UnsignedInt32): String = {
        Integer.toUnsignedString(coerce(u))
    }

    pub def compare(u1: UnsignedInt32, u2: UnsignedInt32): Comparison = {
        Integer.compareUnsigned(coerce(u1), coerce(u2))
        |> Comparison.fromInt32
    }

    pub def toInt64(u: UnsignedInt32): Int64 = {
        let opt = u |> toString |> Int64.fromString;
        match opt {
            case None => unreachable!()
            case Some(i) => i
        }
    }

    pub def fromInt16(u: Int16): UnsignedInt32 = {
        let opt = u |> Int16.toString |> fromString;
        match opt {
            case None => unreachable!()
            case Some(i) => i
        }
    }
}