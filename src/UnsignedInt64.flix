import java.lang.Long

enum UnsignedInt64(Int64) with Eq, Coerce, Hash

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
        UnsignedInt64.map2((i1, i2) -> Long.divideUnsigned(i1, i2), u1, u2)
    }
}


mod UnsignedInt64 {
    import java.lang.Long

    pub def map(f: Int64 -> Int64 \ ef, u: UnsignedInt64): UnsignedInt64 \ ef = {
        UnsignedInt64(f(coerce(u)))
    }

    pub def map2(f: (Int64, Int64) -> Int64 \ ef, u1: UnsignedInt64, u2: UnsignedInt64): UnsignedInt64 \ ef = {
        UnsignedInt64(f(coerce(u1), coerce(u2)))
    }

    pub def fromString(s: String): Option[UnsignedInt64] = {
        Result.tryCatch(() -> Long.parseUnsignedLong(s))
        |> Result.toOption
        |> Option.map(UnsignedInt64)
    }

    pub def toString(u: UnsignedInt64): String = {
        Long.toUnsignedString(coerce(u))
    }

    pub def compare(u1: UnsignedInt64, u2: UnsignedInt64): Comparison = {
        Long.compareUnsigned(coerce(u1), coerce(u2))
        |> Comparison.fromInt32
    }

    pub def toBigInt(u: UnsignedInt64): BigInt = {
        let opt = u |> toString |> BigInt.fromString;
        match opt {
            case None => unreachable!()
            case Some(i) => i
        }
    }

    pub def fromInt32(u: Int32): UnsignedInt64 = {
        let opt = u |> Int32.toString |> fromString;
        match opt {
            case None => unreachable!()
            case Some(i) => i
        }
    }
}