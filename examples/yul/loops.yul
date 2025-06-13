object "loops" {
    code {
        {
            let _1 := datasize("runtime")
            datacopy(0, dataoffset("runtime"), _1)
            return(0, _1)
        }
    }
    object "runtime" {
        code {
            {
                switch shr(224, calldataload(0))
                case 0x83714834 {
                    let fact := 1
                    let k := calldataload(4)
                    for { } gt(k, 1) { k := add(k, not(0)) }
                    { fact := mul(k, fact) }
                    mstore(0, fact)
                    return(0, 32)
                }
                default { revert(0, 0) }
            }
        }
    }
}