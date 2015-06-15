module AB where


    /// Private implementation of the algorithm
    let private searchMinMax (depth : int) (space : SearchSpace<'a>) (root : 'a, lrt : LayerRateType) : ('a * float) option =

        /// the method used for a min-Layer
        let rec findMin d (alpha, beta) (node : 'a) =
            // find all children of current node
            let children = space.CreateChildStates node
            if List.isEmpty children then None         // if there are no children then we found Nothing
            elif d = depth                             // else if we reached max. search-depth
            then                                       // then rate current state and return it
                (node, space.RateState node) |> Some
            else                                       // else recursivly find the best child
                let rec findBestChild (best : ('a*float) option) cs =
                    match cs with
                    | []     -> best
                    | c::cs' -> let minV = match best with
                                              | Some (_, minV) -> minV
                                              | None           -> System.Double.MaxValue
                                // recursive decent into the other layer-type
                                let recur = findMax (d+1) (minV, beta) c
                                // if there were no other children, then rate the current node
                                let cVal = match recur with
                                           | Some (_, v)       -> v
                                           | None              -> space.RateState c
                                // this is the alpha-beta-pruning part - if this is allready worse than
                                // what was found in layer above we can stop here
                                if cVal <= beta  then Some (c, cVal)
                                // else check if this child is better than the current-best one
                                elif cVal < minV then findBestChild (Some (c, cVal)) cs'
                                else                  findBestChild best cs'
                findBestChild None children

        /// the method used for a max-layer - similar to findMin
        and findMax d (alpha, beta) (node : 'a) =
            let children = space.CreateChildStates node
            if List.isEmpty children then None
            elif d = depth
            then
                (node, space.RateState node) |> Some
            else
                let rec findBestChild (best : ('a*float) option) cs =
                    match cs with
                    | []     -> best
                    | c::cs' -> let maxV = match best with
                                              | Some (_, v)    -> v
                                              | None           -> System.Double.MinValue
                                let recur = findMin (d+1) (alpha, maxV) c
                                let cVal = match recur with
                                           | Some (_, v)       -> v
                                           | None              -> space.RateState c
                                if cVal >= alpha then Some (c, cVal)
                                elif cVal > maxV then findBestChild (Some (c, cVal)) cs'
                                else                  findBestChild best cs'
                findBestChild None children

        // start the algorithm with the right layertype
        match lrt with
        | LayerRateType.Max -> findMax 0 (System.Double.MaxValue, System.Double.MinValue) root
        | LayerRateType.Min -> findMin 0 (System.Double.MaxValue, System.Double.MinValue) root

    /// the public search-function - see definition in AlgorithmDefinitions.fs
    let Search : Search<_> = (fun (searchDepth, space) -> searchMinMax searchDepth space)