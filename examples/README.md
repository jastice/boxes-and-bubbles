# Boxes and Bubbles Example

Go ahead and start the example in `elm-reactor`!

## Forces
Play around with the force functions. By default, the example uses a constant gravity function:

    update: Msg -> Model meta -> Model meta
    update (Tick dt) bodies = uncurry step (constgravity dt) bodies

you may choose one of the others:

    -- different force functions to experiment with
    constgravity t = ((0,-0.2), (0,0)) -- constant downward gravity
    sinforce t = ((sin <| radians (t/1000)) * 50, 0) -- sinusoidal sideways force
    counterforces t = ((0,-0.01), (0, t/1000)) -- small gravity, slowly accellerating upward drift

or make your own!
